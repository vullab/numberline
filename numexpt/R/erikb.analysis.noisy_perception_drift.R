
setwd("/Users/erikbrockbank/web/vullab/numberline/numexpt/R/")
rm(list=ls())

# Fetch relevant model and participant data from noisy_perception model
source('erikb.analysis.noisy_perception.R')

### README ###
#' This file takes the output from the model in noisy_perception.R and calculates the drift
#' in the model's estimates (as well as the sample participant estimates) for easy
#' analysis of model drift under various parameters and comparison between model and 
#' participant calibration drift.
#' 
#' Note many of the supporting functions are copied or adapted from:
#' numberline/numexpt/R/fit.block-models.2014-06-02.R
#' numberline/numexpt/R/fit.models.2014-06-02.R
#'  



###############
### GLOBALS ###
###############

BLOCKSIZE = 30 # number of trials per block

# Initialize priors
PRIORS = list()
PRIORS[[1]] = function(x) {-dnorm(x, 1.14, 0.1, log = T)} # TODO are these set arbitrarily?
PRIORS[[2]] = function(x) {-dnorm(x, -0.1, 0.25, log = T)}
PRIORS[[3]] = function(x) {-dnorm(x, -1, 0.05, log = T)}

# Initialize params (mean a, var a, mean b, var b, mean s, var s)
PARAMS = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2) # TODO are these set arbitrarily?
names(PARAMS) = c("ma", "sa", "mb", "sb", "ms", "ss")

SUBJ_DATA = data # use processed participant data from erikb.analysis.noisy_perception.R 
MODEL_DATA = data %>%
  select(subject, trial, num_dots, model.answer) %>%
  mutate(answer = model.answer) # align column names to match participant data
  


#################
### FUNCTIONS ###
#################

# bi-linear power-law mapping
map.bipower = function(x, a, b) {
  crit = a
  slope = 10 ^ b
  lx = log10(x)
  ly = ((lx > crit) * (crit + (lx - crit) * slope) + (lx <= crit) * lx);
  return(10 ^ ly)
}

# general log likelihood function (with robustness)
loglik = function(x, y, map.fx, a, b, s) {
  sum(
    pmax(-6, dnorm(log10(y) - log10(map.fx(x, a, b)), 0, s, log = T))
  )
}

# Compute best fitting params for estimate data
brutefit = function(tmp) {
  nLL = function(a, b, s) {
    -loglik(tmp$num_dots, tmp$answer, map.bipower, a, b, 10 ^ s) + PRIORS[[1]](a) + PRIORS[[2]](b) + PRIORS[[3]](s)
  }
  
  iter = 0
  fits = NULL
  fit = NULL
  while (is.null(fits)) {
    try(fit <- summary(mle(nLL, # NB: this needs to be assigned with `<-` for some reason
                           start = list(a = runif(1, PARAMS["ma"], PARAMS["sa"]), 
                                      b = runif(1, PARAMS["mb"], PARAMS["sb"]), 
                                      s = rnorm(1, PARAMS["ms"], PARAMS["ss"])))), TRUE)
    iter = iter + 1
    
    if (!is.null(fit)) {
      fits = c(tmp$subject[1], -0.5 * fit@m2logL, length(tmp$num_dots), fit@coef[,"Estimate"])
    } else {
      if (iter > 50) {
        fits = c(tmp$subject[1], -9999, 0, 0, 0, 0)
      }      
    }
  }
  names(fits) = c("subject", "logL", "n", "a", "b", "s")
  
  return(fits)
}

# Util function used when fitting slopes: splits trial data into blocks
splitBlock = function(trial, n, total.trials) {
  floor((trial - 1) / (total.trials / n))
}

# Util function used when calculating correlation matrix
namedSlopes = function(x) {
  z = data.frame(x$b)
  rownames(z) = x$subject
  return(z)
}

# Util function used when calculating correlation matrix
cbind.fill = function(...) {
  nm = list(...) 
  rnames = unique(unlist(lapply(nm, rownames)))
  nm = lapply(nm, function(x) {
    newrows = rnames[!rnames %in% rownames(x)]
    newentries = matrix(nrow = length(newrows), ncol = ncol(x))
    rownames(newentries) = newrows
    colnames(newentries) = colnames(x)
    x = rbind(x, newentries)
    return(x)
  })
  nm = lapply(nm, function(x) {
    y = data.frame(x[order(as.numeric(rownames(x))),])
    rownames(y) = as.character(sort(as.numeric(rownames(x))))
    colnames(y) = colnames(x)
    return(y)
  })
  return(do.call(cbind, nm))
}

# Compute best fitting slopes for each participant x trial block of size `blocksizes` in `data`
fit.slopes = function(blocksizes, data) {
  n = blocksizes[1]
  fitsBlock = list()
  for (k in 1:n) {
    tmp = subset(data, splitBlock(data$trial, n, TRIALS) == (k - 1))
    fitsBlock[[k]] = data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
  }

  return(fitsBlock)
}

# Get m blocks by m blocks matrix of fitted slope correlations between each block
get.cor.matrix = function(slopes) {
  # Calculate slope correlations
  block.slopes = do.call(cbind.fill, lapply(slopes, namedSlopes)) # m subjects by n blocks slope values
  cor.matrix = cor(block.slopes, block.slopes, use = "pairwise.complete.obs") # n blocks by n blocks slope correlation matrix
  rownames(cor.matrix) = c() # NB: clearing out rownames and colnames is necessary for the processing below
  colnames(cor.matrix) = c()
  
  return(cor.matrix)
}

# Convert m blocks by m blocks matrix of fitted slope correlations to data frame
# Has column for each block and correlation between those blocks
# Initially has m^2 rows for each pair of blocks, then prunes out lower half
get.cor.df = function(cor.matrix) {
  slope.cor.df = reshape::melt(cor.matrix) # data frame with columns for block x, block y, and slope correlation b/n those blocks, n blocks x n blocks rows
  names(slope.cor.df) = c("block1", "block2", "slope.corr")
  slope.cor.df = slope.cor.df[slope.cor.df$block1 <= slope.cor.df$block2,] # remove redundant lower half of matrix
  slope.cor.df$slope.corr[slope.cor.df$block1 == slope.cor.df$block2] = NA # set correlation to NA in identical blocks
  
  return(slope.cor.df)
}

# Format data frame of correlations by block pair to show mean, se of correlations by block distance across blocks
get.distance.cors = function(cor.df) {
  dist.df = cor.df %>%
    mutate(block.dist = block2 - block1,
           trial.dist = 10 * block.dist) %>%
    group_by(trial.dist) %>%
    summarize(mean.cor = mean(slope.corr),
              se.cor = sd(slope.corr) / sqrt(length(slope.corr))) %>%
    filter(trial.dist > 0)
  
  return(dist.df)
}

individ_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 28),
  axis.title.y = element_text(face = "bold", size = 24),
  axis.title.x = element_text(face = "bold", size = 24),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 14),
  axis.text.x = element_text(size = 14, angle = 90, hjust = 0, vjust = 0),
  # legend text
  legend.text = element_text(size = 14),
  # facet text
  strip.text = element_text(face = "bold", size = 20),
  # backgrounds, lines
  panel.background = element_blank(),
  #strip.background = element_blank(),
  
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)


#######################
### DATA PROCESSING ###
#######################

### Subject Data ###
# Fit slopes, NB: this can take ~10s
fitsBlock.subj = fit.slopes(c(BLOCKSIZE), SUBJ_DATA)
# Get matrix of fitted slope correlations
cor.matrix.subj = get.cor.matrix(fitsBlock.subj)
# Format correlation matrix as data frame to plot slope correlations by trial block in analysis section below
slope.cor.df.subj = get.cor.df(cor.matrix.subj)
# Process slope correlations by trial distance to get mean, se across participants
cor.means.df.blocks.subj = get.distance.cors(slope.cor.df.subj)


### Model Data ###
# Fit slopes, NB: this can take ~10s
fitsBlock.model = fit.slopes(c(BLOCKSIZE), MODEL_DATA)
# Get matrix of fitted slope correlations
cor.matrix.model = get.cor.matrix(fitsBlock.model)
# Format correlation matrix as data frame to plot slope correlations by trial block in analysis section below
slope.cor.df.model = get.cor.df(cor.matrix.model)
# Process slope correlations by trial distance to get mean, se across participants
cor.means.df.blocks.model = get.distance.cors(slope.cor.df.model)


################
### ANALYSIS ###
################

# Plot participant/model estimates
MODEL_DATA %>%
# SUBJ_DATA %>%
  ggplot(aes(x = num_dots, y = answer)) +
  geom_point(alpha = 0.25, color = "red", size = 0.75) +
  geom_abline() +
  mylogx(c(MIN_ESTIMATE, MAX_ESTIMATE)) +
  mylogy(c(MIN_ESTIMATE, MAX_ESTIMATE + 200)) +
  xlab("number presented") +
  ylab("number reported") +
  ggtitle("Number estimates for each participant") +
  theme(axis.title = element_text(size = 16, face = "bold"),
        title = element_text(size = 18, face = "bold")) +
  facet_wrap(~subject, ncol = 6)


# Plot slope correlations by trial block
slope.cor.df.model %>%
# slope.cor.df.subj %>%
  ggplot(aes(x = as.factor(block1), y = as.factor(block2), fill = slope.corr)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "white", mid = "white", high = "red", midpoint = 0.3, limits = c(-0.5, 1)) +
  xlab("") + ylab("") +
  ggtitle("Trial block slope correlations") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(angle = 90),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        title = element_text(size = 18, face = "bold"),
        panel.grid = element_blank())


# Plot slope correlations by trial distance for model and human subjects
ggplot() +
  # model data
  geom_point(data = cor.means.df.blocks.model,
             aes(x = trial.dist,
                 y = mean.cor,
                 color = "model")) +
  geom_errorbar(data = cor.means.df.blocks.model,
                aes(x = trial.dist,
                    ymin = mean.cor - se.cor,
                    ymax = mean.cor + se.cor,
                    color = "model"),
                width = 5) +
  # subject data
  geom_point(data = cor.means.df.blocks.subj,
             aes(x = trial.dist,
                 y = mean.cor,
                 color = "subjects")) +
  geom_errorbar(data = cor.means.df.blocks.subj,
                aes(x = trial.dist,
                    ymin = mean.cor - se.cor,
                    ymax = mean.cor + se.cor,
                    color = "subjects"),
                width = 5) +
  #ylim(-0.25, 1) +
  labs(x = "Distance (trials)", y = "Correlation") +
  ggtitle("Drift in slope correlation") +
  scale_color_manual(name = "Drift", 
                     values = c("model" = "red", "subjects" = "blue")) +
  individ_plot_theme


### UNDERESTIMATION ANALYSIS WITH FITTING SLOPES ###
### Fit slopes to full data rather than blocks, compare humans and model ###
PARAMS = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2)
names(PARAMS) = c("ma", "sa", "mb", "sb", "ms", "ss")

PRIORS = list()
PRIORS[[1]] = function(x){-dnorm(x, 2, 3.5, log = T)}
PRIORS[[2]] = function(x){-dnorm(x, 0, 0.5, log = T)}
PRIORS[[3]] = function(x){-dnorm(x, -1, 0.25, log = T)}

# Fit subject data
bipower.fits.subj = data.frame(do.call(rbind, by(SUBJ_DATA, SUBJ_DATA$subject, brutefit)))
print(paste("Failed bipower fits:", sum(bipower.fits.subj$logL == -9999)))

# Fit model data
bipower.fits.mod = data.frame(do.call(rbind, by(MODEL_DATA, MODEL_DATA$subject, brutefit)))
print(paste("Failed bipower fits:", sum(bipower.fits.mod$logL == -9999)))

predictions = data.frame()
for (s in unique(SUBJ_DATA$subject)){
  stims = seq(1, 300, by = 1)
  # subject params
  biparams.subj = bipower.fits.subj[bipower.fits.subj$subject == s,]
  bipred.subj = (map.bipower(stims, biparams.subj$a, biparams.subj$b))
  # model params
  biparams.mod = bipower.fits.mod[bipower.fits.mod$subject == s,]
  bipred.mod = (map.bipower(stims, biparams.mod$a, biparams.mod$b))
  predictions = rbind(predictions, 
                      data.frame(subject = s, 
                                 num_dots = stims,
                                 bipred.subj = bipred.subj,
                                 bipred.mod = bipred.mod))
}

sample.subjects = c(6, 11, 22)
sdat = subset(SUBJ_DATA, SUBJ_DATA$subject %in% sample.subjects)
mod.sdat = subset(MODEL_DATA, MODEL_DATA$subject %in% sample.subjects)
spredictions = subset(predictions, predictions$subject %in% sample.subjects)


ggplot(sdat, aes(x = num_dots, y = answer)) +
  geom_point(aes(color = "subject"), alpha = 0.25, size = 2) +
  geom_point(data = mod.sdat, aes(x = num_dots, y = answer, color = "model"), alpha = 0.25, size = 2) +
  geom_line(data = spredictions, aes(x = num_dots, y = bipred.subj, color = "subject"), size = 2) +
  geom_line(data = spredictions, aes(x = num_dots, y = bipred.mod, color = "model"), size = 2) +
  geom_abline() +
  mylogx(c(1, 300)) +
  mylogy(c(1, 300)) +
  ggtitle("Sample estimates") +
  xlab("Number presented") +
  ylab("Number reported") +
  scale_color_manual(name = "Estimates", 
                     values = c("subject" = "blue", "model" = "red")) +
  individ_plot_theme +
  facet_wrap(~subject, ncol = 3)






