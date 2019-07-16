setwd("/Users/erikbrockbank/web/vullab/numberline/erikb-2018/")
rm(list=ls())

# Fetch relevant model and participant data from noisy_perception model
source('samples_model.R')

### README ###
#' This file takes the output from the model in noisy_perception.R and calculates the drift
#' in the model's estimates (as well as the sample participant estimates) for easy
#' analysis of model drift under various parameters and comparison between model and 
#' participant calibration drift.
#' 
#' Note many of the supporting functions are copied or adapted from:
#' `numberline/numexpt/R/fit.block-models.2014-06-02.R`
#' `numberline/numexpt/R/fit.models.2014-06-02.R`
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

SUBJ_DATA = data # use processed participant data from `samples_model.R`
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
  # slope = b
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
# NB: keep dplyr:: function designations to ensure that summarize is based on grouping
# See https://stackoverflow.com/questions/26923862/why-are-my-dplyr-group-by-summarize-not-working-properly-name-collision-with
get.distance.cors = function(cor.df) {
  dist.df = cor.df %>%
    dplyr::mutate(block.dist = block2 - block1,
           trial.dist = 10 * block.dist) %>%
    dplyr::group_by(trial.dist) %>%
    dplyr::summarize(mean.cor = mean(slope.corr, na.rm = TRUE),
              se.cor = sd(slope.corr, na.rm = TRUE) / sqrt(length(slope.corr)))

  return(dist.df[-1,]) # remove first row, will have NaN as mean.cor and NA as se.cor
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

# plot theme from perceptual_analysis.R
individ_plot_theme_v2 = theme(
  # titles
  plot.title = element_text(face = "bold", size = 32),
  axis.title.y = element_text(face = "bold", size = 32),
  axis.title.x = element_text(face = "bold", size = 32),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 20),
  axis.text.x = element_text(size = 20, hjust = 1), #, angle = 60, hjust = 0, vjust = 0.1
  # legend text
  legend.text = element_text(size = 24),
  # facet text
  strip.text = element_text(face = "bold", size = 28),
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


### STATIC Model Data ###
# data.static = run.model.baseline(data.path = DATA,
#                                  n.trials = TRIALS,
#                                  n.samples = N_SAMPLES, 
#                                  p.bumper = P_BUMPER, 
#                                  min.est = MIN_ESTIMATE, 
#                                  max.est = MAX_ESTIMATE,
#                                  perceived.mag.sd = PERCEIVED_DOTS_NOISE_SD,
#                                  est.fxn = ESTIMATE_FXN,
#                                  trials.exp = TRIALS_EXP,
#                                  bumper.exp = BUMPER_EXP,
#                                  prior.exp = PRIOR_EXP,
#                                  post.exp = POSTERIOR_SAMPLE_EXP)
# 
# MODEL_DATA_STATIC = data.static %>%
#   select(subject, trial, num_dots, model.answer) %>%
#   mutate(answer = model.answer) # align column names to match participant data

# Fit slopes, NB: this can take ~10s
# fitsBlock.model.static = fit.slopes(c(BLOCKSIZE), MODEL_DATA_STATIC)
# # Get matrix of fitted slope correlations
# cor.matrix.model.static = get.cor.matrix(fitsBlock.model.static)
# # Format correlation matrix as data frame to plot slope correlations by trial block in analysis section below
# slope.cor.df.model.static = get.cor.df(cor.matrix.model.static)
# # Process slope correlations by trial distance to get mean, se across participants
# cor.means.df.blocks.model.static = get.distance.cors(slope.cor.df.model.static)



################
### ANALYSIS ###
################

# Plot participant/model estimates
# MODEL_DATA %>%
# # SUBJ_DATA %>%
# # MODEL_DATA_STATIC %>%
#   ggplot(aes(x = num_dots, y = answer)) +
#   geom_point(alpha = 0.25, color = "red", size = 0.75) +
#   geom_abline() +
#   mylogx(c(MIN_ESTIMATE, MAX_ESTIMATE)) +
#   mylogy(c(MIN_ESTIMATE, MAX_ESTIMATE + 200)) +
#   xlab("number presented") +
#   ylab("number reported") +
#   ggtitle("Number estimates for each participant") +
#   theme(axis.title = element_text(size = 16, face = "bold"),
#         title = element_text(size = 18, face = "bold")) +
#   facet_wrap(~subject, ncol = 6)


# Plot slope correlations by trial block
# slope.cor.df.model %>%
# # slope.cor.df.subj %>%
#   ggplot(aes(x = as.factor(block1), y = as.factor(block2), fill = slope.corr)) +
#   geom_tile() +
#   scale_fill_gradient2(low = "white", mid = "white", high = "red", midpoint = 0.3, limits = c(-0.5, 1)) +
#   xlab("") + ylab("") +
#   ggtitle("Trial block slope correlations") +
#   scale_x_discrete(expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0)) +
#   theme(axis.ticks = element_blank(),
#         axis.text = element_text(size = 16, face = "bold"),
#         axis.text.y = element_text(angle = 90),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 12, face = "bold"),
#         title = element_text(size = 18, face = "bold"),
#         panel.grid = element_blank())
# 

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
  # static model data
  # geom_point(data = cor.means.df.blocks.model.static,
  #            aes(x = trial.dist,
  #                y = mean.cor,
  #                color = "model.static")) +
  # geom_errorbar(data = cor.means.df.blocks.model.static,
  #               aes(x = trial.dist,
  #                   ymin = mean.cor - se.cor,
  #                   ymax = mean.cor + se.cor,
  #                   color = "model.static"),
  #               width = 5) +
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
  xlim(0, 255) + # NB: Tweak this as needed
  ylim(-0.2, 1) + # NB: Tweak this as needed
  labs(x = "Distance (trials)", y = "Slope correlation") +
  ggtitle("Drift in estimate calibration") +
  scale_color_manual(name = element_blank(), 
                     labels = c("model" = "model", "model.static" = "model (baseline)", "subjects" = "subjects"),
                     values = c("model" = "red", "model.static" = "darkseagreen", "subjects" = "blue")) +
  individ_plot_theme_v2 +
  theme(legend.position = c(0.2, 0.1),
        legend.background = element_rect(color = "gray80", size = 0.5, linetype = "solid"))

save(cor.means.df.blocks.model, cor.means.df.blocks.subj, file = 'drift.RData')


### UNDERESTIMATION / BILINEAR FIT ANALYSIS ###
### Fit slopes to full data rather than blocks, compare humans and model ###
# NB: this requires different parameters in `samples_model.R` than those used for the drift analysis above
# -> re-run `samples_model.R` with good-fitting P_BUMPER = 0.6

data.static.under = run.model.baseline(data.path = DATA,
                                       n.trials = TRIALS, 
                                       n.samples = N_SAMPLES, 
                                       p.bumper = 0.6, 
                                       min.est = MIN_ESTIMATE, 
                                       max.est = MAX_ESTIMATE, 
                                       perceived.mag.sd = PERCEIVED_DOTS_NOISE_SD,
                                       est.fxn = ESTIMATE_FXN, 
                                       trials.exp = TRIALS_EXP, 
                                       bumper.exp = BUMPER_EXP,
                                       prior.exp = PRIOR_EXP, 
                                       post.exp = POSTERIOR_SAMPLE_EXP)
SUBJ_DATA_STATIC = data.static.under

MODEL_DATA_STATIC_UNDER = data.static.under %>%
  select(subject, trial, num_dots, model.answer) %>%
  mutate(answer = model.answer) # align column names to match participant data


PARAMS = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2)
names(PARAMS) = c("ma", "sa", "mb", "sb", "ms", "ss")

PRIORS = list()
PRIORS[[1]] = function(x){-dnorm(x, 2, 3.5, log = T)}
PRIORS[[2]] = function(x){-dnorm(x, 0, 0.5, log = T)}
PRIORS[[3]] = function(x){-dnorm(x, -1, 0.25, log = T)}

# Fit subject data
bipower.fits.subj = data.frame(do.call(rbind, by(SUBJ_DATA_STATIC, SUBJ_DATA_STATIC$subject, brutefit)))
print(paste("Failed bipower fits:", sum(bipower.fits.subj$logL == -9999)))

# Fit static model data
bipower.fits.mod = data.frame(do.call(rbind, by(MODEL_DATA_STATIC_UNDER, MODEL_DATA_STATIC_UNDER$subject, brutefit)))
print(paste("Failed bipower fits:", sum(bipower.fits.mod$logL == -9999)))


predictions = data.frame()
for (s in unique(SUBJ_DATA$subject)){
  stims = seq(1, 300, by = 1)
  # subject params, static
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


### 1. Sample subjects plots: model and subject underestimation side by side ###


sample.subjects = c(6, 11, 22)
sdat = subset(SUBJ_DATA, SUBJ_DATA$subject %in% sample.subjects)
mod.sdat = subset(MODEL_DATA_STATIC_UNDER, MODEL_DATA_STATIC_UNDER$subject %in% sample.subjects)
spredictions = subset(predictions, predictions$subject %in% sample.subjects)

ggplot() +
  geom_point(data = sdat, aes(x = num_dots, y = answer, color = "subject"), alpha = 0.25, size = 2) +
  geom_point(data = mod.sdat, aes(x = num_dots, y = answer, color = "model"), alpha = 0.25, size = 2) +
  geom_line(data = spredictions, aes(x = num_dots, y = bipred.subj, color = "subject"), size = 2) +
  geom_line(data = spredictions, aes(x = num_dots, y = bipred.mod, color = "model"), size = 2) +
  geom_abline() +
  mylogx(c(1, 300)) +
  mylogy(c(1, 1000)) +
  ggtitle("Sample estimates") +
  xlab("Number presented") +
  ylab("Number reported") +
  scale_color_manual(name = "Estimates", 
                     values = c("subject" = "blue", "model" = "red")) +
  individ_plot_theme +
  facet_wrap(~subject, ncol = 3)


### 2. Plot bilinear fit histograms and scatter plots ###

bipower.fits.subj = bipower.fits.subj %>%
  mutate(cutoff.trans = 10^a,
         slope.trans = 10^b,
         slope.error.trans = 10^s)

bipower.fits.mod = bipower.fits.mod %>%
  mutate(cutoff.trans = 10^a,
         slope.trans = 10^b,
         slope.error.trans = 10^s)
bipower.fits.mod.summary = bipower.fits.mod %>%
  summarize(mean.cutoff = mean(cutoff.trans),
            mean.slope = mean(slope.trans),
            se.cutoff = sd(cutoff.trans) / sqrt(length(cutoff.trans)),
            se.slope = sd(slope.trans) / sqrt(length(slope.trans)))
  

### Bilinear fit comparison scatterplot (subjects and static model)
ggplot() +
  geom_point(data = bipower.fits.subj, aes(x = cutoff.trans, y = slope.trans, color = "subjects"),
             size = 3) +
  geom_errorbar(data = bipower.fits.subj, aes(x = cutoff.trans, color = "subjects",
                                              ymin = slope.trans - slope.error.trans, 
                                              ymax = slope.trans + slope.error.trans),
                width = 0) +
  geom_point(data = bipower.fits.mod.summary, aes(x = mean.cutoff, y = mean.slope, color = "model"),
             size = 3) +
  geom_errorbar(data = bipower.fits.mod.summary, aes(x = mean.cutoff, color = "model",
                                                     ymin = mean.slope - se.slope,
                                                     ymax = mean.slope + se.slope),
                width = 0.5) +
  geom_errorbarh(data = bipower.fits.mod.summary, aes(y = mean.slope, color = "model",
                                                     xmin = mean.cutoff - se.cutoff,
                                                     xmax = mean.cutoff + se.cutoff),
                 height = 0.05) +
  geom_hline(yintercept = 1.0, linetype = "dashed") +
  scale_color_manual(name = "Data source",
                     values = c("subjects" = "red", "model" = "blue"),
                     labels = c("subjects" = "subjects", "model" = "static model")) +
  ylim(0, 1.5) +
  xlim(0, 30) +
  ggtitle("Model bilinear fit is consistent with subjects") +
  labs(x = "Fitted bilinear cutoff", y = "Fitted slope above cutoff") +
  individ_plot_theme


### Bilinear fit comparison histograms
slopes.plot = bipower.fits.subj %>%
  ggplot(aes(x = slope.trans)) +
  geom_histogram(binwidth = 0.05, fill = I("grey"), col = I("black")) +
  geom_vline(data = bipower.fits.mod.summary, aes(xintercept = mean.slope), color = "red", linetype = "dashed") +
  geom_vline(xintercept = 1.0, color = "black", linetype = "dashed") +
  labs(x = "Fitted slopes (model in red)") +
  ggtitle("Model underestimation is similar to subjects") +
  individ_plot_theme +
  theme(axis.title = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 18))

cutoff.plot = bipower.fits.subj %>%
  ggplot(aes(x = cutoff.trans)) +
  geom_histogram(binwidth = 3, fill = I("grey"), col = I("black")) +
  geom_vline(data = bipower.fits.mod.summary, aes(xintercept = mean.cutoff), color = "red", linetype = "dashed") +
  labs(x = "Fitted cutoffs (model in red)") +
  ggtitle("Model cutoff is similar to subjects") +
  individ_plot_theme +
  theme(axis.title = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 18))

multiplot(cutoff.plot, slopes.plot, cols = 2)


### 3. Plot overall model underestimation ###
# Fit slopes for model estimates in aggregate
MODEL_DATA_STATIC_UNDER$subject = 1 # set one subject for all data points
fitsOverall.mod = data.frame(do.call(rbind, by(MODEL_DATA_STATIC_UNDER, MODEL_DATA_STATIC_UNDER$subject, brutefit)))
print(sum(fitsOverall.mod$logL == -9999))

# Fit slopes for subject estimates in aggregate
SUBJ_DATA$subject = 1 # set one subject for all data points
fitsOverall.subj = data.frame(do.call(rbind, by(SUBJ_DATA, SUBJ_DATA$subject, brutefit)))
print(sum(fitsOverall.subj$logL == -9999))


# NB: IMPORTANT the calls to map.bipower below require modifying map.bipower to take in a transformed b value and not compute log transform in the function
# TODO fix this more cleanly (e.g. add s to map.bipower with default of NULL, when non-null compute b+/-s), or do all transforms in call to map.bipower
true_vals = 1:MAX_ESTIMATE
predictions = data.frame('num_dots' = true_vals,
                         'prediction.mod' = map.bipower(true_vals, fitsOverall.mod$a, 10^fitsOverall.mod$b),
                         'prediction.subj' = map.bipower(true_vals, fitsOverall.subj$a, 10^fitsOverall.subj$b),
                         'prediction.ul.mod' = map.bipower(true_vals, fitsOverall.mod$a, 10^fitsOverall.mod$b + 10^fitsOverall.mod$s),
                         'prediction.ll.mod' = map.bipower(true_vals, fitsOverall.mod$a, 10^fitsOverall.mod$b - 10^fitsOverall.mod$s),
                         'prediction.ul.subj' = map.bipower(true_vals, fitsOverall.subj$a, 10^fitsOverall.subj$b + 10^fitsOverall.subj$s),
                         'prediction.ll.subj' = map.bipower(true_vals, fitsOverall.subj$a, 10^fitsOverall.subj$b - 10^fitsOverall.subj$s))

ggplot() +
  geom_point(data = MODEL_DATA_STATIC_UNDER, aes(x = num_dots, y = answer, color = "model"), size = 2, alpha = 0.05) +
  geom_point(data = SUBJ_DATA, aes(x = num_dots, y = answer, color = "subjects"), size = 2, alpha = 0.05) +
  geom_line(data = predictions, aes(x = num_dots, y = prediction.subj, color = "subjects"), size = 2, alpha = 0.85) +
  geom_line(data = predictions, aes(x = num_dots, y = prediction.mod, color = "model"), size = 2, alpha = 0.85) +
  geom_ribbon(data = predictions, mapping = aes(x = num_dots, ymin = prediction.ll.mod, ymax = prediction.ul.mod), inherit.aes = FALSE, alpha = 0.2) +
  geom_ribbon(data = predictions, mapping = aes(x = num_dots, ymin = prediction.ll.subj, ymax = prediction.ul.subj), inherit.aes = FALSE, alpha = 0.2) +
  geom_abline(position = "identity") +
  mylogx(c(1, MAX_ESTIMATE)) +
  mylogy(c(1, MAX_ESTIMATE)) +
  xlab("Number presented") + 
  ylab("Number reported") + 
  #ggtitle("Model and human underestimation") +
  scale_color_manual(name = element_blank(),
                     values = c("subjects" = "red", "model" = "blue"),
                     labels = c("subjects" = "subjects", "model" = "model")) +
  individ_plot_theme_v2 +
  theme(legend.position = c(0.8, 0.26),
        legend.background = element_rect(color = "gray80", size = 0.5, linetype = "solid"))





### INDIVIDUAL VARIABILITY ANALYSIS ###

data.model.high.var = run.model.individ.memories(data.path = DATA,
                                                 n.trials = TRIALS,
                                                 n.samples = 20,
                                                 p.bumper = 0.6,
                                                 min.est = MIN_ESTIMATE,
                                                 max.est = MAX_ESTIMATE,
                                                 perceived.mag.sd = PERCEIVED_DOTS_NOISE_SD,
                                                 est.fxn = ESTIMATE_FXN,
                                                 trials.exp = TRIALS_EXP,
                                                 bumper.exp = BUMPER_EXP,
                                                 prior.exp = PRIOR_EXP,
                                                 post.exp = POSTERIOR_SAMPLE_EXP,
                                                 n.memories = 50)

data.model.low.var = run.model.individ.memories(data.path = DATA,
                                                 n.trials = TRIALS,
                                                 n.samples = 20,
                                                 p.bumper = 0.6,
                                                 min.est = MIN_ESTIMATE,
                                                 max.est = MAX_ESTIMATE,
                                                 perceived.mag.sd = PERCEIVED_DOTS_NOISE_SD,
                                                 est.fxn = ESTIMATE_FXN,
                                                 trials.exp = TRIALS_EXP,
                                                 bumper.exp = BUMPER_EXP,
                                                 prior.exp = PRIOR_EXP,
                                                 post.exp = POSTERIOR_SAMPLE_EXP,
                                                 n.memories = 500)

SUBJ_DATA_VAR = data.model.high.var # can use either here

MODEL_DATA_HIGH_VAR = data.model.high.var %>%
  select(subject, trial, num_dots, model.answer) %>%
  mutate(answer = model.answer) # align column names to match participant data

MODEL_DATA_LOW_VAR = data.model.low.var %>%
  select(subject, trial, num_dots, model.answer) %>%
  mutate(answer = model.answer) # align column names to match participant data


PARAMS = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2)
names(PARAMS) = c("ma", "sa", "mb", "sb", "ms", "ss")

PRIORS = list()
PRIORS[[1]] = function(x){-dnorm(x, 2, 3.5, log = T)}
PRIORS[[2]] = function(x){-dnorm(x, 0, 0.5, log = T)}
PRIORS[[3]] = function(x){-dnorm(x, -1, 0.25, log = T)}

# Fit static subject data
bipower.fits.subj = data.frame(do.call(rbind, by(SUBJ_DATA_VAR, SUBJ_DATA_VAR$subject, brutefit)))
print(paste("Failed bipower fits:", sum(bipower.fits.subj$logL == -9999)))

# Fit model data, high variability
bipower.fits.mod.high = data.frame(do.call(rbind, by(MODEL_DATA_HIGH_VAR, MODEL_DATA_HIGH_VAR$subject, brutefit)))
print(paste("Failed bipower fits:", sum(bipower.fits.mod.high$logL == -9999)))

# Fit model data, low variability
bipower.fits.mod.low = data.frame(do.call(rbind, by(MODEL_DATA_LOW_VAR, MODEL_DATA_LOW_VAR$subject, brutefit)))
print(paste("Failed bipower fits:", sum(bipower.fits.mod.low$logL == -9999)))


predictions = data.frame()
for (s in unique(SUBJ_DATA_VAR$subject)){
  stims = seq(1, 300, by = 1)
  # subject params, static
  biparams.subj = bipower.fits.subj[bipower.fits.subj$subject == s,]
  bipred.subj = (map.bipower(stims, biparams.subj$a, biparams.subj$b))
  # model params, high var
  biparams.mod.high = bipower.fits.mod.high[bipower.fits.mod.high$subject == s,]
  bipred.mod.high = (map.bipower(stims, biparams.mod.high$a, biparams.mod.high$b))
  # model params, low variability
  biparams.mod.low = bipower.fits.mod.low[bipower.fits.mod.low$subject == s,]
  bipred.mod.low = (map.bipower(stims, biparams.mod.low$a, biparams.mod.low$b))
  predictions = rbind(predictions, 
                      data.frame(subject = s, 
                                 num_dots = stims,
                                 bipred.subj = bipred.subj,
                                 bipred.mod.high = bipred.mod.high,
                                 bipred.mod.low = bipred.mod.low))
}

bipower.fits.subj = bipower.fits.subj %>%
  mutate(cutoff.trans = 10^a,
         slope.trans = 10^b,
         slope.error.trans = 10^s)

bipower.fits.mod.high = bipower.fits.mod.high %>%
  mutate(cutoff.trans = 10^a,
         slope.trans = 10^b,
         slope.error.trans = 10^s)

bipower.fits.mod.low = bipower.fits.mod.low %>%
  mutate(cutoff.trans = 10^a,
         slope.trans = 10^b,
         slope.error.trans = 10^s)


### Bilinear fit comparison scatterplot, variable model: place both these plots alongside each other
# Low variability model (replace model data below with actual low var model data)
ggplot() +
  geom_point(data = bipower.fits.subj, aes(x = cutoff.trans, y = slope.trans, color = "subjects"),
             size = 3) +
  geom_errorbar(data = bipower.fits.subj, aes(x = cutoff.trans, color = "subjects",
                                              ymin = slope.trans - slope.error.trans, 
                                              ymax = slope.trans + slope.error.trans),
                width = 0) +
  geom_point(data = bipower.fits.mod.low, aes(x = cutoff.trans, y = slope.trans, color = "model"),
             size = 3) +
  geom_errorbar(data = bipower.fits.mod.low, aes(x = cutoff.trans,
                                             ymin = slope.trans - slope.error.trans, 
                                             ymax = slope.trans + slope.error.trans,
                                             color = "model"),
                width = 0) +
  geom_hline(yintercept = 1.0, linetype = "dashed") +
  scale_color_manual(name = "Data source",
                     values = c("subjects" = "red", "model" = "blue"),
                     labels = c("subjects" = "subjects", "model" = "low variability model")) +
  ylim(0, 1.5) +
  xlim(0, 30) +
  ggtitle("Low variability model fit") +
  labs(x = "Fitted bilinear cutoff", y = "Fitted slope above cutoff") +
  individ_plot_theme

# High variability model (replace model data below with actual high var model data)
ggplot() +
  geom_point(data = bipower.fits.subj, aes(x = cutoff.trans, y = slope.trans, color = "subjects"),
             size = 3) +
  geom_errorbar(data = bipower.fits.subj, aes(x = cutoff.trans, color = "subjects",
                                              ymin = slope.trans - slope.error.trans, 
                                              ymax = slope.trans + slope.error.trans),
                width = 0) +
  geom_point(data = bipower.fits.mod.var, aes(x = cutoff.trans, y = slope.trans, color = "model"),
             size = 3) +
  geom_errorbar(data = bipower.fits.mod.var, aes(x = cutoff.trans,
                                                 ymin = slope.trans - slope.error.trans, 
                                                 ymax = slope.trans + slope.error.trans,
                                                 color = "model"),
                width = 0) +
  geom_hline(yintercept = 1.0, linetype = "dashed") +
  scale_color_manual(name = "Data source",
                     values = c("subjects" = "red", "model" = "blue"),
                     labels = c("subjects" = "subjects", "model" = "high variability model")) +
  ylim(0, 1.5) +
  xlim(0, 30) +
  ggtitle("High variability model fit") +
  labs(x = "Fitted bilinear cutoff", y = "Fitted slope above cutoff") +
  individ_plot_theme




