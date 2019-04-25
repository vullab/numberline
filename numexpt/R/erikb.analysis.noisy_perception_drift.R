
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

# TODO get this working for model estimates as well


###############
### GLOBALS ###
###############

BLOCKSIZE = 30

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
  # align column names to match participant data
  rename(answer = model.answer)

TARGET_DATA = MODEL_DATA # Toggle this variable to plot drift for subject or model data

#################
### FUNCTIONS ###
#################

# bi-linear power-law mapping
map.bipower = function(x, a, b) {
  crit = a
  slope = 10^b
  lx = log10(x)
  ly = ((lx > crit) * (crit + (lx - crit) * slope) + (lx <= crit) * lx);
  return(10^ly)
}

# general log likelihood function (with robustness)
loglik = function(x, y, map.fx, a, b, s) {
  sum(
    pmax(-6, dnorm(log10(y) - log10(map.fx(x, a, b)), 0, s, log = T))
  )
}


brutefit = function(tmp) {
  nLL = function(a, b, s) {
    -loglik(tmp$num_dots, tmp$answer, map.bipower, a, b, 10^s) + PRIORS[[1]](a) + PRIORS[[2]](b) + PRIORS[[3]](s)
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


splitBlock = function(trial, n, total.trials) {
  floor((trial - 1) / (total.trials / n))
}


namedSlopes = function(x) {
  z = data.frame(x$b)
  rownames(z) = x$subject
  return(z)
}


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


fit.slopes = function(blocksizes, data) {
  for (i in 1:length(blocksizes)) {
    n = blocksizes[i]
    fitsBlock = list()
    for (k in 1:n) {
      tmp = subset(data, splitBlock(data$trial, n, TRIALS) == (k - 1))
      fitsBlock[[k]] = data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
    }
  }
  return(fitsBlock)
}


get.distance.cors = function(cor.matrix) {
  # initialize list of slope correlations by distance
  cors.dist.blocks = list('dist' = numeric(), 'cors' = c())
  for (i in seq(1:(BLOCKSIZE - 1))) {
    cors.dist.blocks[[i]] = list('dist' = i, 'cors' = c())
  }
  
  # populate list of slope correlations by distance based on slope correlation matrix cor.matrix
  for (row in seq(from = 1, to = dim(cor.matrix)[1])) {
    for (col in seq(from = row, to = dim(cor.matrix)[2])) { # NB: this does funky stuff without 'from'
      dist.val = (col - row)
      cor.val = cor.matrix[row, col]
      if (dist.val > 0 & !is.na(cor.val)) {
        cors.dist.blocks[[dist.val]]$cors = c(cors.dist.blocks[[dist.val]]$cors, cor.val)
      }
    }
  }
  
  return(cors.dist.blocks)
}



#######################
### DATA PROCESSING ###
#######################

# Fit slopes, NB: this can take a minute or two
fitsBlock = fit.slopes(c(BLOCKSIZE), TARGET_DATA)

# Calculate slope correlations
block.slopes = do.call(cbind.fill, lapply(fitsBlock, namedSlopes)) # m subjects by n blocks slope values
cor.matrix = cor(block.slopes, block.slopes, use = "pairwise.complete.obs") # n blocks by n blocks slope correlation matrix
rownames(cor.matrix) = c() # NB: clearing out rownames and colnames is necessary for the processing below
colnames(cor.matrix) = c()


# Format cor.matrix as data frame to plot slope correlations by trial block in analysis section below
slope.cor.df = reshape::melt(cor.matrix) # data frame with columns for block x, block y, and slope correlation b/n those blocks, n blocks by n blocks rows
names(slope.cor.df) = c("block1", "block2", "slope.corr")
slope.cor.df = slope.cor.df[slope.cor.df$block1 <= slope.cor.df$block2,] # remove redundant lower half of matrix
slope.cor.df$slope.corr[slope.cor.df$block1 == slope.cor.df$block2] = NA # set correlation to NA in identical blocks


# Format cor.matrix to show slope correlations by trial distance
cors.dist.blocks = get.distance.cors(cor.matrix)

# Process slope correlations by trial distane to get mean, se across participants
cor.means.df.blocks = data.frame()
for (i in seq(1:length(cors.dist.blocks))) {
  dist = cors.dist.blocks[[i]]$dist * 10
  mean = mean(cors.dist.blocks[[i]]$cors)
  se = sd(cors.dist.blocks[[i]]$cors) / sqrt(length(cors.dist.blocks[[i]]$cors))
  row = data.frame('dist' = dist, 'mean.cor' = mean, 'se' = se)
  cor.means.df.blocks = rbind(cor.means.df.blocks, row)
}


################
### ANALYSIS ###
################

# Plot participant/model estimates
TARGET_DATA %>%
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
slope.cor.df %>%
  ggplot(aes(x = as.factor(block1), y = as.factor(block2), fill = slope.corr)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "white", mid = "white", high = "red", midpoint = 0.3, limits = c(-0.5, 1)) +
  #scale_fill_gradient2(low = "white", mid = "white", high = "red", midpoint = 0, limits = c(-0.5, 1)) +
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


# Plot slope correlations by trial distance
cor.means.df.blocks %>%
  ggplot(aes(x = dist, y = mean.cor)) +
  geom_point() +
  geom_errorbar(aes(x = dist, 
                    ymin = mean.cor - se, 
                    ymax = mean.cor + se),
                width = 5) +
  ylim(-0.5, 1) +
  labs(x = "avg trial distance", y = "correlation of slopes") +
  ggtitle("Drift in correlation of slopes at greater trial distances") +
  theme(panel.grid = element_blank(),
        title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"))






