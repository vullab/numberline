

### README ###
#' This file does a basic MCMC slope sampling for 24 "participants" over 300 trials each to show that the drift 
#' in slope correlations for participant data can be thought of (and modeled in a basic sense) as an MCMC-like sampling process.
#' 
#' This can be run all by itself and should produce all the graphs necessary for mcmc simulated data.
#' To get the equivalent participant data, clear the variable set and run `numberline/numexpt/R/fit.models.2014-06-02.R`. 
#' This will read in and fit the participant data (may be possible to just load the Rdata file it writes to).
#' Next run `numberline/numexpt/R/fit.block-models.2014-06-02.R` to get participant graphs that are equivalent to the ones produced here. 
#' Much of the code in this file is copied or adapted from the above two files.



setwd("/Users/erikbrockbank/web/vullab/numberline/erikb-2018/")
rm(list=ls())

library(stats4)
library(tidyverse)


#################
### READ DATA ###
#################

# Copied from fit.models.2014-06-02.R
data.path = "../numexpt/data/"
files <- list.files(data.path)
data <- data.frame()
subject <- 1
for(f in files){
  q = read.csv2(paste(data.path, f, sep = ""), sep = ",", header = T, colClasses = "character")
  q$subject = subject
  data <- rbind(data, q)
  subject <- subject + 1
}

glimpse(data)


####################
### PROCESS DATA ###
####################

# this also copied from fit.models.2014-06-02.R
to.num <- function(x){as.numeric(as.character(x))}

data$run <- to.num(data$run)
data$index <- to.num(data$index)
data$num_dots <- to.num(data$num_dots)
data$answer1 <- to.num(data$answer1) # participant's first guess answer
data$answer2 <- to.num(data$answer2) # participant's second guess answer
data$points1 <- to.num(data$points1) # participant's points earned for their first guess
data$points2 <- to.num(data$points2) # participant's points earned for their second guess
data$time <- to.num(data$time) # participant's response time
data$answer <- 10 ^ (log10(pmax(1, data$answer1)) / 2 + log10(pmax(1, data$answer2)) / 2) # blended average of participant answers for this array

glimpse(data)

#' Notes on data:
#' Each participant did two runs (`run`) of the same arrays: 
#' each time they were allowed to make two guesses about the number of dots in that array.
#' Their total points accumulated were shown periodically throughout the experiment but 
#' were not provided as direct feedback for any one guess


load('../numexpt/R/model.fits.Rdata') # Avoids having to re-fit model just for looking at data
#load('../num-density.2013-06/R/fit-block-models.Rdata') # Necessary for graph below
# Copying these here instead of loading the above
my.log.breaks = function(lims){
  majors = seq(floor(log10(lims[1])), ceiling(log10(lims[2])), by = 1)
  minors = log10(unlist(lapply(majors[-1], function(x){seq(10^(x - 1), 9 * 10^(x - 1), by = 10^(x - 1))})))
  return(list(majors, minors))
}
mylogx = function(lims){
  breaks = my.log.breaks(lims)
  scale_x_log10(limits = lims, 
                breaks = 10^breaks[[1]], 
                minor_breaks = breaks[[2]])
}
mylogy = function(lims){
  breaks <- my.log.breaks(lims)
  scale_y_log10(limits = lims, 
                breaks = 10^breaks[[1]], 
                minor_breaks = breaks[[2]])
}

ggplot(data, aes(x = num_dots, y = answer)) +
  geom_point(alpha = 0.25, color = "red") +
  #geom_line(data = predictions, aes(x = num_dots, y = bipred), color = rgb(0, 0.6, 0), size = 1) +
  geom_abline(position = "identity") +
  mylogx(c(1, 300)) +
  mylogy(c(1, 300)) +
  xlab("Number presented") +
  ylab("Number reported") +
  #ggtitle("Experiment data for 24 participants") +
  annotate("rect", xmin = 1, xmax = 5, ymin = 1, ymax = 300, fill = "black", alpha = 0.3) +
  facet_wrap(~subject, ncol = 6)


###################
### RUN MH-MCMC ###
###################

# globals
PROPOSAL_JITTER_MEAN = 0 # this is probably the only reasonable choice
PROPOSAL_JITTER_SD = 0.01 # this is tweakable

SLOPE_PRIOR_MEAN = 1 # used to score new proposals
SLOPE_PRIOR_SD = 1 # used to score proposals: changes here have somewhat dramatic effects, worth looking into

# Support functions
propose.params = function(params) {
  new.slope = params[['slope']] + rnorm(1, PROPOSAL_JITTER_MEAN, PROPOSAL_JITTER_SD)
  return(list('slope' = new.slope))
}

score.params = function(params) {
  prior.score = log(dnorm(params[['slope']], SLOPE_PRIOR_MEAN, SLOPE_PRIOR_SD))
  # no need to compute likelihood | data here, we just imagine that every slope is a new sample
  return(prior.score)
}

accept.proposal = function(curr.params.score, proposal.score) {
  cutoff = runif(1)
  mh.ratio = exp(curr.params.score - proposal.score)
  return(cutoff < mh.ratio)
}

run.mcmc = function(start.params, iterations) {
  chain = list(start.params)
  chain.scores = score.params(start.params)
  for (i in 2:iterations) {
    curr.params = chain[[i-1]]
    curr.score = chain.scores[i-1]
    proposal = propose.params(curr.params)
    proposal.score = score.params(proposal)
    if (accept.proposal(curr.score, proposal.score)) {
      chain[[i]] = proposal
      chain.scores[i] = proposal.score
    } else {
      chain[[i]] = curr.params
      chain.scores[i] = curr.score
    }
  }
  return(list(chain, chain.scores))
}

### Run MCMC ###

participants = 24
iterations = 300
start.params = list('slope' = SLOPE_PRIOR_MEAN) # conservative starting slope

# Validate MCMC for a single participant
mcmc.single = run.mcmc(start.params, iterations)
chain = mcmc.single[[1]]
chain.scores = mcmc.single[[2]]


# Run MCMC for all participants
mcmc.exp = list('subj' = numeric(), 'chain' = c(), 'scores' = c())
for (x in seq(1:participants)) {
  mcmc.participant = run.mcmc(start.params, iterations)
  mcmc.exp[[x]] = list('subj' = x, 'chain' = mcmc.participant[[1]], 'scores' = mcmc.participant[[2]])
}

# Validate MCMC for all participants
mcmc.exp[[1]]$subj # subject 1
chain = mcmc.exp[[1]]$chain # chain 1
chain.scores = mcmc.exp[[1]]$scores # scores 1


########################
### VISUALIZING MCMC ###
########################

tidy.params = function(l.params) {
  df.params = data.frame('slope' = NA)
  for (n in names(l.params)){
    df.params[[n]] = l.params[[n]]
  }
  return(df.params)
}

# Convert to data frame for each subject
exp.df = list('subj' = numeric(), 'mcmc.df' = data.frame())
for (x in seq(1:participants)) {
  chain = mcmc.exp[[x]]$chain
  chain.df = map_dfr(chain, tidy.params)
  
  chain.scores = mcmc.exp[[x]]$scores
  
  mcmc.df = data.frame('iter' = 1:iterations,
                       'slope' = chain.df$slope,
                       'score' = chain.scores)
  exp.df[[x]] = list('subj' = x, 'mcmc.df' = mcmc.df)
}


# Validate exp.df generated above
exp.df[[1]]$subj
#exp.df[[1]]$mcmc.df

# plot slopes for a sample subject (subj 1)
exp.df[[1]]$mcmc.df %>%
  ggplot(aes(x = iter, y = slope)) +
  geom_point() +
  labs(x = "trial (1-300)", y = "slope") +
  ggtitle("MCMC slope results, 300 trials")

# plot scores for a sample subject (subj 1)
exp.df[[1]]$mcmc.df %>%
  ggplot(aes(x = iter, y = score)) +
  geom_point() +
  labs(x = "trial (1-300)", y = "score")



#########################################
### VISUALIZING OVER ALL PARTICIPANTS ###
#########################################
# 1. get dataframe where each row is a participant, each col is the slope on a given iter
#   Using exp.df[[1]]$mcmc.df for participant 1, exp.df[[2]]$mcmc.df for participant 2, etc.
# 2. Auto-correlate this dataframe with pairwise obs to get a 300x300 matrix
#   where each cell is the pairwise correlation between each participant's slopes in that row,col trial
# 3. Iterate through this matrix (top half, i=1:n, j=i:n), calculate distance as j-i, get cor in that cell
#   Make list where each row is a distance and has a c() of corr vals,
#   then convert to dataframe with distance, mean, se

convert.exp.df = function(item) {
  item = item %>% select(iter, slope)
  item.row = item %>% spread(iter, slope)
  return(item.row)
}

# Generate matrix of participant x trial slope data
exp.df.rowwise = data.frame()
for (x in seq(1:participants)) {
  exp.df.row = convert.exp.df(exp.df[[x]]$mcmc.df)
  exp.df.rowwise = rbind(exp.df.rowwise, exp.df.row)
}

# Generate correlation matrix based on participant x slope matrix
exp.matrix.cors = cor(exp.df.rowwise, exp.df.rowwise, use = "pairwise.complete.obs")


# Validate correlation matrix exp.matrix.cors
cor(exp.df.rowwise[,2], exp.df.rowwise[,3], use = "pairwise.complete.obs")
exp.matrix.cors[2,3]


# Visualize correlation matrix exp.matrix.cors (taken from `fit.block-models.2014-06-02`)
mcor = reshape::melt(exp.matrix.cors)
names(mcor) = c("Var1", "Var2", "value")
mcor = mcor[mcor$Var1 <= mcor$Var2,]
mcor$value[mcor$Var1 == mcor$Var2] = NA
ggplot(mcor, aes(x = as.factor(Var1), y = as.factor(Var2), fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "white", mid = "white", high = "red", midpoint = 0.3, limits = c(-0.5, 1))+
  xlab("") + ylab("") +
  ggtitle("Trial block slope correlations for original model data") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  #mytheme + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        title = element_text(size = 12, face = "bold"))


# Convert trial correlation matrix to distance correlations
cors.dist = list('dist' = numeric(), 'cors' = c())
for (x in seq(1:(iterations - 1))) {
  cors.dist[[x]] = list('dist' = x, 'cors' = c())
}

for (row in seq(from = 1, to = dim(exp.matrix.cors)[1])) {
  for (col in seq(from = row, to = dim(exp.matrix.cors)[2])) { # NB: this does funky stuff without 'from'
    dist.val = col - row
    cor.val = exp.matrix.cors[row, col]
    if (dist.val > 0 & !is.na(cor.val)) {
      cors.dist[[dist.val]]$cors = c(cors.dist[[dist.val]]$cors, cor.val)
    }
  }
}

# Validate distance correlations
length(cors.dist[[1]]$cors)
cors.dist[[1]]$cors[1]
exp.matrix.cors[2,3]

# cleanup
cors.dist = cors.dist[-299]


# Get summary data frame for displaying distance correlations
cor.means.df = data.frame()
for (x in seq(1:length(cors.dist))) {
  dist = cors.dist[[x]]$dist
  mean = mean(cors.dist[[x]]$cors)
  se = sd(cors.dist[[x]]$cors) / sqrt(length(cors.dist[[x]]$cors))
  row = data.frame('dist' = dist, 'mean.cor' = mean, 'se' = se)
  cor.means.df = rbind(cor.means.df, row)
}

cor.means.df %>%
  ggplot(aes(x = dist, y = mean.cor)) +
  geom_point() +
  geom_errorbar(aes(x = dist, ymin = mean.cor - se, ymax = mean.cor + se)) +
  labs(x = "trial distance", y = "correlation of slopes (mean correlation, error bars = se)") +
  ggtitle("Drift in correlation of slopes at greater trial distances")



##################################
### RE-CREATE PARTICIPANT DATA ###
##################################
PERCEIVED_DOTS_NOISE_MEAN = 0
PERCEIVED_DOTS_NOISE_SD = 1

# Add true_index for each participant
data.model = data %>%
  select(subject, run, index, num_dots) %>% # Note including answers may not be relevant
  mutate(trial = ((run - 1) * max(index)) + index)

# Add noisy perceived_dots based on num_dots
# Add slope_est from MCMC process for each (participant, true_index) trial
# Add number_est using slope_est and perceived_dots
# NB: trying to do all of this in mutate call above causes strange recursion bottoming out errors for slope_est calculation
data.model = data.model %>%
  group_by(subject, trial) %>%
  mutate(perceived_dots = num_dots + rnorm(1, PERCEIVED_DOTS_NOISE_MEAN, PERCEIVED_DOTS_NOISE_SD), # random noise in perception of true magnitude
         slope_est = exp.df[[subject]]$mcmc.df$slope[exp.df[[subject]]$mcmc.df$iter == trial],
         number_est = slope_est * perceived_dots) # TODO what to do about non-integer perceived_dots and hence non-integer number_est?

# Visualize data in same manner as above
ggplot(data.model, aes(x = num_dots, y = number_est)) +
  geom_point(alpha = 0.25, color = "red") +
  geom_abline(position = "identity") +
  #mylogx(c(1, 300)) +
  #mylogy(c(1, 300)) +
  xlab("Number presented") +
  ylab("Number reported") +
  ggtitle("Model predictions for 24 (randomly generated) participants") +
  annotate("rect", xmin = 1, xmax = 5, ymin = 1, ymax = 300, fill = "black", alpha = 0.3) +
  facet_wrap(~subject, ncol = 6)
  
# TODO divide this data into blocks of similar size to fit.block-models.2014-06-02.R
# Run identical calculations to determine best fitting slope for each participant, block
# Using these slope calculations, run identical analysis to create:
# a.) block-block slope correlations
# b.) trial distance - slope correlation drift
# Compare these to the original data



BLOCKSIZE = 30

# Coerce data to match format from fit.block-models.2014-06-02.R
glimpse(data)
glimpse(data.model)

# Copied from fit.models.2014-06-02.R
# bi-linear power-law mapping
map.bipower <- function(x, a, b){
  crit <- a
  slope <- 10^b
  lx <- log10(x)
  ly <- ((lx > crit) * (crit + (lx - crit) * slope) + (lx <= crit) * lx);
  return(10^ly)
}

## general log likelihood function (with robustness)
loglik <- function(x, y, map.fx, a, b, s, p){
  sum(
    pmax(-6, dnorm(log10(y) - log10(map.fx(x, a, b)), 0, s, log = T))
  )
}

brutefit <- function(tmp){
  nLL <- function(a, b, s){
    -loglik(tmp$perceived_dots, tmp$number_est, usefx, a, b, 10^s) + priors[[1]](a) + priors[[2]](b) + priors[[3]](s)
  }
  
  iter = 0
  fits = NULL
  fit = NULL
  while(is.null(fits)){
    try(fit <- summary(mle(nLL, 
                           start = list(a = runif(1, ps["ma"], ps["sa"]), 
                                      b = runif(1, ps["mb"], ps["sb"]), 
                                      s = rnorm(1, ps["ms"], ps["ss"])))), TRUE)
    iter = iter + 1
    
    if(!is.null(fit)){
      fits <- c(tmp$subject[1], -0.5 * fit@m2logL, length(tmp$perceived_dots), fit@coef[,"Estimate"])
    } else {
      if(iter > 50){
        fits <- c(tmp$subject[1], -9999, 0, 0, 0, 0)
      }      
    }
  }
  names(fits) <- c("subject", "logL", "n", "a", "b", "s")
  return(fits)
}

# function call setup copied from fit.block-models.2014-06-06.R
usefx <- map.bipower
ps = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2)
names(ps) <- c("ma", "sa", "mb", "sb", "ms", "ss")
priors = list()
priors[[1]] <- function(x){-dnorm(x, 1.14, 0.1, log = T)} #
priors[[2]] <- function(x){-dnorm(x, -0.1, 0.25, log = T)} #
priors[[3]] <- function(x){-dnorm(x, -1, 0.05, log = T)} # 

splitBlock <- function(trial, n){floor((trial - 1) / (iterations / n))}

ns = c(BLOCKSIZE)
xcorBlock = list()
dcor = list()
for (i in 1:length(ns)){
  n = ns[i]
  fitsBlock = list()
  for (k in 1:n) {
    tmp <- subset(data.model, splitBlock(data.model$trial, n) == (k - 1))
    fitsBlock[[k]] <- data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
  }
}

# analysis copied from fits.block-models.2014-06-02.R
namedSlopes = function(x) {
  z = data.frame(x$b)
  rownames(z) = x$subject
  return(z)
}

cbind.fill = function(...){
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

s1 = do.call(cbind.fill, lapply(fitsBlock, namedSlopes))

# Look at the slopes that get fit by block
slopes = data.frame(s1)
slopes$subject = rownames(s1)
slopes = reshape::melt(slopes)
names(slopes) = c("subject", "block", "slope")
levels(slopes$block) = 1:ncol(s1)
slopes$block = as.numeric(as.character(slopes$block))
ggplot(slopes, aes(x = block, y = slope)) +
  stat_summary(fun.data = mean_se, 
               geom = "ribbon", 
               alpha = 0.5, colour = NA, fill = "red") +
  geom_point(size = 5, alpha = 0.4, color = "red") +
  geom_line(aes(group = subject), color = "red", size = 0.5, alpha = 0.25) +
  stat_summary(fun.y = mean, geom = "line", color = "red", size = 2) +
  scale_x_continuous(minor_breaks = c()) +
  scale_y_continuous(minor_breaks = c())
  #mytheme


R = cor(s1, s1, use = "pairwise.complete.obs")
rownames(R) = c()
colnames(R) = c()
mcor = reshape::melt(R)
names(mcor) = c("Var1", "Var2", "value")
mcor = mcor[mcor$Var1 <= mcor$Var2,]
mcor$value[mcor$Var1 == mcor$Var2] = NA
ggplot(mcor, aes(x = as.factor(Var1), y = as.factor(Var2), fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "white", mid = "white", high = "red", midpoint = 0.3, limits = c(-0.5, 1))+
  xlab("") + ylab("") +
  ggtitle("Trial block slope correlations for model data") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(angle = 90),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        title = element_text(size = 12, face = "bold"),
        panel.grid = element_blank())




cors.dist.blocks = list('dist' = numeric(), 'cors' = c())
for (x in seq(1:(BLOCKSIZE - 1))) {
  cors.dist.blocks[[x]] = list('dist' = x, 'cors' = c())
}

for (row in seq(from = 1, to = dim(R)[1])) {
  for (col in seq(from = row, to = dim(R)[2])) { # NB: this does funky stuff without 'from'
    dist.val = (col - row)
    cor.val = R[row, col]
    if (dist.val > 0 & !is.na(cor.val)) {
      cors.dist.blocks[[dist.val]]$cors = c(cors.dist.blocks[[dist.val]]$cors, cor.val)
    }
  }
}

# Validate distance correlations
length(cors.dist.blocks[[1]]$cors)
cors.dist.blocks[[1]]$cors[1]




# Get summary data frame for displaying distance correlations
cor.means.df.blocks = data.frame()
for (x in seq(1:length(cors.dist.blocks))) {
  dist = cors.dist.blocks[[x]]$dist * 10
  mean = mean(cors.dist.blocks[[x]]$cors)
  se = sd(cors.dist.blocks[[x]]$cors) / sqrt(length(cors.dist.blocks[[x]]$cors))
  row = data.frame('dist' = dist, 'mean.cor' = mean, 'se' = se)
  cor.means.df.blocks = rbind(cor.means.df.blocks, row)
}

cor.means.df.blocks %>%
  ggplot(aes(x = dist, y = mean.cor)) +
  geom_point() +
  geom_errorbar(aes(x = dist, ymin = mean.cor - se, ymax = mean.cor + se)) +
  ylim(0.25, 1) +
  labs(x = "trial distance", y = "correlation of slopes (mean correlation, error bars = se)") +
  ggtitle("Drift in correlation of slopes at greater trial distances for model data") +
  theme(panel.grid = element_blank())






