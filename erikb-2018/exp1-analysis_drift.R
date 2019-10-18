setwd("/Users/erikbrockbank/web/vullab/numberline/erikb-2018/")
rm(list=ls())

library(stats4)
library(tidyverse)
library(Matrix)


###############
### GLOBALS ###
###############

# Globals for experiment setup and data
DATA = "../numexpt/data/"
MIN_ESTIMATE = 1 # lowest number in the range of dots
MAX_ESTIMATE = 1000 # highest number in the range of dots
TRIALS = 300 # number of trials in the experiment (easier to set this as a global than compute it with e.g. max(trial))
BLOCKSIZE = 30 # number of blocks in split-nth fitting


#################
### FUNCTIONS ###
#################

# Data reading
read.data = function(filepath, n.trials) {
  # read data
  files = list.files(filepath)
  dat = data.frame()
  subject = 1
  for (f in files) {
    q = read.csv2(paste(filepath, f, sep = ""), sep = ",", header = T, colClasses = "character")
    q$subject = subject
    dat = rbind(dat, q)
    subject = subject + 1
  }
  
  # format existing columns
  to.num = function(x) {as.numeric(as.character(x))} # supporting util
  dat$run = to.num(dat$run)
  dat$index = to.num(dat$index)
  dat$num_dots = to.num(dat$num_dots)
  dat$answer1 = to.num(dat$answer1)
  dat$answer2 = to.num(dat$answer2)
  dat$points1 = to.num(dat$points1)
  dat$points2 = to.num(dat$points2)
  dat$time = to.num(dat$time)
  
  # add relevant new columns
  dat$answer = 10 ^ (log10(pmax(1, dat$answer1)) / 2 + log10(pmax(1, dat$answer2)) / 2) # blended average of participant answers for this array
  dat$trial = 0
  for (s in unique(dat$subject)) {
    dat$trial[dat$subject == s] = 1:n.trials
  }
  dat$model.answer = 0
  dat$bumper.trial = FALSE
  
  return(dat)
}

# Model fitting functions
# simple power-law mapping
map.power = function(x, a, b) {
  10^(a + 10^b * log10(x))
}

# bi-linear mapping
map.bipower = function(x, a, b) {
  crit = a
  slope = 10^b
  lx = log10(x)
  ly = ((lx > crit) * (crit + (lx - crit) * slope) + (lx <= crit) * lx);
  return(10^ly)
}

## general log likelihood function (with robustness)
loglik = function(x, y, map.fx, a, b, s) {
  sum(
    pmax(-6, dnorm(log10(y) - log10(map.fx(x, a, b)), 0, s, log = T))
  )
}


# fit function
brutefit = function(tmp) {
  nLL = function(a, b, s) {
    -loglik(tmp$num_dots, tmp$answer, usefx, a, b, 10^s) + 
      priors[[1]](a) + priors[[2]](b) + priors[[3]](s)
  }
  
  iter = 0
  fits = NULL
  fit = NULL
  while (is.null(fits)) {
    try(fit <- summary(mle(nLL,
                          start = list(a = runif(1, ps["ma"], ps["sa"]),
                                       b = runif(1, ps["mb"], ps["sb"]), 
                                       s = rnorm(1, ps["ms"], ps["ss"])))), TRUE)
    iter = iter + 1
    
    if (!is.null(fit)) {
      fits = c(tmp$subject[1], -0.5*fit@m2logL, length(tmp$num_dots), fit@coef[,"Estimate"])
    } else {
      if (iter > 50) {
        fits = c(tmp$subject[1], -9999, 0, 0, 0, 0)
      }      
    }
  }
  names(fits) = c("subject", "logL", "n", "a", "b", "s")
  return(fits)
}

# Util function used when fitting slopes: splits trial data into sequential blocks
splitBlock = function(trial, n, total.trials) {
  floor((trial - 1) / (total.trials / n))
}
# Util function used when fitting slopes: splits trial data into modular blocks
splitMod = function(trial, n){
  trial %% n
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
fit.slopes = function(blocksizes, dat, total.trials) {
  n = blocksizes[1]
  fitsBlock = list()
  fitsMod = list()
  for (k in 1:n) {
    tmp = subset(dat, splitBlock(dat$trial, n, total.trials) == (k - 1))
    fitsBlock[[k]] = data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
    
    tmp = subset(dat, splitMod(dat$trial, n) == (k - 1))
    fitsMod[[k]] = data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
  }
  
  ret = list()
  ret[['block']] = fitsBlock
  ret[['mod']] = fitsMod
  return(ret)
}

# Get m blocks by m blocks matrix of fitted slope correlations between each block
get.cor.matrix = function(slopes) {
  # Calculate slope correlations
  block.slopes = do.call(cbind.fill, lapply(slopes, namedSlopes)) # m subjects by n blocks slope values
  cor.matrix = cor(block.slopes, block.slopes, use = "pairwise.complete.obs") # n blocks by n blocks slope correlation matrix
  rownames(cor.matrix) = c() # NB: clearing out rownames and colnames is necessary for subsequent processing
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
get.distance.cors = function(cor.df, total.trials, split.blocks) {
  dist.df = cor.df %>%
    dplyr::mutate(block.dist = block2 - block1,
                  trial.dist = (total.trials / split.blocks) * block.dist) %>%
    dplyr::group_by(trial.dist) %>%
    dplyr::summarize(mean.cor = mean(slope.corr, na.rm = TRUE),
                     se.cor = sd(slope.corr, na.rm = TRUE) / sqrt(length(slope.corr)))
  
  return(dist.df)
}

# Shuffle function for shuffling trial number
shuffle.data = function(dat) {
  dat %>%
    group_by(subject) %>%
    mutate(trial = sample(trial, length(trial), replace = F))
}

# Plotting functions
my.log.breaks = function(lims) {
  majors = seq(floor(log10(lims[1])), ceiling(log10(lims[2])), by = 1)
  minors = log10(unlist(lapply(majors[-1], function(x){seq(10^(x - 1), 9 * 10^(x - 1), by = 10^(x - 1))})))
  return(list(majors, minors))
}

mylogx = function(lims) {
  breaks = my.log.breaks(lims)
  scale_x_log10(limits = lims, 
                breaks = 10^breaks[[1]], 
                minor_breaks = breaks[[2]])
}

mylogy = function(lims) {
  breaks = my.log.breaks(lims)
  scale_y_log10(limits = lims, 
                breaks = 10^breaks[[1]], 
                minor_breaks = breaks[[2]])
}

# theme for plots of individual data
individ_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 28),
  axis.title.y = element_text(face = "bold", size = 28),
  axis.title.x = element_text(face = "bold", size = 28),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 16),
  axis.text.x = element_text(size = 14, angle = 90, hjust = 0, vjust = 0),
  # legend text
  legend.text = element_text(size = 24),
  # facet text
  strip.text = element_text(face = "bold", size = 12),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)



### FIT MODELS (aggregate) ###
data = read.data(DATA, TRIALS)

usefx = map.power
priors = list()
priors[[1]] = function(x){0}
priors[[2]] = function(x){-dnorm(x, 0, 0.2, log = T)}
priors[[3]] = function(x){0} #dnorm(x, -1, 0.5, log=T)
ps = c(0.2, 0.4, -0.3, 0.3, -0.7, 0.2)
names(ps) = c("ma", "sa", "mb", "sb", "ms", "ss")
power.fits = data.frame(do.call(rbind, by(data, data$subject, brutefit)))
print(paste("Failed power fits:", sum(power.fits$logL == -9999)))

usefx = map.bipower
ps = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2)
priors = list()
priors[[1]] = function(x){-dnorm(x, 2, 3, log = T)} #
priors[[2]] = function(x){-dnorm(x, 0, 0.5, log = T)} #
priors[[3]] = function(x){-dnorm(x, -1, 0.25, log = T)} # 
names(ps) = c("ma", "sa", "mb", "sb", "ms", "ss")
bipower.fits = data.frame(do.call(rbind, by(data, data$subject, brutefit)))
print(paste("Failed bipower fits:", sum(bipower.fits$logL == -9999)))

predictions = data.frame()
for (s in unique(data$subject)) {
  stims = seq(1, 300, by = 1)
  powparams = power.fits[power.fits$subject == s,]
  powpred = (map.power(stims, powparams$a, powparams$b))
  biparams = bipower.fits[bipower.fits$subject == s,]
  bipred = (map.bipower(stims, biparams$a, biparams$b))
  predictions = rbind(predictions, 
                       data.frame(subject = s, 
                                  num_dots = stims, 
                                  powpred = powpred, 
                                  bipred = bipred))
}



### MAKE PLOTS ###

# FIGURE: individual data
data.subj = data %>%
  filter(subject %in% c(18)) # sample subject


ggplot(data.subj, aes(x = num_dots, y = answer)) +
  geom_point(alpha = 0.75, color = "red", size = 2) +
  geom_abline(position = "identity") +
  mylogx(c(1, 200)) +
  mylogy(c(1, 200)) +
  xlab("Number presented") +
  ylab("Number reported") +
  individ_plot_theme 


# FIGURE: all subjects, fitted data
#load('../numexpt/R/model.fits.Rdata') # NB: this is a fallback if the generation process above doesn't work

# TODO this is hacky AF
strip.labels = c("1" = "Subject 1", "2" = "Subject 2", "3" = "Subject 3", "4" = "Subject 4", "5" = "Subject 5", "6" = "Subject 6",
                 "7" = "Subject 7", "8" = "Subject 8", "9" = "Subject 9", "10" = "Subject 10", "11" = "Subject 11", "12" = "Subject 12",
                 "13" = "Subject 13", "14" = "Subject 14", "15" = "Subject 15", "16" = "Subject 16", "17" = "Subject 17", "18" = "Subject 18",
                 "19" = "Subject 19", "20" = "Subject 20", "21" = "Subject 21", "22" = "Subject 22", "23" = "Subject 23", "24" = "Subject 24")

# TODO try to add borders around each plot by tweaking `annotate` line below
ggplot(data, aes(x = num_dots, y = answer)) +
  geom_point(alpha = 0.25, color = "red") +
  geom_line(data = predictions, aes(x = num_dots, y = bipred), color = rgb(0, 0.6, 0), size = 1) +
  geom_line(data = predictions, aes(x = num_dots, y = powpred), color = "blue", size = 1) +
  geom_abline(position = "identity") +
  mylogx(c(1, 300)) +
  mylogy(c(1, 300)) +
  xlab("Number presented") +
  ylab("Number reported") +
  #annotate("rect", xmin = 0, xmax = 10^mylogx(c(1, 300))$limits[2], ymin = 0, ymax = 300, color = "black", fill = NA) +
  individ_plot_theme +
  facet_wrap(~subject, ncol = 6,
             labeller = labeller(subject = strip.labels))



# ANALYSIS CoV at higher magnitudes
# add predicted value from power law fit for each subject's estimates
data$power.pred = predictions$powpred[predictions$subject == data$subject & predictions$num_dots == data$num_dots]
# add residual between power law predictions and true estimate
data$power.pred.residsq = (data$power.pred - data$answer)^2

# remove outliers
data = data %>%
  filter(power.pred.residsq < 1000000)

# look at the data (all subjects)
data %>% ggplot(aes(x = num_dots, y = power.pred.residsq)) +
  geom_point(alpha = 0.25) +
  mylogx(c(1, 300)) +
  individ_plot_theme +
  facet_wrap(~subject, ncol = 6, scales = "free_y",
             labeller = labeller(subject = strip.labels))


# representative subject
model.cov.subj = with(data[data$subject == 24,], lm(log10(power.pred.residsq) ~ log10(num_dots)))
ci.cov.subj = confint.lm(model.cov.subj, level = 0.95)
model.cov.preds = data.frame(x = seq(1:MAX_ESTIMATE))
model.cov.preds$y = model.cov.subj$coefficients[1] + model.cov.subj$coefficients[2] * model.cov.preds$x
model.cov.preds$y.lower = ci.cov.subj[1] + ci.cov.subj[2] * model.cov.preds$x
model.cov.preds$y.upper = ci.cov.subj[3] + ci.cov.subj[4] * model.cov.preds$x
data %>% filter(subject == 24) %>%
  ggplot(aes(x = num_dots, y = power.pred.residsq)) +
  geom_point(alpha = 0.5, color = "red") +
  geom_line(data = model.cov.preds, aes(x = x, y = y), color = "blue", size = 1) +
  geom_ribbon(data = model.cov.preds, aes(x = x, y = y, ymin = y.lower, ymax = y.upper), alpha = 0.3, fill = "blue") +
  mylogx(c(1, 300)) +
  mylogy(c(1, 200000)) +
  ggtitle("Sample subject estimate residuals") +
  labs(x = "Number presented", y = "Log squared residuals") +
  individ_plot_theme

slopes = data.frame('subject' = numeric(), 'slope' = numeric())
for (subj in unique(data$subject)) {
  mod = data %>% filter(subject %in% c(subj)) %>% lm(data = ., log10(power.pred.residsq) ~ log10(num_dots))
  slopes = rbind(slopes, data.frame('subject' = subj[1], 'slope' = mod$coefficients[2]))
}
slopes %>% 
  ggplot(aes(x = slope)) +
  geom_histogram(binwidth = 0.15, fill = I("grey"), col = I("black")) +
  geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed") +
  ggtitle("Individual slope estimates on (log) squared residuals") +
  labs(x = "slope estimate") +
  individ_plot_theme

t.test(slopes$slope)



### FIT MODELS (blocks) ###
usefx = map.bipower
ps = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2)
names(ps) = c("ma", "sa", "mb", "sb", "ms", "ss")
priors = list()
priors[[1]] = function(x){-dnorm(x, 1.14, 0.1, log = T)} #
priors[[2]] = function(x){-dnorm(x, -0.1, 0.25, log = T)} #
priors[[3]] = function(x){-dnorm(x, -1, 0.05, log = T)} #

# Fit slopes, NB: this can take ~10s
slopes = fit.slopes(c(BLOCKSIZE), data, TRIALS)
fitsBlock.subj = slopes[["block"]]
# Get matrix of fitted slope correlations
cor.matrix.subj = get.cor.matrix(fitsBlock.subj)
# Format correlation matrix as data frame to plot slope correlations by trial block in analysis section below
slope.cor.df.subj = get.cor.df(cor.matrix.subj)
# Process slope correlations by trial distance to get mean, se across participants
cor.means.df.blocks.subj = get.distance.cors(slope.cor.df.subj, TRIALS, BLOCKSIZE)
cor.means.df.blocks.subj = cor.means.df.blocks.subj %>% filter(trial.dist > 0)

# Analysis: fit line to drift
dist.df.subj = slope.cor.df.subj %>%
  dplyr::mutate(block.dist = block2 - block1,
                trial.dist = (TRIALS / BLOCKSIZE) * block.dist) %>%
  dplyr::group_by(trial.dist)

model.drift = lm(data = dist.df.subj, slope.corr ~ trial.dist)
summary(model.drift)
confint.lm(model.drift, level = 0.95)

preds = data.frame(trial.dist = seq(1, 300)) %>%
  mutate(corr.pred = model.drift$coefficients[1] + trial.dist * model.drift$coefficients[2])


# Analysis: ensure high correlation even at distant trial blocks
cor.test(fitsBlock.subj[[1]]$b, fitsBlock.subj[[30]]$b)


# Analysis: make sure people aren't getting better at the task
df.slopes = data.frame()
for (x in 1:length(fitsBlock.subj)) {
  mean.slope = mean(10^fitsBlock.subj[[x]]$b)
  se.slopes = sd(10^fitsBlock.subj[[x]]$b) / sqrt(length(fitsBlock.subj[[x]]$b))
  df.slopes = rbind(df.slopes, data.frame(x, mean.slope, se.slopes))
}
ggplot(data = df.slopes, aes(x = x, y = mean.slope)) + 
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_ribbon(aes(ymin = mean.slope - se.slopes, ymax = mean.slope + se.slopes),
              color = "gray", alpha = 0.3) +
  labs(x = "trial block", y = "mean slope estimate") +
  individ_plot_theme

# TODO is it a problem that people seem to get worse over the course of the experiment?

# Analysis: make sure people's estimates aren't getting more "fine tuned"
# (plot residuals against predictions by block)
data = data %>% # add block col to data
  mutate(block = splitBlock(trial, BLOCKSIZE[1], TRIALS) + 1)

data = data %>%
  # need to group by variables that give us one row at a time in `data` for list indexing in fitsBlock.subj
  group_by(subject, trial) %>%
  mutate(block.cutoff = fitsBlock.subj[[block]]$a[fitsBlock.subj[[block]]$subject == subject],
         block.slope = fitsBlock.subj[[block]]$b[fitsBlock.subj[[block]]$subject == subject],
         block.pred = map.bipower(num_dots, block.cutoff, block.slope),
         block.residsq = (block.pred - answer)^2) %>%
  filter(block.residsq < 100000) # is this necessary?

data.block.preds.summary = data %>%
  group_by(block) %>%
  summarize(mean.residsq = mean(block.residsq),
            num.subj = length(unique(subject)),
            se.residsq = sd(block.residsq) / num.subj)

data.block.preds.summary %>%
  ggplot(aes(x = block, y = mean.residsq)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_ribbon(aes(ymin = mean.residsq - se.residsq, ymax = mean.residsq + se.residsq),
              color = "gray", alpha = 0.3) +
  labs(x = "trial block", y = "squared residual of estimates") +
  individ_plot_theme
  


fitsMod.subj = slopes[["mod"]]
# Get matrix of fitted slope correlations
mod.cor.matrix.subj = get.cor.matrix(fitsMod.subj)
# Format correlation matrix as data frame to plot slope correlations by trial block in analysis section below
mod.slope.cor.df.subj = get.cor.df(mod.cor.matrix.subj)
# Process slope correlations by trial distance to get mean, se across participants
mod.cor.means.df.blocks.subj = get.distance.cors(mod.slope.cor.df.subj, TRIALS, BLOCKSIZE)
mod.cor.means.df.blocks.subj = mod.cor.means.df.blocks.subj %>% filter(trial.dist > 0)

# Shuffled fit
data.shuffle = shuffle.data(data)
# Fit slopes, NB: this can take ~10s
fitsBlock.shuffle = fit.slopes(c(BLOCKSIZE), data.shuffle, TRIALS)[["block"]]
# Get matrix of fitted slope correlations
cor.matrix.shuffle = get.cor.matrix(fitsBlock.shuffle)
# Format correlation matrix as data frame to plot slope correlations by trial block in analysis section below
slope.cor.df.shuffle = get.cor.df(cor.matrix.shuffle)
# Process slope correlations by trial distance to get mean, se across participants
cor.means.df.blocks.shuffle = get.distance.cors(slope.cor.df.shuffle, TRIALS, BLOCKSIZE)
cor.means.df.blocks.shuffle = cor.means.df.blocks.shuffle %>% filter(trial.dist > 0)

# Analysis: fit line to shuffled drift
dist.df.shuffle = slope.cor.df.shuffle %>%
  dplyr::mutate(block.dist = block2 - block1,
                trial.dist = (TRIALS / BLOCKSIZE) * block.dist) %>%
  dplyr::group_by(trial.dist)

model.drift.shuffle = lm(data = dist.df.shuffle, slope.corr ~ trial.dist)
summary(model.drift.shuffle)
confint.lm(model.drift.shuffle, level = 0.95)



# FIGURE: slope correlation matrix

# Plot slope correlations by trial block
slope.cor.df.subj %>%
  # slope.cor.df.subj %>%
  ggplot(aes(x = as.factor(block1), y = as.factor(block2), fill = slope.corr)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", mid = "white", high = "red", midpoint = 0.5, limits = c(0, 1)) + # midpoint = 0.25, limits = c(0, 1)
  xlab("Trial block") + ylab("Trial block") +
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


# FIGURE: slope correlations by trial distance
ggplot() +
  # block split correlations
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
  # shuffle correlations
  geom_point(data = cor.means.df.blocks.shuffle,
             aes(x = trial.dist,
                 y = mean.cor,
                 color = "shuffle")) +
  geom_errorbar(data = cor.means.df.blocks.shuffle,
                aes(x = trial.dist,
                    ymin = mean.cor - se.cor,
                    ymax = mean.cor + se.cor,
                    color = "shuffle"),
                width = 5) +
  #geom_line(data = preds, aes(x = trial.dist, y = corr.pred)) +
  ylim(0.45, 0.95) + xlim(0, 300) +
  labs(x = "Distance (trials)", y = "Correlation") +
  ggtitle("Drift in slope correlation") +
  scale_color_manual(name = "",
                     labels = c("subjects" = "block split", "shuffle" = "shuffled trials"),
                     values = c("subjects" = "red", "shuffle" = "seagreen")) +
  individ_plot_theme



