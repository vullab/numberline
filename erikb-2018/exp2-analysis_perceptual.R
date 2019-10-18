### Analysis script for Cog Sci numberline revisions, first year project paper, and larger paper submission ###
#' Specifically, this script does all analysis of the *perceptual modality experiment*. 
#' The other files doing analysis of this data are in `/numberline/num-density.2013-06/R/`
#' but this script represents a cleaner, final version.
#' The process of reading in, processing, and cleaning in data is mostly taken from `/numberline/num-density.2013-06/R/load.density.data.R`
#' The bulk of the analysis is taken from `/numberline/num-density.2013-06/R/fit.models.density-blocks.2014-06-02.R`, 
#' with some code taken from `/numberline/num-density.2013-06/R/fit.models.density.2014-06-02.R`
#' 

#' TODO
#' - declare globals (or some variable...) for plots that look at limited subsets of trial blocks (some inline TODOs about this)
#' - other general cleanup
#' - migrate this to an rmarkdown for easier sharing?


setwd("/Users/erikbrockbank/web/vullab/numberline/erikb-2018/")
rm(list=ls())

library(tidyverse)
library(reshape)
library(psych) # needed for fisherz
library(HardyWeinberg) # needed for ifisherz
library(Rmisc)

# taken from exp 1
library(stats4) # for some reason we need this here for mle to work below


### GLOBALS ###
DATA_FILEPATH = "../num-density.2013-06/data/"
FEEDBACK_TRIALS = 1:25 # trials in which feedback was given
COLNAMES = c("trial", "t.start", "time", "vary", "num_dots", "answer", "r.dot", "r.space", "n.rings", "feedback", 
             "points", "score", "subject") # names of columns for reading in data
COR_THRESHOLD = 0.6 # minimum correlation threshold between participant answers and true num_dots (for dropping outliers)
MAX_PRESENTED = 750 # highest number presented during the task
MAX_ESTIMATE = 1000 # max value for displaying number reported (note some estimates were larger than this)

PARAMS = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2)
names(PARAMS) = c("ma", "sa", "mb", "sb", "ms", "ss")
PRIORS = list()
# PRIORS[[1]] = function(x){-dnorm(x, 1.5, 0.1, log = T)} #
# PRIORS[[2]] = function(x){-dnorm(x, -0.2, 0.25, log = T)} #
# PRIORS[[3]] = function(x){-dnorm(x, -0.9, 0.1, log = T)} # 

RUN_SHUFFLE_ANALYSIS = FALSE # whether to do shuffle analysis of data

### FUNCTIONS ###
# Data processing functions
to.num = function(x){as.numeric(as.character(x))}

read.data = function(fp, col.names) {
  files = list.files(fp)
  dat = data.frame()
  subject = 1
  for (f in files) {
    q = read.csv2(paste(fp, f, sep = ""), sep = "\t", header = T, colClasses = "character", skip = 1)
    q$subject = subject
    dat = rbind(dat, q)
    subject = subject + 1
  }
  
  names(dat) = col.names
  
  dat$trial = to.num(dat$trial)
  dat$t.start = to.num(dat$t.start)
  dat$time = to.num(dat$time) / 1000
  dat$answer = to.num(dat$answer)
  dat$num_dots = to.num(dat$num_dots)
  dat$r.dot = to.num(dat$r.dot)
  dat$r.space = to.num(dat$r.space)
  dat$n.rings = to.num(dat$n.rings)
  dat$feedback = to.num(dat$feedback)
  dat$points = to.num(dat$points)
  dat$score = to.num(dat$score)
  dat$vary = as.character(dat$vary)
  dat$vary[dat$vary == "space"] = "density" # NB: added this to make plots more clear (erikb)
  dat$subject = as.numeric(dat$subject)

  # Drop outliers
  rs = rbind(by(dat, dat$subject, function(tmp){cor(tmp$num_dots, tmp$answer, method = "spearman")}))
  dat = dat[!(dat$subject %in%  which(rs < COR_THRESHOLD)),] # drop subjects w/ less than COR_THRESHOLD correlation with true num_dots
  #ns = rbind(by(dat, dat$subject, nrow)) # number of observations for each included subject
  
  return(dat)
}

### Modeling functions ###

# These functions modified from `fit.models.density-blocks.2014-06-02.R`
# NB: some overlap between these functions and others used in modeling scripts in this directory
map.bipower = function(x, a, b) {
  crit = a
  slope = 10^b
  lx = log10(x)
  ly = ((lx > crit) * (crit + (lx - crit) * slope) + (lx <= crit) * lx);
  return(10^ly)
}

# Highly similar function but takes in slope already raised to 10 for easier CI calcs
map.bipower.ci = function(x, a, b) {
  crit = a
  slope = b
  lx = x
  ly = ((lx > crit) * (crit + (lx - crit) * slope) + (lx <= crit) * lx);
  return(ly)
}

# general log likelihood function (with robustness)
loglik = function(x, y, map.fx, a, b, s) {
  sum(
    pmax(-6, dnorm(log10(y) - log10(map.fx(x, a, b)), 0, s, log = T))
  )
}

# fit power function
brutefit = function(tmp) {
  nLL = function(a, b, s) {
    -loglik(tmp$num_dots, tmp$answer, map.bipower, a, b, 10^s) + # NB: can use other function instead of `map.bipower`
      PRIORS[[1]](a) +
      PRIORS[[2]](b) +
      PRIORS[[3]](s)
  }

  iter = 0
  fits = NULL
  fit = NULL
  while (is.null(fits)) {
    try(fit <- summary(mle(nLL,
                           start = list(a = runif(1, PARAMS["ma"], PARAMS["sa"]),
                                      b = runif(1, PARAMS["mb"], PARAMS["sb"]),
                                      s = rnorm(1, PARAMS["ms"], PARAMS["ss"])))), TRUE)
    iter = iter + 1

    if (!is.null(fit)) {
      # print(fit)
      # fits = c(tmp$subject[1], -0.5 * fit@m2logL, length(tmp$num_dots), fit@coef[,"Estimate"], fit@coef[,"Std. Error"])
      fits = c(tmp$subject[1], -0.5 * fit@m2logL, length(tmp$num_dots), fit@coef[,"Estimate"])
    } else {
      if (iter > 50) { # NB increase this for larger data than individual participants/blocks
      # if (iter > 500) {
        #print("Unable to fit slope")
        # fits = c(tmp$subject[1], -9999, 0, 0, 0, 0, 0, 0, 0)
        fits = c(tmp$subject[1], -9999, 0, 0, 0, 0)
      }
    }
  }
  # names(fits) = c("subject", "logL", "n", "a", "b", "s", "se.a", "se.b", "se.s")
  names(fits) = c("subject", "logL", "n", "a", "b", "s")
  return(fits)
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

# Util function for shuffle analysis
shuffle.data = function(dat) {
  dat %>%
    group_by(subject) %>%
    mutate(trial = sample(trial, length(trial), replace = F))
}

# Util function for split-half analysis
sample.fn = function(vals) {
  sample(vals, length(vals), replace = F)
}


### Graphing functions ###

offdiagonal = function(mat, n) {
  mat[(row(mat) - col(mat)) == (-n)]
}

get.slope.cors = function(mat, modality1, modality2){
  offdiagonal(mat[[modality1]][[modality2]], 0)[1:12] # TODO why 1-12 here?
}

get.slope.conf = function(df, modality1, modality2) {
  type = paste(modality1, modality2, sep = "-")
  lower = df$Lower.conf[df$Block1 == df$Block2 & df$Type == type][1:12] # TODO 1-12 here is arbitrarily matching the above
  upper = df$Upper.conf[df$Block1 == df$Block2 & df$Type == type][1:12]
  return(data.frame("lower" = lower, "upper" = upper))
}

gettcor = function(vi, vj, R) {
  out = data.frame()
  for (D in 1:10) { # TODO make this less arbitrary?
    rs = fisherz(offdiagonal(R[[MODALITIES[vi]]][[MODALITIES[vj]]][2:12, 2:12], D)) # TODO make this less arbitrary?
    mu = ifisherz(mean(rs))
    se = sd(rs) / sqrt(length(rs))
    ul = ifisherz(fisherz(mu) + se) # upper limit
    ll = ifisherz(fisherz(mu) - se) # lower limit
    out = rbind(out, data.frame(mu = mu, se = se, ul = ul, ll = ll))
  }
  return(out)
}

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
  breaks = my.log.breaks(lims)
  scale_y_log10(limits = lims, 
                breaks = 10^breaks[[1]], 
                minor_breaks = breaks[[2]])
}

# Function for creating ggplot with block x block slope correlation matrices
# NB: we make six of these so it's easiest to make this a function
create.correlation.matrix = function(name, color, data) {
  plot = ggplot(subset(data, data$type == name), 
                aes(x = as.factor(Block1), y = as.factor(Block2), 
                    fill = Correlation, label = sprintf("%0.2f", Correlation))) + 
    geom_tile() + 
    geom_text(size = 5) +
    scale_fill_gradient2(low = "white", mid = "white", high = color, midpoint = 0, limits = c(-1, 1)) +
    xlab("") +
    ylab("") +
    ggtitle(name) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    matrix_plot_theme +
    theme(plot.title = element_text(face = "bold", size = 32, color = color))
  
  return(plot)
}

# theme for plots of individual data
individ_plot_theme = theme(
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
  strip.background = element_blank(),
  
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)

# theme for plots with many individual plots (smaller text sizes, etc.)
large_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 20),
  axis.title.y = element_text(face = "bold", size = 14),
  axis.title.x = element_text(face = "bold", size = 14),
  # axis text
  axis.text.y = element_text(size = 10),
  axis.text.x = element_text(size = 10, angle = 90, hjust = 0, vjust = 0),
  # facet text
  strip.text = element_text(face = "bold", size = 8),
  # backgrounds, lines
  panel.background = element_blank(),
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)

# theme for plots with a handful of individual plots (slightly smaller text sizes, etc.)
medium_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 20),
  axis.title.y = element_text(face = "bold", size = 14),
  axis.title.x = element_text(face = "bold", size = 14),
  # axis text
  axis.text.y = element_text(size = 12),
  axis.text.x = element_text(size = 12, angle = 90, hjust = 0, vjust = 0),
  # facet text
  strip.text = element_text(face = "bold", size = 12),
  # backgrounds, lines
  panel.background = element_blank(),
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)

# theme for correlation plots (drift by modality, original plot of modality correlations by block)
corr_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 32),
  axis.title.y = element_text(face = "bold", size = 32),
  axis.title.x = element_text(face = "bold", size = 32),
  legend.title = element_text(face = "bold", size = 28),
  # axis text
  axis.text.y = element_text(size = 14),
  axis.text.x = element_text(size = 14),
  # legend text
  legend.text = element_text(size = 24),
  # background color, axis lines
  panel.background = element_blank(),
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)

# theme for plots of trial block correlation matrices
matrix_plot_theme = theme(
  axis.ticks = element_blank(), 
  axis.text = element_text(size = 16, face = "bold"),
  axis.text.y = element_text(angle = 90),
  legend.title = element_blank(),
  legend.text = element_text(size = 12, face = "bold"),
  panel.background = element_blank(),
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"))



### DATA PROCESSING ###
# NB: the below is mostly adapted from `/numberline/num-density.2013-06/R/load.density.data.R`
dat = read.data(DATA_FILEPATH, COLNAMES)
MODALITIES = unique(dat$vary) # global assigned after reading in data

glimpse(dat)
length(unique(dat$subject))

### SHUFFLING: don't run this unless shuffling!! ###
# shuffle trial order by participant
if (RUN_SHUFFLE_ANALYSIS) {
  dat = shuffle.data(dat)
  # check that it worked
  table(dat$trial)
  glimpse(dat)
}


# NB: still some major outliers, see below.
# These are mostly ignored by subsequent analyses when we e.g. fit calibration lines, but good to know they exist
sort(dat$answer, decreasing = T)[1:25]
dat$subject[dat$answer %in% sort(dat$answer, decreasing = T)[1:5]]
# TODO Maybe we should get rid of subject 49...


### FIGURE: INDIVIDUAL DATA ###

# graph example data for three individual subjects, chosen below
sample.subjects = c(12, 25, 36) # Sample subjects
strip.labels = c("12" = "Subject 12", "25" = "Subject 25", "36" = "Subject 36")

dat %>%
  filter(subject %in% sample.subjects) %>%
  ggplot(aes(x = num_dots, y = answer)) +
    geom_point(alpha = 0.25, color = "blue", size = 2) +
    geom_abline(position = "identity") +
    mylogx(c(1, MAX_PRESENTED)) +
    mylogy(c(1, MAX_ESTIMATE)) +
    ggtitle("Estimation data for sample participants") +
    xlab("Number presented") +
    ylab("Number reported") +
    facet_wrap(~subject, ncol = 3, labeller = labeller(subject = strip.labels)) +
    individ_plot_theme

# graph all subjects in dat
# NB: this can be slow
ggplot(dat, aes(x = num_dots, y = answer)) +
  geom_point(alpha = 0.25, color = "blue", size = 0.5) +
  geom_abline(position = "identity") +
  mylogx(c(1, MAX_PRESENTED)) +
  mylogy(c(1, MAX_ESTIMATE)) +
  ggtitle("Estimation data, all participants") +
  xlab("Number presented") +
  ylab("Number reported") +
  large_plot_theme +
  facet_wrap(~subject, ncol = 11)


### FIGURE: PERFORMANCE BY MODALITY ###

# Get median reported answers across log-equidistant blocks of number presented (for each modality)
# 10 equidistant buckets within accurate estimate range, 20 buckets equidistant in log space
cuts = c(9.5:20.5, 10^seq(log10(21.5), log10(MAX_PRESENTED), length.out = 20))
summary.stats = data.frame()
for (i in 1:length(MODALITIES)) {
  sdat = subset(dat, dat$vary == MODALITIES[i])
  sdat$bin = cut(sdat$num_dots, breaks = cuts, labels = seq_len(length(cuts) - 1), include.lowest = T)
  truens = cbind(by(sdat$num_dots, sdat$bin, median)) # median true number presented in this range
  medians = cbind(by(sdat$answer, sdat$bin, median)) # median estimate in this range
  acc = cbind(by(sdat, sdat$bin, function(tmp){mean(tmp$num_dots == tmp$answer)}))
  summary.stats = rbind(summary.stats,
             data.frame(truens = truens[,1],
                        medians = medians[,1],
                        vary = rep(MODALITIES[i], length(acc[,1]))))
}

# Plot median estimates across modalities
ggplot(dat, aes(x = num_dots, y = answer)) +
  geom_point(colour = "blue", size = 2, alpha = 0.05) +
  geom_point(data = summary.stats, aes(x = truens, y = medians), color = "red", size = 2) +
  geom_line(data = summary.stats, aes(x = truens, y = medians), color = "red", size = 1) +
  geom_abline(position = "identity") +
  # geom_vline(aes(xintercept = 40)) + # check where underestimation begins
  mylogx(c(1, MAX_PRESENTED)) +
  mylogy(c(1, MAX_ESTIMATE)) +
  xlab("Number presented") + 
  ylab("Number reported") + 
  ggtitle("Accuracy across estimate conditions") +
  individ_plot_theme +
  facet_wrap(~vary, ncol = 3,
             labeller = labeller(vary = c("area" = "area", "size" = "size", "density" = "density")))


### FIGURE: SLOPE COMPARISONS BY MODALITY ###
PARAMS = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2)
names(PARAMS) = c("ma", "sa", "mb", "sb", "ms", "ss")
PRIORS = list()

# Strong priors, seem to converge pretty well
PRIORS[[1]] = function(x){-dnorm(x, 1.5, 0.1, log = T)} #
PRIORS[[2]] = function(x){-dnorm(x, -0.2, 0.05, log = T)} #
PRIORS[[3]] = function(x){-dnorm(x, -1, 0.1, log = T)} #


fitsModality = list()
for (i in 1:length(MODALITIES)) {
  A = (dat$vary == MODALITIES[i])
  tmp = subset(dat, A)
  fitsModality[[MODALITIES[i]]] = data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
  print(c(i, sum(fitsModality[[MODALITIES[i]]]$logL == -9999)))
}

for (i in 1:length(MODALITIES)) {
  A = fitsModality[[MODALITIES[i]]]
  print(MODALITIES[i])
  print(paste("cutoff mean: ", mean(A$a)))
  print(paste("cutoff sd: ", sd(A$a)))
  print(paste("slope mean: ", mean(A$b)))
  print(paste("slope sd: ", sd(A$b)))
  fitsModality[[MODALITIES[i]]] = fitsModality[[MODALITIES[i]]] %>%
    mutate(cutoff.trans = 10^a,
           slope.trans = 10^b)
}


fitSummary = data.frame('vary' = character(),
                        'obs' = numeric(),
                        'cutoff.mean' = numeric(),
                        'cutoff.se' = numeric(),
                        'slope.mean' = numeric(),
                        'slope.se' = numeric())
for (i in 1:length(MODALITIES)) {
  A = fitsModality[[MODALITIES[i]]]
  fitSummary = rbind(fitSummary, data.frame(
    vary = MODALITIES[i],
    obs = length(unique(A$subject)),
    cutoff.mean = mean(10^A$a),
    cutoff.se = sd(10^A$a) / sqrt(length(unique(A$subject))),
    slope.mean = mean(10^A$b),
    slope.se = sd(10^A$b) / sqrt(length(unique(A$subject)))
  ))
}

predictions = data.frame('vary' = character(),
                         'num_dots' = numeric(),
                         'prediction' = numeric(),
                         'prediction.ul' = numeric(),
                         'prediction.ll' = numeric())

for (i in 1:length(MODALITIES)) {
  true_vals = 1:MAX_PRESENTED
  predictions = rbind(predictions,
                      data.frame(vary = MODALITIES[i],
                                 num_dots = true_vals,
                                 prediction = map.bipower.ci(true_vals, 
                                                             fitSummary$cutoff.mean[fitSummary$vary == MODALITIES[i]], 
                                                             fitSummary$slope.mean[fitSummary$vary == MODALITIES[i]]),
                                 prediction.ul = map.bipower.ci(true_vals, 
                                                                fitSummary$cutoff.mean[fitSummary$vary == MODALITIES[i]], 
                                                                fitSummary$slope.mean[fitSummary$vary == MODALITIES[i]] +
                                                                  fitSummary$slope.se[fitSummary$vary == MODALITIES[i]]),
                                 prediction.ll = map.bipower.ci(true_vals, 
                                                                fitSummary$cutoff.mean[fitSummary$vary == MODALITIES[i]], 
                                                                fitSummary$slope.mean[fitSummary$vary == MODALITIES[i]] -
                                                                  fitSummary$slope.se[fitSummary$vary == MODALITIES[i]])))
  
}

# POSTER FIGURE 1
ggplot(dat, aes(x = num_dots, y = answer)) +
  geom_point(color = "blue", size = 2, alpha = 0.05) +
  geom_point(data = predictions, aes(x = num_dots, y = prediction), color = "red", size = 1) +
  geom_line(data = predictions, aes(x = num_dots, y = prediction), color = "red", size = 2) +
  geom_ribbon(data = predictions, mapping = aes(x = num_dots, ymin = prediction.ll, ymax = prediction.ul), inherit.aes = FALSE, color = "red", size = 2, alpha = 0.75) +
  geom_abline(position = "identity", alpha = 0.5) +
  mylogx(c(1, MAX_PRESENTED)) +
  mylogy(c(1, MAX_ESTIMATE)) +
  xlab("Number presented") + 
  ylab("Number reported") + 
  ggtitle("Accuracy across estimate conditions") +
  individ_plot_theme +
  facet_wrap(~vary, ncol = 3,
             labeller = labeller(vary = c("area" = "Area trials", "size" = "Size trials", "density" = "Density trials")))


### FIGURE: SLOPE CORRELATIONS BY BLOCK ###

# Fit slope data
trialcuts = c(0, 25, 75, 125, 175, 225, 275, 325, 375, 425, 475, 525, 575, 1000) # TODO this is somewhat arbitrary
dat$block = cut(dat$trial, trialcuts, labels = 0:(length(trialcuts) - 2), include.lowest = T)
dat$mod = (dat$trial - 26) %% 11 + 1
dat$mod[dat$block == 0] = 0
dat$mod[dat$block == (length(trialcuts) - 2)] = (length(trialcuts) - 2)

fitsBlock = list()
fitsMod = list() # NB: we only use fitsMod for final plot

# NB: this takes several minutes
for (i in 1:length(MODALITIES)) {
  fitsBlock[[MODALITIES[i]]] = list() # create list of best fitting slopes for trials from each modality
  for (k in 0:(length(trialcuts) - 2)) { # TODO why the -2?
    # fit slopes by block
    A = (dat$vary == MODALITIES[i]) & (dat$block == k)
    tmp = subset(dat, A)
    fitsBlock[[MODALITIES[i]]][[k + 1]] = data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
    # fit slopes by modular split
    A = (dat$vary == MODALITIES[i]) & (dat$mod == k)
    tmp = subset(dat, A)
    fitsMod[[MODALITIES[i]]][[k + 1]] = data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
    # sanity check
    print(c(i, k, sum(fitsMod[[MODALITIES[i]]][[k + 1]]$logL == -9999), 
            sum(fitsBlock[[MODALITIES[i]]][[k + 1]]$logL == -9999)))
  }
}

# Validating the fits above
fitsBlock[["size"]][[13]] # best fitting slopes by subject for block 13 of "size" trials
fitsBlock[["density"]][[13]]
summary(fitsBlock[["size"]][[10]][['b']])


# Analysis: check that participants don't get better over the course of the experiment
df.slopes = data.frame('modality' = character(), 'block' = numeric(), 'slope.mean' = numeric(), 'slope.se' = numeric())
for (modality in 1:length(MODALITIES)) {
  for (block in 1:length(fitsBlock[[modality]])) {
    slopes.data = fitsBlock[[MODALITIES[modality]]][[block]]$b
    mean.slope = mean(10^slopes.data)
    se.slopes = sd(10^slopes.data) / sqrt(length(slopes.data))
    df.slopes = rbind(df.slopes, data.frame('modality' = MODALITIES[modality], 'block' = block, 
                                            'slope.mean' = mean.slope, 'slope.se' = se.slopes))
  }
}
df.slopes


ggplot(data = df.slopes, aes(x = block, y = slope.mean, color = modality)) + 
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = slope.mean - slope.se, ymax = slope.mean + slope.se), alpha = 0.3) +
  labs(x = "trial block", y = "mean slope estimate") +
  individ_plot_theme






# Get slope correlation matrix
R = list() # primary slope correlation matrix
Rr = list() # NB: Rr only gets used for very final plot
mcor = data.frame()
mconf = data.frame("Type" = character(), "Block1" = numeric(), "Block2" = numeric(), "Lower.conf" = numeric(), "Upper.conf" = numeric())
for (i in 1:length(MODALITIES)) {
  R[[MODALITIES[i]]] = list() # make separate slope correlations for each trial type
  for (j in i:length(MODALITIES)) {
    # get slopes for n participant rows by m trial blocks in modality i or j
    s1 = do.call(cbind.fill, lapply(fitsBlock[[MODALITIES[i]]], namedSlopes))
    s2 = do.call(cbind.fill, lapply(fitsBlock[[MODALITIES[j]]], namedSlopes))
    # get confint on correlation
    for (block1 in 1:dim(s1)[2]) {
      for (block2 in block1:dim(s2)[2]) {
        conf = cor.test(s1[, block1], s2[, block2])$conf.int
        mconf = rbind(mconf, data.frame("Type" = paste(MODALITIES[i], MODALITIES[j], sep = "-"),
                               "Block1" = block1, "Block2" = block2, 
                               "Lower.conf" = conf[1], "Upper.conf" = conf[2]))
      }
    }
    # NB: R[[MODALITIES[i]]][[MODALITIES[j]]] is m blocks by m blocks pairwise slope correlations (n pairwise participant observations)
    R[[MODALITIES[i]]][[MODALITIES[j]]] = cor(s1, s2, use = "pairwise.complete.obs")
    rownames(R[[MODALITIES[i]]][[MODALITIES[j]]]) = c()
    colnames(R[[MODALITIES[i]]][[MODALITIES[j]]]) = c()
    m = melt(R[[MODALITIES[i]]][[MODALITIES[j]]][2:12, 2:12]) # TODO make this less arbitrary
    names(m) = c("Block1", "Block2", "Correlation") # correlation of slopes
    m = m[m$Block2 >= m$Block1,] # keep only top half of correlation matrix
    m$type = paste(MODALITIES[i], MODALITIES[j], sep = "-")
    mcor = rbind(mcor, m)
    
    s1 = do.call(cbind.fill, lapply(fitsMod[[MODALITIES[i]]], namedSlopes))
    s2 = do.call(cbind.fill, lapply(fitsMod[[MODALITIES[j]]], namedSlopes))
    Rr[[MODALITIES[i]]][[MODALITIES[j]]] = cor(s1, s2, use = "pairwise.complete.obs")
    rownames(Rr[[MODALITIES[i]]][[MODALITIES[j]]]) = c()
    colnames(Rr[[MODALITIES[i]]][[MODALITIES[j]]]) = c()
  }
}
names(mcor) = c("Block1", "Block2", "Correlation", "type")
mcor$type = as.character(mcor$type)

# Validating the above
mcor
levels(as.factor(mcor$type))

# Make data frame with each comparison, slope correlations and confidence intervals per block
# based on R calculated above
slope.cors = data.frame('comparison' = character(),
                        'block' = numeric(),
                        'corr' = numeric(),
                        'conf.lower' = numeric(),
                        'conf.upper' = numeric())

for (i in 1:length(MODALITIES)) {
  for (j in i:length(MODALITIES)) {
    comp = paste(MODALITIES[[i]], MODALITIES[[j]], sep = "-")
    blocks = 0:11
    corr = get.slope.cors(R, MODALITIES[[i]], MODALITIES[[j]])
    conf = get.slope.conf(mconf, MODALITIES[[i]], MODALITIES[[j]])
    slope.cors = rbind(slope.cors, data.frame('comparison' = comp,
                                              'block' = blocks,
                                              'corr' = corr,
                                              'conf.lower' = conf$lower,
                                              'conf.upper' = conf$upper))
  }
}

slope.cors.comparison = slope.cors %>%
  filter(comparison %in% c("density-area", "size-area", "size-density"))

# Plot slope correlations by block
slope.cors.comparison %>%
  ggplot(aes(x = block, y = corr, color = comparison)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = conf.lower,
                    ymax = conf.upper),
                width = 0.25, size = 1) +
  ggtitle("Slope correlations by trial block") +
  ylab("Correlation") + 
  xlab("Block") + 
  scale_x_continuous(breaks = blocks, minor_breaks = c()) +
  scale_y_continuous(limits = c(-0.25, 1), breaks = seq(-0.2, 1, by = 0.2)) +
  corr_plot_theme +
  scale_color_manual(name = "slope comparison",
                     values = c("size-density" = "red", "size-area" = "blue", "density-area" = "green4"),
                     labels = c("size-density" = "size-density", "size-area" = "size-area", "density-area" = "density-area"))




### FIGURE: INDIVIDUAL EXAMPLE SLOPES BY TRIAL MODALITY (ALL BLOCKS) ###

subjects = c(2, 31, 47)
subjectsDat = dat %>%
  filter(subject %in% subjects,
         as.numeric(block) %in% 2:11)

fitsModalitySubj = list()

for (i in 1:length(MODALITIES)) {
  A = (subjectsDat$vary == MODALITIES[i])
  tmp = subset(subjectsDat, A)
  fitsModalitySubj[[MODALITIES[i]]] = data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
  print(c(i, sum(fitsModalitySubj[[MODALITIES[i]]]$logL == -9999)))
}

predictions = data.frame('subject' = character(),
                         'vary' = character(),
                         'num_dots' = numeric(),
                         'prediction' = numeric())

for (i in 1:length(MODALITIES)) {
  for (j in 1:length(subjects)) {
    true_vals = 1:MAX_PRESENTED
    predictions = rbind(predictions,
                        data.frame(subject = subjects[j],
                                   vary = MODALITIES[i],
                                   num_dots = true_vals,
                                   prediction = map.bipower(true_vals, fitsModalitySubj[[i]]$a[j], fitsModalitySubj[[i]]$b[j])))
  }
}

# POSTER FIGURE 2
ggplot(subjectsDat, aes(x = num_dots, y = answer)) +
  geom_point(color = "blue", size = 2, alpha = 0.25) +
  #geom_point(data = predictions, aes(x = num_dots, y = prediction), color = "red", size = .8) +
  geom_line(data = predictions, aes(x = num_dots, y = prediction), color = "red", size = 2) +
  geom_abline(position = "identity") +
  mylogx(c(1, MAX_PRESENTED)) +
  mylogy(c(1, MAX_ESTIMATE)) +
  xlab("Number presented") + 
  ylab("Number reported") + 
  ggtitle("Fitted slopes for sample participants") +
  individ_plot_theme +
  facet_grid(subject~vary, scales = "free",
             labeller = labeller(subject = c("2" = "Subject 2", "31" = "Subject 31", "47" = "Subject 47"),
               vary = c("area" = "Area trials", "size" = "Size trials", "density" = "Density trials")))


### FIGURE: INDIVIDUAL DIFFERENCES ACROSS MODALITIES, ALL BLOCKS ###

individ.dat = dat %>%
  filter(as.numeric(block) %in% 2:11) %>% # blocks 1-10
  group_by(subject, vary) %>%
  mutate(split.half = as.numeric(trial %% 2 == 0)) %>% # this is just a stand-in, we shuffle these below
  mutate(split.half = replace(split.half, values = sample.fn(split.half)))

# make sure the above worked (check order of split.half)
individ.dat %>%
  group_by(subject, vary, trial) %>%
  glimpse()

# fit slopes to each split half for each participant, modality trials
fitsModalitySplit = list()

for (i in 1:length(MODALITIES)) {
  fitsModalitySplit[[MODALITIES[i]]] = list()
  
  # split half == 0
  A = (individ.dat$vary == MODALITIES[i] & individ.dat$split.half == 0)
  tmp = subset(individ.dat, A)
  fitsModalitySplit[[MODALITIES[i]]][['0']] = data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
  print(c(i, sum(fitsModalitySplit[[MODALITIES[i]]][['0']]$logL == -9999)))
  
  # split half == 1
  A = (individ.dat$vary == MODALITIES[i] & individ.dat$split.half == 1)
  tmp = subset(individ.dat, A)
  fitsModalitySplit[[MODALITIES[i]]][['1']] = data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
  print(c(i, sum(fitsModalitySplit[[MODALITIES[i]]][['1']]$logL == -9999)))
}

mconf = data.frame("Comparison" = character(), "corr" = numeric(), "Lower.conf" = numeric(), "Upper.conf" = numeric(), "Within" = numeric())

for (i in 1:length(MODALITIES)) {
  for (j in i:length(MODALITIES)) {
    s1.label = MODALITIES[i]
    s2.label = MODALITIES[j]
    # outputs df of slopes for 0 pairwise, 1 pairwise for each participant in that modality
    s1 = do.call(cbind.fill, lapply(fitsModalitySplit[[MODALITIES[i]]], namedSlopes))
    s2 = do.call(cbind.fill, lapply(fitsModalitySplit[[MODALITIES[j]]], namedSlopes))
    names(s1) = c("half0", "half1")
    names(s2) = c("half0", "half1")
    
    if (i == j) { # when i == j, s1 and s2 are identical
      cortest = cor.test(s1$half0, s1$half1)
    } else {
      cortest = cor.test(s1$half0, s2$half0) # choice of half in s1 and s2 is arbitrary
    }
    mconf = rbind(mconf, data.frame(Comparison = paste(s1.label, s2.label, sep = "-"),
                                    corr = cortest$estimate,
                                    Lower.conf = cortest$conf.int[1],
                                    Upper.conf = cortest$conf.int[2],
                                    Within = as.numeric(i == j)))
  }
}

# POSTER FIGURE 3
individ_plot_theme$axis.text.x = element_text(size = 20)
individ_plot_theme$plot.title = element_text(face = "bold", size = 28)

mconf %>%
  ggplot(aes(x = fct_reorder(Comparison, corr, .desc = TRUE), 
             y = corr, 
             fill = as.factor(Within))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = Lower.conf, ymax = Upper.conf), width = 0.25) +
  ggtitle("Estimate calibration comparisons by modality") +
  labs(x = "", y = "Split-half slope correlation") +
  scale_fill_manual(name = "",
                     values = c("0" = "#999999", "1" = "#56B4E9"),
                     labels = c("0" = "Across-modality", "1" = "Within-modality")) +
  ylim(0, 1) +
  individ_plot_theme +
  geom_label(aes(x = 2, y = 0.99, label = "Within modality", fontface = "bold"), color = "#56B4E9", size = 8, fill = "white") +
  geom_label(aes(x = 5, y = 0.99, label = "Across modality", fontface = "bold"), color = "#999999", size = 8, fill = "white") +
  theme(legend.position = "none")


### FIGURE: INDIVIDUAL EXAMPLE SLOPES BY BLOCK ONLY ###
# NB: relies on trialcuts and other columns added to `dat` above

subjects = c(2)
dat.subject = dat %>%
  filter(subject %in% subjects, as.numeric(block) %in% 1:10)

dat.subject$block = as.numeric(dat.subject$block)

fitsBlockOnly = list()
for (k in 0:(length(trialcuts) - 2)) { # TODO why the -2?
  # fit slopes by block
  A = (dat$block == k)
  tmp = subset(dat, A)
  fitsBlockOnly[[k + 1]] = data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
  # sanity check
  print(c(i, k, sum(fitsBlockOnly[[k + 1]]$logL == -9999)))
}

df.slopes.block.only = data.frame('subject' = numeric(),
                       'block' = numeric(),
                       'num_dots' = numeric(),
                       'pred' = numeric())

for (i in 1:length(fitsBlockOnly)) {
  subj.slopes = fitsBlockOnly[[i]]
  subj.slopes = subj.slopes[subj.slopes$subject %in% subjects,]
  df.slopes.block.only = rbind(df.slopes.block.only, data.frame(
    subject = subjects[1], # hacky
    block = i,
    num_dots = 1:MAX_PRESENTED,
    pred = map.bipower(1:MAX_PRESENTED, subj.slopes$a, subj.slopes$b)
  ))
}



df.slopes.block.only = df.slopes.block.only %>%
  filter(block %in% c(1, 2, 3, 10)) # select a subset of trial blocks

# POSTER FIGURE 4
dat.subject %>%
  filter(block %in% c(1, 2, 3, 10)) %>% # select a subset of trial blocks
  ggplot(aes(x = num_dots, y = answer)) +
  geom_point(alpha = 0.5, size = 3, color = "blue") +
  geom_line(data = df.slopes.block.only, aes(x = num_dots, y = pred), color = "red", size = 2) +
  mylogx(c(1, MAX_PRESENTED)) +
  mylogy(c(1, MAX_ESTIMATE)) +
  xlab("Number presented") + 
  ylab("Number reported") + 
  #ggtitle("Sample slope estimates by block") +
  ggtitle("Sample subject estimates across trial blocks") +
  #medium_plot_theme +
  individ_plot_theme +
  facet_wrap(. ~ block, ncol = 2,
             labeller = labeller(block = c("1" = "Trials 1-25", "2" = "Trials 26-75",
                                           "3" = "Trials 76-125", "10" = "Trials 426-475")))


### FIGURE: INDIVIDUAL EXAMPLE SLOPES BY BLOCK AND TRIAL MODALITY ###

subjects = c(2)
dat.subject = dat %>%
  filter(subject %in% subjects, as.numeric(block) %in% 1:11)

df.slopes = data.frame('vary' = character(),
                       'subject' = numeric(),
                       'block' = numeric(),
                       'num_dots' = numeric(),
                       'pred' = numeric())

for (i in 1:length(MODALITIES)) {
  modality = MODALITIES[[i]]
  slopes = fitsBlock[[modality]]
  for (j in 1:length(slopes)) {
    subj.slopes = slopes[[j]]
    subj.slopes = subj.slopes[subj.slopes$subject %in% subjects,]
    df.slopes = rbind(df.slopes, data.frame(
      vary = modality,
      subject = subjects[1], # hacky
      block = j,
      num_dots = 1:MAX_PRESENTED,
      pred = map.bipower(1:MAX_PRESENTED, subj.slopes$a, subj.slopes$b)
    ))
  }
}

df.slopes = df.slopes %>%
  filter(as.numeric(block) %in% 1:11)

dat.subject$block = as.numeric(dat.subject$block)
dat.subject %>%
  ggplot(aes(x = num_dots, y = answer)) +
  geom_point(alpha = 0.5, size = 1.5, color = "blue") +
  geom_line(data = df.slopes, aes(x = num_dots, y = pred), color = "red", size = 0.8) +
  mylogx(c(1, MAX_PRESENTED)) +
  mylogy(c(1, MAX_ESTIMATE)) +
  xlab("Number presented") + 
  ylab("Number reported") + 
  ggtitle("Sample slope estimates by trial block, modality") +
  medium_plot_theme +
  theme(strip.background = element_blank()) +
  facet_grid(vary~block, scales = "free",
             labeller = labeller(block = c("1" = "Block 1", "2" = "Block 2", "3" = "Block 3", "4" = "Block 4",
                                           "5" = "Block 5", "6" = "Block 6", "7" = "Block 7", "8" = "Block 8",
                                           "9" = "Block 9", "10" = "Block 10", "11" = "Block 11"),
                                 vary = c("area" = "Area trials", "size" = "Size trials", "density" = "Density trials")))


### FIGURE: SLOPE CORRELATION MATRICES, ALL COMPARISONS ###
# NB: this relies on the `mcor` matrix calculated above for the SLOPE CORRELATIONS BY BLOCK plot

# color scheme
r = 1.0
g = 0.7
b = 1
s = 1.5

size.size.label = "size-size"
size.size = create.correlation.matrix(size.size.label, rgb(r, 0, 0), mcor)
density.density.label = "density-density"
density.density = create.correlation.matrix(density.density.label, rgb(0, g, 0), mcor)
area.area.label = "area-area"
area.area = create.correlation.matrix(area.area.label, rgb(0, 0, b), mcor)

size.density.label = "size-density"
size.density = create.correlation.matrix(size.density.label, rgb(r / s, g / s, 0), mcor)
size.area.label = "size-area"
size.area = create.correlation.matrix(size.area.label, rgb(r / s, 0, b / s), mcor)
density.area.label = "density-area"
density.area = create.correlation.matrix(density.area.label, rgb(0, g / s, b / s), mcor)

multiplot(size.size, density.density, area.area, size.density, size.area, density.area, cols = 2)


### FIGURE: SLOPE CORRELATIONS BY DISTANCE ###
# NB: this relies on the correlation matrix `R` calculated above for the SLOPE CORRELATIONS BY BLOCK plot
# as well as the Rr matrix

ds = 1:10
df = NULL
dfr = NULL
for (i in 1:length(MODALITIES)) {
  for (j in i:length(MODALITIES)) {
    t = gettcor(i, j, R)
    t$distances = ds * 50
    t$type = paste(MODALITIES[i], MODALITIES[j], sep = "-")
    t2 = gettcor(i, j, Rr)
    # normalize by modular correlations to correct for overall lower correlations when the sample size is smaller for smaller set sizes
    t$nmu = t$mu #/ t2$mu
    t$nll = t$ll #/ t2$mu
    t$nul = t$ul #/ t2$mu
    df = rbind(df, t)
    t2$distances = ds * 50 # NB: switched this from 'distances * 50' to match above
    t2$type = paste(MODALITIES[i], MODALITIES[j], sep = "-")
    dfr = rbind(dfr, t2)
  }
}

r = 1.0
g = 0.7
b = 1
s = 1.5

df$type = factor(df$type, levels = c("size-size", "density-density", "area-area", "size-density", "size-area", "density-area"))
dfr$type = factor(dfr$type, levels = c("size-size", "density-density", "area-area", "size-density", "size-area", "density-area"))

# POSTER FIGURE 5
ggplot(data = df) +
  geom_point(aes(x = distances, y = nmu, color = type), size = 2) +
  geom_errorbar(aes(x = distances, ymin = nll, ymax = nul, color = type), width = 15, size = 1) +
  geom_line(aes(x = distances, y = nmu, color = type), size = 1) +
  scale_colour_manual(name = element_blank(),
                      values = c(rgb(r, 0, 0), rgb(0, g, 0), rgb(0, 0, b), 
                                 rgb(r/s, g/s, 0), rgb(r/s, 0, b/s), rgb(0, g/s, b/s))) +
  scale_x_continuous(breaks = ds * 50, minor_breaks = c()) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), minor_breaks = c()) +
  ylab("Slope correlation") + 
  xlab("Distance (trials)") + 
  ggtitle("Estimate calibration comparisons by trial distance") +
  #corr_plot_theme +
  individ_plot_theme +
  theme(legend.position = c(0.2, 0.3),
        legend.background = element_rect(fill = "gray90", 
                                         size = 0.5, linetype = "solid"))


#' Analysis (FOR SHUFFLED DATA ONLY)
#' Check that slopes of regressions fit to each correlation line are all ~0
glimpse(df)
df$dist = df$distances / 50 # get back to trial blocks so slopes are more meaningful
for (i in 1:length(MODALITIES)) {
  for (j in i:length(MODALITIES)) {
    currtype = paste(MODALITIES[i], MODALITIES[j], sep = "-")
    subset = df[df$type == currtype,]
    print(currtype)
    mod = lm(data = subset, nmu ~ dist)
    print(summary(mod))
    print(confint.lm(mod))
  }
}
# these fitted slopes are all highly significant (and very similar) for normal data
# for shuffled data, none are significantly different from 0







