### Analysis script for Cog Sci numberline revisions, first year project paper, and larger paper submission ###
#' This has overlap with other analysis files in this directory but represents a cleaner, final version
#' The process of reading in, processing, and cleaning in data is mostly taken from `load.density.data.R`
#' The bulk of the analysis is taken from `fit.models.density-blocks.2014-06-02.R`, 
#' with some code taken from `fit.models.density.2014-06-02.R`
#' 



setwd("/Users/erikbrockbank/web/vullab/numberline/num-density.2013-06/R/")
rm(list=ls())

library(reshape)
library(psych) # needed for fisherz
library(HardyWeinberg) # needed for ifisherz


### GLOBALS ###
FEEDBACK_TRIALS = 1:25 # trials in which feedback was given
COLNAMES = c('trial', 't.start', 'time', 'vary', 'num_dots', 'answer', 'r.dot', 'r.space', 'n.rings', 'feedback', 
             'points', 'score', 'subject') # names of columns for reading in data
COR_THRESHOLD = 0.6 # minimum correlation threshold between participant answers and true num_dots (for dropping outliers)
MAX_PRESENTED = 750 # highest number presented during the task
MAX_ESTIMATE = 1000 # max value for displaying number reported (note some estimates were larger than this)

PARAMS = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2)
names(PARAMS) = c("ma", "sa", "mb", "sb", "ms", "ss")
PRIORS = list()
PRIORS[[1]] = function(x){-dnorm(x, 1.5, 0.1, log = T)} #
PRIORS[[2]] = function(x){-dnorm(x, -0.2, 0.25, log = T)} #
PRIORS[[3]] = function(x){-dnorm(x, -0.9, 0.1, log = T)} # 


### FUNCTIONS ###
# Data processing functions
to.num = function(x){as.numeric(as.character(x))}

# Modeling functions

# These functions modified from `fit.models.density-blocks.2014-06-02.R`
# NB: these are very similar to functions used in `erikb.analysis.noisy_perception_drift.R`
map.bipower = function(x, a, b){
  crit = a
  slope = 10 ^ b
  lx = log10(x)
  ly = ((lx > crit) * (crit + (lx - crit) * slope) + (lx <= crit) * lx);
  return(10 ^ ly)
}

## general log likelihood function (with robustness)
loglik = function(x, y, map.fx, a, b, s){
  sum(
    pmax(-6, dnorm(log10(y) - log10(map.fx(x, a, b)), 0, s, log = T))
  )
}

# fit power function
brutefit = function(tmp){
  nLL = function(a, b, s){
    -loglik(tmp$num_dots, tmp$answer, map.bipower, a, b, 10 ^ s) + # NB: can use other function instead of `map.bipower`
      PRIORS[[1]](a) + 
      PRIORS[[2]](b) + 
      PRIORS[[3]](s)
  }
  
  iter = 0
  fits = NULL
  fit = NULL
  while (is.null(fits)){
    try(fit <- summary(mle(nLL, 
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




# Graphing functions

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
  minors = log10(unlist(lapply(majors[-1], function(x){seq(10^(x - 1), 9 * 10 ^ (x - 1), by = 10 ^ (x - 1))})))
  return(list(majors, minors))
}

mylogx = function(lims){
  breaks = my.log.breaks(lims)
  scale_x_log10(limits = lims, 
                breaks = 10 ^ breaks[[1]], 
                minor_breaks = breaks[[2]])
}

mylogy = function(lims){
  breaks <- my.log.breaks(lims)
  scale_y_log10(limits = lims, 
                breaks = 10 ^ breaks[[1]], 
                minor_breaks = breaks[[2]])
}

# theme
individ_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 32),
  axis.title.y = element_text(face = "bold", size = 32),
  axis.title.x = element_text(face = "bold", size = 32),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 16),
  axis.text.x = element_text(size = 14, angle = 90, hjust = 0, vjust = 0),
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
# The below is mostly adapted from load.density.data.R in this same directory

files = list.files('../data/')
dat = data.frame()
subject = 1
for (f in files) {
  q = read.csv2(paste('../data/', f, sep = ""), sep = "\t", header = T, colClasses = "character", skip = 1)
  q$subject = subject
  dat = rbind(dat, q)
  subject = subject + 1
}

names(dat) = COLNAMES


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

MODALITIES = unique(dat$vary)

glimpse(dat)
length(unique(dat$subject))

# Drop outliers
rs = rbind(by(dat, dat$subject, function(tmp){cor(tmp$num_dots, tmp$answer, method = "spearman")}))
dat = dat[!(dat$subject %in%  which(rs < COR_THRESHOLD)),] # drop subjects w/ less than COR_THRESHOLD correlation with true num_dots
ns = rbind(by(dat, dat$subject, nrow)) # number of observations for each included subject

glimpse(dat)
length(unique(dat$subject))
ns

# TODO what's going on with these outliers?? did people really guess 34,000??
tmp = order(dat$answer, decreasing = T)
tmp[1:8]
# also why doesn't this show up in the above?
max(dat$answer)
dat[dat$answer == max(dat$answer),]



### FIGURE: INDIVIDUAL DATA ###
# graphs example data for three individual subjects, chosen below
subjects = c(8, 27, 49) # Sample subjects
sdat = subset(dat, dat$subject %in% subjects)

ggplot(sdat, aes(x = num_dots, y = answer)) +
  geom_point(alpha = 0.25, color = "blue", size = 6) +
  geom_abline(position = "identity") +
  mylogx(c(1, MAX_PRESENTED)) +
  mylogy(c(1, MAX_ESTIMATE)) +
  ggtitle("Estimation data for sample participants") +
  xlab("Number presented") +
  ylab("Number reported") +
  facet_wrap(~subject, ncol = 3) +
  individ_plot_theme



### FIGURE: PERFORMANCE BY MODALITY ###

# Get median reported answers across log-equidistant blocks of number presented (for each modality)
cuts = c(9.5:20.5, 10 ^ seq(log10(21.5), log10(MAX_PRESENTED), length.out = 20)) # TODO clarify what's happening here
summary.stats = data.frame()
for (i in 1:length(MODALITIES)) {
  sdat = subset(dat, dat$vary == MODALITIES[i])
  sdat$bin = cut(sdat$num_dots, breaks = cuts, labels = seq_len(length(cuts) - 1), include.lowest = T)
  truens = cbind(by(sdat$num_dots, sdat$bin, median))
  medians = cbind(by(sdat$answer, sdat$bin, median))
  acc = cbind(by(sdat, sdat$bin, function(tmp){mean(tmp$num_dots == tmp$answer)}))
  summary.stats = rbind(summary.stats,
             data.frame(truens = truens[,1],
                        medians = medians[,1],
                        block = rep(MODALITIES[i], length(acc[,1]))))
}

ggplot(dat, aes(x = num_dots, y = answer)) +
  geom_point(colour = "blue", size = 2, alpha = 0.05) +
  geom_point(data = summary.stats, aes(x = truens, y = medians), color = "red", size = 2) +
  geom_line(data = summary.stats, aes(x = truens, y = medians), color = "red", size = 1) +
  geom_abline(position = "identity") +
  mylogx(c(1, MAX_PRESENTED)) +
  mylogy(c(1, MAX_ESTIMATE)) +
  xlab("Number presented") + 
  ylab("Number reported") + 
  ggtitle("Accuracy across estimate conditions") +
  individ_plot_theme +
  facet_wrap(~vary, ncol = 3,
             labeller = labeller(vary = c("area" = "area", "size" = "size", "density" = "density")))


### FIGURE: SLOPE CORRELATIONS BY BLOCK ###

# Fit slope data
trialcuts = c(0, 25, 75, 125, 175, 225, 275, 325, 375, 425, 475, 525, 575, 1000) # TODO clarify what's happening here
dat$block = cut(dat$trial, trialcuts, labels = 0:(length(trialcuts) - 2), include.lowest = T)
# TODO what's going on with mod logic here?
dat$mod = (dat$trial - 26) %% 11 + 1
dat$mod[dat$block == 0] = 0
dat$mod[dat$block == (length(trialcuts) - 2)] = (length(trialcuts) - 2)

fitsBlock = list()
fitsMod = list() # NB: we only use fitsMod for final plot

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


# Get slope correlation matrix
R = list() # primary slope correlation matrix
Rr = list() # NB: Rr only gets used for very final plot
mcor = data.frame()
mconf = data.frame("Type" = character(), "Block1" = numeric(), "Block2" = numeric(), "Lower.conf" = numeric(), "Upper.conf" = numeric())
for (i in 1:length(MODALITIES)) {
  R[[MODALITIES[i]]] = list() # make separate slope correlations for each trial type
  for (j in i:length(MODALITIES)) {
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
    R[[MODALITIES[i]]][[MODALITIES[j]]] = cor(s1, s2, use = "pairwise.complete.obs")
    rownames(R[[MODALITIES[i]]][[MODALITIES[j]]]) = c()
    colnames(R[[MODALITIES[i]]][[MODALITIES[j]]]) = c()
    m = melt(R[[MODALITIES[i]]][[MODALITIES[j]]][2:12,2:12]) # TODO make this less arbitrary
    names(m) = c("Block1", "Block2", "Correlation") # correlation of slopes
    m = m[m$Block2 >= m$Block1,] # keep only top half of correlation matrix
    m$type = paste(MODALITIES[i], MODALITIES[j], sep = "-")
    mcor = rbind(mcor, m)
    
    s1 = do.call(cbind.fill, lapply(fitsMod[[MODALITIES[i]]], namedSlopes))
    s2 = do.call(cbind.fill, lapply(fitsMod[[MODALITIES[j]]], namedSlopes))
    Rr[[MODALITIES[i]]][[MODALITIES[j]]] = cor(s1, s2, use = "pairwise.complete.obs")
    rownames(Rr[[MODALITIES[i]]][[MODALITIES[j]]]) <- c()
    colnames(Rr[[MODALITIES[i]]][[MODALITIES[j]]]) <- c()
  }
}
names(mcor) = c("Block1", "Block2", "Correlation", "type")
mcor$type = as.character(mcor$type)

# Validating the above
mcor
levels(as.factor(mcor$type))


# Plot
blocks = 0:11
ggplot() +
  geom_point(aes(x = blocks, y = get.slope.cors(R, "size", "density"), color = "size-density"), size = 2) +
  geom_line(aes(x = blocks, y = get.slope.cors(R, "size", "density"), color = "size-density"), size = 1) +
  geom_errorbar(aes(x = blocks, 
                    ymin = get.slope.conf(mconf, "size", "density")$lower,
                    ymax = get.slope.conf(mconf, "size", "density")$upper,
                    color = "size-density"),
                width = 0.25, size = 1) +
  geom_point(aes(x = blocks, y = get.slope.cors(R, "size", "area"), color = "size-area"), size = 2) +
  geom_line(aes(x = blocks, y = get.slope.cors(R, "size", "area"), color = "size-area"), size = 1) +
  geom_errorbar(aes(x = blocks, 
                    ymin = get.slope.conf(mconf, "size", "area")$lower,
                    ymax = get.slope.conf(mconf, "size", "area")$upper,
                    color = "size-area"),
                width = 0.25, size = 1) +
  geom_point(aes(x = blocks, y = get.slope.cors(R, "density", "area"), color = "density-area"), size = 2) +
  geom_line(aes(x = blocks, y = get.slope.cors(R, "density", "area"), color = "density-area"), size = 1) +
  geom_errorbar(aes(x = blocks, 
                    ymin = get.slope.conf(mconf, "density", "area")$lower,
                    ymax = get.slope.conf(mconf, "density", "area")$upper,
                    color = "density-area"),
                width = 0.25, size = 1) +
  ggtitle("Slope correlations by trial block") +
  ylab("Correlation") + 
  xlab("Block") + 
  scale_x_continuous(breaks = blocks, minor_breaks = c()) +
  scale_y_continuous(limits = c(-0.2, 1), breaks = seq(-0.2, 1, by = 0.2)) +
  corr_plot_theme +
  scale_color_manual(name = "slope comparison",
                     values = c("size-density" = "red", "size-area" = "blue", "density-area" = "green4"),
                     labels = c("size-density" = "size-density", "size-area" = "size-area", "density-area" = "density-area"))


### FIGURE: SLOPE CORRELATION MATRICES, ALL COMPARISONS ###
# NB: this relies on the `mcor` matrix calculated above for the SLOPE CORRELATIONS BY BLOCK plot

# color scheme
r = 1.0
g = 0.7
b = 1
s = 1.5


size.size.label = "size-size"
size.size = ggplot(subset(mcor, mcor$type == size.size.label), 
                   aes(x = as.factor(Block1), y = as.factor(Block2), 
                       fill = Correlation, label = sprintf("%0.2f", Correlation))) + 
  geom_tile() + 
  geom_text(size = 5) +
  scale_fill_gradient2(low = "white", mid = "white", high = rgb(r, 0, 0), midpoint = 0, limits=c(-1, 1)) +
  xlab("") +
  ylab("") +
  ggtitle(size.size.label) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  matrix_plot_theme +
  theme(plot.title = element_text(face = "bold", size = 32, color = rgb(r, 0, 0)))

density.density.label = "density-density"
density.density = ggplot(subset(mcor, mcor$type == density.density.label), 
                     aes(x = as.factor(Block1), y = as.factor(Block2), 
                         fill = Correlation, label = sprintf("%0.2f", Correlation))) + 
  geom_tile() + 
  geom_text(size = 5) +
  scale_fill_gradient2(low = "white", mid = "white", high = rgb(0, g, 0), midpoint = 0, limits = c(-1, 1)) +
  xlab("") +
  ylab("") +
  ggtitle(density.density.label) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  matrix_plot_theme +
  theme(plot.title = element_text(face = "bold", size = 32, color = rgb(0, g, 0)))

area.area.label = "area-area"
area.area = ggplot(subset(mcor, mcor$type == area.area.label), 
                   aes(x = as.factor(Block1), y = as.factor(Block2), 
                       fill = Correlation, label = sprintf("%0.2f", Correlation))) + 
  geom_tile() + 
  geom_text(size = 5) +
  scale_fill_gradient2(low = "white", mid = "white", high = rgb(0, 0, b), midpoint = 0, limits = c(-1, 1)) +
  xlab("") +
  ylab("") +
  ggtitle(area.area.label) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  matrix_plot_theme +
  theme(plot.title = element_text(face = "bold", size = 32, color = rgb(0, 0, b)))

size.density.label = "size-density"
size.density = ggplot(subset(mcor, mcor$type == size.density.label), 
                      aes(x = as.factor(Block1), y = as.factor(Block2), 
                          fill = Correlation, label = sprintf("%0.2f", Correlation))) + 
  geom_tile() + 
  geom_text(size = 5) +
  scale_fill_gradient2(low = "white", mid = "white", high = rgb(r/s, g/s, 0), midpoint = 0, limits = c(-1, 1)) +
  xlab("") +
  ylab("") +
  ggtitle(size.density.label) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  matrix_plot_theme +
  theme(plot.title = element_text(face = "bold", size = 32, color = rgb(r/s, g/s, 0)))

size.area.label = "size-area"
size.area = ggplot(subset(mcor, mcor$type == size.area.label), 
                   aes(x = as.factor(Block1), y = as.factor(Block2), 
                       fill = Correlation, label = sprintf("%0.2f", Correlation))) + 
  geom_tile() + 
  geom_text(size = 5) +
  scale_fill_gradient2(low = "white", mid ="white", high = rgb(r/s, 0, b/s), midpoint = 0, limits = c(-1, 1)) + 
  xlab("") +
  ylab("") +
  ggtitle(size.area.label) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  matrix_plot_theme +
  theme(plot.title = element_text(face = "bold", size = 32, color = rgb(r/s, 0, b/s)))

density.area.label = "density-area"
density.area = ggplot(subset(mcor, mcor$type == density.area.label), 
                      aes(x = as.factor(Block1), y = as.factor(Block2), 
                          fill = Correlation, label = sprintf("%0.2f", Correlation))) + 
  geom_tile() + 
  geom_text(size = 5) +
  scale_fill_gradient2(low = "white", mid = "white", high = rgb(0, g/s, b/s), midpoint = 0, limits = c(-1, 1)) +
  xlab("") +
  ylab("") +
  ggtitle(density.area.label) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  matrix_plot_theme +
  theme(plot.title = element_text(face = "bold", size = 32, color = rgb(0, g/s, b/s)))

multiplot(size.size, density.density, area.area, size.density, size.area, density.area, cols = 2)



### FIGURE: SLOPE CORRELATIONS BY DISTANCE ###
# NB: this relies on the correlation matrix `R` calculated above for the SLOPE CORRELATIONS BY BLOCK plot
# as well as the Rr matrix


# TODO what's going on here???
fullD = data.frame()
ds = 1:10
df = NULL
dfr = NULL
for (i in 1:length(MODALITIES)) {
  for (j in i:length(MODALITIES)) {
    t = gettcor(i, j, R)
    t$distances = ds * 50
    t$type = paste(MODALITIES[i], MODALITIES[j], sep = "-")
    t2 = gettcor(i, j, Rr) 
    t$nmu = t$mu / t2$mu
    t$nll = t$ll / t2$mu
    t$nul = t$ul / t2$mu
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
ggplot(data = df) +
  geom_point(aes(x = distances, y = nmu, color = type), size = 2) +
  geom_errorbar(aes(x = distances, ymin = nll, ymax = nul, color = type), width = 15, size = 1) +
  geom_line(aes(x = distances, y = nmu, color = type), size = 1) +
  scale_colour_manual(name = "slope comparison",
                      values = c(rgb(r, 0, 0), rgb(0, g, 0), rgb(0, 0, b), 
                                 rgb(r/s, g/s, 0), rgb(r/s, 0, b/s), rgb(0, g/s, b/s))) +
  scale_x_continuous(breaks = ds * 50, minor_breaks = c()) +
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1.1, by = 0.1), minor_breaks = c()) +
  ylab("Correlation") + 
  xlab("Distance (trials)") + 
  ggtitle("Slope correlations by trial distance") +
  theme(legend.position = "bottom") +
  corr_plot_theme
  