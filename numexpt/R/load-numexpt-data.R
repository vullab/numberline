setwd('~/PROJECTS/number-line/numexpt/R/')
rm(list=ls())
source('~/CODE/R/utilities.R')
source('~/PROJECTS/number-line/numerline-utilities.R')


## load all data and clean up format.
files <- list.files('../data/')
dat <- data.frame()
subject <- 1
for(f in files){
  q = read.csv2(paste('../data/', f, sep=""), sep=",", header=T, colClasses="character")
  q$subject = subject
  dat <- rbind(dat, q)
  subject <- subject+1
}
to.num <- function(x){as.numeric(as.character(x))}
dat$num_dots <- to.num(dat$num_dots)
dat$answer1 <- as.numeric(dat$answer1)
dat$answer2 <- as.numeric(dat$answer2)
dat$answer <- 10^(log10(pmax(1, as.numeric(dat$answer1)))/2+log10(pmax(1, as.numeric(dat$answer2)))/2)
dat$time <- as.numeric(dat$time)

subjects <- unique(dat$subject)
maxn <- max(dat$num_dots)

dat$trial = 0
for(s in subjects){
  dat$trial[dat$subject==s] = 1:300
}

maincol = "red"
seccol = "blue"