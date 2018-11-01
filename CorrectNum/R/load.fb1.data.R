# fit power and bipower models, generate curves.

setwd('~/PROJECTS/number-line/CorrectNum/R/')
rm(list=ls())

fbtrials1 = 1:25

files <- list.files('../data/')

dat <- data.frame()
subject <- 1
for(f in files){
  q = read.csv2(paste('../data/', f, sep=""), sep=",", header=T, colClasses="character", skip=0)
  q$subject = subject
  q$trial = 1:nrow(q)
  q$order = ifelse(as.numeric(q$a[120])==0.75, "LU", "UL")
  dat <- rbind(dat, q)
  subject <- subject+1
}

to.num <- function(x){as.numeric(as.character(x))}
dat$trial <- to.num(dat$trial)
dat$time <- to.num(dat$time)
dat$answer <- to.num(dat$answer)
dat$num_dots <- to.num(dat$num_dots)
dat$points <- to.num(dat$points)
dat$ay <- as.character(dat$a)
dat$subject <- as.numeric(dat$subject)

# drop outliers
rs <- rbind(by(dat, dat$subject, function(tmp){cor(tmp$num_dots, tmp$answer, method="spearman")}))
dat <- dat[! dat$subject %in%  which(rs<0.6),]
ns <- rbind(by(dat, dat$subject, nrow))
dat <- dat[! dat$subject %in% which(ns < 500), ]
maxn <- max(dat$num_dots)
