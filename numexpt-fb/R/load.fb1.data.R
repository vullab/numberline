# fit power and bipower models, generate curves.

setwd('~/PROJECTS/number-line/numexpt-fb/R/')
rm(list=ls())

files <- list.files('../data/')

dat <- data.frame()
subject <- 1
for(f in files){
  q = read.csv2(paste('../data/', f, sep=""), sep=",", header=F, colClasses="character", skip=1)
  names(q) <- c("part", "num_dots", "answer", "feedback", "a", "fbnum", "points", "time")
  q$subject = subject
  q$trial = 1:nrow(q)
  q$order = ifelse(as.numeric(q$a[151])==0.9, "LU", "UL")
  dat <- rbind(dat, q)
  subject <- subject+1
}

to.num <- function(x){as.numeric(as.character(x))}
dat$trial <- to.num(dat$trial)
dat$time <- to.num(dat$time)
dat$answer <- to.num(dat$answer)
dat$fbnum <- to.num(dat$fbnum)
dat$num_dots <- to.num(dat$num_dots)
dat$points <- to.num(dat$points)
dat$a <- as.numeric(dat$a)
dat$subject <- as.numeric(dat$subject)
dat$order <- as.character(dat$order)

# drop outliers
rs <- rbind(by(dat, dat$subject, function(tmp){cor(tmp$num_dots, tmp$answer, method="spearman")}))
ns <- rbind(by(dat$trial, dat$subject, max))
dat <- dat[! dat$subject %in%  which(rs<0.6),]
dat <- dat[! dat$subject %in% which(ns < 700), ]
maxn <- max(dat$num_dots)

maincol = rgb(0, 0.7, 0)
seccol = rgb(1,0,0)
