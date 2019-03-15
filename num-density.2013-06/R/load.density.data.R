# fit power and bipower models, generate curves.

#setwd('~/PROJECTS/number-line/num-density.2013-06/R/')
setwd("/Users/erikbrockbank/web/vullab/numberline/num-density.2013-06/R/")
rm(list=ls())

fbtrials = 1:25

files <- list.files('../data/')

dat <- data.frame()
subject <- 1
for(f in files){
  q = read.csv2(paste('../data/', f, sep=""), sep="\t", header=T, colClasses="character", skip=1)
  q$subject = subject
  dat <- rbind(dat, q)
  subject <- subject+1
}

names(dat) <- c('trial', 't.start', 'time', 'vary', 'num_dots', 'answer', 'r.dot', 'r.space', 'n.rings', 'feedback', 'points', 'score', 'subject')

to.num <- function(x){as.numeric(as.character(x))}
dat$trial <- to.num(dat$trial)
dat$t.start <- to.num(dat$t.start)
dat$time <- to.num(dat$time)/1000
dat$answer <- to.num(dat$answer)
dat$num_dots <- to.num(dat$num_dots)
dat$r.dot <- to.num(dat$r.dot)
dat$r.space <- to.num(dat$r.space)
dat$n.rings <- to.num(dat$n.rings)
dat$feedback <- to.num(dat$feedback)
dat$points <- to.num(dat$points)
dat$score <- to.num(dat$score)
dat$vary <- as.character(dat$vary)
dat$subject <- as.numeric(dat$subject)

# drop outliers
rs <- rbind(by(dat, dat$subject, function(tmp){cor(tmp$num_dots, tmp$answer, method="spearman")}))
dat <- dat[! dat$subject %in%  which(rs<0.6),]
ns <- rbind(by(dat, dat$subject, nrow))

maxn <- max(dat$num_dots)

