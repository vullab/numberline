# fit power and bipower models, generate curves.

setwd('~/PROJECTS/number-line/numexpt/R/')
rm(list=ls())

source('~/CODE/R/utilities.R')
library(stats4)


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
dat$answer <- 10^(log10(pmax(1, as.numeric(dat$answer1)))/2+log10(pmax(1, as.numeric(dat$answer2)))/2)
dat$trial = 0
for(s in unique(dat$subject)){
  dat$trial[dat$subject==s] = 1:300
}

map.bipower <- function(x, a, b){
  crit <- a
  slope <- 10^b
  lx <- log10(x)
  ly <- ((lx>crit)*(crit+(lx-crit)*slope)+(lx<=crit)*lx);
  return(10^ly)
}

burp <- function(R0, N0, N1, k, a){
  sn0 = k*N0^a
  sn1 = k*N1^a
  w0 = sn1^2/(sn1^2+sn0^2 ) #+(R0-N1)^2
  R1 = (1-w0)*rnorm(length(N1), N1, sn1)+w0*R0
  return(R1)
}

thisburp <- function(k, a){
  k <- 10^k
  a <- 10^a
  R0 <- tmp$answer[-(length(tmp$answer))]
  N0 <- tmp$num_dots[-(length(tmp$answer))]
  N1 <- tmp$num_dots[-1]
  R1 <- burp(R0, N0, N1, k, a)
  return(list(tmp$answer[-1], R1))
}


logistic = function(x){1/(1+exp(-x))}
murp <- function(R0, N0, N1, a, b){
  crit <- 1.06
  slope <- 10^a
  b <- 10^b
  lN1 <- log10(N1)
  lN0 <- log10(N0)
  lR0 <- log10(R0)
  R1 = c()
  for(i in 1:length(N1)){
    if(lN0[i] > crit){
      s0 <- (lR0[i]-crit)/(lN0[i]-crit)
      p0 <- (1-exp(-((lN0[i]-crit)/b)^2))
      s1 = s0*p0+slope*(1-p0)
    }
    R1[i] <- 10^((lN1[i]>crit)*(crit+(lN1[i]-crit)*(s1)) + (lN1[i]<=crit)*lN1[i])
  }
  return(R1)
}

thismurp <- function(a, b){
  R0 <- tmp$answer[-(length(tmp$answer))]
  N0 <- tmp$num_dots[-(length(tmp$answer))]
  N1 <- tmp$num_dots[-1]
  R1 <- murp(R0, N0, N1, a, b)
  return(list(tmp$answer[-1], R1))
}


tmp <- subset(dat, dat$subject==13)

nLL <- function(a, b){
  resp <- thismurp(a, b)
  return(sum((resp[[1]]-resp[[2]])^2))
}

fit <- summary(mle(nLL, start=list(a=-0.3, #runif(1, -2, 0)
                       b=-1)))      # runif(1, -2, 2)

F <- fit@coef[,"Estimate"]
resp <- thismurp(F[1], F[2])

xlims <- c(1, max(tmp$num_dots))
plot(tmp$num_dots[-1], resp[[2]], log="xy", xlim=xlims, ylim=xlims)
points(tmp$num_dots[-1], resp[[1]], col='red')
lines(c(1, max(tmp$num_dots[-1])), c(1, max(tmp$num_dots[-1])), col='blue')

xlims <- c(1, max(resp[[1]]))
plot(resp[[1]], resp[[2]], log="xy", xlim=xlims, ylim=xlims)
lines(c(1, max(tmp$num_dots[-1])), c(1, max(tmp$num_dots[-1])), col='blue')


