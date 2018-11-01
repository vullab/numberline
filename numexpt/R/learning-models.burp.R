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


burp <- function(R0, N0, N1, a, b){
  sn0 = b*N0^a
  sn1 = b*N1^a
  w0 = sn1^2/(sn1^2+sn0^2 ) #+(R0-N1)^2
  R1 = (1-w0)*rnorm(length(N1), N1, 0.01)+w0*R0
  return(R1)
}

thisburp <- function(a, b){
  a <- 10^a
  b <- 10^b
  R0 <- tmp$answer[-(length(tmp$answer))]
  N0 <- tmp$num_dots[-(length(tmp$answer))]
  N1 <- tmp$num_dots[-1]
  R1 <- pmax(1, burp(R0, N0, N1, a, b))
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
      slope = s0*p0+slope*(1-p0)
    }
    R1[i] <- 10^((lN1[i]>crit)*(crit+(lN1[i]-crit)*(slope)) + (lN1[i]<=crit)*lN1[i])
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


dat$subject <- as.numeric(as.character(dat$subject))
load(file='model.fits.Rdata')
SUBS <- unique(dat$subject)
tfits <- data.frame()
fake = data.frame()
for(i in 1:length(SUBS)){
  tmp <- subset(dat, dat$subject==SUBS[i])
  print(c(i, tmp$subject[1]))
  
  nLL <- function(a, b){
    resp <- thisburp(a, b)
    return(sum(pmin(4, (log10(resp[[1]])-log10(resp[[2]]))^2)))
  }
  
  iter = 0
  fits = NULL
  fit = NULL
  while(is.null(fits)){
    try(fit <- summary(mle(nLL, start=list(a=rnorm(1, -0.6, 0.5), #runif(1, -2, 0)
                                           b=rnorm(1, 0, 0.2)))), TRUE)
    iter = iter+1
    
    if(! is.null(fit)){
      fits <- c(tmp$subject[1], -0.5*fit@m2logL, length(tmp$num_dots), fit@coef[,"Estimate"], fit@coef[, "Std. Error"])
    } else {
      if(iter>50){
        fits <- c(tmp$subject[1], -9999, 0, 0, 0, 0, 0, 0, 0)
      }      
    }
  }
  
  names(fits) <- c("subject", "logL", "n", "a", "b", "sa", "sb")
  if(! (fits["logL"] == -999)){
    resp <- thisburp(fits["a"], fits["b"])
    tmp$fanswer <- c(tmp$answer[1], resp[[2]])
    fake <- rbind(fake, tmp)
  } else {
    print(c("failed on ", subject[i]))
  }
  tfits <- rbind(tfits, fits)
}
names(tfits) <- c("subject", "logL", "n", "a", "b", "sa", "sb")

save(file='fake3data.Rdata', fake, tfits)