# fit power and bipower models, generate curves.

source('~/PROJECTS/number-line/CorrectNum/R/load.fb1.data.R')
source('~/CODE/R/utilities.R')
library(stats4)
maincol = rgb(0, 0.7, 0)
seccol = rgb(1,0,0)

## define models
# bi-linear power-law mapping
map.bipower <- function(x, a, b){
  crit <- a
  slope <- 10^b
  lx <- log10(x)
  ly <- ((lx>crit)*(crit+(lx-crit)*slope)+(lx<=crit)*lx);
  return(10^ly)
}

## general log likelihood function (with robustness)
loglik <- function(x, y, map.fx, a, b, s, p){
  sum(
    pmax(-6, dnorm(log10(y)-log10(map.fx(x, a, b)), 0, s, log=T))
  )
}


# fit power function

brutefit <- function(tmp){
  nLL <- function(a, b, s){
    -loglik(tmp$num_dots, tmp$answer, usefx, a, b, 10^s) + priors[[1]](a) + priors[[2]](b) + priors[[3]](s)
  }
  
  iter = 0
  fits = NULL
  fit = NULL
  while(is.null(fits)){
    try(fit <- summary(mle(nLL, 
                           start=list(a=runif(1, ps["ma"], ps["sa"]), 
                                      b=runif(1, ps["mb"], ps["sb"]), 
                                      s=rnorm(1, ps["ms"], ps["ss"])))), TRUE)
    iter = iter+1
    
    if(! is.null(fit)){
      fits <- c(tmp$subject[1], -0.5*fit@m2logL, length(tmp$num_dots), fit@coef[,"Estimate"])
    } else {
      if(iter>50){
        fits <- c(tmp$subject[1], -9999, 0, 0, 0, 0)
      }      
    }
  }
  names(fits) <- c("subject", "logL", "n", "a", "b", "s")
  return(fits)
}

usefx <- map.bipower
ps = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2)
priors = list()
priors[[1]] <- function(x){-dnorm(x, 3, 3.5, log=T)} #
priors[[2]] <- function(x){-dnorm(x, 0, 0.5, log=T)} #
priors[[3]] <- function(x){-dnorm(x, -1, 0.25, log=T)} # 
#priors[[1]] <- function(x){0}
#priors[[2]] <- function(x){0}
names(ps) <- c("ma", "sa", "mb", "sb", "ms", "ss")
bipower.fits <- data.frame(do.call(rbind, by(dat, dat$subject, brutefit)))
print(paste("Failed bipower fits:", sum(bipower.fits$logL==-9999)))

predictions <- data.frame()
for(s in unique(dat$subject)){
  stims <- seq(1,300,by=1)
  biparams <- bipower.fits[bipower.fits$subject==s,]
  bipred = (map.bipower(stims, biparams$a, biparams$b))
  predictions <- rbind(predictions, 
                       data.frame(subject=s, 
                                  num_dots=stims, 
                                  bipred=bipred))
}

save(file='model.fits.Rdata', bipower.fits, predictions)

xlims <- c(1, maxn)
ylims <- xlims

subjects = unique(dat$subject)#c(22, 27)
sdat = subset(dat, dat$subject %in% subjects)
spredictions = subset(predictions, predictions$subject %in% subjects)
ggplot(sdat, aes(x=num_dots, y=answer))+
  geom_point(alpha=0.25, color=maincol, size=2)+
  geom_line(data=spredictions, aes(x=num_dots, y=bipred), color=seccol, size=2)+
  geom_abline(position="identity")+
  mylogx(c(1,300))+
  mylogy(c(1,300))+
  xlab("Number presented")+
  ylab("Number reported")+
  mytheme+
  facet_wrap(~subject, ncol=6)
