# fit power and bipower models, generate curves.

source('~/PROJECTS/number-line/num-density.2013-06/R/load.density.data.R')
source('~/CODE/R/utilities.R')
library(stats4)

## define models
# simple power-law mapping
map.power <- function(x, a, b){10^(a+10^b*log10(x))}
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

usefx <- map.power
priors = list()
priors[[1]] <- function(x){0}
priors[[2]] <- function(x){-dnorm(x, 0, 0.2, log=T)} # -dnorm(x, 0, 0.2, log=T)
priors[[3]] <- function(x){-dnorm(x, -1, 0.25, log=T)} # -dnorm(x, -1, 0.5, log=T)
ps = c(0.2, 0.4, -0.3, 0.3, -0.7, 0.2)
names(ps) <- c("ma", "sa", "mb", "sb", "ms", "ss")
power.fits <- data.frame(do.call(rbind, by(dat, dat$subject, brutefit)))
print(paste("Failed power fits:", sum(power.fits$logL==-9999)))

usefx <- map.bipower
ps = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2)
priors = list()
priors[[1]] <- function(x){-dnorm(x, 2, 3.5, log=T)} #
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
  powparams <- power.fits[power.fits$subject==s,]
  powpred = (map.power(stims, powparams$a, powparams$b))
  biparams <- bipower.fits[bipower.fits$subject==s,]
  bipred = (map.bipower(stims, biparams$a, biparams$b))
  predictions <- rbind(predictions, 
                       data.frame(subject=s, 
                                  num_dots=stims, 
                                  powpred=powpred, 
                                  bipred=bipred))
}

save(file='model.fits.Rdata', bipower.fits, power.fits, predictions)

xlims <- c(1, 750)
ylims <- xlims

subjects = c(22, 27)
sdat = subset(dat, dat$subject %in% subjects)
spredictions = subset(predictions, predictions$subject %in% subjects)
ggplot(sdat, aes(x=num_dots, y=answer))+
  geom_point(alpha=0.25, color="blue", size=6)+
  geom_line(data=spredictions, aes(x=num_dots, y=powpred), color=rgb(1, 0, 0), size=3)+
  geom_line(data=spredictions, aes(x=num_dots, y=bipred), color=rgb(0, 0.7, 0), size=3)+
  geom_abline(position="identity")+
  mylogx(c(1,300))+
  mylogy(c(1,300))+
  xlab("Number presented")+
  ylab("Number reported")+
  #mytheme+
  facet_wrap(~subject, ncol=2)




new_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 32),
  axis.title.y = element_text(face = "bold", size = 32),
  axis.title.x = element_text(face = "bold", size = 32),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  # legend text
  legend.text = element_text(size = 14),
  # background color, axis lines
  panel.background = element_blank(),
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)
subjects = c(8, 27, 49)
sdat = subset(dat, dat$subject %in% subjects)
spredictions = subset(predictions, predictions$subject %in% subjects)
ggplot(sdat, aes(x=num_dots, y=answer))+
  geom_point(alpha=0.25, color="blue", size=6)+
  #geom_line(data=spredictions, aes(x=num_dots, y=powpred), color=rgb(1, 0, 0), size=3)+
  #geom_line(data=spredictions, aes(x=num_dots, y=bipred), color=rgb(0, 0.7, 0), size=3)+
  geom_abline(position="identity")+
  mylogx(c(1,750))+
  mylogy(c(1,750))+
  xlab("Number presented")+
  ylab("Number reported")+
  #new_theme +
  #mytheme+
  facet_wrap(~subject, ncol=3) +
  ggtitle("Estimation data for sample participants") +
  theme(
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
  
