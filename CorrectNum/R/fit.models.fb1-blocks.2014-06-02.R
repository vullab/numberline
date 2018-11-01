# fit power and bipower models, generate curves.

source('~/PROJECTS/number-line/CorrectNum/R/load.fb1.data.R')
source('~/CODE/R/utilities.R')
library(stats4)
maincol = rgb(0, 0.7, 0)
seccol = rgb(1,0,0)

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
    -loglik(tmp$num_dots, tmp$answer, usefx, a, b, 10^s) + 
      priors[[1]](a) + 
      priors[[2]](b) + 
      priors[[3]](s)
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
names(ps) <- c("ma", "sa", "mb", "sb", "ms", "ss")
priors = list()
priors[[1]] <- function(x){-dnorm(x, 1.5, 0.1, log=T)} #
priors[[2]] <- function(x){-dnorm(x, -0.2, 0.25, log=T)} #
priors[[3]] <- function(x){-dnorm(x, -0.9, 0.1, log=T)} # 


trialcuts <- seq(0, 500, by=20)
ncuts <- length(trialcuts)-1
dat$block <- cut(dat$trial, trialcuts, labels=1:ncuts, include.lowest=T)
dat$mod <- (dat$trial-1) %% ncuts+1

fitsBlock = list()
fitsMod = list()
for(k in 1:ncuts){
  A <- (dat$mod==k)
  tmp <- subset(dat, A)
  fitsMod[[k]] <- data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
  A <- (dat$block==k)
  tmp <- subset(dat, A)
  fitsBlock[[k]] <- data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
  print(c(k, sum(fitsMod[[k]]$logL==-9999), sum(fitsBlock[[k]]$logL==-9999)))
}

namedSlopes <- function(x){
  z <- data.frame(x$b)
  rownames(z) <- x$subject
  return(z)
}

cbind.fill<-function(...){
  nm <- list(...) 
  rnames<-unique(unlist(lapply(nm, rownames)))
  nm <- lapply(nm, function(x){newrows <- rnames[! rnames %in% rownames(x)]
                              newentries <- matrix(nrow=length(newrows), ncol=ncol(x))
                              rownames(newentries) <- newrows
                              colnames(newentries) <- colnames(x)
                              x <- rbind(x, newentries)
                             return(x)})
  nm <- lapply(nm, function(x){y<-data.frame(x[order(as.numeric(rownames(x))),])
                             rownames(y) <- as.character(sort(as.numeric(rownames(x))))
                             colnames(y) <- colnames(x)
                             return(y)})
  return(do.call(cbind, nm))
}

s1 <- do.call(cbind.fill, lapply(fitsBlock, namedSlopes))
s2 <- do.call(cbind.fill, lapply(fitsBlock, namedSlopes))
R = cor(s1,s2, use="pairwise.complete.obs")
rownames(R) <- c()
colnames(R) <- c()
mcor <- melt(R)
#m <- m[m$Var2>=m$Var1,]
s1 <- do.call(cbind.fill, lapply(fitsMod, namedSlopes))
s2 <- do.call(cbind.fill, lapply(fitsMod, namedSlopes))
Rr = cor(s1,s2, use="pairwise.complete.obs")
rownames(Rr) <- c()
colnames(Rr) <- c()

names(mcor) <- c("Var1", "Var2", "Correlation")
ggplot(mcor, aes(x=as.factor(Var1), y=as.factor(Var2), fill=Correlation, label = sprintf("%0.2f", Correlation))) + 
  geom_tile() + geom_text(size=5) +
  scale_fill_gradient2(low = "red", mid="white", high = "blue", midpoint=0.5, limits=c(-1,1))+
  xlab("")+ylab("")+
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  mytheme + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(angle = 90),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face="bold"))



gettcor <- function(R){
  out = data.frame()
  for(D in ds){
    rs <- fisherz(offdiagonal(R,D))
    mu <- ifisherz(mean(rs))
    se <- sd(rs)/sqrt(length(rs))
    ul <- ifisherz(fisherz(mu)+se)
    ll <- ifisherz(fisherz(mu)-se)
    out <- rbind(out, data.frame(mu=mu, se=se, ul=ul, ll=ll))
  }
  return(out)
}


ds = 1:(ncuts-1)
df = NULL
dfr = NULL
t <- gettcor(R)
t$distances = ds*20
t2 <- gettcor(Rr)
t$nmu = t$mu/t2$mu
t$nll = t$ll/t2$mu
t$nul = t$ul/t2$mu
t2$distances = ds*20

ggplot(data=t)+
  geom_point(aes(x=distances, y=mu), colour=maincol, size=5)+
  geom_linerange(aes(x=distances, ymin=ll, ymax=ul), colour=maincol, size=2)+
  geom_line(aes(x=distances, y=mu), colour=maincol, size=2)+
  geom_point(data=t2, aes(x=distances, y=mu), colour=seccol, size=5)+
  geom_linerange(data=t2, aes(x=distances, ymin=ll, ymax=ul), colour=seccol, size=2)+
  geom_line(data=t2, aes(x=distances, y=mu), colour=seccol, size=2)+
  scale_x_continuous(breaks=ds*20, minor_breaks=c())+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1), minor_breaks=c())+
  ylab("Normalized Correlation")+
  xlab("Distance (trials)")+
  mytheme

save(file='fit-block-models.Rdata', list=ls())
