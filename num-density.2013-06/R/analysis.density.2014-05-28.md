Number experiment analysis
========================================================

Experiment design
--------------------

24 subjects were presented with arrays of dots sampled from a ?? distribution.

They made two guesses about the number of dots on that trial.

Each array was presented twice (on different trials).

Read in and clean data
-------------------

(number subjects sequentially)


```r
source('~/PROJECTS/number-line/num-density.2013-06/R/load.density.data.R')
source('~/CODE/R/utilities.R')
```

Drop outliers
------


Basic Checks
-------------

What is the overall histogram of presented numbers?

```r
xlims <- c(1, maxn)
xbreaks <- my.log.breaks(xlims)
ggplot(dat, aes(x=num_dots)) +
  geom_histogram(fill="blue", binwidth=1) +
  xlab("Number") +
  ylab("Frequency presented") +
  scale_x_continuous(limits=xlims, minor_breaks=10^xbreaks[[2]])+
  mytheme
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-21.png) 

```r
ggplot(dat, aes(x=num_dots)) +
  geom_histogram(alpha=1.0, fill="blue", binwidth=1) +
  geom_histogram(data=dat, aes(x=answer), fill="red", alpha=0.5, binwidth=1) +
  xlab("Number") +
  ylab("Frequency presented") +
  scale_x_continuous(limits=xlims, minor_breaks=10^xbreaks[[2]])+
  scale_y_continuous(minor_breaks=c())+
  mytheme
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-22.png) 


Overall calibration over all subjects and subsets


```r
ylims = xlims
cuts <- c(9.5:20.5, 10^seq(log10(21.5), log10(maxn), length.out=20))
dat$bin <- cut(dat$num_dots, breaks=cuts, labels=seq_len(length(cuts)-1), include.lowest=T)
truens <- cbind(by(dat$num_dots, 
                   dat$bin, 
                   median))
medians <- cbind(by(dat$answer, 
                    dat$bin, 
                    median))
RT <- cbind(by(dat$time, 
               dat$bin, 
               function(x){median(x)}))
errsd <- cbind(by(dat, 
                  dat$bin, 
                  function(x){sd(log10(x$answer)-log10(x$num_dots))}))
acc <- cbind(by(dat, dat$bin, function(tmp){mean(tmp$num_dots == tmp$answer)}))
X <- data.frame(truens=truens[,1], medians=medians[,1], RT=RT[,1], err=1-acc[,1])
ggplot(dat, aes(x=num_dots, y=answer))+
  geom_point(colour="blue", size=5, alpha=0.05)+
  geom_point(data=X, aes(x=truens, y=medians), colour="red", size=5)+
  geom_line(data=X, aes(x=truens, y=medians), colour="red", size=2)+
  geom_abline(position="identity")+
  mylogx(xlims)+
  mylogy(ylims)+
  xlab("Number presented")+
  ylab("Median reported")+
  mytheme
```

```
## Warning: Removed 78 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

calibration only during feedback trials


```r
ylims = xlims
cuts <- c(9.5:20.5, 10^seq(log10(21.5), log10(maxn), length.out=20))
sdat <- subset(dat, dat$trial %in% fbtrials)
sdat$bin <- cut(sdat$num_dots, breaks=cuts, labels=seq_len(length(cuts)-1), include.lowest=T)
truens <- cbind(by(sdat$num_dots, 
                   sdat$bin, 
                   median))
medians <- cbind(by(sdat$answer, 
                    sdat$bin, 
                    median))
RT <- cbind(by(sdat$time, 
               sdat$bin, 
               function(x){median(x)}))
errsd <- cbind(by(sdat, 
                  sdat$bin, 
                  function(x){sd(log10(x$answer)-log10(x$num_dots))}))
acc <- cbind(by(sdat, sdat$bin, function(tmp){mean(tmp$num_dots == tmp$answer)}))
X <- data.frame(truens=truens[,1], medians=medians[,1], RT=RT[,1], err=1-acc[,1])
ggplot(sdat, aes(x=num_dots, y=answer))+
  geom_point(colour="blue", size=5, alpha=0.15)+
  geom_point(data=X, aes(x=truens, y=medians), colour="red", size=5)+
  geom_line(data=X, aes(x=truens, y=medians), colour="red", size=2)+
  geom_abline(position="identity")+
  mylogx(xlims)+
  mylogy(ylims)+
  xlab("Number presented")+
  ylab("Median reported")+
  ggtitle("Feedback trials")+
  mytheme
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


Calibration during first 100 no feedback trials


```r
subsets <- list(fbtrials, 26:125, 126:225, 226:max(dat$trial))
subnames <- c("FB", "100", "200", "rest")
ylims = xlims
cuts <- c(9.5:20.5, 10^seq(log10(21.5), log10(maxn), length.out=20))
plots = list()
X <- data.frame()
for(i in 1:length(subsets)){
  sdat <- subset(dat, dat$trial %in% subsets[[i]])
  sdat$bin <- cut(sdat$num_dots, breaks=cuts, labels=seq_len(length(cuts)-1), include.lowest=T)
  truens <- cbind(by(sdat$num_dots, 
                     sdat$bin, 
                     median))
  medians <- cbind(by(sdat$answer, 
                      sdat$bin, 
                      median))
  RT <- cbind(by(sdat$time, 
                 sdat$bin, 
                 function(x){median(x)}))
  errsd <- cbind(by(sdat, 
                    sdat$bin, 
                    function(x){sd(log10(x$answer)-log10(x$num_dots))}))
  acc <- cbind(by(sdat, sdat$bin, function(tmp){mean(tmp$num_dots == tmp$answer)}))
  X <- rbind(X, 
             data.frame(truens=truens[,1], 
                        medians=medians[,1], 
                        RT=RT[,1], 
                        err=1-acc[,1], 
                        block = rep(subnames[i], length(acc[,1]))))
}
sdat <- dat
sdat$block <- cut(sdat$trial, breaks=c(0, 25, 125, 225, max(dat$trial)), labels=subnames)
ggplot(sdat, aes(x=num_dots, y=answer))+
    geom_point(colour="blue", size=5, alpha=0.05)+
    geom_point(data=X, aes(x=truens, y=medians), colour="red", size=5)+
    geom_line(data=X, aes(x=truens, y=medians), colour="red", size=2)+
    geom_abline(position="identity")+
    mylogx(xlims)+
    mylogy(ylims)+
    xlab("Number presented")+
    ylab("Median reported")+
    ggtitle(subnames[i])+
    mytheme+
    facet_wrap(~block, ncol=4)
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
## Warning: Removed 3 rows containing missing values (geom_point).
## Warning: Removed 3 rows containing missing values (geom_point).
## Warning: Removed 71 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


Calibration during second 100 no feedback trials


```r
ylims = xlims
cuts <- c(9.5:20.5, 10^seq(log10(21.5), log10(maxn), length.out=20))
sdat <- subset(dat, dat$trial %in% 126:225)
sdat$bin <- cut(sdat$num_dots, breaks=cuts, labels=seq_len(length(cuts)-1), include.lowest=T)
truens <- cbind(by(sdat$num_dots, 
                   sdat$bin, 
                   median))
medians <- cbind(by(sdat$answer, 
                    sdat$bin, 
                    median))
RT <- cbind(by(sdat$time, 
               sdat$bin, 
               function(x){median(x)}))
errsd <- cbind(by(sdat, 
                  sdat$bin, 
                  function(x){sd(log10(x$answer)-log10(x$num_dots))}))
acc <- cbind(by(sdat, sdat$bin, function(tmp){mean(tmp$num_dots == tmp$answer)}))
X <- data.frame(truens=truens[,1], medians=medians[,1], RT=RT[,1], err=1-acc[,1])
ggplot(sdat, aes(x=num_dots, y=answer))+
  geom_point(colour="blue", size=5, alpha=0.15)+
  geom_point(data=X, aes(x=truens, y=medians), colour="red", size=5)+
  geom_line(data=X, aes(x=truens, y=medians), colour="red", size=2)+
  geom_abline(position="identity")+
  mylogx(xlims)+
  mylogy(ylims)+
  xlab("Number presented")+
  ylab("Median reported")+
  ggtitle("First 200 no-feedback trials")+
  mytheme
```

```
## Warning: Removed 3 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 
