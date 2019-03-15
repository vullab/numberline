# fit power and bipower models, generate curves.

source('~/PROJECTS/number-line/num-density.2013-06/R/load.density.data.R')
source('~/CODE/R/utilities.R')
library(stats4)


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


trialcuts <- c(0, 25, 75, 125, 175, 225, 275, 325, 375, 425, 475, 525, 575, 1000)
dat$block <- cut(dat$trial, trialcuts, labels=0:(length(trialcuts)-2), include.lowest=T)
dat$mod <- (dat$trial-26) %% 11 + 1
dat$mod[dat$block==0] = 0
dat$mod[dat$block==(length(trialcuts)-2)] = (length(trialcuts)-2)

vary <- unique(dat$vary)
fitsBlock = list()
fitsMod = list()
dcor = list()
for(i in 1:length(vary)){
  fitsBlock[[i]] = list()
  fitsMod[[i]] = list()
  for(k in 0:(length(trialcuts)-2)){
    A <- (dat$vary==vary[i]) & (dat$mod==k)
    tmp <- subset(dat, A)
    fitsMod[[i]][[k+1]] <- data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
    A <- (dat$vary==vary[i]) & (dat$block==k)
    tmp <- subset(dat, A)
    fitsBlock[[i]][[k+1]] <- data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
    print(c(i, k, sum(fitsMod[[i]][[k+1]]$logL==-9999), sum(fitsBlock[[i]][[k+1]]$logL==-9999)))
  }
}

# TODO namedSlopes not declared prior to call below?
# NB: works with 
load('fit-block-models.Rdata')
library(reshape)

SLOPES = data.frame()
for(i in 1:length(vary)){
  slopes <- data.frame(do.call(cbind.fill, lapply(fitsBlock[[i]], namedSlopes)))
  slopes$subject <- rownames(s1)
  slopes <- melt(slopes)
  names(slopes) <- c("subject", "block", "slope")
  slopes$vary = vary[i]
  levels(slopes$block) <- 0:(length(trialcuts)-2)
  SLOPES <- rbind(SLOPES, slopes)
}

# TODO erikb this doesn't seem to work but code after it runs fine

ggplot(SLOPES, aes(x=as.numeric(block), y=slope, color=vary, fill=vary)) +
  stat_summary(fun.data=mean_se, 
               geom="ribbon", 
               alpha=0.5, colour=NA) +
  geom_point(size=5, alpha=0.25) +
  geom_line(aes(group=subject), size=0.5, alpha=0.25)+
  scale_color_manual(values = c(rgb(0,0,1), rgb(1,0,0), rgb(0,0.7,0)))+
  scale_fill_manual(values = c(rgb(0,0,1), rgb(1,0,0), rgb(0,0.7,0)))+
  stat_summary(fun.y=mean, geom="line", size=2)+
  scale_x_continuous(minor_breaks=c())+
  scale_y_continuous(minor_breaks=c())+
  xlab("Block")+
  mytheme

R = list()
Rr = list()
mcor= data.frame()
for(i in 1:3){
  R[[i]] = list()
  Rr[[i]] = list()
  for(j in i:3){
    s1 <- do.call(cbind.fill, lapply(fitsBlock[[i]], namedSlopes))
    s2 <- do.call(cbind.fill, lapply(fitsBlock[[j]], namedSlopes))
    R[[i]][[j]] = cor(s1,s2, use="pairwise.complete.obs")
    rownames(R[[i]][[j]]) <- c()
    colnames(R[[i]][[j]]) <- c()
    m <- melt(R[[i]][[j]][2:12,2:12])
    names(m) <- c("Var1", "Var2", "value")
    m <- m[m$Var2>=m$Var1,]
    m$type <- paste(vary[i],vary[j], sep="-")
    mcor <- rbind(mcor, m)
    s1 <- do.call(cbind.fill, lapply(fitsMod[[i]], namedSlopes))
    s2 <- do.call(cbind.fill, lapply(fitsMod[[j]], namedSlopes))
    Rr[[i]][[j]] = cor(s1,s2, use="pairwise.complete.obs")
    rownames(R[[i]][[j]]) <- c()
    colnames(R[[i]][[j]]) <- c()
  }
}
names(mcor) <- c("Var1", "Var2", "Correlation", "type")
mcor$type <- as.character(mcor$type)

r = 1.0
g = 0.7
b = 1
s = 1.5
# c("size-size", "space-space", "area-area", "size-space", "size-area", "space-area"))
# c(rgb(r,0,0), rgb(0,g,0), rgb(0,0,b), rgb(r/s,g/s,0), rgb(r/s,0,b/s), rgb(0,g/s,b/s)))+
p1<- ggplot(subset(mcor, mcor$type=="size-size"), aes(x=as.factor(Var1), y=as.factor(Var2), fill=Correlation, label = sprintf("%0.2f", Correlation))) + 
  geom_tile() + geom_text(size=5) +
  scale_fill_gradient2(low = "white", mid="white", high = rgb(r,0,0), midpoint=0, limits=c(-1,1))+
  xlab("")+ylab("")+
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0)) +
  mytheme + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(angle = 90),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face="bold")) 

# c("size-size", "space-space", "area-area", "size-space", "size-area", "space-area"))
# c(rgb(r,0,0), rgb(0,g,0), rgb(0,0,b), rgb(r/s,g/s,0), rgb(r/s,0,b/s), rgb(0,g/s,b/s)))+
p2 <- ggplot(subset(mcor, mcor$type=="space-space"), aes(x=as.factor(Var1), y=as.factor(Var2), fill=Correlation, label = sprintf("%0.2f", Correlation))) + 
  geom_tile() + geom_text(size=5) +
  scale_fill_gradient2(low = "white", mid="white", high = rgb(0,g,0), midpoint=0, limits=c(-1,1))+
  xlab("")+ylab("")+
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  mytheme + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(angle = 90),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face="bold")) 
# c("size-size", "space-space", "area-area", "size-space", "size-area", "space-area"))
# c(rgb(r,0,0), rgb(0,g,0), rgb(0,0,b), rgb(r/s,g/s,0), rgb(r/s,0,b/s), rgb(0,g/s,b/s)))+
p3<- ggplot(subset(mcor, mcor$type=="area-area"), aes(x=as.factor(Var1), y=as.factor(Var2), fill=Correlation, label = sprintf("%0.2f", Correlation))) + 
  geom_tile() + geom_text(size=5) +
  scale_fill_gradient2(low = "white", mid="white", high = rgb(0,0,b), midpoint=0, limits=c(-1,1))+
  xlab("")+ylab("")+
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  mytheme + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(angle = 90),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face="bold")) 
# c("size-size", "space-space", "area-area", "size-space", "size-area", "space-area"))
# c(rgb(r,0,0), rgb(0,g,0), rgb(0,0,b), rgb(r/s,g/s,0), rgb(r/s,0,b/s), rgb(0,g/s,b/s)))+
p4<- ggplot(subset(mcor, mcor$type=="size-space"), aes(x=as.factor(Var1), y=as.factor(Var2), fill=Correlation, label = sprintf("%0.2f", Correlation))) + 
  geom_tile() + geom_text(size=5) +
  scale_fill_gradient2(low = "white", mid="white", high = rgb(r/s,g/s,0), midpoint=0, limits=c(-1,1))+
  xlab("")+ylab("")+
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  mytheme + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(angle = 90),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face="bold")) 
# c("size-size", "space-space", "area-area", "size-space", "size-area", "space-area"))
# c(rgb(r,0,0), rgb(0,g,0), rgb(0,0,b), rgb(r/s,g/s,0), rgb(r/s,0,b/s), rgb(0,g/s,b/s)))+
p5<- ggplot(subset(mcor, mcor$type=="size-area"), aes(x=as.factor(Var1), y=as.factor(Var2), fill=Correlation, label = sprintf("%0.2f", Correlation))) + 
  geom_tile() + geom_text(size=5) +
  scale_fill_gradient2(low = "white", mid="white", high = rgb(r/s,0,b/s), midpoint=0, limits=c(-1,1))+
  xlab("")+ylab("")+
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  mytheme + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(angle = 90),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face="bold")) 
# c("size-size", "space-space", "area-area", "size-space", "size-area", "space-area"))
# c(rgb(r,0,0), rgb(0,g,0), rgb(0,0,b), rgb(r/s,g/s,0), rgb(r/s,0,b/s), rgb(0,g/s,b/s)))+
p6<- ggplot(subset(mcor, mcor$type=="space-area"), aes(x=as.factor(Var1), y=as.factor(Var2), fill=Correlation, label = sprintf("%0.2f", Correlation))) + 
  geom_tile() + geom_text(size=5) +
  scale_fill_gradient2(low = "white", mid="white", high = rgb(0,g/s,b/s), midpoint=0, limits=c(-1,1))+
  xlab("")+ylab("")+
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  mytheme + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(angle = 90),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face="bold"))


multiplot(p1,p4,p2,p5,p3,p6,cols=3)

getbits <- function(i,j){offdiagonal(R[[i]][[j]],0)[1:12]}

# c("size-size", "space-space", "area-area", "size-space", "size-area", "space-area"))
# c(rgb(r,0,0), rgb(0,g,0), rgb(0,0,b), rgb(r/s,g/s,0), rgb(r/s,0,b/s), rgb(0,g/s,b/s)))+

# TODO erikb below is the first slope correlation chart we want
  
blocks = 0:11
ggplot()+
  geom_point(aes(x=blocks, y=getbits(1,2)), col="red", size=5)+
  geom_line(aes(x=blocks, y=getbits(1,2)), col="red", size=2)+
  geom_point(aes(x=blocks, y=getbits(1,3)), col="blue", size=5)+
  geom_line(aes(x=blocks, y=getbits(1,3)), col="blue", size=2)+
  geom_point(aes(x=blocks, y=getbits(2,3)), col=rgb(0, 0.7 ,0), size=5)+
  geom_line(aes(x=blocks, y=getbits(2,3)), col=rgb(0, 0.7 ,0), size=2)+
  ylab("Correlation") + xlab("Block")+
  scale_x_continuous(breaks=blocks, minor_breaks=c())+
  mytheme

# TODO erikb new version with legend
new_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 32),
  axis.title.y = element_text(face = "bold", size = 32),
  axis.title.x = element_text(face = "bold", size = 32),
  legend.title = element_text(face = "bold", size = 28),
  # axis text
  axis.text.y = element_text(size = 14),
  axis.text.x = element_text(size = 14),
  # legend text
  legend.text = element_text(size = 24),
  # background color, axis lines
  panel.background = element_blank(),
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)

ggplot()+
  geom_point(aes(x=blocks, y=getbits(1,2)), color="red", size=2)+
  geom_line(aes(x=blocks, y=getbits(1,2), color="size-density"), size=1)+
  geom_point(aes(x=blocks, y=getbits(1,3)), color="blue", size=2)+
  geom_line(aes(x=blocks, y=getbits(1,3), color="size-area"), size=1)+
  geom_point(aes(x=blocks, y=getbits(2,3)), color="green4", size=2)+
  geom_line(aes(x=blocks, y=getbits(2,3), color="density-area"), size=1)+
  ylab("Correlation") + xlab("Block")+ ggtitle("Slope correlations by trial block") +
  scale_x_continuous(breaks=blocks, minor_breaks=c())+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.2)) +
  #mytheme +
  new_theme +
  scale_color_manual(name = "slope comparison",
                     values = c("size-density" = "red", "size-area" = "blue", "density-area" = "green4"),
                     labels = c("size-density" = "size-density", "size-area" = "size-area", "density-area" = "density-area"))



gettcor <- function(vi,vj, R){
  out = data.frame()
  for(D in 1:10){
    rs <- fisherz(offdiagonal(R[[vi]][[vj]][2:12,2:12],D))
    mu <- ifisherz(mean(rs))
    se <- sd(rs)/sqrt(length(rs))
    ul <- ifisherz(fisherz(mu)+se)
    ll <- ifisherz(fisherz(mu)-se)
    out <- rbind(out, data.frame(mu=mu, se=se, ul=ul, ll=ll))
  }
  return(out)
}

# TODO erikb added packages to run the code below
library(psych)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("graph", version = "3.8")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("RBGL", version = "3.8")
library(graph)
library(RBGL)
library(QuACN)


fullD = data.frame()
ds = 1:10
df = NULL
dfr = NULL
for(i in 1:length(vary)){
  for(j in i:length(vary)){
    t <- gettcor(i,j,R)
    t$distances = ds*50
    t$type <- paste(vary[i],vary[j],sep="-")
    t2 <- gettcor(i,j,Rr)
    t$nmu = t$mu/t2$mu
    t$nll = t$ll/t2$mu
    t$nul = t$ul/t2$mu
    df = rbind(df, t)
    t2$distances = distances*50
    t2$type <- paste(vary[i],vary[j],sep="-")
    dfr = rbind(dfr, t2)
  }
}

# TODO erikb this is the second graph we want but it only works with mytheme commented out

r = 1.0
g = 0.7
b = 1
s = 1.5
df$type <- factor(df$type, levels=c("size-size", "space-space", "area-area", "size-space", "size-area", "space-area"))
dfr$type <- factor(dfr$type, levels=c("size-size", "space-space", "area-area", "size-space", "size-area", "space-area"))
ggplot(data=df)+
  geom_point(aes(x=distances, y=nmu, color=type), size=5)+
  geom_linerange(aes(x=distances, ymin=nll, ymax=nul, color=type), size=2)+
  geom_line(aes(x=distances, y=nmu, color=type), size=2)+
  scale_colour_manual(values=c(rgb(r,0,0), rgb(0,g,0), rgb(0,0,b), rgb(r/s,g/s,0), rgb(r/s,0,b/s), rgb(0,g/s,b/s)))+
  scale_x_continuous(breaks=ds*50, minor_breaks=c())+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1), minor_breaks=c())+
  ylab("Normalized Correlation")+
  xlab("Distance (trials)")
  #new_theme
  #mytheme

# TODO erikb new version cleaned up
df$type <- factor(df$type, levels=c("size-size", "space-space", "area-area", "size-space", "size-area", "space-area"))
dfr$type <- factor(dfr$type, levels=c("size-size", "space-space", "area-area", "size-space", "size-area", "space-area"))
ggplot(data=df)+
  geom_point(aes(x=distances, y=nmu, color=type), size=2)+
  geom_linerange(aes(x=distances, ymin=nll, ymax=nul, color=type), size=1)+
  geom_line(aes(x=distances, y=nmu, color=type), size=1)+
  scale_colour_manual(values=c(rgb(r,0,0), rgb(0,g,0), rgb(0,0,b), rgb(r/s,g/s,0), rgb(r/s,0,b/s), rgb(0,g/s,b/s)))+
  scale_x_continuous(breaks=ds*50, minor_breaks=c())+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1), minor_breaks=c())+
  ylab("Correlation") + xlab("Distance (trials)") + ggtitle("Slope correlations by trial distance") +
  theme(legend.position="bottom") +
  new_theme +
  scale_color_manual(name = "slope comparison",
                     values = c(rgb(r,0,0), rgb(0,g,0), rgb(0,0,b), rgb(r/s,g/s,0), rgb(r/s,0,b/s), rgb(0,g/s,b/s)),
                     labels = c("size-size" = "size-size", "space-space" = "density-density", "area-area" = "area-area", "size-space" = "size-density", "size-area" = "size-area", "space-area" = "density-area"))
  #mytheme

# TODO erikb graph copied over from `analysis.density.2014-05-28.Rmd`
ylims = xlims
subnames <- unique(dat$vary)
#subnames[2] = "density"
cuts <- c(9.5:20.5, 10^seq(log10(21.5), log10(maxn), length.out=20))
X <- data.frame()
for(i in 1:length(subnames)){
  sdat <- subset(dat, dat$vary == subnames[i])
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
ggplot(dat, aes(x=num_dots, y=answer))+
  geom_point(colour="blue", size=2, alpha=0.05)+
  geom_point(data=X, aes(x=truens, y=medians), colour="red", size=2)+
  geom_line(data=X, aes(x=truens, y=medians), colour="red", size=1)+
  geom_abline(position="identity")+
  mylogx(xlims)+
  mylogy(ylims)+
  xlab("Number presented") + ylab("Number reported") + ggtitle("Accuracy across estimate conditions") +
  #mytheme+
  #new_theme +
  facet_wrap(~vary, ncol=3,
             labeller = labeller(vary = c("area" = "area", "size" = "size", "space" = "density"))) +
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
    legend.text = element_text(size = 14),
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




save(file='fit-block-models.Rdata', list=ls())


trialspercut = 25
trialcuts <- seq(0, 1000, by=25)
ncuts = length(trialcuts)-1
dat$block <- cut(dat$trial, trialcuts, labels=1:ncuts, include.lowest=T)
dat$mod <- (dat$trial-1) %% ncuts + 1

fitsBlock = list()
fitsMod = list()
dcor = list()
fitsBlock = list()
fitsMod = list()
for(k in 1:(ncuts)){
  A <- (dat$mod==k)
  tmp <- subset(dat, A)
  fitsMod[[k]] <- data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
  A <- (dat$block==k)
  tmp <- subset(dat, A)
  fitsBlock[[k]] <- data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
  print(c(k, sum(fitsMod[[k]]$logL==-9999), sum(fitsBlock[[k]]$logL==-9999)))
}

estB <- do.call(cbind.fill, lapply(fitsBlock, function(x){namedField(x,'b','subject')}))
estBse <- do.call(cbind.fill, lapply(fitsBlock, function(x){namedField(x,'sb','subject')}))

slopes <- data.frame(estB)
slopes$subject <- rownames(estB)
slopes <- melt(slopes)
names(slopes) <- c("subject", "block", "slope")
levels(slopes$block) <- 1:50
slopes$block <- as.numeric(as.character(slopes$block))

ggplot(slopes, aes(x=block, y=slope)) +
  stat_summary(fun.data=mean_se, 
               geom="ribbon", 
               alpha=0.5, colour=NA, fill="blue") +
  geom_point(size=5, alpha=0.25, colour="blue") +
  geom_line(aes(group=subject), colour="blue", size=0.5, alpha=0.25)+
  stat_summary(fun.y=mean, geom="line", size=2, colour="blue")+
  scale_x_continuous(minor_breaks=c())+
  scale_y_continuous(minor_breaks=c())+
  mytheme

R = cor(s1,s1, use="pairwise.complete.obs")
rownames(R) <- c()
colnames(R) <- c()
mcor <- melt(R)
names(mcor) <- c("Var1", "Var2", "value")
mcor <- mcor[mcor$Var2 >= mcor$Var1, ]
mcor$value[mcor$Var1==mcor$Var2] = NA
ggplot(mcor, aes(x=as.factor(Var1), y=as.factor(Var2), fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "white", mid="white", high = "blue", midpoint=0.0, limits=c(-1,1))+
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
  for(D in 1:(nrow(R)-1)){
    rs <- fisherz(offdiagonal(R,D))
    mu <- ifisherz(mean(rs))
    se <- sd(rs)/sqrt(length(rs))
    ul <- ifisherz(fisherz(mu)+se)
    ll <- ifisherz(fisherz(mu)-se)
    out <- rbind(out, data.frame(mu=mu, se=se, ul=ul, ll=ll))
  }
  return(out)
}


t <- gettcor(R)
t$distances = 1:(ncuts-1)*trialspercut
t$type <- paste(vary[i],vary[j],sep="-")
t2 <- gettcor(i,j,Rr)
t$nmu = t$mu/t2$mu
t$nll = t$ll/t2$mu
t$nul = t$ul/t2$mu
df = rbind(df, t)
t2$distances = distances*50
t2$type <- paste(vary[i],vary[j],sep="-")
dfr = rbind(dfr, t2)

ggplot(data=t)+
  geom_point(aes(x=distances, y=mu), color="blue", size=5)+
  geom_linerange(aes(x=distances, ymin=ll, ymax=ul), color="blue", size=2)+
  geom_line(aes(x=distances, y=mu), color="blue", size=2)+
  scale_x_continuous(breaks=(1:(ncuts-1))*trialspercut, minor_breaks=c())+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1), minor_breaks=c())+
  ylab("Correlation")+
  xlab("Distance (trials)")+
  mytheme
