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
  xlab("Distance (trials)")+
  mytheme

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
