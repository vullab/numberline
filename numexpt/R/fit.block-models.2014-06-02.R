# fit power and bipower models, generate curves.


usefx <- map.bipower
ps = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2)
names(ps) <- c("ma", "sa", "mb", "sb", "ms", "ss")
priors = list()
priors[[1]] <- function(x){-dnorm(x, 1.14, 0.1, log=T)} #
priors[[2]] <- function(x){-dnorm(x, -0.1, 0.25, log=T)} #
priors[[3]] <- function(x){-dnorm(x, -1, 0.05, log=T)} # 

dat$trial = 0
for(s in unique(dat$subject)){
  dat$trial[dat$subject==s] = 1:300
}

splitBlock <- function(trial, n){floor((trial-1)/(300/n))}
splitMod <- function(trial, n){trial %% n}

ns = c(30)
xcorBlock = list()
xcorMod = list()
dcor = list()
for(i in 1:length(ns)){
  n = ns[i]
  fitsMod = list()
  fitsBlock = list()
  for(k in 1:n){
    tmp <- subset(dat, splitMod(dat$trial,n)==(k-1))
    fitsMod[[k]] <- data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
    tmp <- subset(dat, splitBlock(dat$trial,n)==(k-1))
    fitsBlock[[k]] <- data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
  }
}
#   xcorBlock[[i]] = matrix(nrow=n, ncol=n)
#   xcorMod[[i]] = matrix(nrow=n, ncol=n)
#   for(k in 1:n){
#     for(j in k:n){
#       xcorBlock[[i]][k,j] = cor(fitsBlock[[k]]$b, fitsBlock[[j]]$b)    
#       xcorMod[[i]][k,j] = cor(fitsMod[[k]]$b, fitsMod[[j]]$b)    
#     }
#   }
#   Bms <- unlist(lapply(1:(n-1), function(dist){mean(fisherz(offdiagonal(xcorBlock[[i]], dist)))}))
#   Mms <- unlist(lapply(1:(n-1), function(dist){mean(fisherz(offdiagonal(xcorMod[[i]], dist)))}))
#   Bse <- unlist(lapply(1:(n-1), function(dist){sd(fisherz(offdiagonal(xcorBlock[[i]], dist)))/sqrt(n-dist)}))
#   Mse <- unlist(lapply(1:(n-1), function(dist){sd(fisherz(offdiagonal(xcorMod[[i]], dist)))/sqrt(n-dist)}))
#   dcor[[i]] <- data.frame(block.mu=Bms, mod.mu=Mms, block.se=Bse, mod.se=Mse)
# }

# plot(c(0, 301), c(0, 0), type='l', ylim=c(0.5,1))
# i= 9
# lines(1:(ns[i]-1)*(300/(ns[i])), ifisherz(dcor[[i]]$block.mu), type='p', col='red', pch=19)
#   lines(1:(ns[i]-1)*(300/(ns[i])), ifisherz(dcor[[i]]$mod.mu), type='p', col='blue', pch=19)
#   t <- lapply(1:(ns[i]-1), function(j){
#     lines(c(1,1)*(j*300/ns[i]), 
#           ifisherz(dcor[[i]]$block.mu[j]+c(-1,1)*dcor[[i]]$block.se[j]), col="red", type='l', lwd=2)
#     lines(c(1,1)*(j*300/ns[i]), 
#           ifisherz(dcor[[i]]$mod.mu[j]+c(-1,1)*dcor[[i]]$mod.se[j]), col="blue", type='l', lwd=2)
#   })
# 
# plot(c(0, 301), c(1, 1), type='l', ylim=c(0.5,1.1))
# i = 9
#   lines(1:(ns[i]-1)*(300/(ns[i])), ifisherz(dcor[[i]]$block.mu)/ifisherz(dcor[[i]]$mod.mu), type='p', col='red', pch=19)
# 
#   t <- lapply(1:(ns[i]-1), function(j){
#     lines(c(1,1)*(j*300/ns[i]), 
#           ifisherz(dcor[[i]]$block.mu[j]+c(-1,1)*dcor[[i]]$block.se[j])/ifisherz(dcor[[i]]$mod.mu[j]), col="red", type='l', lwd=2)})


namedSlopes <- function(x){
  z <- data.frame(x$b)
  rownames(z) <- x$subject
  return(z)
}

namedSEs <- function(x){
  z <- data.frame(x$sb)
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
ss1 <- do.call(cbind.fill, lapply(fitsBlock, namedSEs))
slopes <- data.frame(s1)
slopes$subject <- rownames(s1)
slopes <- reshape::melt(slopes)
names(slopes) <- c("subject", "block", "slope")
levels(slopes$block) <- 1:ncol(s1)
slopes$block <- as.numeric(as.character(slopes$block))


ggplot(slopes, aes(x=block, y=slope)) +
  stat_summary(fun.data=mean_se, 
               geom="ribbon", 
               alpha=0.5, colour=NA, fill="red") +
  geom_point(size=5, alpha=0.4, colour="red") +
  geom_line(aes(group=subject), colour="red", size=0.5, alpha=0.25)+
  stat_summary(fun.y=mean, geom="line", colour="red", size=2)+
  scale_x_continuous(minor_breaks=c())+
  scale_y_continuous(minor_breaks=c())+
  #mytheme+
  ggtitle("Slopes fit to data by block")
  

R = cor(s1, s1, use="pairwise.complete.obs")
rownames(R) = c()
colnames(R) = c()
mcor = reshape::melt(R)
names(mcor) = c("Var1", "Var2", "value")
mcor = mcor[mcor$Var1 <= mcor$Var2,]
mcor$value[mcor$Var1 == mcor$Var2] = NA
ggplot(mcor, aes(x = as.factor(Var1), y = as.factor(Var2), fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "white", mid = "white", high = "red", midpoint = 0.3, limits = c(-0.5, 1))+
  xlab("") + ylab("") +
  ggtitle("Trial block slope correlations for participant data") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  #mytheme + 
  theme(axis.ticks = element_blank(), 
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(angle = 90),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        title = element_text(size = 12, face = "bold"),
        panel.grid = element_blank())



# addition erikb: show drift at greater distances
cors.dist.blocks = list('dist' = numeric(), 'cors' = c())
for (x in seq(1:(30 - 1))) {
  cors.dist.blocks[[x]] = list('dist' = x, 'cors' = c())
}

for (row in seq(from = 1, to = dim(R)[1])) {
  for (col in seq(from = row, to = dim(R)[2])) { # NB: this does funky stuff without 'from'
    dist.val = (col - row)
    cor.val = R[row, col]
    if (dist.val > 0 & !is.na(cor.val)) {
      cors.dist.blocks[[dist.val]]$cors = c(cors.dist.blocks[[dist.val]]$cors, cor.val)
    }
  }
}

# Validate distance correlations
length(cors.dist.blocks[[1]]$cors)
cors.dist.blocks[[1]]$cors[1]




# Get summary data frame for displaying distance correlations
cor.means.df.blocks = data.frame()
for (x in seq(1:length(cors.dist.blocks))) {
  dist = cors.dist.blocks[[x]]$dist * 10
  mean = mean(cors.dist.blocks[[x]]$cors)
  se = sd(cors.dist.blocks[[x]]$cors) / sqrt(length(cors.dist.blocks[[x]]$cors))
  row = data.frame('dist' = dist, 'mean.cor' = mean, 'se' = se)
  cor.means.df.blocks = rbind(cor.means.df.blocks, row)
}

cor.means.df.blocks %>%
  ggplot(aes(x = dist, y = mean.cor)) +
  geom_point() +
  geom_errorbar(aes(x = dist, ymin = mean.cor - se, ymax = mean.cor + se)) +
  ylim(0.25, 1) +
  labs(x = "trial distance", y = "correlation of slopes (mean correlation, error bars = se)") +
  ggtitle("Drift in correlation of slopes at greater trial distances for participants") +
  theme(panel.grid = element_blank())

