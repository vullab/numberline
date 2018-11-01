# fit power and bipower models, generate curves.

ntrials = 1000

splitBlock <- function(trial, n){floor((trial-1)/(ntrials/n))}
splitMod <- function(trial, n){trial %% n}
usefx <- map.bipower
parameters = list()
parameters[["a"]] <- c("prior.mu"=0, "prior.sd"=1.0)
parameters[["b"]] <- c("prior.mu"=0, "prior.sd"=0.15)
parameters[["s"]] <- c("prior.mu"=-1, "prior.sd"=0.15)
parameters[["p"]] <- c("prior.mu"=4, "prior.sd"=0.6)

nblocks = 1
dat$binblk = splitBlock(dat$trial, nblocks)
dat$binmod = splitMod(dat$trial, nblocks)

dat$subject = dat$sid
dat$num_dots = dat$stimulus
dat$answer = dat$response

allfits = data.frame()
for(y in unique(dat$year)){
  for(s in unique(dat$sid)){
    fits <- brutefit(subset(dat, dat$sid==s & dat$year==y & dat$type=='test'))
    f <- c(y, s, fits)
    names(f) <- c()
    allfits = rbind(allfits, f)
  }
}

# getResults <- function(fieldname, transform, decimals){
#   matrix(round(transform(t(unname(do.call(cbind.fill, lapply(fit.mod, function(x){namedField(x,fieldname,'subject')}))))),decimals), ncol=24)
# }

# use <- which(apply(getResults('b', identity, 2), 2, function(x){diff(range(x))})>0.1)
# plot(getResults('a', identity, 5)[,use],
#      getResults('b', identity, 5)[,use])

  
# fit.mod = list()
# fit.blk = list()
# for(k in 1:nblocks){
#   tmp <- subset(dat, dat$binmod==(k-1))
#   fit.mod[[k]] <- data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
#   tmp <- subset(dat, dat$binblk==(k-1))
#   fit.blk[[k]] <- data.frame(do.call(rbind, by(tmp, tmp$subject, brutefit)))
#   print(c(k, sum(fit.mod[[k]]$logL==-9999), sum(fit.blk[[k]]$logL==-9999)))
# }


# estB <- do.call(cbind.fill, lapply(fitsBlock, function(x){namedField(x,'b','subject')}))
# estBse <- do.call(cbind.fill, lapply(fitsBlock, function(x){namedField(x,'sb','subject')}))
# 
# slopes <- data.frame(estB)
# slopes$subject <- rownames(estB)
# slopes <- melt(slopes)
# names(slopes) <- c("subject", "block", "slope")
# levels(slopes$block) <- 1:nblocks
# 
# ggplot(slopes, aes(x=block, y=slope)) +
#   stat_summary(fun.data=mean_se, 
#                geom="ribbon", 
#                alpha=0.5, colour=NA, fill="red") +
#   geom_point(size=5, alpha=0.4, colour="red") +
#   geom_line(aes(group=subject), colour="red", size=0.5, alpha=0.25)+
#   stat_summary(fun.y=mean, geom="line", colour="red", size=2)+
#   scale_x_continuous(minor_breaks=c())+
#   scale_y_continuous(minor_breaks=c())+
#   mytheme
# 
# R = cor(s1,s1, use="pairwise.complete.obs")
# rownames(R) <- c()
# colnames(R) <- c()
# mcor <- melt(R)
# names(mcor) <- c("Var1", "Var2", "value")
# mcor <- mcor[mcor$Var1<=mcor$Var2,]
# mcor$value[mcor$Var1==mcor$Var2] <- NA
# ggplot(mcor, aes(x=as.factor(Var1), y=as.factor(Var2), fill=value)) + 
#   geom_tile() + 
#   scale_fill_gradient2(low = "white", mid="white", high = "red", midpoint=0.3, limits=c(-0.5,1))+
#   xlab("")+ylab("")+
#   scale_x_discrete(expand = c(0,0)) +
#   scale_y_discrete(expand = c(0, 0)) + 
#   mytheme + 
#   theme(axis.ticks = element_blank(), 
#         axis.text = element_text(size = 16, face = "bold"),
#         axis.text.y = element_text(angle = 90),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 12, face="bold"))
