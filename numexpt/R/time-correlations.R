
dat$error <- 10^(log10(dat$answer)-log10(dat$num_dots))

sources <- c("num_dots", "answer", "error")
nlags = 100


crosscors = data.frame()
for(i in 1:length(sources)){
  for(j in i:length(sources)){
    XC <- data.frame(do.call(cbind, 
                             by(dat, 
                                dat$subject,
                                function(tmp){
                                  crosscor(nlags, log10(tmp[,sources[i]]), log10(tmp[,sources[j]]))})))
    names(XC) <- paste('S', subjects, sep='.')
    XC$type <- paste(sources[i], sources[j], sep="-")
    XC$lag = 1:nlags
    crosscors <- rbind(crosscors, XC)
  }
}

crosscors <- melt(crosscors, id.vars=c('type', 'lag'))
# acf.stim <- data.frame()
# acf.resp <- data.frame()
# acf.err <- data.frame()
# 
# for(s in subjects){
#   tmp <- acf(subset(log10(dat$num_dots), dat$subject==s), lag.max=nlags, plot=F)
#   acf.stim <- rbind(acf.stim, c(s, tmp[[1]][2:(nlags+1)]))
#   tmp <- acf(subset(log10(dat$answer), dat$subject==s), lag.max=nlags, plot=F)
#   acf.resp <- rbind(acf.resp, c(s, tmp[[1]][2:(nlags+1)]))
#   tmp <- acf(subset(log10(dat$answer)-log10(dat$num_dots), dat$subject==s), lag.max=nlags, plot=F)
#   acf.err <- rbind(acf.err, c(s, tmp[[1]][2:(nlags+1)]))
# }
# names(acf.resp) <- c("Subject", paste("lag.", 1:nlags, sep=''))
# names(acf.stim) <- c("Subject", paste("lag.", 1:nlags, sep=''))
# names(acf.err) <- c("Subject", paste("lag.", 1:nlags, sep=''))
# 
# acf.resp.mu <- apply(acf.resp[,2:(nlags+1)], 2, fish.mu)
# acf.resp.se <- apply(acf.resp[,2:(nlags+1)], 2, fish.se)
# acf.stim.mu <- apply(acf.stim[,2:(nlags+1)], 2, fish.mu)
# acf.stim.se <- apply(acf.stim[,2:(nlags+1)], 2, fish.se)
# acf.err.mu <- apply(acf.err[,2:(nlags+1)], 2, fish.mu)
# acf.err.se <- apply(acf.err[,2:(nlags+1)], 2, fish.se)
# 
# tmp<-subset(crosscors, crosscors$type=='num_dots-num_dots')
# plot(acf.stim.mu, apply(tmp[,1:length(subjects)], 1, fish.mu))
# 
# 
# plotErrorCurve <- function(mu, se, color=rgb(0,0,0)){
#   for(z in c(1)){
#     polygon(x=c(1:length(mu), rev(1:length(mu))), 
#             y=c(ifisherz(mu-z*se), rev(ifisherz(mu+z*se))), 
#             col=paste(color,"40",sep=""), lty="blank")
#   }
#   lines(1:length(mu), ifisherz(mu), type='b', pch=19, col=color, lwd=5)
# }
# 
# plot(c(0,nlags+1), c(0,0), type='l', col='black', ylim=c(-0.02, 0.2), xlim=c(1, nlags), xlab="Lag", ylab="Autocorrelation")
# plotErrorCurve(acf.err.mu, acf.err.se, rgb(0.7, 0, 0))
# plotErrorCurve(acf.stim.mu, acf.stim.se, rgb(0, 0, 1))
# plotErrorCurve(acf.resp.mu, acf.resp.se, rgb(0, 0.5, 0))
# title(main="Time dependence in numerosity", sub="Autocorrelation functions for stimulus, response, error")
