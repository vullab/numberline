# analyze summary stats within bins

cuts <- c(1, sort(unique(dat$stimulus))+0.5)
dat$bin <- cut(dat$stimulus, breaks=cuts, labels=seq_len(length(cuts)-1), include.lowest=T)
truens <- cbind(by(dat$stimulus, 
                   dat$bin, 
                   median))
medians <- cbind(by(dat$response, 
                    dat$bin, 
                    median))
RT <- cbind(by(dat$rt1, 
               dat$bin, 
               function(x){median(x)}))
errsd <- cbind(by(dat, 
                  dat$bin, 
                  function(tmp){sd(log10(tmp$response)-log10(tmp$stimulus))}))
acc <- cbind(by(dat, 
                dat$bin, 
                function(tmp){mean(tmp$stimulus == tmp$response)}))
bin.stats <- data.frame(truens=truens[,1], medians=medians[,1], RT=RT[,1], acc=1-acc[,1], errsd=errsd)
