# analyze summary stats within bins

cuts <- c(1.5:15.5, 10^seq(log10(16.5), log10(300), length.out=20))
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
                  function(tmp){sd(log10(tmp$answer)-log10(tmp$num_dots))}))
acc <- cbind(by(dat, 
                dat$bin, 
                function(tmp){mean(tmp$num_dots == tmp$answer1 | tmp$num_dots == tmp$answer2)}))
bin.stats <- data.frame(truens=truens[,1], medians=medians[,1], RT=RT[,1], acc=1-acc[,1], errsd=errsd)