d = read.table('~/Research/Misc/cheerleader/data/cheer1_FinalData.csv', sep=',', header=TRUE)

ss = unique(d$Ss)

for(s in ss){
  rs = d$rating[d$Ss==s]
  d$zr[d$Ss==s] = (rs-mean(rs))/sd(rs)
}

m = lm(data = d, zr~Image+condition-1)

summary(m)

summary.aov(m)

# this looks lovely -- by analyzing the within-subject z-scores, we have some idea of the effect size:
# about 5.5% of a standard deviation over ratings of all images: not insignificant at all!

# now we are going to do the conservative analysis, where we treat each subject as a random effect:
# that is: perhaps some folks get a cheerleader effect, and others don't... do people on avg have such an effect?
im = unique(d$Image)

# for each subject, for each image, find the difference in z-score ratings...
q = lapply(ss, function(s){unlist(lapply(im, function(i){d$zr[d$Ss==s & d$Image==i & d$condition == "t"] - d$zr[d$Ss==s & d$Image==i & d$condition == "s"]}))})

# take the average difference of z-score ratings across images within subjects
mq = unlist(lapply(q, mean))


# for amusement, we can ask: how many subjects is the cheerleader significant in?

seq = unlist(lapply(q,sd))/sqrt(299)

si = sort(mq, index.return=TRUE)
tc = qt(0.975,298)
plot(mq[si$ix])
for(s in 1:length(ss)){
lines(c(1,1)*s, c(-1,1)*seq[si$ix[s]]+mq[si$ix[s]], type='l')
}
lines(c(0, length(mq)+1), c(0, 0), type='l')

# so most are near, but slightly above 0, one is sig. below, and 4 or 5 are sig above.  
# total group effect is significant with a t-test or with a non-parametric sign test.

t.test(mq)

binom.test(sum(mq>0), sum(mq!=0), 0.5)
