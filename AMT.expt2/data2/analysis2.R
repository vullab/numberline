x = read.table('~/Research/Misc/number-line/AMT.expt2/data2/data.2012-11-08.txt', header=TRUE, sep=';')
x$response = as.numeric(x$response)
x$shown = as.numeric(x$shown)
x$logerr = log10(x$response)-log10(x$shown)
as = unique(x$assignmentId)
alens = unlist(lapply(as, function(w){length(x$trialn[x$assignmentId == w])}))
acorrs = unlist(lapply(as, function(a){cor(x$response[x$assignmentId==a], x$shown[x$assignmentId==a], method='spearman')}))
averr = unlist(lapply(as, function(a){with(x[x$assignmentId==a,], sqrt(sum(mean(logerr^2))))}))
apout = unlist(lapply(as, function(a){with(x[x$assignmentId==a,], sum(abs(logerr)>1.0)/length(logerr))}))
goodas = as[alens>=90 & apout<0.2]

# here's what we do: for each 

q = c()
for(a in goodas){
  temp = with(x[x$assignmentId == a & x$trialn<41 & x$trialn>10,], acf(logerr,lag.max=5,type='correlation'))
  q = c(q,temp$acf)
}

q = matrix(q,ncol = length(goodas))
q = data.frame(t(q))

plot(seq(1,5), mean(q[,-1]))
for(i in seq(2,6)){ 
  lines(c(1,1)*(i-1), c(-1,1)*sd(q[,i])/sqrt(44)+mean(q[,i]), type='l')
}