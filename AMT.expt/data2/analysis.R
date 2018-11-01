x = read.table('~/Research/Misc/number-line/AMT.expt/data2/data.2012-11-07.txt', header=TRUE, sep='\t')
x$response = as.numeric(x$response)
x$shown = as.numeric(x$shown)
ws = unique(x$X0.workerId)
as = unique(x$assignmentId)
alens = unlist(lapply(as, function(w){length(x$trialn[x$assignmentId == w])}))
acorrs = unlist(lapply(as, function(a){cor(x$response[x$assignmentId==a], x$shown[x$assignmentId==a], method='spearman')}))
goodas = as[alens>=90 & acorrs>0.2]



# here's what we do: for each 

q = c()
for(a in goodas){
  temp = with(x[x$assignmentId == a & x$trialn<92 & x$trialn>30,], acf(log10(response/shown),lag.max=5,type='correlation'))
  q = c(q,temp$acf)
}

q = matrix(q,ncol = length(goodas))
q = data.frame(t(q))