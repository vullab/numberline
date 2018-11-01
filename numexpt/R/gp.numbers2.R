# Load in the required libraries for data manipulation
# and multivariate normal distribution
require(MASS)
require(plyr)
require(reshape2)
require(ggplot2)
#set.seed(12345)
n.samples = 50

a = 2

dscale <- function(x,y){exp(-0.5*(abs(x^(1/a)-y^(1/a))/0.5)^2)}
dintervals <- function(x,y){pmax(0,diff(range(x))+diff(range(y))-diff(range(c(x,y))))/diff(range(c(x,y)))}
dxint <- function(x,y){
  r <- range(y)
  inside = x>=r[1] & x<=r[2]
  if(inside){
    return(dscale(0,0))
  } else {
    return(dscale(x, ifelse(x<r[1], r[1], r[2])))
  }
}
dxx <- function(x,y){dscale(x,y)}
calcSigma <- function(X1,X2) {
  Sigma <- matrix(rep(0, length(X1)*length(X2)), nrow=length(X1))
  for (i in 1:nrow(Sigma)) {
    for (j in 1:ncol(Sigma)) {
      if(is.list(X1) & is.list(X2)) {Sigma[i,j] = dintervals(X1[[i]], X2[[j]])}
      else if(is.list(X2)){Sigma[i,j] = dxint(X1[i], X2[[j]])}
      else if(is.list(X1)){Sigma[i,j] = dxint(X2[j], X1[[i]])}
      else {Sigma[i,j] = dxx(X1[i], X2[j])}
    }
  }
  return(Sigma)
}

myround <- function(x){
  u <- (10^floor(log10(x)))
  round(x/u)*u
}

x.star <- log10(c(1:100, seq(110, 1000, by=10)))

nobs = 10
s.n = 0.05

x <- log10(c(1,round(10^abs(rnorm(nobs, 0, 0.8)))+1))
y <- map.bipower(10^x, 1.1, -0.2)
y <- unlist(lapply(y, myround))
y <- log10(y)
x <- x + rnorm(length(x), 0, s.n)
X <- lapply(2:length(x), function(i){c(x[i-1], x[i])})
Y <- unlist(lapply(2:length(x), function(i){((y[i]-y[i-1])/(x[i]-x[i-1]))}))
s.n = 0.05/unlist(lapply(X, function(x){diff(range(x))}))
s.n = 0.05


f <- data.frame(x=x, y=y)
k.xx <- calcSigma(X,X)
k.xxs <- calcSigma(X,x.star)
k.xxs <- k.xxs/t(matrix(rep(colSums(k.xxs),10), ncol=10))
k.xsx <- calcSigma(x.star,X)
k.xsx <- k.xsx/matrix(rep(rowSums(k.xsx),10), ncol=10)
k.xsxs <- calcSigma(x.star,x.star)

# Recalculate the mean and covariance functions
wts <- k.xsx%*%solve(k.xx + s.n^2*diag(1, ncol(k.xx)))
f.bar.star <- (wts%*%Y)/rowSums(wts)
cov.f.star <- k.xsxs - k.xsx%*%solve(k.xx + s.n^2*diag(1, ncol(k.xx)))%*%k.xxs

# Recalulate the sample functions
values <- matrix(rep(0,length(x.star)*n.samples), ncol=n.samples)
estimates <- matrix(rep(0,length(x.star)*n.samples), ncol=n.samples)
for (i in 1:n.samples) {
  values[,i] <- mvrnorm(1, f.bar.star, cov.f.star)
  estimates[,i] <- cumsum(c(0, diff(x.star))*values[,i])
}
values <- cbind(x=x.star,as.data.frame(values))
values <- melt(values,id="x")

estimates <- cbind(x=x.star,as.data.frame(estimates))
estimates <- melt(estimates,id="x")

est.f.bar.star <- cumsum(c(0, diff(x.star))*f.bar.star)

# Plot the result, including error bars on the observed points
gg <- ggplot(estimates, aes(x=x,y=value)) + 
  geom_abline(position="identity", color="blue")+
  geom_line(aes(group=variable), colour="grey80") +
  geom_line(data=NULL,aes(x=x.star,y=est.f.bar.star),colour="red", size=1) + 
  geom_errorbar(data=f,aes(x=x,y=NULL,ymin=y-2*s.n, ymax=y+2*s.n), width=0.2) +
  geom_point(data=f,aes(x=x,y=y)) +
  theme_bw() +
  scale_y_continuous(lim=c(0,3), name="output, f(x)") +
  scale_x_continuous(lim=c(0,3), name="x")
gg
