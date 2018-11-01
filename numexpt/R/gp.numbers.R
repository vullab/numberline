# Load in the required libraries for data manipulation
# and multivariate normal distribution
require(MASS)
require(plyr)
require(reshape2)
require(ggplot2)
#set.seed(12345)
n.samples = 50

map.bipower <- function(x, a, b){
  crit <- a
  slope <- 10^b
  lx <- log10(x)
  ly <- ((lx>crit)*(crit+(lx-crit)*slope)+(lx<=crit)*lx);
  return(10^ly)
}

a = 2
calcSigma <- function(X1,X2) {
  Sigma <- matrix(rep(0, length(X1)*length(X2)), nrow=length(X1))
  for (i in 1:nrow(Sigma)) {
    for (j in 1:ncol(Sigma)) {
#       Sigma[i,j] <- 1-(1-0.0*exp(-0.5*(abs(X1[i]-X2[j])/5)^2))*
#         (1-exp(-0.5*(abs(log10(X1[i])-log10(X2[j]))/0.25)^2))
#       Sigma[i,j] <- 0.0*exp(-0.5*(abs(X1[i]-X2[j])/25))+
#         exp(-0.5*(abs(log10(X1[i])-log10(X2[j]))/1)^2)*1.0
      Sigma[i,j] <- exp(-0.5*(abs((X1[i])^(1/a)-(X2[j])^(1/a))/0.5)^2)
    }
  }
  return(Sigma)
}

myround <- function(x){
  u <- (10^floor(log10(x)))
  round(x/u)*u
}

x.star <- log10(c(1:100, seq(110, 1000, by=10)))

nobs = 100
s.n = 0.1

x <- log10(c(round(10^abs(rnorm(nobs, 0, 0.5)))+2))
y <- map.bipower(10^x, 1.1, 0)
y <- unlist(lapply(y, round))
y <- log10(y)
# x <- x + rnorm(length(x), 0, s.n)

f <- data.frame(x=x, y=y)
k.xx <- calcSigma(x,x)
k.xxs <- calcSigma(x,x.star)
k.xsx <- calcSigma(x.star,x)
k.xsxs <- calcSigma(x.star,x.star)

# Recalculate the mean and covariance functions
f.bar.star <- k.xsx%*%solve(k.xx + s.n^2*diag(1, ncol(k.xx)))%*%(y/x)
cov.f.star <- (k.xsxs - k.xsx%*%solve(k.xx + s.n^2*diag(1, ncol(k.xx)))%*%k.xxs)

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

f.bar.star <- cumsum(c(0, diff(x.star))*f.bar.star)

# Plot the result, including error bars on the observed points
gg <- ggplot(estimates, aes(x=x,y=value)) + 
  geom_abline(position="identity", color="blue")+
  geom_line(aes(group=variable), colour="grey80") +
  geom_line(data=NULL,aes(x=x.star,y=f.bar.star),colour="red", size=1) + 
  #geom_errorbar(data=f,aes(x=x,y=NULL,ymin=y-2*s.n, ymax=y+2*s.n), width=0.2) +
  geom_point(data=f,aes(x=x,y=y)) +
  theme_bw() +
  scale_y_continuous(lim=c(0,3), name="output, f(x)") +
  scale_x_continuous(lim=c(0,3), name="x")
gg
