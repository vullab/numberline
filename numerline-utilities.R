fish.mu <- function(x){mean(fisherz(x))}
fish.se <- function(x){sd(fisherz(x))/sqrt(length(x))}

namedField <- function(x, colfield, colname){
  z <- data.frame(x[,colfield])
  rownames(z) <- x[,colname]
  return(z)
}

crosscor <- function(nlags, s1, s2){
  OUT <- ccf(s1, s2, lag.max=nlags, plot=F)
  return(rev(OUT[[1]][1:nlags]))
}


map.bipower <- function(x, a, b){
  crit <- a
  slope <- 10^b
  lx <- log10(x)
  ly <- ((lx>crit)*(crit+(lx-crit)*slope)+(lx<=crit)*lx);
  return(10^ly)
}

loglik <- function(x, y, map.fx, a, b, s, p){
  sum(
    log(
      p*dnorm(log10(y)-log10(map.fx(x, a, b)), 0, s)+
        (1-p)*dnorm(log10(y), 2, 2)
    )
  )
}

brutefit <- function(tmp){
  nLL <- function(a, b, s, p){
    -loglik(tmp$num_dots, tmp$answer, usefx, a, b, 10^s, logistic(p)) + 
      -dnorm(a, parameters[[1]]["prior.mu"], parameters[[1]]["prior.sd"], log=T) + 
      -dnorm(b, parameters[[2]]["prior.mu"], parameters[[2]]["prior.sd"], log=T) + 
      -dnorm(s, parameters[[3]]["prior.mu"], parameters[[3]]["prior.sd"], log=T) + 
      -dnorm(p, parameters[[4]]["prior.mu"], parameters[[4]]["prior.sd"], log=T)
  }
  
  iter = 0
  fits = NULL
  fit = NULL
  while(is.null(fits)){
    startList <- list(a=rnorm(1, parameters[[1]]["prior.mu"], parameters[[1]]["prior.sd"]), 
                      b=rnorm(1, parameters[[2]]["prior.mu"], parameters[[2]]["prior.sd"]), 
                      s=rnorm(1, parameters[[3]]["prior.mu"], parameters[[3]]["prior.sd"]), 
                      p=rnorm(1, parameters[[4]]["prior.mu"], parameters[[4]]["prior.sd"]))
#    print(c(startList$a, startList$b, startList$s))
#    print(nLL(startList$a, startList$b, startList$s))
    
    tryCatch(fit <- summary(mle(nLL, start=startList)), 
             error = function(e){print(e)})
    iter = iter+1
    
    if(! is.null(fit)){
      fits <- c(tmp$subject[1], 
                iter,
                -0.5*fit@m2logL, 
                length(tmp$num_dots), 
                fit@coef[,"Estimate"], 
                fit@coef[, "Std. Error"])
    } else {
      if(iter>50){
        fits <- c(tmp$subject[1], iter, -9999, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      }      
    }
  }
  names(fits) <- c("subject", "iter", "logL", "n", "a", "b", "s", "p", "sa", "sb", "ss", "sp")
  return(fits)
}

