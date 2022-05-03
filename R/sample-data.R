#' A function for creating random regression data.
#'
#' This function returns a random
#' @param n The number of observations in the random data set.
#' @param p The number of parameters in the random data set. The function randomly chooses how many will be binary or continuous.
#' @param ytype Determines whether to have a binary or continuous response variable. Defaults to "continuous"
#' @export
#' @examples
#' create.regr.data(100,4,ytype="continuous")
#' create.regr.data(100,4,ytype="binary")

create.regr.data <- function(n,p,ytype="continuous"){
  p.continuous <- round(p*runif(1))
  p.binary <- p-p.continuous
  noise <- runif(1,0,10)

  betas <- runif(p+1,-10,10)
  # set random means and random sampling probabilities for binomial
  random.params <- c(runif(p.continuous,-100,100),runif(p.binary))

  X <- matrix(rep(NA,n*p),nrow=n,ncol=p)
  for(i in 1:p){
    if(i <= p.continuous){
      X[,i] <- rnorm(n,random.params[i])
    }
    else{
      X[,i] <- sample(c(0,1),n,replace = TRUE,prob=c(random.params[i],1-random.params[i]))
    }
  }
  if(ytype=="continuous"){
    Y <- X %*% betas[2:(p+1)] + betas[1] + rnorm(nrow(X))
  }
  else if(ytype=="binary"){
    Z <- X %*% betas[2:(p+1)] + betas[1] + rnorm(nrow(X))
    pr <- 1/(1+exp(-Z))
    Y <- rbinom(n,1,pr)
  }
  else{
    warning("ytype must be either continuous or binary")
  }

  df <- data.frame(cbind(X,Y))
  names(df)[p+1] <- "y"
  return(list(True_Betas=betas,
         Data=df))
}
