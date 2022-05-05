#' A function for creating random regression data.
#'
#' This function creates a random data.frame with n observations of p parameters.
#' You can choose whether or not the response variable y is binary or continuous.
#' It returns a list with the data.frame as well as a vector of the true beta values used to create the data.
#' @param n The number of observations in the random data set.
#' @param p The number of parameters in the random data set. The function randomly chooses how many will be binary or continuous.
#' @param ytype Determines whether to have a binary or continuous response variable. Defaults to "continuous"
#' @param include.categorical A boolean condition that determines whether or not to include categorical predictors. By default it is set to FALSE
#' @export
#' @examples
#' create.regr.data(100,4,ytype="continuous")
#' create.regr.data(100,4,ytype="binary")

create.regr.data <- function(n,p,ytype="continuous",include.categorical=FALSE){

  if(ytype != "continuous" & ytype != "binary"){
    stop("ytype must be 'continuous' or 'binary'")
  }

  if(include.categorical==TRUE){
    p.continuous <- round(p*runif(1))
  }
  else{
    p.continuous <- p
  }

  p.binary <- p-p.continuous

  betas <- runif(p+1,-10,10)
  # set random means and random sampling probabilities for binomial
  random.params <- c(runif(p.continuous,-10,10),runif(p.binary))

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
    Y <- X %*% betas[2:(p+1)] + betas[1] + rnorm(nrow(X),0,200)
  } else if(ytype=="binary"){
    Z <- X %*% betas[2:(p+1)] + betas[1] + rnorm(nrow(X),0,200)
    #pr <- 1/(1+exp(-Z))
    pr=exp(Z)/(1+exp(Z))
    Y <- rbinom(n,1,pr)
  }

  df <- data.frame(cbind(X,Y))
  names(df)[p+1] <- "y"
  # make binary predictors factors
  if(include.categorical == TRUE){
    df[(p.continuous+1):p] <- lapply(df[(p.continuous+1):p] , factor)
  }
  return(list(True_Betas=betas,
         Data=df))
}

