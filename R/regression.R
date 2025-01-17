#' A custom wrapper for base function lm()
#'
#' This function takes a matrix of candidate predictors X and a vector of response variables Y.
#' @param X an n by p matrix of candidate predictors.
#' @param Y an n by 1 vector of responses
#' @export
#' @examples
#' require(glmnet)
#' data("QuickStartExample")
#' x <- QuickStartExample$x
#' y <- QuickStartExample$y
#' extended.lm(x,y)

extended.lm <- function(X,Y){
  data <- as.data.frame(cbind(X,y=Y))
  return(lm(y~.,data=data))
}

#' A custom wrapper for base function glm(family=binomial("logit))
#'
#' This function takes a matrix of candidate predictors X and a vector of binary response variables Y. .
#' @param X an n by p matrix of candidate predictors.
#' @param Y an n by 1 vector of binary responses
#' @export
#' @examples
#' n <- 1000
#' B0 <- 3
#' B1 <- 5
#' X1 <- rnorm(n)
#' B2 <- -2
#' X2 <- rnorm(n)
#' X <- as.matrix(cbind(X1=X1,X2=X2))
#' Z <- B0 + B1*X1 + B2*X2
#' pr=exp(Z)/(1+exp(Z))
#' Y <- rbinom(n,1,pr)
#' fit <- extended.glm(X,Y2)
#' coef(fit)

extended.glm <- function(X,Y){
  data <- as.data.frame(cbind(X,y=Y))
  return(glm(y~.,data=data,family=binomial("logit")))
}

#' A custom wrapper for base function glmnet::glmnet(alpha=0).
#'
#' This function takes a matrix of candidate predictors X and a vector of response variables Y and performs ridge regression.
#' @param X an n by p matrix of candidate predictors.
#' @param Y an n by 1 vector of responses
#' @param lambda an optional lambda value. If no lambda is provided, an optimal lambda is
#' automatically determined using glmnet::cv.glmnet(alpha=0).
#' @param ytype description as to whether or not the response variable y is binary or continuous. Defaults to 'continuous'.
#' @export
#' @examples
#' n <- 200      
#' p <- 500
#' s <- 10
#' beta <- rep(0, p)
#' beta[1:s] <- runif(s, 1/3, 1)
#' x <- rmvnorm(n = n, mean = rep(0, p), method = "svd")
#' signal <- sqrt(mean((x %*% beta)^2))
#' sigma <- as.numeric(signal / sqrt(10))  # SNR=10
#' y <- x %*% beta + rnorm(n)
#' extended.ridge(x,y)

extended.ridge <- function(X,Y,lambda=NULL,ytype="continuous"){
  #data <- as.data.frame(cbind(X,y=Y))

  if(ytype != "continuous" & ytype != "binary"){
    stop("ytype must be 'continuous' or 'binary'")
  } else if(is.numeric(lambda)){
    if(lambda <= 0){
      stop("lambda must be greater than 0")
    }
  }

  if(ytype=="continuous"){
    fam <- "gaussian"
  } else if(ytype=="binary"){
    fam <- "binomial"
  }

  if(is.null(lambda)){
    cv.lasso <- glmnet::cv.glmnet(data.matrix(X), Y, alpha=0, nfolds=5, family=fam)
    l = cv.lasso$lambda.min
  }else{
    l = lambda
  }

  fit <- glmnet::glmnet(X,Y,family=fam,alpha=0,lambda=l)
  return(fit)
}

#' A custom wrapper for base function glmnet::glmnet(alpha=1).
#'
#' This function takes a matrix of candidate predictors X and a vector of response variables Y and performs lasso regression.
#' @param X an n by p matrix of candidate predictors.
#' @param Y an n by 1 vector of responses
#' @param lambda an optional lambda value. If no lambda is provided, an optimal lambda is
#' automatically determined using glmnet::cv.glmnet(alpha=1).
#' @param ytype description as to whether or not the response variable y is binary or continuous. Defaults to 'continuous'.
#' @export
#' @examples
#' n <- 200      
#' p <- 500
#' s <- 10
#' beta <- rep(0, p)
#' beta[1:s] <- runif(s, 1/3, 1)
#' x <- rmvnorm(n = n, mean = rep(0, p), method = "svd")
#' signal <- sqrt(mean((x %*% beta)^2))
#' sigma <- as.numeric(signal / sqrt(10))  # SNR=10
#' y <- x %*% beta + rnorm(n)
#' extended.lasso(x,y)

extended.lasso <- function(X,Y,lambda=NULL,ytype="continuous"){

  if(ytype != "continuous" & ytype != "binary"){
    stop("ytype must be 'continuous' or 'binary'")
  } else if(is.numeric(lambda)){
    if(lambda <= 0){
      stop("lambda must be greater than 0")
    }
  }

  data <- as.data.frame(cbind(X,y=Y))

  if(ytype=="continuous"){
    fam <- "gaussian"
  } else if(ytype=="binary"){
    fam <- "binomial"
  }

  if(is.null(lambda)){
    cv.lasso <- glmnet::cv.glmnet(data.matrix(X), Y, alpha=1, nfolds=5, family=fam)
    l = cv.lasso$lambda.min
  }
  else{
    l = lambda
  }

  fit <- glmnet::glmnet(X,Y,family=fam,alpha=1,lambda=l)

  return(fit)
}

#' An implementation of random lasso regression.
#'
#' This function takes a matrix of candidate predictors X and a vector of response variables Y and performs random lasso regression.
#' @param X an n by p matrix of candidate predictors.
#' @param Y an n by 1 vector of responses
#' @param lambda an optional lambda value. If no lambda is provided, an optimal lambda is automatically determined using glmnet::cv.glmnet(alpha=1).
#' @param B the number of Bootstrap samples. Default is 200.
#' @param q1 the number of random predictors to use in step 1 of the algorithm. Default is length(Y). Must be less than or equal to this value.
#' @param q2 the number of random predictors to use in step 2 of the algorithm. Default is length(Y). Must be less than or equal to this value.
#' @param ytype description as to whether or not the response variable y is binary or continuous. Defaults to 'continuous'.
#' @export
#' @examples
#' extended.randomlasso(X,Y)

extended.randomlasso <- function(X,Y,lambda=NULL,B=200,q1=length(Y),q2=length(Y),ytype="continuous"){

  if(ytype != "continuous" & ytype != "binary"){
    stop("ytype must be 'continuous' or 'binary'")
  } else if(is.numeric(lambda)){
    if(lambda <= 0){
      stop("lambda must be greater than 0")
    }
  } else if(q1 > length(Y) | q2 > length(Y)){
    stop("q values must both be less than or equal to length(Y)")
  }

  # Step 1a

  # make matrix of indices for Bootstrap samples
  # B = number of Bootstraps
  # n = number of observations

  n <- length(Y)

  if(ytype=="continuous"){
    fam <- "gaussian"
  } else if(ytype=="binary"){
    fam <- "binomial"
  }

  S1 <- matrix(sample(1:n,n*B,replace=T),nrow=n,ncol=B)

  # Step 1b

  B.estimate <- matrix(rep(0,B*ncol(X)),nrow=ncol(X),ncol=B)
  B.estimate.s <- matrix(rep(0,B*ncol(X)),nrow=ncol(X),ncol=B)

  for(i in 1:B){
    # select q candidate variables based on number of predictors in X (ncol(X))
    cand <- sort(sample(1:ncol(X),q1,replace=F))
    # Make X matrix from Bootstrap indices from step1 (S1 matrix is nxB)
    # select only the candidate variables as well (cand)
    X1 <- X[S1[,i],cand]
    # Make Y vector from Bootstrap indices from step1
    Y1 <- Y[S1[,i]]

    if(is.null(lambda)){
      # Use cross-validation to find lambda
      cv.lasso <- glmnet::cv.glmnet(data.matrix(X1), Y1, alpha=1, nfolds=5, family=fam,intercept=FALSE)
      l <- cv.lasso$lambda.min
    }
    else{
      l <- lambda
    }

    # Create model based on lambda, X1, Y1
    # glmnet mean-centers data by default: https://statisticaloddsandends.wordpress.com/2018/11/15/a-deep-dive-into-glmnet-standardize/
    # (standardize=TRUE)
    # change intercept parameter to FALSE
    m1 <- glmnet::glmnet(X1,Y1,family=fam,alpha=1,lambda=l,intercept=FALSE)

    # assign coefficients to their correct spot in matrix
    # Logic = cand variable is sorted, so coef(m) will match that ordering (+2 due to intercept row and unknown second row)
    for(j in 1:q1){
      B.estimate[cand[j],i] <- coef(m1)[j+2]
    }
  }

  # Step 1c

  Importance <- abs(rowSums(B.estimate)/B)

  # Step 2a

  S2 <- matrix(sample(1:n,n*B,replace=T),nrow=n,ncol=B)

  # Step 2b

  # Normalize importance weights

  N.Importance <- Importance/sum(Importance)

  B.estimate2 <- matrix(rep(0,B*ncol(X)),nrow=ncol(X),ncol=B)

  int.val <- rep(NA,B)

  for(i in 1:B){
    # select q candidate variables based on number of predictors in X (ncol(X))
    cand2 <- sort(sample(1:ncol(X),q2,replace=F,prob=N.Importance))
    # Make X matrix from Bootstrap indices from step1 (S1 matrix is nxB)
    # select only the candidate variables as well (cv)
    X2 <- X[S2[,i],cand]
    # Make Y vector from Bootsrap indices from step1
    Y2 <- Y[S2[,i]]

    if(is.null(lambda)){
      # Use cross-validation to find lambda
      cv.lasso2 <- glmnet::cv.glmnet(data.matrix(X2), Y2, alpha=1, nfolds=5, family=fam,intercept=FALSE)
      l2 <- cv.lasso2$lambda.min
    }
    else{
      l2 <- lambda
    }

    # Create model based on lambda, X2, Y2
    # glmnet mean-centers data by default: https://statisticaloddsandends.wordpress.com/2018/11/15/a-deep-dive-into-glmnet-standardize/
    # (standardize=TRUE)
    # change intercept parameter to FALSE
    m2 <- glmnet::glmnet(X2,Y2,family=fam,alpha=1,lambda=l2,intercept=FALSE)

    # assign coefficients to their correct spot in matrix
    # Logic = cand variable is sorted, so coef(m) will match that ordering (+1 due to interept)
    for(j in 1:q2){
      B.estimate2[cand[j],i] <- coef(m2)[j+2]
    }
  }

  # Step 2c

  Final.Betas <- rowSums(B.estimate2)/B

  return(list(B.coefs=Final.Betas))
}


