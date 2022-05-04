#' A custom wrapper for base function lm()
#'
#' This function takes a matrix of candidate predictors X and a vector of response variables Y.
#' @param X an n by p matrix of candidate predictors.
#' @param Y an n by 1 vector of responses
#' @export
#' @examples
#' extended.lm(X,Y)

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
#' extended.lm(X,Y)

extended.glm <- function(X,Y){
  data <- as.data.frame(cbind(X,y=Y))
  return(glm(y~.,data=data,family=binomial("logit")))
}

#' A custom wrapper for base function lm().
#'
#' This function takes a matrix of candidate predictors X and a vector of response variables Y.
#' @param X an n by p matrix of candidate predictors.
#' @param Y an n by 1 vector of responses
#' @export
#' @examples
#' extended.lm(X,Y)

extended.ridge <- function(X,Y,lambda=NULL,ytype="continuous"){
  data <- as.data.frame(cbind(X,y=Y))
  if(ytype=="continuous"){
    if(is.null(lambda)){
      cv.lasso <- glmnet::cv.glmnet(X, Y, alpha=0, nfolds=5, family="gaussian")
      l = cv.lasso$lambda.min
    }
    else{
      l = lambda
    }
    fit <- glmnet::glmnet(X,Y,family="gaussian",alpha=0,lambda=l)
  }
  else if(ytype=="binary"){
    if(is.null(lambda)){
      cv.lasso <- glmnet::cv.glmnet(X, Y, alpha=0, nfolds=5, family="binomial")
      l <- cv.lasso$lambda.min
    }
    else{
      l = lambda
    }

    fit <- glmnet::glmnet(X,Y,family="binomial",alpha=0,lambda=l)
  }
  return(fit)
}

# dd <- create.regr.data(50,100)
# X <- as.matrix(dd$Data[,1:ncol(dd$Data)-1])
# Y <- dd$Data[,"y"]
#
# dd2 <- create.regr.data(50,100,ytype="binary")
# X2 <- as.matrix(dd2$Data[,1:ncol(dd2$Data)-1])
# Y2 <- dd2$Data[,"y"]


