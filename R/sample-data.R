create.regr.data <- function(n,p,t="continuous"){
  p.continuous <- round(p*runif(1))
  p.binary <- p-p.continuous
  noise <- runif(1,0,10)

  betas <- runif(p+1,-10,10)
  # set random means and random sampling probabilities for binomial
  random.params <- c(runif(p.continuous,-10,10),runif(p.binary))

  X <- matrix(rep(NA,n*p),nrow=n,ncol=p)
  for(i in 1:p){
    if(i <= p.continuous){
      #X[,i] <- rnorm(n,random.params[i])
      X[,i] <- rnorm(n)
    }
    else{
      #X[,i] <- sample(c(0,1),n,replace = TRUE,prob=c(random.params[i],1-random.params[i]))
      X[,i] <- sample(c(0,1),n,replace = TRUE)
    }
  }
  if(t=="continuous"){
    Y <- X %*% betas[2:(p+1)] + betas[1]
  }
  else if(t=="binary"){
    Z <- X %*% betas[2:(p+1)] + betas[1]
    pr <- 1/(1+exp(-Z))
    Y <- rbinom(n,1,pr)
  }
  else{
    warning("t must be either continuous or binary")
  }

  df <- data.frame(cbind(X,Y))
  names(df)[p+1] <- "y"
  return(list(True_Betas=betas,
         Data=df))
}

dd <- create.regr.data(100,2)
dd.data <- dd$Data
dd.beta <- dd$True_Betas

lm(y~.,data=dd.data)
dd.beta

