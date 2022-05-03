extended.lm <- function(X,Y){
  data <- as.data.frame(cbind(X,y=Y))
  return(lm(y~.,data=data))
}

extended.glm <- function(X,Y){
  data <- as.data.frame(cbind(X,y=Y))
  return(glm(y~.,data=data,family=binomial("logit")))
}



X <- dd.data[,1:ncol(dd.data)-1]
Y <- dd.data[,ncol(dd.data)]

extended.lm(X,Y)
