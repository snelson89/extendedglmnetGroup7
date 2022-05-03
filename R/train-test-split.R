train_test_split <- function(data,test_size=0.2){
  if(test_size <= 0 | test_size >=1){
    warning("test_size must not be less than or equal to 0 or greater than or equal to 1")
  }
  else{
    n <- nrow(data)
    test_index <- sample(1:n,n*test_size,replace = FALSE)
    data.test <- data[test_index,]
    data.train <- data[-test_index,]
    return(list(Train_data = data.train,
                Test_data = data.test))
  }
}
