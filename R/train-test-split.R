#' A function for splitting data into a train/test set
#'
#' This function splits data into a training and testing set.
#' You can choose the test size.
#' It returns a list with the training data and test data as separate objects.
#' @param data a data.frame or matrix where rows equal observations. Location of xs and ys does not matter.
#' @param test_size a real value in (0,1). Default is 0.2. If less than 0 or greater than 1 an error is returned.
#' @export
#' @examples
#' df <- create.regr.data(100,4,ytype="continuous")$Data
#' train_test_slit(df)

train_test_split <- function(data,test_size=0.2){
  if(test_size <= 0 | test_size >=1){
    stop("test_size must not be less than or equal to 0 or greater than or equal to 1")
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
