+norml <- function(test){
  norml_dataset <- c()
  for (i in 1:ncol(test)){
    temp <- c()
    for (j in 1:nrow(test)){
      if (is.na(test[j, i])){
        temp <- append(temp, NA)
      }else{
        temp <- append(temp, (test[j,i] - min(na.omit(test[,i])))/(max(na.omit(test[,i])) - min(na.omit(test[,i]))) )
      }
    }
    norml_dataset <- cbind(norml_dataset, temp)
  }
  return (norml_dataset)
}