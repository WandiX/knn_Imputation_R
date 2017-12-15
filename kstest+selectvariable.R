
#Use Kolmogorov-Smirnov Tests to evaluate whether estimated values are from the same distribution
test_of_samedist <-function(dataset, dadj_val){
    datasetWithNoNulls <- na.omit(dataset)
    if (ks.test(dadj_val, datasetWithNoNulls)$p.value > 0.05){
        return (dadj_val)
    }
    else {
        return (dataset)
    }
}  
  
#Normalization of data
norml <- function(test){
    rn <- rownames(test)
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
    rownames(norml_dataset) <- rn
    return (norml_dataset)
}


