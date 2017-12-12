
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
  
#Select instrumental variables which has correlation with the target master variable to perform the estimation
select_variable <-function (i, dataset){
    temp <- lm(dataset[,i] ~ dataset[, 1:(i-1)] + dataset[, (i+1):dim(dataset)[2]])
    pr_tvalue <- temp$coefficients[, 4]
    list_variable_name <- dataset[0,][-i] #construct a vector of names without no.i
    for (j in length(pr_tvalue)){
        variable_list <- c()
        if (pr_tvalue[j] < 0.05){
            append(variable_list, list_variable_name[j])
        }
    }
    return (variable_list)
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


