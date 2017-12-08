knn_imputation <- function(dataset, k=3, method='weighted') {
    
    #Get the positions of NA
    posNA <- which(is.na(dataset), arr.ind=TRUE)
    dim_pos <- dim(posNA)
    if (dim_pos[1] == 0) return(dataset)
    
    #Get k near elements
    d <- dist(dataset, method="euclidean")
    dm <- as.matrix(d)
    diag(dm) <- NA
    
    #Get the order of distance for each data point  
    dorder <- apply(dm, 1, order, na.last=NA)
    ds_row <- nrow(dataset)
    if (k > ds_row) k <- ds_row
    
    if (is.list(dorder) == TRUE)
        dadj_pos = sapply(dorder, "[", c(1:k))
    else
        dadj_pos = dorder[c(1:k),]
    
    #Get the estimated value
    dadj_val = dataset
    if (method == 'weighted') {
        #Weighted k-nearest
        if (is.vector(dadj_pos)) {
            dadj_val = dataset[dadj_pos,]
        }
        else {
            d_sort <- apply(dm, 1, sort, na.last=NA, decreasing = FALSE)
            d_sort <- 1/d_sort
            if (is.list(d_sort))
                d_sort <- sapply(d_sort, "[", c(1:k))
            else
                d_sort <- d_sort[c(1:k),]
            d_sort_sum <- colSums(d_sort, na.rm = TRUE)
            weight <- apply(d_sort, 1, "/", d_sort_sum)
            #dadj_val = t(apply(dadj_pos, 2, get_imputation, ds=dataset, method=method, weight=weight))
            dadj_val = get_imputation_loop(dataset, method, dadj_pos, weight)
        }
    }
    else {
        #k-nearest
        if (is.vector(dadj_pos)) {
            dadj_val = dataset[dadj_pos,]
        }
        else {
            #dadj_val = t(apply(dadj_pos, 2, get_imputation, ds=dataset, method=method))
            dadj_val = get_imputation_loop(dataset, method, dadj_pos)
        }  
    }
    
    #Fill in the NAs in the dataset
    if (dim_pos[1] == 1) {
        dataset[posNA[1], posNA[2]] <- dadj_val[posNA[1], posNA[2]]
    }
    else {
        for (r in 1:dim_pos[1]) {
            dataset[posNA[r,1], posNA[r,2]] <- dadj_val[posNA[r,1], posNA[r,2]]
        }
    }
    
    return(dataset)
}

get_imputation_loop <- function(ds, method, pos, weight=NULL) {
    
    col_num <- ncol(pos)
    res <- NULL
    for (p in 1:col_num) {
        col_ <- pos[,p]
        val <- ds[col_,]
        if (method == 'weighted') {
            w <- weight[p,]
            dataset_weighted <- apply(val, 2, '*', w)
            sum_ <- colSums(dataset_weighted, na.rm=TRUE)
        }
        else {
            sum_ <- colMeans(val, na.rm=TRUE)
        }
        res <- rbind(res, sum_)
    }
    return(res);
}

