knn_imputation <- function(dataset, k=0, method='weighted', distance='euclidean') {
    #nNA <- sum(is.na(data))
    
    #Get the positions of NA
    posNA <- which(is.na(dataset), arr.ind=TRUE)
    dim_pos <- dim(posNA)
    if (dim_pos[1] == 0 || is.vector(dataset)) return(dataset)
    
    #Get k near elements
    d <- NULL
    if (distance == "euclidean")
        d <- get_dist_matrix(dataset)
    else
        d <- dist(dataset, method="euclidean")
    dm <- as.matrix(d)
    diag(dm) <- NA
    
    #Get the order of distance for each data point  
    dorder <- apply(dm, 1, order, na.last=NA)
    print(dorder)
    ds_row <- nrow(dataset)
    if (k > ds_row) k <- ds_row
    if (k < 1) k <- ds_row
    #print(k)
    #print(dorder)
    
    dadj_pos <- intercept_k(dorder, k)
    
    #Get the estimated value
    dadj_val = get_estimate_value(method, dataset, dadj_pos)
    
    #Fill in the NAs in the dataset
    if (dim_pos[1] == 1) {
        #print(posNA)
        #print(dadj_val)
        dataset[posNA[1], posNA[2]] <- dadj_val[posNA[1], posNA[2]]
    }
    else {
        for (r in 1:dim_pos[1]) {
            dataset[posNA[r,1], posNA[r,2]] <- dadj_val[posNA[r,1], posNA[r,2]]
        }
    }
    
    return(dataset)
}

get_estimate_value <- function(method, dataset, dadj_pos) {
    if (method == 'weighted') {
        #Weighted k-nearest
        if (is.vector(dadj_pos)) {
            #print(dadj_pos)
            dadj_val = dataset[dadj_pos,]
        }
        else {
            d_sort <- apply(dm, 1, sort, na.last=NA, decreasing = FALSE)
            d_sort <- 1/d_sort
            d_sort <- intercept_k(d_sort, k)
            #print(d_sort)
            d_sort_sum <- colSums(d_sort, na.rm = TRUE)
            weight <- apply(d_sort, 1, "/", d_sort_sum)
            #dadj_val = t(apply(dadj_pos, 2, get_imputation, ds=dataset, method=method, weight=weight))
            print(weight)
            dadj_val = get_imputation(dataset, method, dadj_pos, weight)
        }
    }
    else {
        #k-nearest
        if (is.vector(dadj_pos)) {
            dadj_val = dataset[dadj_pos,]
        }
        else {
            #dadj_val = t(apply(dadj_pos, 2, get_imputation, ds=dataset, method=method))
            dadj_val = get_imputation(dataset, method, dadj_pos)
        }  
    }

}

intercept_k <- function(ds, k) {
    #Extract the closest k data points
    if (is.list(ds))
        ds <- sapply(ds, "[", c(1:k))
    else
        ds <- ds[c(1:k),]
    return(ds)
}

get_dist_matrix <- function(ds) {
    #Use standard euclidean method to compute distance
    row_num <- nrow(ds)
    res <- matrix(, nrow=row_num, ncol=row_num)
    for (r in 1:row_num) {
        row_ <- ds[r,]
        ds_rm <- ds[-r,]
        ds_rm <- ds_rm[complete.cases(ds_rm),]
        res_row <- apply(ds_rm, 1, compute_dist, row1=row_)
        rows <- strtoi(rownames(data.frame(res_row)))
        res[rows, r] <- res_row
        res[r, rows] <- res_row
    }
    return(res)
}

compute_dist <- function(row1, row2) {
    return(sqrt(sum((row1-row2)^2, na.rm = TRUE)))
}

get_imputation <- function(ds, method, pos, weight=NULL) {
    
    col_num <- ncol(pos)
    res <- NULL
    #print(pos)
    #print(col_num)
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

