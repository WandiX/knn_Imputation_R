
#knn_imputation: Compute the knn imputation values for dataset
#k: the number of closest elements used (If k < 1 or k > number of rows in dataset, k will be set using the number of rows in dataset)
#method:  
# - When method == 'weighted', the function used distance weight to compute the weighted average
# - Otherwise, use unweighted average
#distance:
# - When distance == "euclidean", use classic euclidean method to compute distance
# - When distance == "quad", divide quadrants according to each incomplete data point
#L: the longest distance that takes into consideration
#If L == 0, consider all the complete points
#If L > 0, use given L. Return NULL if L is too small
#If L < 0, L = median of all complete points  
knn_imputation <- function(dataset, k=0, method='weighted', distance='euclidean', L=0) {

    #Get the positions of NA
    posNA <- which(is.na(dataset), arr.ind=TRUE)
    nrow_pos <- nrow(posNA)
    if (nrow_pos == 0 || is.vector(dataset)) return(dataset)
    
    ds_row <- nrow(dataset)
    if (k > ds_row) k <- ds_row
    if (k < 1) k <- ds_row
    
    
    if (distance == "quad") {
        dataset <- get_quad_val(dataset, posNA, L, k, method)
        return(dataset)
    }
    
    if (distance != "euclidean") return(NULL)
    dm <- get_dist_matrix(dataset, posNA)
    d_order <- NULL
    if (is.vector(d_order))
        d_order <- order(dm, na.last=NA)
    else
        d_order <- apply(dm, 1, order, na.last=NA)
    d_order <- intercept_k(d_order, k)
    dm <- apply(dm, 1 ,sort)
    dm <- intercept_k(dm, k)
    dataset <- get_estimate_value(method, dataset, d_order, posNA, dm)
    
    return(dataset)
}

#Get the estimate value when using quad distance
get_quad_val <- function(dataset, posNA, L, k, method) {

    nrow_pos <- nrow(posNA)
    
    quad_order <- get_quad_order(dataset, posNA, k)
    
    if (L < 0) {
        med <- median(unlist(quad_order))
        res <- intercept_L(quad_order, med)
        quad_order <- res
    }
    else if (L > 0) {
        res <- intercept_L(quad_order, L)
        if (is.null(res)) return(NULL)   #If L is too small so that some NAs don't have nearest data points
        quad_order <- res
    }
    for (i in 1:nrow_pos) {
        if (!is.list(quad_order))    #When there is only one missing value
            li <- quad_order
        else
            li <- quad_order[[i]]

        col_ind <- names(li)
        if (nrow_pos == 1) {
            col_val<-dataset[col_ind, posNA[2]]
            if (length(col_val) == 1) dataset[posNA[1], posNA[2]] <- col_val
            else {
                if (method == "weighted") {
                    t_ <- 1 / li
                    t_sum <- sum(t_)
                    weight <- t_ / t_sum
                    dataset[posNA[1], posNA[2]] <- sum(weight * col_val)
                }
                else {
                    dataset[posNA[1], posNA[2]] <- mean(col_val)
                }
            }
        }
        else {
            col_val<-dataset[col_ind, posNA[i,2]]
            if (length(col_val) == 1) dataset[posNA[i,1], posNA[i,2]] <- col_val
            else {
                if (method == "weighted") {
                    t_ <- 1 / li
                    t_sum <- sum(t_)
                    weight <- t_ / t_sum
                    dataset[posNA[i,1], posNA[i,2]] <- sum(weight * col_val)
                }
                else {
                    dataset[posNA[i,1], posNA[i,2]] <- mean(col_val)
                }
            }
        }
    }
    return(dataset)

}

#Get the estimate value when using euclidean distance
get_estimate_value <- function(method, dataset, d_order, posNA, dm) {
    
    nrow_pos <- nrow(posNA)
    
    #If ig == TRUE, there is no need to do computation
    ig <- ((is.vector(d_order) && length(d_order) > 1 && nrow_pos > 1) || (is.vector(d_order) && length(d_order) == 1 && nrow_pos == 1))
        
    weight <- NULL
    if (!ig && method == "weighted") {

        t_ <- 1 / dm
        
        if (is.vector(dm)) {
            t_sum <- sum(t_)
            weight <- t_ / t_sum
        }
        else  {
            t_sum <- colSums(t_)
            weight <- apply(t_, 1, "/", t_sum)
        }
        
    }
    
    for (i in 1:nrow_pos) {
        pos <- NULL
        ord <- NULL
        
        #Only one nearest data point
        if (ig) {
            ord <- d_order[i]
        }
        else {
            if (is.vector(d_order))
                ord <- d_order
            else
                ord <- d_order[,i]      
        }
        
        #Only one missing value    
        if (nrow_pos == 1)
            pos <- posNA
        else
            pos <- posNA[i,]
        
        ds_use <- dataset[ord, pos[2]]
        if (ig) {
            dataset[pos[1], pos[2]] <- ds_use
        }
        else {
            est <- NULL
            if (method == "weighted") {
                w <- NULL
                if (is.vector(weight))
                    w <- weight
                else
                    w <- weight[i,]            
                est <- sum(ds_use*w)
            }
            else {
                est <- mean(ds_use)
            }
            dataset[pos[1], pos[2]] <- est 
        }
                   
    }

    return(dataset)
}

#Only maintain the data points that are nearer than L
intercept_L <- function(quad_order, L) {
    len <- length(quad_order)

    for (i in 1:len) {
        li <- lapply(quad_order[[i]], sort)
        len_li <- length(li)
        for (j in 1:len_li) {
            if (li[[j]] > L) {
                if (j == 1) {
                    return(NULL)
                }
                else
                    quad_order[[i]] <- li[1:(j-1)]
                break;
            }
        }
    }
    return(quad_order)
}

#Extract the closest k data points for each incomplete data point
intercept_k <- function(ds, k) {

    if (is.list(ds))
        ds <- sapply(ds, "[", c(1:k))
    else {
        row_ <- NULL
        if (is.matrix(ds)) {
            row_ <- nrow(ds)
            if (k > row_) k <- row_
            else if (k < row_) ds<-ds[c(1:k)]
        }            
        else { 
            row_ <- length(ds)
            if (k > row_) k <- row_
            else if (k < row_) ds <- ds[c(1:k),]
        }
    }
    return(ds)
}

#Use standard euclidean method to compute distance
get_dist_matrix <- function(ds, posNA) {

    row_num <- nrow(posNA)
    res <- matrix(, nrow=row_num, ncol=nrow(ds))
    for (r in 1:row_num) {
        row_pos <- posNA[r,]
        row_ds <- ds[row_pos[1],-row_pos[2]]
        ds_rm <- ds[-row_pos[1],-row_pos[2]]
        ds_rm <- ds_rm[complete.cases(ds_rm),]
        res_row <- apply(ds_rm, 1, compute_dist, row1=row_ds)
        rows <- strtoi(rownames(data.frame(res_row)))
        res[r, rows] <- res_row  
        
    }
    return(res)
}

#Divide quadrants and get close data points for each incomplete data point
get_quad_order <- function(ds, posNA, k) {
    row_num <- 1
    if (is.matrix(posNA))
        row_num <- nrow(posNA)
    curr <- NULL
    lists <- NULL
    for (r in 1:row_num) {
        if (is.vector(posNA))
            curr <- posNA
        else
            curr <- posNA[r,]
        row_ <- ds[curr[1],-curr[2]]
        ds_rm <- ds[-curr[1], -curr[2]]
        ds_rm <- ds_rm[complete.cases(ds_rm),]
        val <- do.call(rbind, apply(ds_rm, 1, "-", row_))
        sqrtSum <- apply(val, 1, sqrt_sum)
        quad_vec <- do.call(rbind, apply(val, 1, get_quad))
        quad_list <- get_quad_list(sqrtSum, quad_vec)
        quad_list <- lapply(quad_list, sort)
        if (k != 0) quad_list <- lapply(quad_list, intercept_k_quad, k=k)
        merged_list <- unlist(quad_list)
        if (is.null(lists)) {
            lists <- merged_list
        }
        else {
            lists <- list(lists, merged_list)
        }
    }
    return(lists)
}

intercept_k_quad <- function(li, k) {
    len <- length(li)
    if (k >= len) return(li) 
    else return(li[1:k])
}

#Get the lists of quadrants and divide data
get_quad_list <- function(sqrtSum, quad_vec) {

    row_num <- NULL
    if (is.vector(quad_vec))
        row_num <- 1
    else
        row_num <- nrow(quad_vec)
    
    quad_list <- NULL;
    rowNames <- row.names(quad_vec)

    while (row_num > 0) {
        if (is.vector(quad_vec))
            curr <- quad_vec
        else
            curr <- quad_vec[1,]
        li <- NULL
        rm_list <- NULL
        for (r in 1:row_num) {
            ind <- rowNames[1]
            if (is.vector(quad_vec)) {
                cmp <- quad_vec
            }
            else {
                cmp <- quad_vec[r, ]
                ind <- rowNames[r]
            }
            if (isEqual(curr, cmp)) {
                li <- c(li, sqrtSum[r])
                rm_list <- c(rm_list, ind)
            }
        }
        names(li) <- rm_list

        if (is.vector(quad_vec))
            row_num <- 0
        else {
            quad_vec <- quad_vec[-which(row.names(quad_vec)%in%rm_list),]
            rowNames <- rowNames[!rowNames%in%rm_list]
            if (is.vector(quad_vec)) {
                row_num <- 1
            }
            else {
                row_num <- nrow(quad_vec) 
            }
        }
        
        if (is.null(quad_list))
            quad_list <- li
        else
            quad_list <- list(quad_list, li)
        
    }

    return(quad_list)
}

#Compare row1 and row2, Return TRUE if row1 == row2
isEqual <- function(row1, row2) {
    
    isequal <- TRUE
    len <- length(row1)
    for (r in 1:len) {
        if (row1[[r]] != row2[[r]]) {
            isequal <- FALSE
            break
        }
    }
    
    return(isequal)
}

get_quad <- function(r) {
    quad <- lapply(r, get_symbol)
    return(quad)
}

get_symbol <- function(col_) {
    if (col_ > 0) return(1)
    else return(0)
} 

sqrt_sum <- function(r) {
    res <- sqrt(sum(r^2))
    return(res)
}

compute_dist <- function(row1, row2) {
    return(sqrt(sum((row1-row2)^2, na.rm = TRUE)))
}


