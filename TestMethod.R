# Computation of max quantization relative error 
mxre <- function(estimated, real){
  temp <- abs(estimated - real)
  res <- max(temp)
  return(res)
  }

# Computation of mean relative error
mre <- function(estimated, real){
  temp <- abs(estimated - real)
  res <- mean(sum(temp))
  return(res)
}

# Generating random positions, and replace them with NA
change_po <- function(data, num, range_min, range_max){
  temp <- round(runif(num, range_min, range_max))
  data[temp] = NA
  return(data)
}
