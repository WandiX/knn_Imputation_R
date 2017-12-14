# Computation of max quantization relative error 
mxre <- function(estimated, real){
  temp <- abs(estimated - real)/real
  res <- max(temp) * 100
  return(res)
  }

# Computation of mean relative error
mre <- function(estimated, real){
  temp <- abs(estimated - real)/ real
  res <- mean(sum(temp)) *100
  return(res)
}

# Generating random positions, and replace them with NA
change_po <- function(data, num, range_min, range_max){
  temp <- round(runif(num, range_min, range_max))
  data[temp] = NA
  return(data)
}