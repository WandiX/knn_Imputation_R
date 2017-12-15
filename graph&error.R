knn_w<-c(0.567, 0.474, 0.451, 0.348, 0.419, 0.618, 0.452, 0.256, 0.385, 0.488, 0.25, 0.458, 0.281, 0.368, 0.211, 0.414, 0.379, 0.394, 0.387, 0.455, 0.368, 0.407, 0.387, 0.395, 0.447, 0.457, 0.371, 0.413, 0.34, 0.424)
oknn <- c(0.456, 0.385, 0.442,0.356,0.296,0.662,0.461,0.159,0.360,0.432,0.054,0.432,0.284,0.367,0.147,0.362,0.347,0.315,0.430,0.469,0.344,0.425,0.340,0.141,0.530,0.328,0.339,0.354,0.432)
survival <- real_value
plot(survival, ylim = c(-0.5, 1.2), col = 2, lty = 2)
lines(knn_w,pch = 3, col = 4)
lines(knn_w, type="b",pch = 3, col = 4)
lines(oknn,type = "c", pch = 4, col = 3)
lines(oknn, pch = 4, col = 3)
mxre <- function(estimated, real){
  +     temp <- abs(estimated - real)
  +     res <- max(temp)
  +     return(res)
  + }
mre <- function(estimated, real){
  +     temp <- abs(estimated - real)
  +     res <- mean(sum(temp))
  +     return(res)
  + }
mre(knn_w, survival)
[1] 14.3
mre(oknn, survival)
[1] 13.745
mxre(knn_w,survival)
[1] 0.75
mxre(oknn,survival)
[1] 0.946