#' Compute CER
#'
#' @param reference True labels
#' @param estimate Clustering labels of the estimated model
#'
#' @return Classification error rate (CER, measures the clustering accuracy)
#'
Spartaco_CER = function(reference, estimate){
  if(length(reference) != length(estimate)) stop("The two objects have different length")
  n <- length(reference)
  value <- 0
  for(i in 1:(n-1)){
    mP <- mQ <- numeric(n-i)
    for(j in (i+1):(n)){
      if(reference[i] == reference[j]) mP[j-i] <- 1
      if(estimate[i] == estimate[j]) mQ[j-i] <- 1
    }
    value <- value + sum(abs(mP - mQ))
  }
  return(value/(n*(n-1)/2))
}