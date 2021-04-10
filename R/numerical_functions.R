#' returns the mode of a numeric vector
#'
#' @param v Input vector
#'
#' @return The mode of the vector
#' @export getmode
#'
#' @examples
#' v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
#' result <- getmode(v)
#' print(result)
#'
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Returns the expected value and confidence intervals for a poisson-binomial distribution (large number of samples)
#'
#' @param p probabilities for each group
#' @param n weights (number of samples for each group)
#' @param alpha one-sided significance level default 0.025
#'
#' @return a vector containing expected_value, expected_positives_proportion, confidence_interval_proportion
#' @export poissonBinomialExpectedValue
#' @import poibin
#'
#' @examples
#' probabilita_cella <- c(0.53,0.52,0.78,0.64)
#' campione <- c(10,3,50,15)
#' poissonBinomialExpectedValue(probabilita_cella, campione)
#'
poissonBinomialExpectedValue <- function(p, n, alpha = 0.025) {
  expected_values = 0:sum(n)
  distrib <- dpoibin(kk=expected_values, pp=p, wts=n)
  expected_value_index <- which.max(distrib)
  expected_value <- expected_values[which.max(distrib)]

  cdistrib <- ppoibin(kk=expected_values, pp=p, wts=n)
  lower_bound_idx <- min(which(cdistrib[1:expected_value_index] >= alpha))

  expected_positives_proportion <- expected_value / length(distrib)
  confidence_interval_proportion <- (expected_value_index - lower_bound_idx) / length(distrib)
  return(c(expected_value, expected_positives_proportion, confidence_interval_proportion))
}



#' Standard Error of the Mean
#'
#' @param x a vector
#'
#' @return the SE
#' @export se
#'
#' @examples
#' 
#' se(rnorm(100,5,2))
#' 
se <- function(x) { 
  return (sqrt(var(x) / length(x)))
}

#' Confidence interval of the mean of a vector
#'
#' At a given alpha
#' 
#' @param x the vector
#' @param alpha the required two-sided confidence level (default 0.05 = 95%)
#'
#' @return the number corresponding to the one-sided confidence interval around the mean
#' @export ci
#'
ci <- function(x, alpha = 0.05) {
  t.value = qt((1 - alpha / 2), length(x) - 1)
  standard.error <- se(x)
  ci <- t.value * standard.error
}
