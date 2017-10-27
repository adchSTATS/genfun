#' Calculate fraction
#'
#' Function for calculating fractions.
#' @param a A vector of numeric values that constitute the numerator of the fractions
#' @param b A vector of numeric values that constitute the denominator of the fractions. The vector must have the same length as a.
#' @return A numeric vector of fractions.
#' @details This function has been created because a/0 returns Inf by base R. This function returns NA when dividing by 0. When a and b are vectors the function returns a vector of fractions of the same length as a and b.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @export

ratio <- function(a, b){
  result <- a/b
  result[b == 0] = NA
  result
}
