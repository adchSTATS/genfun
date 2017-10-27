#' Calculate the angle between two vectors
#'
#' Calculates the angle in radians between two vectors.
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @return A numeric value.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @export

angle <- function(x,y){
  dp <- x%*%y
  xNorm <- norm(x, type = "2")
  yNorm <- norm(y, type = "2")
  ang <- acos(dp / (xNorm * yNorm))
  as.numeric(ang)
}
