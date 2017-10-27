#' Pairwise angles
#'
#' Computes the matrix of angles between all pairs of vectors.
#' @param mat A numeric matrix with each row representing a vector.
#' @return A square matrix whose \code{[i,j]} entry is the angle between the two vectors
#' represented in row \code{i} and \code{j} of \code{mat}.
#' @details The (i, j)-entry is the angle between the vectors represented by the i'th and j'th row of the input matrix
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @export

pairangle <- function(mat){
  dps <- tcrossprod(mat)
  norm.prods <- 1/tcrossprod(sqrt(diag(dps)))
  normalized.dps <- dps*norm.prods
  diag(normalized.dps) <- 1
  out <- acos(normalized.dps)
  return(out)
}
