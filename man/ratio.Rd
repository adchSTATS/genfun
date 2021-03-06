% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ratio.R
\name{ratio}
\alias{ratio}
\title{Calculate fraction}
\usage{
ratio(a, b)
}
\arguments{
\item{a}{A vector of numeric values that constitute the numerator of the fractions}

\item{b}{A vector of numeric values that constitute the denominator of the fractions. The vector must have the same length as a.}
}
\value{
A numeric vector of fractions.
}
\description{
Function for calculating fractions.
}
\details{
This function has been created because a/0 returns Inf by base R. This function returns NA when dividing by 0. When a and b are vectors the function returns a vector of fractions of the same length as a and b.
}
\author{
Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
}
