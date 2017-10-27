#include <Rcpp.h>
using namespace Rcpp;

//' Is the difference of two points contained in a cylinder?
//'
//' The function finds out if the vector between two point \code{x_1} and \code{x_2}
//' is contained in a specified cylinder.
//' Moreover, is \code{x_2} contained in some cylinder extending from \code{x_1}.
//' @param mat A matrix in which the rows represent a point in some three dimensional space.
//' @param dir A vector of the same dimension as the points.
//' @param r_sq A vector or single value of squared radii of a cylinder.
//' @param t A vector or single value of heights of a cylinder.
//' @return A \code{\link{list}} of \code{\link{data.frame}s}
//' where each list element is a unique combination of radius and height of the cylinder.
//' Each \code{\link{data.frame}} have two columns: \code{index1} and \code{index2}, s.t.
//' \code{mat[index2, ] - mat[index1, ]} is contained in the specified cylinder.
//' @details This function also works for two dimensions. In which case \code{r_sq} is half the width squared
//' @useDynLib genfun
//' @importFrom Rcpp sourceCpp
//' @export
// [[Rcpp::export]]
List whichdiffincyl(NumericMatrix mat, NumericVector dir, NumericVector r_sq, NumericVector t){
  int nrow = mat.nrow();
  int len = nrow * (nrow-1) / 2;
  IntegerVector indices1(len);
  IntegerVector indices2(len);
  int indexSkip;
  for(int i = 0; i != nrow; ++i){
    indexSkip = nrow * i - ((i+1) * i)/2;
    for(int j = 0; j != (nrow-1-i); ++j){
      indices1(indexSkip+j) = i+1;
      indices2(indexSkip+j) = i+j+1+1;
    }
  }
  NumericMatrix out1(nrow, nrow);
  NumericMatrix out2(nrow, nrow);
  for(int i = 0; i != len; ++i){
    NumericVector temp = mat(indices1(i) - 1, _);
    NumericVector temp2 = mat(indices2(i) - 1, _);
    NumericVector diff = temp - temp2;
    out1(indices1(i) - 1, indices2(i) - 1) = fabs(std::inner_product(diff.begin(), diff.end(), dir.begin(), 0.0));
    out2(indices1(i) - 1, indices2(i) - 1) = std::inner_product(diff.begin(), diff.end(), diff.begin(), 0.0);
  }
  List out;
  NumericVector::iterator i;
  NumericVector::iterator j;
  int itr = 1;
  for(i = r_sq.begin(); i != r_sq.end(); ++i){
    int itt = 1;
    for(j = t.begin(); j != t.end(); ++j){
      IntegerVector it1 = IntegerVector::create(itr, itt);
      CharacterVector it2 = as<CharacterVector>(it1);
      std::ostringstream it3;
      it3 << "r";
      it3 << it2(0);
      it3 << ".t";
      it3 << it2(1);
      std::vector<int> ind1;
      std::vector<int> ind2;
      for(int p = 0; p != (nrow - 1); ++p){
        for(int q = p + 1; q != nrow; ++q){
          if((out2(p, q) - (out1(p, q)*out1(p, q)) <= *i) && (out1(p, q) <= *j)){
            ind1.push_back(p + 1);
            ind2.push_back(q + 1);
          }
        }
      }
      out(it3.str()) = DataFrame::create(_["index1"] = ind1, _["index2"] = ind2);
      itt += 1;
    }
    itr += 1;
  }
  return out;
}
