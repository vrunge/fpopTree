#include <iostream>
#include <string>
#include<math.h>


#include<Rcpp.h>

using namespace Rcpp;

//' fonctionTest
//'
//' @param n an integer
//' @return the same integer + 3
//' @export
// [[Rcpp::export]]
double fonctionTest(double n)
{
  n = n + 3;
  return n;
}
