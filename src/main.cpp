#include <iostream>
#include <string>
#include<math.h>

#include<Rcpp.h>

#include"Omega.h"

using namespace Rcpp;

// [[Rcpp::export]]
List fpopTreeTransfer(NumericVector vectData, List tree_t, std::string type, NumericVector vectWeight, bool testMode)
{
  /////////////////////////////
  /////////// OMEGA ///////////
  /////////////////////////////

  Omega omega;
  if(testMode == FALSE){omega.fpopTree();}else{omega.fpopTreeTestMode();}

  /////////////////////////////
  /////////// RETURN //////////
  /////////////////////////////

  List res = List::create(
    _["changepoints"] = omega.GetChangepoints(),
    _["param"] = omega.GetParameters(),
    _["cost"] = omega.GetGlobalCost()
  );

  return res;
}
