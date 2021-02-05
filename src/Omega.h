#ifndef OMEGA_H
#define OMEGA_H

#include <Rcpp.h>
#include <math.h>
#include<vector>

#include <stdlib.h>

class Omega
{
  public:
    Omega();
    ~Omega();

    std::vector< std::vector< int > > GetChangepoints() const;
    std::vector< std::vector< double > > GetParameters() const;
    std::vector< double > GetGlobalCost() const;

    void fpopTree();
    void fpopTreeTestMode();

  private:

    std::vector< std::vector< int > > changepoints; ///vector of changepoints build by fpop (first index of each segment). size c
    std::vector< std::vector< double > > parameters; ///vector of means build by fpop. size c
    std::vector< std::vector< int > > states; ///vector of states build by fpop. size c
    std::vector< std::vector< int > > forced; ///vector of forced = 0 or 1. 1 = forced value. size c-1
    std::vector< double > globalCost;
};

std::ostream &operator<<(std::ostream &s, const Omega &om);


#endif // OMEGA_H
