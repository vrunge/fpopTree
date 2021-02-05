#include "Omega.h"

#include<iostream>
#include <stdlib.h>
#include <algorithm>

//####### constructor #######////####### constructor #######////####### constructor #######//
//####### constructor #######////####### constructor #######////####### constructor #######//

Omega::Omega()
{

}

//####### destructor #######////####### destructor #######////####### destructor #######//
//####### destructor #######////####### destructor #######////####### destructor #######//

Omega::~Omega()
{

}

void Omega::fpopTree()
{

}

void Omega::fpopTreeTestMode()
{

}

//####### accessors #######////####### accessors #######////####### accessors #######//
//####### accessors #######////####### accessors #######////####### accessors #######//
std::vector< std::vector< int > > Omega::GetChangepoints() const{return(changepoints);}
std::vector< std::vector< double > > Omega::GetParameters() const{return(parameters);}
std::vector< double > Omega::GetGlobalCost() const{return(globalCost);}
