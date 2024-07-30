#include <vector>
#include "Sscma.h"
#include "SpecialArrays.h"

namespace extendedleaps {

symtwodarray::symtwodarray(const vind dim) 
  :	dimension(dim)
{	
	data.assign(dim,vector<real>());
	for (vind i=0;i<dim;i++) data[i].resize(i+1);
}

symtwodarray::~symtwodarray()   {  }

symtwodarray::symtwodarray(const symtwodarray& org ) 
:	dimension(org.dimension), data(org.data)   {  }

symtwodarray& symtwodarray::operator=(const symtwodarray& org ) 
{	
	if (this != &org)  {
		dimension = org.dimension;
		for (vind i=0;i<dimension;i++)
			for (vind j=0;j<=i;j++)  data[i][j] = org.data[i][j];
	}
	return *this;
}

matvectarray::matvectarray(const vind dim,symtwodarray* m,vind const mr) 
: dimension(dim), mat(m), matrowind(mr)
{
	owndata.resize(dimension);
}


void matvectarray::setvalue(const vind j,const real val)
{
	owndata[j] = val; 
}

const real matvectarray::operator[] (const vind j) const 
{ 
	if (mat) return (*mat)(matrowind,j);
	else return owndata[j];
}

}

