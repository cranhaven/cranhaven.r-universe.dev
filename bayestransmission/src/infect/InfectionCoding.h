// infect/InfectionCoding.h
#ifndef INFECTIONCODING_H
#define INFECTIONCODING_H


#include <string>

class InfectionCoding
{
public:

	enum InfectionStatus
	{
		nullstatus	= 0,
		uncolonized 	= 1,
		latent		= 2,
		colonized 	= 3
	};

	static std::string codeString(InfectionStatus x)
	{
		switch(x)
		{
		case nullstatus:	return "null infection status";
		case uncolonized:	return "uncolonized";
		case latent:		return "latent";
		case colonized:		return "colonized";
		default: 		return "CAN'T REACH THIS";
		}
	}
};

#endif // INFECTIONCODING_H
