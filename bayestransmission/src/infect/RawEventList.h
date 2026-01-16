// infect/RawEventList.h
#ifndef ALUN_INFECT_RAWEVENTLIST_H
#define ALUN_INFECT_RAWEVENTLIST_H

#include <vector>

#include "RawEvent.h"

class RawEventList : public SortedList
{
public:

	static const int maxline = 1000;

	RawEventList(istream &is, stringstream &err) : SortedList()
	{
		char *c = new char[maxline];

		for (int line=1; !is.eof(); line++)
		{
			is.getline(c,maxline);
			if (is.eof())
				break;

			int facility = 0;
			int unit = 0;
			double time = 0;
			int patient = 0;
			int type = 0;

			if (sscanf(c,"%d%d%lf%d%d",&facility,&unit,&time,&patient,&type) != 5)
			{
				err << "Line " << line << ": Format problem:\n";
				err << "\t" << c << "\n";
				continue;
			}

			append(new RawEvent(facility,unit,time,patient,type));
		}

		init();

		delete [] c;
	}

	RawEventList(
	    std::vector<int> facilities,
	    std::vector<int> units,
	    std::vector<double> times,
	    std::vector<int> patients,
	    std::vector<int> types
    ) : SortedList()
	{

        if(facilities.size() != units.size() || facilities.size() != times.size() || facilities.size() != patients.size() || facilities.size() != types.size())
        {
            throw std::invalid_argument("All vectors must have the same size");
        }

	    for (auto i=0UL; i < facilities.size(); i++)
	    {
	        append(new RawEvent(facilities[i],units[i],times[i],patients[i],types[i]));
	    }

	    init();

	}

	~RawEventList()
	{
		for (init(); hasNext(); )
		{
			delete next();
		}

		clear();
	}

	double firstTime()
	{
		double x = 0;
		init();
		if (hasNext())
			x = (dynamic_cast<RawEvent *>(next()))->getTime();

		for (init(); hasNext(); )
		{
			double y = (dynamic_cast<RawEvent *>(next()))->getTime();
			if (y < x)
				x = y;
		}

		return x;
	}

	double lastTime()
	{
		double x = 0;
		init();
		if (hasNext())
			x = (dynamic_cast<RawEvent *>(next()))->getTime();


		for (init(); hasNext(); )
		{
			double y = (dynamic_cast<RawEvent *>(next()))->getTime();
			if (y > x)
				x = y;
		}

		return x;
	}

	// virtual void write(ostream &os) override
	// {
	// 	int i = 0;
	// 	for (init(); hasNext(); )
	// 	{
	// 		if (i++ != 0)
	// 			os << "\n";
	// 		os << next();
	// 	}
	// }
};

#endif // ALUN_INFECT_RAWEVENTLIST_H
