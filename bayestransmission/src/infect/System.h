#ifndef ALUN_INFECT_SYSTEM_H
#define ALUN_INFECT_SYSTEM_H

#include "../util/util.h"
#include "EventCoding.h"
#include "RawEventList.h"
#include "Patient.h"
#include "Episode.h"
#include <exception>

class lab : public Object, public EventCoding
{
public:

	int f;
	int u;
	lab(int a, int b):f(a),u(b){}

};

class System : public Object, EventCoding
{
private:

	double start;
	double end;
	std::shared_ptr<IntMap> pat;
	std::shared_ptr<IntMap> fac;
	std::shared_ptr<Map> pepis;

	void handleOutOfRangeEvent(Patient *p, int t);
	void init(RawEventList *l, stringstream &err);
	void setInsitus();

protected:
    stringstream errlog;

public:

	System(RawEventList *l);
	System(RawEventList *l, stringstream &err);
	System(istream &is, stringstream &err);
	System(
	    std::vector<int> facilities,
        std::vector<int> units,
        std::vector<double> times,
        std::vector<int> patients,
        std::vector<int> types
	);
	~System();
	std::shared_ptr<Map> getEpisodes(Patient *p);
	// void write(ostream &os) override;
	// void write2(ostream &os,int opt);

	std::string className() const override { return "System";}

	inline std::shared_ptr<IntMap> getFacilities()
	{
		if (fac == nullptr) {
			return std::make_shared<IntMap>();
		}
		fac->init();
		return fac;
	}

	inline List *getUnits()
	{
		List *l = new List();

		if (fac == nullptr) {
			return l;
		}

		for (fac->init(); fac->hasNext(); )
		{
			Facility *facility = (Facility *)fac->nextValue();
			if (facility != nullptr) {
				IntMap *u = facility->getUnits();
				if (u != nullptr) {
					for (; u->hasNext(); ) {
						util::Object *unit = u->nextValue();
						if (unit != nullptr) {
							l->append(unit);
						}
					}
				}
			}
		}

		return l;
	}

	inline std::shared_ptr<IntMap> getPatients()
	{
		if (pat == nullptr) {
			return std::make_shared<IntMap>();
		}
		pat->init();
		return pat;
	}

	inline double startTime()
	{
		return start;
	}

	inline double endTime()
	{
		return end;
	}

    string get_log();
    
    /**
     * @brief Count total number of episodes across all patients
     * 
     * @return int Total episode count
     */
    inline int countEpisodes() const
    {
        if (pepis == nullptr) {
            return 0;
        }
        
        int count = 0;
        for (pepis->init(); pepis->hasNext(); ) {
            util::Map *eps = (util::Map *) pepis->nextValue();
            if (eps != nullptr) {
                count += eps->size();
            }
        }
        return count;
    }
    
    /**
     * @brief Count total number of events across all episodes
     * 
     * Counts all events (admissions, discharges, and internal events)
     * across all episodes for all patients.
     * 
     * @return int Total event count
     */
    inline int countEvents() const
    {
        if (pepis == nullptr) {
            return 0;
        }
        
        int count = 0;
        for (pepis->init(); pepis->hasNext(); ) {
            util::Map *eps = (util::Map *) pepis->nextValue();
            if (eps == nullptr) {
                continue;
            }
            
            for (eps->init(); eps->hasNext(); ) {
                Episode *ep = (Episode *) eps->next();
                if (ep == nullptr) {
                    continue;
                }
                
                util::List *events = ep->getEvents();
                if (events != nullptr) {
                    count += events->size();
                }
            }
        }
        return count;
    }
    
    /**
     * @brief Get counts of various system components
     * 
     * Returns a summary of the system structure including counts of
     * facilities, units, patients, episodes, and events.
     * 
     * @return util::List* Pointer to list containing Integer objects with counts
     *         Caller is responsible for deleting the returned list.
     */
    inline util::List* getSystemCounts() const
    {
        util::List *counts = new util::List();
        
        // Count facilities (with null check)
        counts->append(new Integer(fac != nullptr ? fac->size() : 0));
        
        // Count units (with null checks)
        int nUnits = 0;
        if (fac != nullptr) {
            for (fac->init(); fac->hasNext(); ) {
                Facility *f = (Facility *) fac->nextValue();
                if (f != nullptr) {
                    util::IntMap *units = f->getUnits();
                    if (units != nullptr) {
                        nUnits += units->size();
                    }
                }
            }
        }
        counts->append(new Integer(nUnits));
        
        // Count patients (with null check)
        counts->append(new Integer(pat != nullptr ? pat->size() : 0));
        
        // Count episodes (null checks inside countEpisodes)
        counts->append(new Integer(countEpisodes()));
        
        // Count events (null checks inside countEvents)
        counts->append(new Integer(countEvents()));
        
        return counts;
    }
    
private:

	double timeOfLastKnownEvent(Episode *ep);
	Event *makeEvent(Facility *f, Unit *u, double t, Patient *p, EventCode c);
	void addEpisode(Patient *p, Episode *ep);
	RawEvent *getEvent(List *l, EventCode c, int f, int u);
	RawEvent *getEvent(List *l, int f, int u);
	bool gotAdmission(List *l, lab *x);
	void nextUnitId(List *l, int *fid, int *uid);
	void makeEvents(List *n, Patient *p, Episode **cur, Facility **f, Unit **u, stringstream &err);
	void makePatientEpisodes(List *s, stringstream &err);
	void makeAllEpisodes(RawEventList *l, stringstream &err);
	void getOrMakeFacUnit(int m, int n, Facility **f, Unit **u);
	Patient *getOrMakePatient(int n);
};

#endif // ALUN_INFECT_SYSTEM_H
