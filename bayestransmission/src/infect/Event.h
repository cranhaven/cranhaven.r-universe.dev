// infect/Event.h
#ifndef ALUN_INFECT_EVENT_H
#define ALUN_INFECT_EVENT_H

#include "Facility.h"
#include "Patient.h"
#include "EventCoding.h"

/**
 * @class Event
 * @brief Represents an event in the infection system.
 *
 * The Event class encapsulates details about the facility, unit, time, patient, and type of event.
 */
class Event : public Object, public EventCoding
{
private:
    Facility *fac; ///< Pointer to the facility where the event occurred.
    Unit *unit; ///< Pointer to the unit within the facility.
    double time; ///< Time at which the event occurred.
    Patient *pat; ///< Pointer to the patient involved in the event.
    EventCode type; ///< Type of the event.

public:
    /**
     * @brief Constructor for the Event class.
     *
     * @param fa Pointer to the facility.
     * @param un Pointer to the unit.
     * @param tm Time of the event.
     * @param pt Pointer to the patient.
     * @param tp Type of the event.
     */
    Event(Facility *fa, Unit *un, double tm, Patient *pt, EventCode tp);
    Event();

    /**
     * @brief Gets the time of the event.
     *
     * @return Time of the event.
     */
    inline double getTime() const
    {
        return time;
    }

    /**
     * @brief Sets the time of the event.
     *
     * @param t Time to set.
     */
    inline void setTime(double t)
    {
        time = t;
    }

    /**
     * @brief Gets the facility of the event.
     *
     * @return Pointer to the facility.
     */
    inline Facility *getFacility() const
    {
        return fac;
    }

    /**
     * @brief Gets the unit of the event.
     *
     * @return Pointer to the unit.
     */
    inline Unit *getUnit() const
    {
        return unit;
    }

    /**
     * @brief Gets the patient of the event.
     *
     * @return Pointer to the patient.
     */
    inline Patient* getPatient() const
    {
        return pat;
    }

    /**
     * @brief Gets the type of the event.
     *
     * @return Type of the event.
     */
    inline EventCode getType() const
    {
        return type;
    }

    /**
     * @brief Gets the type of the event.
     *
     * @return Type of the event.
     */
    inline string getTypeAsString() const
    {
        return eventString(type);
    }

    /**
     * @brief Sets the type of the event.
     *
     * @param c Type to set.
     */
    inline void setType(EventCode c)
    {
        type = c;
    }

    /**
     * @brief Checks if the event is a test event.
     *
     * @return True if the event is a test event, false otherwise.
     */
    inline bool isTest() const
    {
        switch(type)
        {
        case postest:
        case negtest:
        case negsurvtest:
        case possurvtest:
        case negclintest:
        case posclintest:
            return true;
        default:
            return false;
        }
    }

    /**
     * @brief Checks if the event is a positive test event.
     *
     * @return True if the event is a positive test event, false otherwise.
     */
    inline bool isPositiveTest() const
    {
        switch(type)
        {
        case postest:
        case possurvtest:
        case posclintest:
            return true;
        default:
            return false;
        }
    }

    /**
     * @brief Checks if the event is a clinical test event.
     *
     * @return True if the event is a clinical test event, false otherwise.
     */
    inline bool isClinicalTest() const
    {
        switch(type)
        {
        case posclintest:
        case negclintest:
            return true;
        default:
            return false;
        }
    }

    /**
     * @brief Checks if the event is an admission event.
     *
     * @return True if the event is an admission event, false otherwise.
     */
    inline bool isAdmission() const
    {
        switch(type)
        {
        case admission:
        case admission0:
        case admission1:
        case admission2:
            return true;
        default:
            return false;
        }
    }

    /**
     * @brief Checks if the event is an in-situ event.
     *
     * @return True if the event is an in-situ event, false otherwise.
     */
    inline bool isInsitu() const
    {
        switch(type)
        {
        case insitu:
        case insitu0:
        case insitu1:
        case insitu2:
            return true;
        default:
            return false;
        }
    }

    /**
     * @brief Checks if the event is an observable event.
     *
     * @return True if the event is an observable event, false otherwise.
     */
    inline bool isObservable() const
    {
        switch(type)
        {
        case insitu:
        case insitu0:
        case insitu1:
        case insitu2:
        case admission:
        case admission0:
        case admission1:
        case admission2:
        case discharge:
        case negsurvtest:
        case possurvtest:
        case negclintest:
        case posclintest:
        case abxdose:
        case abxon:
        case abxoff:
            return true;
        default:
            return false;
        }
    }

    /**
     * @brief Checks if the event is a colonization event.
     *
     * @return True if the event is a colonization event, false otherwise.
     */
    inline bool isCollonizationEvent() const
    {
        switch(type)
        {
        case acquisition:
        case progression:
        case clearance:
            return true;
        default:
            return false;
        }
    }

    /**
     * @brief Writes the event details to an output stream.
     *
     * @param os Output stream to write to.
     */
    void write(ostream &os) const override;

    /**
     * @brief Writes the event details to an output stream with options.
     *
     * @param os Output stream to write to.
     * @param opt Option flag for writing.
     */
    void write2(ostream &os, int opt) const;
};

#endif // ALUN_INFECT_EVENT_H
