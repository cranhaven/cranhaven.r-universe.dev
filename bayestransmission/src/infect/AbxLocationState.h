// infect/AbxLocationState.h
#ifndef ALUN_INFECT_ABXLOCATIONSTATE_H
#define ALUN_INFECT_ABXLOCATIONSTATE_H

#include "../util/util.h"
#include "AbxCoding.h"
#include "CountLocationState.h"


class AbxLocationState : public CountLocationState, AbxCoding
{
protected:
    int abxinf;
    int abxlat;
    int everinf;
    int everlat;

public:
    Map *abx;
    Map *ever;

    AbxLocationState(Object *own, int nstates);
    ~AbxLocationState();

    void clear() override;
    void copy(State *s) override;
    int onAbx(Patient *p) const;
    int everAbx(Patient *p) const;
    int getAbxTotal() const;
    int getEverAbxTotal() const;
    int getAbxColonized() const;
    int getEverAbxColonized() const;
    int getAbxLatent() const;
    int getEverAbxLatent() const;
    int getAbxSusceptible() const;
    int getEverAbxSusceptible() const;
    int getNoAbxTotal() const;
    int getNeverAbxTotal() const;
    int getNoAbxColonized() const;
    int getNeverAbxColonized() const;
    int getNoAbxLatent() const;
    int getNeverAbxLatent() const;
    int getNoAbxSusceptible() const;
    int getNeverAbxSusceptible() const;
    void write(ostream &os) const override;
    void apply(Event *e) override;
    void unapply(Event *e) override;
};

#endif // ALUN_INFECT_ABXLOCATIONSTATE_H
