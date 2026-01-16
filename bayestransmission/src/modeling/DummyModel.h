#ifndef ALUN_MODELING_DUMMYMODEL_H
#define ALUN_MODELING_DUMMYMODEL_H

#include "BasicModel.h"

namespace models{

class DummyModel : public BasicModel
{
public:

	DummyModel(int ns) : BasicModel(ns,0,1){}

	virtual infect::PatientState *makePatientState(infect::Patient *p) override;
    virtual int needEventType(EventCode e) override;

};
} // namespace models
#endif // ALUN_MODELING_DUMMYMODEL_H
