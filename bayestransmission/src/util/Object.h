// util/Object.h
#ifndef ALUN_UTIL_OBJECT_H
#define ALUN_UTIL_OBJECT_H

#include <string>
using std::string;
using std::ostream;

#include "Allocator.h"

namespace util{
class Object : public Allocator
{
private:
	static unsigned long indexcounter;
	unsigned long index;

public:
	Object();

	virtual ~Object();

	virtual long hash();

	virtual int compare(Object *y);

	virtual std::string className() const;

    virtual void write(std::ostream &os) const;
    virtual void write(std::ostream &os);

	//friend std::ostream& operator <<(std::ostream&, Object*);
};
}
#endif // ALUN_UTIL_OBJECT_H
