#ifndef ALUN_UTIL_INTEGER_H
#define ALUN_UTIL_INTEGER_H
#include "Object.h"

namespace util{

class Integer : public Object
{
public:
	int value;

	Integer(int x);
	int intValue() const;
	void set(int n);
	std::string className() const override;
	void write(std::ostream &os) const override;
};
} // namespace util
#endif // ALUN_UTIL_INTEGER_H
