// util/Vector.h
#ifndef ALUN_UTIL_VECTOR_H
#define ALUN_UTIL_VECTOR_H

#include "Object.h"

namespace util{
class Vector : public Object
{
private:
	static const unsigned long defcap = 10UL;

	Object **x;
	int cap;
	int n;

	void ensure(int m);

public:
	Vector(int c = defcap);
	~Vector();

	inline void add(Object *o)
	{
		ensure(n+1);
		x[n++] = o;
	}

	inline Object *remove(Object *o)
	{
		int i=0;
		Object *res = 0;

		for (; i<n; i++)
			if (x[i] == o)
			{
				res = o;
				n--;
				for (; i<n; i++)
					x[i] = x[i+1];
				x[i] = 0;
				break;
			}

		return  res;
	}

	inline int size() const
	{
		return n;
	}

	inline Object *get(int i) const
	{
		return x[i];
	}

	inline void put(int i, Object *o)
	{
		ensure(i+1);
		x[i] = o;
	}

	string className() const override;
	void write(ostream &os) const override;
};
} // namespace util
#endif // ALUN_UTIL_VECTOR_H
