
#ifndef CORE_CORETOOLS_MATH_TMEANVARMAP_H_
#define CORE_CORETOOLS_MATH_TMEANVARMAP_H_

#include <map>
#include "coretools/Math/TMeanVar.h"

namespace coretools {

template<class U, class T> // U is for type of ID (e.g. uint64_t), T is for type of value (e.g. double)
class TMeanVarMap {
private:
	std::map<U, TMeanVar<T>> _meanVar;

public:
	TMeanVarMap(){};

	void clear() { _meanVar.clear(); };

	void add(const U ID, const T Value) { _meanVar[ID].add(Value); };

	size_t size() const { return _meanVar.size(); };

	T sum() const {
		T sum = 0;
		for (auto &it : _meanVar) {
			// check for numeric under- and overflow
			if (!checkForNumericOverflow_addition(sum, it.second.sum())) {
				DEVERROR("Numeric under- or overflow occured!");
			}
			sum += it.second.sum();
		}
		return sum;
	};

	bool exists(const U ID) const { return _meanVar.find(ID) != _meanVar.end(); };

	const TMeanVar<T> &operator[](const U ID) {
		if (!exists(ID)) DEVERROR("No entry with key " + str::toString(ID) + " in TMeanVarMap!");
		return _meanVar[ID];
	};
};
	
}

#endif
