
#ifndef CORE_CORETOOLS_MATH_TMEANVARVECTOR_H_
#define CORE_CORETOOLS_MATH_TMEANVARVECTOR_H_

#include <vector>

#include "coretools/Math/TMeanVar.h"
namespace coretools {

//----------------------------------------------
// TMeanVarVector
//----------------------------------------------
template<class T> class TMeanVarVector {
private:
	std::vector<TMeanVar<T>> _meanVar;

public:
	TMeanVarVector() = default;
	TMeanVarVector(size_t Size) { _meanVar.resize(Size); }

	void clear() { _meanVar.clear(); }

	void add(const uint32_t ID, const T Value) {
		if (_meanVar.size() <= ID) { _meanVar.resize(ID + 1); }
		_meanVar[ID].add(Value);
	}

	size_t size() const { return _meanVar.size(); }

	T sum() const {
		T sum = 0;
		for (auto &it : _meanVar) {
			// check for numeric under- and overflow
			DEV_ASSERT(checkForNumericOverflow_addition(sum, it.sum()));
			sum += it.sum();
		}
		return sum;
	}

	bool exists(const uint32_t ID) const {
		return ID < _meanVar.size();
	}

	const TMeanVar<T> &operator[](const uint32_t ID) {
		DEV_ASSERT(exists(ID));

		return _meanVar[ID];
	}
};
	
}
#endif
