#ifndef BIRP_TUNIQUECONTAINER_H
#define BIRP_TUNIQUECONTAINER_H

#include "BirpTypes.h"
#include <algorithm>
#include <vector>
#include "coretools/Main/TLog.h"

template<typename T> class TUniqueContainer {
	// class that stores a vector of unique elements
	// vector is ordered
protected:
	std::vector<T> _names;

public:
	bool exists(const T &Value) const { return std::find(_names.begin(), _names.end(), Value) != _names.end(); }

	size_t add(const T &Value) {
		if (!exists(Value)) {
			_names.push_back(Value);
			return _names.size() - 1;
		}
		return getIndex(Value);
	}

	size_t getIndex(const T &Value) const {
		auto it = std::find(_names.begin(), _names.end(), Value);
		if (it != _names.end()) { return it - _names.begin(); }
		DEVERROR("Value ", Value, " not found in vector. This should not happen.");
	}

	size_t size() const { return _names.size(); }
	void resize(size_t size) { _names.resize(size); }

	void print() const { coretools::cout << coretools::str::toString(_names) << std::endl; }

	const T &operator[](size_t Index) const { return _names[Index]; }

	typename std::vector<T>::const_iterator cbegin() const { return _names.cbegin(); }
	typename std::vector<T>::iterator begin() { return _names.begin(); }
	typename std::vector<T>::const_iterator cend() const { return _names.cend(); }
	typename std::vector<T>::iterator end() { return _names.end(); }
	const std::vector<T> &vec() const { return _names; }

	bool empty() const { return _names.size() == 0; }
	T back() const { return _names.back(); }

	void clear() { _names.clear(); }
};

#endif // BIRP_TUNIQUECONTAINER_H
