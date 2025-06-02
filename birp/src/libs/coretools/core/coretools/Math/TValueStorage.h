#ifndef MATH_TVALUESTORAGE_H_
#define MATH_TVALUESTORAGE_H_

#include <cstddef>
#include <string>

#include "coretools/Containers/TView.h"
#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Strings/concatenateString.h"
#include "coretools/Types/probability.h"

namespace coretools::probdist {

template <typename valueType>
class TValueStorage_base{
public:
	TValueStorage_base(){};
	virtual ~TValueStorage_base(){};

	valueType operator[](size_t index) const {
		return index2value(index);
	}

	valueType front() const {
		return index2value(0);
	}

	virtual size_t size() const  = 0;
	virtual valueType index2value(size_t) const = 0;
	virtual std::pair<bool, size_t> value2index(valueType) const = 0;
	virtual size_t value2indexAtOrBefore(valueType) const = 0;
	virtual valueType min() const  = 0;
	virtual valueType max() const = 0;
	virtual std::string valueString() const = 0;
	virtual valueType sample(TConstView<Probability> CumulativeProbabilities) const = 0;
};

template <typename valueType>
class TValueStorageFixed final : public TValueStorage_base<valueType>{
private:
	valueType _value;

public:
	TValueStorageFixed(valueType Val) : _value(Val) {}

	size_t size() const override {
		return 1;
	}

	valueType index2value(size_t) const override {
		return _value;
	}

	valueType min() const override {
		return _value;
	}

	valueType max() const override {
		return _value;
	}

	std::pair<bool, size_t> value2index(valueType x) const override {
		if(_value == x){
			return std::pair<bool, size_t>(true, 0);
		}
		return std::pair<bool, size_t>(false, 0);
	}

	size_t value2indexAtOrBefore(valueType) const override {
		return 0;
	}

	valueType sample(TConstView<Probability>) const override {
		return _value;
	}

	std::string valueString() const override {
		return coretools::str::toString(_value);
	}
};

template <typename valueType>
class TValueStorageDispersed final : public TValueStorage_base<valueType>{
private:
	std::vector<valueType> _values;

public:
	TValueStorageDispersed(TConstView<valueType> Vals) : _values(Vals.begin(), Vals.end()) {}

	size_t size() const override {
		return _values.size();
	}

	valueType index2value(size_t index) const override {
		return _values[index];
	}

	valueType min() const override {
		return _values.front();
	}

	valueType max() const override {
		return _values.back();
	}

	std::pair<bool, size_t> value2index(valueType x) const override {
		for(size_t i = 0; i < size(); ++i){
			if(_values[i] == x){
				return std::pair<bool, size_t>(true, i);
			} else if(_values[i] > x){
				return std::pair<bool, size_t>(false, 0);
			}
		}
		return std::pair<bool, size_t>(false, 0);
	}

	size_t value2indexAtOrBefore(valueType x) const override {
		if(x > _values.back()){
			return size() - 1;
		}
		size_t i = 0;
		while(x > _values[i]){
			++i;
		}
		return i - 1;
	}

	valueType sample(TConstView<Probability> CumulativeProbabilities) const override {
		return index2value(coretools::instances::randomGenerator().pickOne(CumulativeProbabilities));
	}

	std::string valueString() const override {
		return coretools::str::concatenateString(_values, ",");
	}
};

template <typename valueType>
class TValueStorageRange final : public TValueStorage_base<valueType>{
private:
	valueType _min, _max;

public:
	TValueStorageRange(valueType Min, valueType Max) : _min(Min), _max(Max){
		if(_min > _max){
			std::swap(_min, _max);
		}
	}

	size_t size() const override {
		return underlying(_max) - underlying(_min) + 1;
	}

	valueType index2value(size_t index) const override {
		return valueType( underlying(_min) + static_cast<underlyingType_t<valueType>>(index) );
	}

	valueType min() const override {
		return _min;
	}

	valueType max() const override {
		return _max;
	}

	std::pair<bool, size_t> value2index(valueType x) const override {
		if(x < _min || x > _max){
			return std::pair<bool, size_t>(false, 0);
		}
		return std::pair<bool, size_t>(true, underlying(x) - underlying(_min));
	}

	size_t value2indexAtOrBefore(valueType x) const override {
		if(x > _max){
			return underlying(_max) - underlying(_min);
		}
		return underlying(x) - underlying(_min);
	}

	valueType sample(TConstView<Probability> CumulativeProbabilities) const override {
		return index2value(coretools::instances::randomGenerator().pickOne(CumulativeProbabilities));
	}

	std::string valueString() const override {
		std::string s = coretools::str::toString(_min);
		for(size_t i = underlying(_min) + 1; i <= underlying(_max); ++i){
			s += "," + coretools::str::toString(valueType(underlying(_min) + i));
		}
		return s;
	}
};


}
#endif
