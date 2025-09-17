/*
 *  AntMAN Package
 *
 */

#ifndef ANTMAN_SRC_ANTMANLOGGER_H_
#define ANTMAN_SRC_ANTMANLOGGER_H_

#include <armadillo>
#include <map>
#include <vector>
#include "verbose.h"

//typedef size_t  std::vector<void*>::size_type;
enum AntMANType {
	AM_INT, AM_UINT, AM_DOUBLE, AM_ARMA_VEC, AM_ARMA_MAT, AM_ARMA_CUBE, AM_ARMA_IVEC, AM_ARMA_IMAT, AM_ARMA_ICUBE, AM_VEC_DOUBLE, AM_UNKNOWN
};

template <typename T>
inline AntMANType getType(const T* v) {
	VERBOSE_ASSERT(false, "Better to give the type of something, this was " << *v);
	return AM_UNKNOWN;
}
template<> inline AntMANType getType(const int* v) {return AntMANType::AM_INT;}
template<> inline AntMANType getType(const double* v) {return AntMANType::AM_DOUBLE;}
template<> inline AntMANType getType(const unsigned int* v) {return AntMANType::AM_UINT;}
template<> inline AntMANType getType(const arma::vec* v) {return AntMANType::AM_ARMA_VEC;}
template<> inline AntMANType getType(const arma::mat* v) {return AntMANType::AM_ARMA_MAT;}
template<> inline AntMANType getType(const arma::cube* v) {return AntMANType::AM_ARMA_CUBE;}
template<> inline AntMANType getType(const arma::ivec* v) {return AntMANType::AM_ARMA_IVEC;}
template<> inline AntMANType getType(const arma::imat* v) {return AntMANType::AM_ARMA_IMAT;}
template<> inline AntMANType getType(const arma::icube* v) {return AntMANType::AM_ARMA_ICUBE;}
template<> inline AntMANType getType(const std::vector<double>* v) {return AntMANType::AM_VEC_DOUBLE;}


class AntMANLogger {

private :
	const size_t _max_size;
	const std::vector<std::string> _filter ;
	std::vector<std::string>       _names  ;
	std::map<std::string, void *>  _database;
	std::map<std::string, AntMANType>  _database_types;

public :
	AntMANLogger (const std::vector<std::string> & filter , size_t maxlogsize) : _max_size(maxlogsize), _filter(filter) 	{}

	template <typename T>
	bool addlog(const std::string& tag , const T& v) {

		if (not _database.count(tag)) {
			void * newptr = new std::vector<T>;
			_database[tag] = newptr;
			_names.push_back(tag);
			_database_types[tag] = getType<T>(&v);
		}

		void * ptr = _database[tag];
		VERBOSE_ASSERT(ptr, "Error allocating memory");
		((std::vector<T>*) _database[tag])->push_back(v);
		return true;

	}

	const std::vector<std::string>& getNames () const {
		return _names;
	}

	bool haslog(const std::string& tag) const {
		return _database.count(tag);
	}

	AntMANType getlogtype(const std::string& tag) const {
		return _database_types.at(tag);
	}
	template <typename T>
	const std::vector<T>& getlog(const std::string&  tag) const {
		return * ((std::vector<T>*) (_database.at(tag)));
	}



};



#endif /* ANTMAN_SRC_ANTMANLOGGER_H_ */
