#ifndef __EXCEPTIONS_HPP__
#define __EXCEPTIONS_HPP__
#include<cstdlib>
#include<string>
#include<iostream>
extern "C"{
#include"matrix.h"
}
using std::string;
class exceptionBase{
public:
  exceptionBase(int linum, string fname):
    line(linum), file(fname){}
  virtual ~exceptionBase(){}
  const int& getLine(){return line;}
  const string& getFile(){return file;}
protected:
  int line;
  string file;
};

class cholException: public exceptionBase{
public:
  cholException(int linum, string fname, int ecode, int ndim,
		double nug, double* dparam):
    exceptionBase(linum, fname), info(ecode), dim(ndim), g(nug){
    d = new_dup_vector(dparam,ndim);
  }
  ~cholException(){
    free(d);
  };
  int getInfo(){return info;}
  friend std::ostream& operator<<(std::ostream& os, cholException& rhs);
private:
  int info, dim;
  double g;
  double *d;
};

class optException: public exceptionBase{
public:
  optException(int linum, string fname, double tmin, double tmax):
    exceptionBase(linum, fname), min(tmin), max(tmax){}
  friend std::ostream& operator<<(std::ostream& os, optException& rhs);
private:
  double min;
  double max;
};

class svdException: public exceptionBase{
public:
  svdException(int linum, string fname, int ecode):
    exceptionBase(linum, fname), info(ecode){};
  int getInfo(){return info;}
  friend std::ostream& operator<<(std::ostream& os, svdException& rhs);
private:
  int info;
};
  
#endif
