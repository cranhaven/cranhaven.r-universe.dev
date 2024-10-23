#ifndef __QS_MATRIX_H
#define __QS_MATRIX_H

#include <vector>
#include <iostream>

template <typename T> class QSMatrix {
 private:
  std::vector<std::vector<T> > mat;
  unsigned rows;
  unsigned cols;

 public:
  QSMatrix(unsigned _rows = 5, unsigned _cols =5, const T& _initial = 0);
  QSMatrix(const QSMatrix<T>& rhs);
  virtual ~QSMatrix();

  // Operator overloading, for "standard" mathematical matrix operations                                                                                                                                                          
  QSMatrix<T>& operator=(const QSMatrix<T>& rhs);
  
  // Matrix mathematical operations                                                                                                                                                                                               
  QSMatrix<T> operator+(const QSMatrix<T>& rhs);
  QSMatrix<T>& operator+=(const QSMatrix<T>& rhs);
  QSMatrix<T> operator-(const QSMatrix<T>& rhs);
  QSMatrix<T>& operator-=(const QSMatrix<T>& rhs);
  QSMatrix<T> operator*(const QSMatrix<T>& rhs);
  QSMatrix<T>& operator*=(const QSMatrix<T>& rhs);
  
  // unary transpose operator
  friend QSMatrix<T> operator~(const QSMatrix<T>& rhs)
  {
	  QSMatrix<T> result(rhs.ColNo(),rhs.RowNo());
	  
	  for (unsigned i=0; i < rhs.RowNo(); i++) {
      
	    for (unsigned j=0; j < rhs.ColNo(); j++) {
           T x = rhs(i,j);
	       result(j,i) = x;
        }
     }
   
	  return result;
   }
   
   // output stream function
   friend std::ostream& operator<<(std::ostream& ostrm, const QSMatrix<T>& rhs) {
		unsigned printSep = rhs.ColNo() - 1;

		for (unsigned i=0; i < rhs.RowNo(); i++) {
			for (unsigned j=0; j < rhs.ColNo(); j++) {
				T x = rhs(i,j);
				
				if(j < printSep)
				{
					ostrm << x << '\t';
				}
				else
				{
					ostrm << x;
				}
		  }
		  ostrm << std::endl;
	   }
	   return ostrm;
	}
	
  // Matrix/scalar operations                                                                                                                                                                                                     
  QSMatrix<T> operator+(const T& rhs);
  QSMatrix<T> operator-(const T& rhs);
  QSMatrix<T> operator*(const T& rhs);
  QSMatrix<T> operator/(const T& rhs);

  // Matrix/vector operations                                                                                                                                                                                                     
  std::vector<T> operator*(const std::vector<T>& rhs);
  std::vector<T> diag_vec();

  // Access the individual elements                                                                                                                                                                                               
  T& operator()(const unsigned& row, const unsigned& col);
  const T& operator()(const unsigned& row, const unsigned& col) const;

  // Access the row and column sizes                                                                                                                                                                                              
  unsigned RowNo() const;
  unsigned ColNo() const;

};

#include "matrix.cpp"

#endif
