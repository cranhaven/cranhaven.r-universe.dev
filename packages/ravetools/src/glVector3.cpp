#include "glVector3.h"
#include "glMatrix4.h"
#include "glQuaternion.h"
// #include "threepp/math/MathUtils.hpp"
// #include "threepp/math/Matrix3.hpp"
// #include "threepp/math/Quaternion.hpp"
// #include "threepp/math/Spherical.hpp"
//
// #include "threepp/cameras/Camera.hpp"

// #include <algorithm>
// #include <cmath>
// #include <stdexcept>

using namespace rave3d;

// [[Rcpp::interfaces(r, cpp)]]



namespace {
thread_local Quaternion _quaternion;
}

Vector3::Vector3() : data({}) {}
Vector3::~Vector3() {}

// [[Rcpp::export]]
SEXP Vector3__new() {
  Rcpp::XPtr<Vector3> ptr( new Vector3(), true );
  // return the external pointer to the R side
  return ptr;
}

Vector3& Vector3::fromArray(std::vector<double>& array, const int& offset,
                            const int& nElements) {
  int n_ = nElements;
  size_t dlen = array.size();
  if( n_ <= 0 ) {
    n_ = (dlen - offset) / 3;
    if( n_ * 3 + offset > dlen ) { n_--; }
  }
  if( n_ <= 0 ) {
    Rcpp::stop("C++ Vector3::fromArray - no data to set. Please make sure length(data) >= offset + 3*nElements");
  }
  if( offset < 0 || n_ * 3 + offset > dlen ) {
    Rcpp::stop("C++ Vector3::fromArray - invalid offset, no sufficient data to set");
  }
  this->resize(n_);
  std::vector<double>::iterator vin = array.begin() + offset;
  this->data.assign(vin, vin + n_ * 3);
  return *this;
}


// [[Rcpp::export]]
void Vector3__from_array(const SEXP& self, std::vector<double>& array, const int& offset = 0, const int& n_elems = -1) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->fromArray(array, offset, n_elems);
}

Vector3& Vector3::resize(const size_t& nElement) {
  if( nElement <= 0 ) {
    Rcpp::stop("C++ Vector3::resize - invalid number of elements, must be positive");
  }
  if( this->data.empty() || this->data.size() != nElement * 3 ) {
    this->data.resize( nElement * 3 );
  }
  return *this;
}

// [[Rcpp::export]]
void Vector3__resize(const SEXP& self, const int& n_elems) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->resize(n_elems);
}

size_t Vector3::getSize() {
  return this->data.size() / 3;
}

// [[Rcpp::export]]
size_t Vector3__get_size(const SEXP& self) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  return ptr_self->getSize();
}


SEXP Vector3::toArray(const int& nSkip, const int& maxElements) {

  if( this->data.empty() || maxElements == 0 ) { return R_NilValue; }

  int nSkip_ = nSkip;
  size_t nElements = this->getSize();
  if( nSkip_ < 0 ) {
    nSkip_ += nElements;
    if( nSkip_ ) {
      Rcpp::stop("C++ Vector3::toArray - nSkip out of bound.");
    }
  } else if ( nSkip >= nElements ) {
    return R_NilValue;
  }

  int retSize = nElements - nSkip_;
  if( maxElements >= 0 && maxElements < retSize ) {
    retSize = maxElements;
  }
  if( retSize == 0 ) { return R_NilValue; }

  SEXP re = PROTECT(Rf_allocVector(REALSXP, 3 * retSize));
  SEXP dm = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(dm)[0] = 3;
  INTEGER(dm)[1] = retSize;
  Rf_setAttrib(re, R_DimSymbol, dm);

  std::vector<double>::iterator pinp = this->data.begin() + nSkip_ * 3;
  double* pout = REAL(re);

  for(size_t ii = 0; ii < retSize * 3; ii++, pinp++, pout++) {
    *pout = *pinp;
  }

  UNPROTECT(2);
  return re;
}

// [[Rcpp::export]]
SEXP Vector3__to_array(const SEXP& self, const int& n_skip = 0, const int& max_n_elems = -1) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  return ptr_self->toArray(n_skip, max_n_elems);
}

// Vector3& Vector3::set(const double &x, const double &y, const double &z) {
//
//   this->xyz[0] = x;
//   this->xyz[1] = y;
//   this->xyz[2] = z;
//
//   return *this;
// }
//
//

Vector3& Vector3::setScalar(double value) {
  std::fill(this->data.begin(), this->data.end(), value);
  return *this;
}

// [[Rcpp::export]]
void Vector3__set_scalar(const SEXP& self, double& value) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->setScalar(value);
}

Vector3& Vector3::setX(double value) {
  if( this->data.empty() ) { return *this; }
  std::vector<double>::iterator ptr = this->data.begin();

  for(size_t i = 0; i < this->getSize(); i++, ptr += 3 ) {
    *ptr = value;
  }

  return *this;
}

// [[Rcpp::export]]
void Vector3__set_x(const SEXP& self, double& value) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->setX(value);
}

Vector3& Vector3::setY(double value) {
  if( this->data.empty() ) { return *this; }
  std::vector<double>::iterator ptr = this->data.begin() + 1;

  for(size_t i = 0; i < this->getSize(); i++, ptr += 3 ) {
    *ptr = value;
  }

  return *this;
}

// [[Rcpp::export]]
void Vector3__set_y(const SEXP& self, double& value) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->setY(value);
}

Vector3& Vector3::setZ(double value) {
  if( this->data.empty() ) { return *this; }
  std::vector<double>::iterator ptr = this->data.begin() + 2;

  for(size_t i = 0; i < this->getSize(); i++, ptr += 3 ) {
    *ptr = value;
  }

  return *this;
}

// [[Rcpp::export]]
void Vector3__set_z(const SEXP& self, double& value) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->setZ(value);
}

double Vector3::getX(const size_t& nSkip) {
  const size_t s = this->getSize();
  if( this->data.empty() || nSkip >= s ) {
    Rcpp::stop("C++ Vector3::get* - subscript out of bound.");
  }
  return *(this->data.begin() + nSkip * 3);
}

// [[Rcpp::export]]
double Vector3__get_x(const SEXP& self, const size_t& i) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  return ptr_self->getX(i - 1);
}


double Vector3::getY(const size_t& nSkip) {
  const size_t s = this->getSize();
  if( this->data.empty() || nSkip >= s ) {
    Rcpp::stop("C++ Vector3::get* - subscript out of bound.");
  }
  return *(this->data.begin() + nSkip * 3 + 1);
}

// [[Rcpp::export]]
double Vector3__get_y(const SEXP& self, const size_t& i) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  return ptr_self->getY(i - 1);
}

double Vector3::getZ(const size_t& nSkip) {
  const size_t s = this->getSize();
  if( this->data.empty() || nSkip >= s ) {
    Rcpp::stop("C++ Vector3::get* - subscript out of bound.");
  }
  return *(this->data.begin() + nSkip * 3 + 2);
}

// [[Rcpp::export]]
double Vector3__get_z(const SEXP& self, const size_t& i) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  return ptr_self->getZ(i - 1);
}

SEXP Vector3::getItem(const size_t& nSkip) {
  const size_t s = this->getSize();
  if( this->data.empty() || nSkip >= s ) {
    Rcpp::stop("C++ Vector3::get* - subscript out of bound.");
  }
  SEXP re = PROTECT(Rf_allocVector(REALSXP, 3));
  std::vector<double>::iterator pin = this->data.begin() + nSkip * 3;
  double* ptr = REAL(re);

  *ptr++ = *pin++;
  *ptr++ = *pin++;
  *ptr = *pin;
  UNPROTECT(1);
  return re;
}

// [[Rcpp::export]]
SEXP Vector3__get_item(const SEXP& self, const size_t& i) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  SEXP re = PROTECT(ptr_self->getItem(i - 1));
  UNPROTECT(1);
  return re;
}

//
// double& Vector3::operator[](size_t index) {
//   switch (index) {
//   case 0:
//     return x;
//   case 1:
//     return y;
//   case 2:
//     return z;
//   default:
//     throw std::runtime_error("index out of bound: " + std::to_string(index));
//   }
// }
//

Vector3& Vector3::copy(Vector3& v, const size_t& nElem) {
  if( this != &v ) {
    if( nElem > 0 ) {
      this->fromArray(v.data, 0, nElem);
      this->resize(nElem);
    } else {
      this->fromArray(v.data);
    }
  } else if( nElem > 0 ) {
    this->resize(nElem);
  }
  return *this;
}

// [[Rcpp::export]]
void Vector3__copy(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  ptr_self->copy(*ptr_v, 0);
}

Vector3& Vector3::add(Vector3& v) {
  size_t la = this->getSize();
  size_t lb = v.getSize();
  if( lb != 1 && lb != la) {
    Rcpp::stop("C++ Vector3::add - inconsistent size of input `v`.");
  }
  if(la == 0) { return *this; }

  if( lb == la ) {
    std::transform(this->data.begin(), this->data.end(),
                   v.data.begin(), this->data.begin(),
                   [](double& a, double& b) { return a + b; });
  } else {
    const double vx = v.data[0];
    const double vy = v.data[1];
    const double vz = v.data[2];
    std::vector<double>::iterator ptr = this->data.begin();
    for(size_t i = 0; i < la; i++, ptr+=3) {
      *ptr += vx;
      *(ptr+1) += vy;
      *(ptr+2) += vz;
    }
  }
  return *this;
}

// [[Rcpp::export]]
void Vector3__add(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  ptr_self->add(*ptr_v);
}

Vector3& Vector3::addScalar(std::vector<double>& s) {

  // s.getSize() must be either 1 or length of data
  size_t l = this->getSize();
  if( l == 0 ) { return *this; }
  if( s.size() == 1 ) {
    double scalar = s[0];
    std::transform(this->data.begin(), this->data.end(),
                   this->data.begin(),
                   [scalar](double& a) { return a + scalar; });
  } else {

    if( s.size() != l ) {
      Rcpp::stop("C++ Vector3::addScalar - length of scalar must be either 1 or the number of points in Vector3.");
    }
    std::vector<double>::iterator ptr = this->data.begin();
    std::vector<double>::iterator ptr2 = s.begin();
    for(size_t i = 0; i < l; i++, ptr+=3, ptr2++) {
      *ptr += *ptr2;
      *(ptr+1) += *ptr2;
      *(ptr+2) += *ptr2;
    }

  }


  return *this;
}

// [[Rcpp::export]]
void Vector3__add_scalar(const SEXP& self, std::vector<double>& s) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->addScalar(s);
}

Vector3& Vector3::addVectors(Vector3& a, Vector3& b) {
  // s.getSize() must be either 1 or length of data
  const size_t la = a.getSize();
  const size_t lb = b.getSize();
  if( la != lb ) {
    Rcpp::stop("C++ Vector3::addVectors - inconsistent size of input `a` and `b`.");
  }
  this->resize(la);
  if( la == 0 ) { return *this; }

  std::transform(a.data.begin(), a.data.end(),
                 b.data.begin(),
                 this->data.begin(),
                 [](double& va, double& vb) { return va + vb; });
  return *this;
}

// [[Rcpp::export]]
void Vector3__add_vectors(const SEXP& self, const SEXP& a, const SEXP& b) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_a(a);
  Rcpp::XPtr<Vector3> ptr_b(b);
  ptr_self->addVectors(*ptr_a, *ptr_b);
}

Vector3& Vector3::addScaledVector(Vector3& v, double s) {

  size_t la = this->getSize();
  size_t lb = v.getSize();
  if(la > lb) {
    la = lb;
  }
  if( la > 0 ) {
    std::transform(v.data.begin(), v.data.begin() + la * 3,
                   this->data.begin(), this->data.begin(),
                   [s](double& a, double& b) { return a + b * s; });
  }

  return *this;
}

// [[Rcpp::export]]
void Vector3__add_scaled(const SEXP& self, const SEXP& v, const double& s) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  ptr_self->addScaledVector(*ptr_v, s);
}

Vector3& Vector3::sub(Vector3& v) {

  size_t la = this->getSize();
  size_t lb = v.getSize();
  if( lb != 1 && lb != la) {
    Rcpp::stop("C++ Vector3::sub - inconsistent size of input `v`.");
  }
  if(la == 0) { return *this; }

  if( lb == la ) {
    std::transform(this->data.begin(), this->data.end(),
                   v.data.begin(), this->data.begin(),
                   [](double& a, double& b) { return a - b; });
  } else {
    const double vx = v.data[0];
    const double vy = v.data[1];
    const double vz = v.data[2];
    std::vector<double>::iterator ptr = this->data.begin();
    for(size_t i = 0; i < la; i++, ptr+=3) {
      *ptr -= vx;
      *(ptr+1) -= vy;
      *(ptr+2) -= vz;
    }
  }
  return *this;
}

// [[Rcpp::export]]
void Vector3__sub(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  ptr_self->sub(*ptr_v);
}

Vector3& Vector3::subScalar(std::vector<double>& s) {

  // s.getSize() must be either 1 or length of data
  size_t l = this->getSize();
  if( l == 0 ) { return *this; }
  if( s.size() == 1 ) {
    double scalar = s[0];
    std::transform(this->data.begin(), this->data.end(),
                   this->data.begin(),
                   [scalar](double& a) { return a - scalar; });
  } else {

    if( s.size() != l ) {
      Rcpp::stop("C++ Vector3::addScalar - length of scalar must be either 1 or the number of points in Vector3.");
    }
    std::vector<double>::iterator ptr = this->data.begin();
    std::vector<double>::iterator ptr2 = s.begin();
    for(size_t i = 0; i < l; i++, ptr+=3, ptr2++) {
      *ptr -= *ptr2;
      *(ptr+1) -= *ptr2;
      *(ptr+2) -= *ptr2;
    }

  }

  return *this;
}

// [[Rcpp::export]]
void Vector3__sub_scalar(const SEXP& self, std::vector<double>& s) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->subScalar(s);
}

Vector3& Vector3::subVectors(Vector3& a, Vector3& b) {
  const size_t la = a.getSize();
  const size_t lb = b.getSize();
  if( la != lb ) {
    Rcpp::stop("C++ Vector3::subVectors - inconsistent size of input `a` and `b`.");
  }
  this->resize(la);
  if( la == 0 ) { return *this; }

  std::transform(a.data.begin(), a.data.end(),
                 b.data.begin(),
                 this->data.begin(),
                 [](double& va, double& vb) { return va - vb; });
  return *this;
}

// [[Rcpp::export]]
void Vector3__sub_vectors(const SEXP& self, const SEXP& a, const SEXP& b) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_a(a);
  Rcpp::XPtr<Vector3> ptr_b(b);
  ptr_self->subVectors(*ptr_a, *ptr_b);
}

Vector3& Vector3::multiply(Vector3& v) {

  size_t la = this->getSize();
  size_t lb = v.getSize();
  if( lb != 1 && lb != la) {
    Rcpp::stop("C++ Vector3::multiply - inconsistent size of input `v`.");
  }
  if(la == 0) { return *this; }

  if( lb == la ) {
    std::transform(this->data.begin(), this->data.end(),
                   v.data.begin(), this->data.begin(),
                   [](double& a, double& b) { return a * b; });
  } else {
    const double vx = v.data[0];
    const double vy = v.data[1];
    const double vz = v.data[2];
    std::vector<double>::iterator ptr = this->data.begin();
    for(size_t i = 0; i < la; i++, ptr+=3) {
      *ptr *= vx;
      *(ptr+1) *= vy;
      *(ptr+2) *= vz;
    }
  }
  return *this;
}

// [[Rcpp::export]]
void Vector3__multiply(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  ptr_self->multiply(*ptr_v);
}

Vector3& Vector3::multiplyScalar(std::vector<double>& s) {
  // s.getSize() must be either 1 or length of data
  size_t l = this->getSize();
  if( l == 0 ) { return *this; }
  if( s.size() == 1 ) {
    double scalar = s[0];
    std::transform(this->data.begin(), this->data.end(),
                   this->data.begin(),
                   [scalar](double& a) { return a * scalar; });
  } else {

    if( s.size() != l ) {
      Rcpp::stop("C++ Vector3::addScalar - length of scalar must be either 1 or the number of points in Vector3.");
    }
    std::vector<double>::iterator ptr = this->data.begin();
    std::vector<double>::iterator ptr2 = s.begin();
    for(size_t i = 0; i < l; i++, ptr+=3, ptr2++) {
      *ptr *= *ptr2;
      *(ptr+1) *= *ptr2;
      *(ptr+2) *= *ptr2;
    }

  }

  return *this;
}

// [[Rcpp::export]]
void Vector3__multiply_scalar(const SEXP& self, std::vector<double>& s) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->multiplyScalar(s);
}

Vector3& Vector3::multiplyVectors(Vector3& a, Vector3& b) {
  const size_t la = a.getSize();
  const size_t lb = b.getSize();
  if( la != lb ) {
    Rcpp::stop("C++ Vector3::multiplyVectors - inconsistent size of input `a` and `b`.");
  }
  this->resize(la);
  if( la == 0 ) { return *this; }

  std::transform(a.data.begin(), a.data.end(),
                 b.data.begin(),
                 this->data.begin(),
                 [](double& va, double& vb) { return va * vb; });
  return *this;
}

// [[Rcpp::export]]
void Vector3__multiply_vectors(const SEXP& self, const SEXP& a, const SEXP& b) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_a(a);
  Rcpp::XPtr<Vector3> ptr_b(b);
  ptr_self->multiplyVectors(*ptr_a, *ptr_b);
}


Vector3& Vector3::applyAxisAngle(Vector3& axis, const double& angle) {
  return this->applyQuaternion(_quaternion.setFromAxisAngle(axis, angle));
}

void Vector3__apply_axis_angle(const SEXP& self, const SEXP& axis, const double& angle) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_axis(axis);
  ptr_self->applyAxisAngle(*ptr_axis, angle);
}

// Vector3& Vector3::applyEuler(const Euler& euler) {
//
//   return this->applyQuaternion(_quaternion.setFromEuler(euler));
// }

Vector3& Vector3::applyMatrix3(const std::vector<double>& m) {

  if( m.size() != 9 ) {
    Rcpp::stop("C++ Vector3::applyMatrix3 - matrix `m` is not a 3x3 matrix.");
  }
  const size_t l = this->getSize();
  if( l == 0 ) { return *this; }

  double m11 = m[0], m12 = m[3], m13 = m[6];
  double m21 = m[1], m22 = m[4], m23 = m[7];
  double m31 = m[2], m32 = m[5], m33 = m[8];

  std::vector<double>::iterator ptr = this->data.begin();
  double x_, y_, z_;

  for(size_t i = 0; i < l; i++, ptr+=3) {
    x_ = *ptr;
    y_ = *(ptr+1);
    z_ = *(ptr+2);
    *ptr = m11 * x_ + m12 * y_ + m13 * z_;
    *(ptr+1) = m21 * x_ + m22 * y_ + m23 * z_;
    *(ptr+2) = m31 * x_ + m32 * y_ + m33 * z_;
  }

  return *this;
}

// [[Rcpp::export]]
void Vector3__apply_matrix3(const SEXP& self, const std::vector<double>& m) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->applyMatrix3(m);
}

// Vector3& Vector3::applyNormalMatrix(const Matrix3& m) {
//
//   return applyMatrix3(m).normalize();
// }
//
Vector3& Vector3::applyMatrix4(const std::vector<double>& m) {

  if( m.size() != 16 ) {
    Rcpp::stop("C++ Vector3::applyMatrix4 - matrix `m` is not a 4x4 matrix");
  }
  if( this->data.empty() ) { return *this; }

  double x_, y_, z_, w;
  double  m11 = m[0], m12 = m[4], m13 = m[8], m14 = m[12],
          m21 = m[1], m22 = m[5], m23 = m[9], m24 = m[13],
          m31 = m[2], m32 = m[6], m33 = m[10], m34 = m[14],
          m41 = m[3], m42 = m[7], m43 = m[11], m44 = m[15];
  std::vector<double>::iterator ptr = this->data.begin();
  for(size_t i = 0; i < this->getSize(); i++, ptr += 3) {
    w = 1.0 / (m41 * ptr[0] + m42 * ptr[1] + m43 * ptr[2] + m44);
    x_ = (m11 * ptr[0] + m12 * ptr[1] + m13 * ptr[2] + m14) * w;
    y_ = (m21 * ptr[0] + m22 * ptr[1] + m23 * ptr[2] + m24) * w;
    z_ = (m31 * ptr[0] + m32 * ptr[1] + m33 * ptr[2] + m34) * w;
    ptr[0] = x_;
    ptr[1] = y_;
    ptr[2] = z_;
  }

  return *this;
}

Vector3& Vector3::applyMatrix4(Matrix4& m) {
  return this->applyMatrix4(m.elements);
}


// [[Rcpp::export]]
void Vector3__apply_matrix4(const SEXP& self, const SEXP& m) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Matrix4> ptr_m(m);
  ptr_self->applyMatrix4(ptr_m->elements);
}


Vector3& Vector3::applyQuaternion(const Quaternion& q) {
  if( this->data.empty() ) { return *this; }
  double qx = q.x, qy = q.y, qz = q.z, qw = q.w;
  double ix, iy, iz, iw;

  std::vector<double>::iterator ptr = this->data.begin();
  for(size_t i = 0 ; i < this->getSize(); i++, ptr += 3) {
    // calculate quat * vector
    ix = qw * ptr[0] + qy * ptr[2] - qz * ptr[1];
    iy = qw * ptr[1] + qz * ptr[0] - qx * ptr[2];
    iz = qw * ptr[2] + qx * ptr[1] - qy * ptr[0];
    iw = -qx * ptr[0] - qy * ptr[1] - qz * ptr[2];

    // calculate result * inverse quat
    ptr[0] = ix * qw + iw * -qx + iy * -qz - iz * -qy;
    ptr[1] = iy * qw + iw * -qy + iz * -qx - ix * -qz;
    ptr[2] = iz * qw + iw * -qz + ix * -qy - iy * -qx;
  }

  return *this;
}


Vector3& Vector3::applyQuaternion(const std::vector<double>& q) {

  if(q.size() != 4) {
    Rcpp::stop("C++ Vector3::applyQuaternion - `q` is not a quaternion.");
  }
  if( this->data.empty() ) { return *this; }

  double qx = q[0], qy = q[1], qz = q[2], qw = q[3];
  double ix, iy, iz, iw;

  std::vector<double>::iterator ptr = this->data.begin();
  for(size_t i = 0 ; i < this->getSize(); i++, ptr += 3) {
    // calculate quat * vector
    ix = qw * ptr[0] + qy * ptr[2] - qz * ptr[1];
    iy = qw * ptr[1] + qz * ptr[0] - qx * ptr[2];
    iz = qw * ptr[2] + qx * ptr[1] - qy * ptr[0];
    iw = -qx * ptr[0] - qy * ptr[1] - qz * ptr[2];

    // calculate result * inverse quat
    ptr[0] = ix * qw + iw * -qx + iy * -qz - iz * -qy;
    ptr[1] = iy * qw + iw * -qy + iz * -qx - ix * -qz;
    ptr[2] = iz * qw + iw * -qz + ix * -qy - iy * -qx;
  }

  return *this;
}

// [[Rcpp::export]]
void Vector3__apply_quaternion(const SEXP& self, const SEXP& q) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Quaternion> ptr_q(q);
  ptr_self->applyQuaternion(*ptr_q);
}

// Vector3& Vector3::project(const Camera& camera) {
//
//   return this->applyMatrix4(camera.matrixWorldInverse).applyMatrix4(camera.projectionMatrix);
// }
//
// Vector3& Vector3::unproject(const Camera& camera) {
//
//   return this->applyMatrix4(camera.projectionMatrixInverse).applyMatrix4(*camera.matrixWorld);
// }

Vector3& Vector3::transformDirection(Matrix4& m) {

  // input: THREE.Matrix4 affine matrix
  // vector interpreted as a direction

  const size_t l = this->getSize();
  if(l == 0) { return *this; }

  std::vector<double>::iterator e = m.elements.begin();
  const double &e11 = e[0], &e12 = e[4], &e13 = e[8],
               &e21 = e[1], &e22 = e[5], &e23 = e[9],
               &e31 = e[2], &e32 = e[6], &e33 = e[10];
  std::vector<double>::iterator ptr = this->data.begin();

  double nx, ny, nz, nl;
  for(size_t i = 0; i < l; i++, ptr += 3) {
    nx = e11 * ptr[0] + e12 * ptr[1] + e13 * ptr[2];
    ny = e21 * ptr[0] + e22 * ptr[1] + e23 * ptr[2];
    nz = e31 * ptr[0] + e32 * ptr[1] + e33 * ptr[2];
    nl = nx * nx + ny * ny + nz * nz;
    if( nl > 0 ) {
      nl = 1.0 / sqrt(nl);
      ptr[0] = nx * nl;
      ptr[1] = ny * nl;
      ptr[2] = nz * nl;
    } else {
      ptr[0] = 0;
      ptr[1] = 0;
      ptr[2] = 0;
    }
  }

  return *this;
}

// [[Rcpp::export]]
void Vector3__transform_direction(const SEXP& self, const SEXP& m) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Matrix4> ptr_m(m);
  ptr_self->transformDirection(*ptr_m);
}

Vector3& Vector3::divide(Vector3& v) {

  size_t la = this->getSize();
  size_t lb = v.getSize();
  if( lb != 1 && lb != la) {
    Rcpp::stop("C++ Vector3::divide - inconsistent size of input `v`.");
  }
  if(la == 0) { return *this; }

  if( lb == la ) {
    std::transform(this->data.begin(), this->data.end(),
                   v.data.begin(), this->data.begin(),
                   [](double& a, double& b) { return a / b; });
  } else {
    const double vx = 1.0 / v.data[0];
    const double vy = 1.0 / v.data[1];
    const double vz = 1.0 / v.data[2];
    std::vector<double>::iterator ptr = this->data.begin();
    for(size_t i = 0; i < la; i++, ptr+=3) {
      *ptr *= vx;
      *(ptr+1) *= vy;
      *(ptr+2) *= vz;
    }
  }
  return *this;
}

// [[Rcpp::export]]
void Vector3__divide(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  ptr_self->divide(*ptr_v);
}

Vector3& Vector3::divideScalar(std::vector<double>& s) {

  // s.getSize() must be either 1 or length of data
  size_t l = this->getSize();
  if( l == 0 ) { return *this; }
  if( s.size() == 1 ) {
    double scalar = s[0];
    std::transform(this->data.begin(), this->data.end(),
                   this->data.begin(),
                   [scalar](double& a) { return a / scalar; });
  } else {

    if( s.size() != l ) {
      Rcpp::stop("C++ Vector3::addScalar - length of scalar must be either 1 or the number of points in Vector3.");
    }
    std::vector<double>::iterator ptr = this->data.begin();
    std::vector<double>::iterator ptr2 = s.begin();
    for(size_t i = 0; i < l; i++, ptr+=3, ptr2++) {
      *ptr /= *ptr2;
      *(ptr+1) /= *ptr2;
      *(ptr+2) /= *ptr2;
    }

  }

  return *this;
}

// [[Rcpp::export]]
void Vector3__divide_scalar(const SEXP& self, std::vector<double>& s) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->divideScalar(s);
}

Vector3& Vector3::min(Vector3& v) {

  const size_t vlen = v.getSize();
  const size_t tlen = this->getSize();
  if( vlen != 1 && vlen != tlen ) {
    Rcpp::stop("C++ Vector3::min - size of `v` must be either 1 or consistent with vector");
  }
  if( vlen == 1 ) {
    double mx = v.data[0];
    double my = v.data[1];
    double mz = v.data[2];

    std::vector<double>::iterator ptr = this->data.begin();
    for(size_t i = 0 ; i < tlen; i++, ptr++ ) {
      if( *ptr > mx ) {
        *ptr = mx;
      }
      ptr++;
      if( *ptr > my ) {
        *ptr = my;
      }
      ptr++;
      if( *ptr > mz ) {
        *ptr = mz;
      }
    }
  } else {
    std::transform(
      this->data.begin(), this->data.end(),
      v.data.begin(),
      this->data.begin(),
      [](double& a, double& b) { return std::min(a, b); }
    );
  }
  return *this;
}

// [[Rcpp::export]]
void Vector3__min(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  ptr_self->min(*ptr_v);
}

Vector3& Vector3::max(Vector3& v) {

  const size_t vlen = v.getSize();
  const size_t tlen = this->getSize();
  if( vlen != 1 && vlen != tlen ) {
    Rcpp::stop("C++ Vector3::max - size of `v` must be either 1 or consistent with vector");
  }
  if( vlen == 1 ) {
    double mx = v.data[0];
    double my = v.data[1];
    double mz = v.data[2];

    std::vector<double>::iterator ptr = this->data.begin();
    for(size_t i = 0 ; i < tlen; i++, ptr++ ) {
      if( *ptr < mx ) {
        *ptr = mx;
      }
      ptr++;
      if( *ptr < my ) {
        *ptr = my;
      }
      ptr++;
      if( *ptr < mz ) {
        *ptr = mz;
      }
    }
  } else {
    std::transform(
      this->data.begin(), this->data.end(),
      v.data.begin(),
      this->data.begin(),
      [](double& a, double& b) { return std::max(a, b); }
    );
  }
  return *this;
}

// [[Rcpp::export]]
void Vector3__max(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  ptr_self->max(*ptr_v);
}

Vector3& Vector3::clamp(Vector3& min, Vector3& max) {

  // assumes min < max, componentwise
  if( min.getSize() != 1 || max.getSize() != 1 ) {
    Rcpp::stop("C++ Vector3::clamp - min and max must have and only have one 3D point.");
  }
  double minX = min.data[0];
  double minY = min.data[1];
  double minZ = min.data[2];
  double maxX = max.data[0];
  double maxY = max.data[1];
  double maxZ = max.data[2];
  double tmp;
  if( minX > maxX ) {
    tmp = maxX; maxX = minX; minX = tmp;
  }
  if( minY > maxY ) {
    tmp = maxY; maxY = minY; minY = tmp;
  }
  if( minZ > maxZ ) {
    tmp = maxZ; maxZ = minZ; minZ = tmp;
  }

  std::vector<double>::iterator ptr = this->data.begin();
  for(size_t i = 0 ; i < this->getSize(); i++, ptr++ ) {
    *ptr = std::max(minX, std::min(maxX, *ptr));
    ptr++;
    *ptr = std::max(minY, std::min(maxY, *ptr));
    ptr++;
    *ptr = std::max(minZ, std::min(maxZ, *ptr));
  }

  return *this;
}

// [[Rcpp::export]]
void Vector3__clamp(const SEXP& self, const SEXP& min, const SEXP& max) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_min(min);
  Rcpp::XPtr<Vector3> ptr_max(max);
  ptr_self->clamp(*ptr_min, *ptr_max);
}


Vector3& Vector3::floor() {

  std::transform(this->data.begin(), this->data.end(),
                 this->data.begin(),
                 [](double& a) { return std::floor(a); });

  return *this;
}

// [[Rcpp::export]]
void Vector3__floor(const SEXP& self) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->floor();
}

Vector3& Vector3::ceil() {

  std::transform(this->data.begin(), this->data.end(),
                 this->data.begin(),
                 [](double& a) { return std::ceil(a); });

  return *this;
}

// [[Rcpp::export]]
void Vector3__ceil(const SEXP& self) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->ceil();
}

Vector3& Vector3::round() {

  std::transform(this->data.begin(), this->data.end(),
                 this->data.begin(),
                 [](double& a) { return std::round(a); });

  return *this;
}

// [[Rcpp::export]]
void Vector3__round(const SEXP& self) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->round();
}

Vector3& Vector3::roundToZero() {

  std::transform(this->data.begin(), this->data.end(),
                 this->data.begin(),
                 [](double& a) { return (a < 0) ? std::ceil(a) : std::floor(a); });
  return *this;
}

// [[Rcpp::export]]
void Vector3__round_to_zero(const SEXP& self) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->roundToZero();
}

Vector3& Vector3::negate() {

  std::transform(this->data.begin(), this->data.end(),
                 this->data.begin(),
                 [](double& a) { return -a; });

  return *this;
}

// [[Rcpp::export]]
void Vector3__negate(const SEXP& self) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->negate();
}

std::vector<double> Vector3::dot(Vector3& v) {
  const size_t nElems = this->getSize();
  const size_t vSize = v.getSize();
  if( vSize != 1 && vSize != nElems ) {
    Rcpp::stop("C++ Vector3::dot - v must have either one 3D point or equal to the vector size.");
  }
  std::vector<double> re( nElems );
  if( re.empty() ) { return re; }

  std::vector<double>::iterator ptr = this->data.begin();
  std::vector<double>::iterator pre = re.begin();

  if( vSize == 1 ) {
    double vx, vy, vz;
    vx = v.data[0];
    vy = v.data[1];
    vz = v.data[2];

    for( size_t i = 0 ; i < nElems ; i++, ptr += 3, pre++ ) {
      *pre = *ptr * vx + *(ptr+1) * vy + *(ptr + 2) * vz;
    }
  } else {
    std::vector<double>::iterator prv = v.data.begin();
    for( size_t i = 0 ; i < nElems ; i++, ptr += 3, prv += 3, pre++ ) {
      *pre = *ptr * *prv + *(ptr+1) * *(prv+1) + *(ptr + 2) * *(prv + 2);
    }
  }

  return re;
}

// [[Rcpp::export]]
std::vector<double> Vector3__dot(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  return ptr_self->dot(*ptr_v);
}


std::vector<double> Vector3::lengthSq() {
  size_t nElems = this->getSize();
  std::vector<double> re( nElems );
  if( re.empty() ) { return re; }

  std::vector<double>::iterator ptr = this->data.begin();
  std::vector<double>::iterator pre = re.begin();
  for( size_t i = 0 ; i < nElems ; i++, ptr += 3, pre++ ) {
    *pre = *ptr * *ptr + *(ptr+1) * *(ptr+1) + *(ptr + 2) * *(ptr + 2);
  }

  return re;
}

// [[Rcpp::export]]
std::vector<double> Vector3__length_squared(const SEXP& self) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  return ptr_self->lengthSq();
}

std::vector<double> Vector3::length() {
  size_t nElems = this->getSize();
  std::vector<double> re( nElems );
  if( re.empty() ) { return re; }

  std::vector<double>::iterator ptr = this->data.begin();
  std::vector<double>::iterator pre = re.begin();
  for( size_t i = 0 ; i < nElems ; i++, ptr += 3, pre++ ) {
    *pre = std::sqrt(*ptr * *ptr + *(ptr+1) * *(ptr+1) + *(ptr + 2) * *(ptr + 2));
  }

  return re;
}

// [[Rcpp::export]]
std::vector<double> Vector3__length(const SEXP& self) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  return ptr_self->length();
}

std::vector<double> Vector3::manhattanLength() {

  size_t nElems = this->getSize();
  std::vector<double> re( nElems );
  if( re.empty() ) { return re; }

  std::vector<double>::iterator ptr = this->data.begin();
  std::vector<double>::iterator pre = re.begin();
  for( size_t i = 0 ; i < nElems ; i++, ptr += 3, pre++ ) {
    *pre = std::abs(*ptr) + std::abs(*(ptr+1)) + std::abs(*(ptr + 2));
  }

  return re;
}

// [[Rcpp::export]]
std::vector<double> Vector3__length_manhattan(const SEXP& self) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  return ptr_self->manhattanLength();
}

Vector3& Vector3::normalize() {
  size_t nElems = this->getSize();
  if( nElems == 0 ) { return *this; }

  std::vector<double>::iterator ptr = this->data.begin();
  double l;
  for( size_t i = 0 ; i < nElems ; i++, ptr += 3 ) {
    l = *ptr * *ptr + *(ptr+1) * *(ptr+1) + *(ptr + 2) * *(ptr + 2);
    if( !std::isnan(l) && l > 0.0 ) {
      l = std::sqrt(l);
      *ptr /= l;
      *(ptr+1) /= l;
      *(ptr+2) /= l;
    }
  }
  return *this;
}

// [[Rcpp::export]]
void Vector3__normalize(const SEXP& self) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->normalize();
}

Vector3& Vector3::setLength(std::vector<double>& length) {
  this->normalize();
  return this->multiplyScalar(length);
}

// [[Rcpp::export]]
void Vector3__set_length(const SEXP& self, std::vector<double>& length) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->setLength(length);
}

Vector3& Vector3::lerp(Vector3& v, std::vector<double>& alpha) {

  const size_t l = this->getSize();
  if( v.getSize() != l ) {
    Rcpp::stop("C++ Vector3::lerp - input v must have the same size as current vector3");
  }
  if( alpha.size() == 1 ) {
    double a = alpha[0];
    std::transform(v.data.begin(), v.data.end(),
                   this->data.begin(), this->data.begin(),
                   [a](double& vb, double& va) { return va + (vb - va) * a; });
  } else {
    if( alpha.size() != l ) {
      Rcpp::stop("C++ Vector3::lerp - length of `alpha` must be either 1 or the number of points in Vector3.");
    }
    std::vector<double>::iterator ptr_t = this->data.begin();
    std::vector<double>::iterator ptr_v = v.data.begin();
    std::vector<double>::iterator ptr_a = alpha.begin();
    for(size_t i = 0; i < l; i++, ptr_t++, ptr_v++, ptr_a++) {
      *ptr_t += (*ptr_v - *ptr_t) * *ptr_a;
      ptr_t++; ptr_v++;
      *ptr_t += (*ptr_v - *ptr_t) * *ptr_a;
      ptr_t++; ptr_v++;
      *ptr_t += (*ptr_v - *ptr_t) * *ptr_a;
    }
  }

  return *this;
}

// [[Rcpp::export]]
void Vector3__lerp(const SEXP& self, const SEXP& v, std::vector<double>& alpha) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  ptr_self->lerp(*ptr_v, alpha);
}

Vector3& Vector3::lerpVectors(Vector3& v1, Vector3& v2, std::vector<double>& alpha) {
  const size_t l1 = v1.getSize();
  if( l1 != v2.getSize() ) {
    Rcpp::stop("C++ Vector3::lerpVectors - v1 must have the same length as v2.");
  }
  if( alpha.size() != 1 && alpha.size() != l1 ) {
    Rcpp::stop("C++ Vector3::lerpVectors - alpha must be length of 1 or have the same length as v1.");
  }
  this->resize(l1);
  if( alpha.size() == 1 ) {
    double a = alpha[0];
    std::transform(v1.data.begin(), v1.data.end(),
                   v2.data.begin(), this->data.begin(),
                   [a](double& va, double& vb) { return va + (vb - va) * a; });
  } else {
    std::vector<double>::iterator ptr_t = this->data.begin();
    std::vector<double>::iterator ptr_v1 = v1.data.begin();
    std::vector<double>::iterator ptr_v2 = v2.data.begin();
    std::vector<double>::iterator ptr_a = alpha.begin();
    for(size_t i = 0; i < l1; i++, ptr_t++, ptr_v1++, ptr_v2++, ptr_a++) {
      *ptr_t = *ptr_v1 + (*ptr_v2 - *ptr_v1) * *ptr_a;
      ptr_t++; ptr_v1++; ptr_v2++;
      *ptr_t = *ptr_v1 + (*ptr_v2 - *ptr_v1) * *ptr_a;
      ptr_t++; ptr_v1++; ptr_v2++;
      *ptr_t = *ptr_v1 + (*ptr_v2 - *ptr_v1) * *ptr_a;
    }
  }

  return *this;
  return *this;
}

// [[Rcpp::export]]
void Vector3__lerp_vectors(const SEXP& self, const SEXP& v1, const SEXP& v2,
                           std::vector<double>& alpha) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v1(v1);
  Rcpp::XPtr<Vector3> ptr_v2(v2);
  ptr_self->lerpVectors(*ptr_v1, *ptr_v2, alpha);
}

Vector3& Vector3::cross(Vector3& v) {
  const size_t l = this->getSize();
  const size_t lb = v.getSize();
  if( lb != 1 && lb != l ) {
    Rcpp::stop("C++ Vector3::cross - length of v must be 1 or length of applied vector3.");
  }
  if( l == 0 ) { return *this; }
  std::vector<double>::iterator pax = this->data.begin();
  std::vector<double>::iterator pay = pax + 1;
  std::vector<double>::iterator paz = pay + 1;
  std::vector<double>::iterator pbx = v.data.begin();
  std::vector<double>::iterator pby = pbx + 1;
  std::vector<double>::iterator pbz = pby + 1;

  double ax, ay, az, bx, by, bz;
  if( lb == 1 ) {
    for(size_t i = 0 ; i < l ; i++ ) {
      ax = *pax; ay = *pay; az = *paz;
      bx = *pbx; by = *pby; bz = *pbz;
      *pax = ay * bz - az * by;
      *pay = az * bx - ax * bz;
      *paz = ax * by - ay * bx;
      pax += 3; pay += 3; paz += 3;
    }
  } else {
    for(size_t i = 0 ; i < l ; i++ ) {
      ax = *pax; ay = *pay; az = *paz;
      bx = *pbx; by = *pby; bz = *pbz;
      *pax = ay * bz - az * by;
      *pay = az * bx - ax * bz;
      *paz = ax * by - ay * bx;
      pbx += 3; pby += 3; pbz += 3;
      pax += 3; pay += 3; paz += 3;
    }
  }

  return *this;
}

// [[Rcpp::export]]
void Vector3__cross(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  ptr_self->cross(*ptr_v);
}

Vector3& Vector3::crossVectors(Vector3& a, Vector3& b) {

  const size_t l = a.getSize();
  const size_t lb = b.getSize();
  if( lb != 1 && lb != l ) {
    Rcpp::stop("C++ Vector3::crossVectors - length of b must be 1 or length of `a`.");
  }
  this->resize(l);
  if( l == 0 ) { return *this; }
  std::vector<double>::iterator pt = this->data.begin();
  std::vector<double>::iterator pax = a.data.begin();
  std::vector<double>::iterator pay = pax + 1;
  std::vector<double>::iterator paz = pay + 1;
  std::vector<double>::iterator pbx = b.data.begin();
  std::vector<double>::iterator pby = pbx + 1;
  std::vector<double>::iterator pbz = pby + 1;

  double ax, ay, az, bx, by, bz;
  if( lb == 1 ) {
    for(size_t i = 0 ; i < l ; i++ ) {
      ax = *pax; ay = *pay; az = *paz;
      bx = *pbx; by = *pby; bz = *pbz;
      *pt = ay * bz - az * by;
      *(pt + 1) = az * bx - ax * bz;
      *(pt + 2) = ax * by - ay * bx;
      pax += 3; pay += 3; paz += 3;
      pt += 3;
    }
  } else {
    for(size_t i = 0 ; i < l ; i++ ) {
      ax = *pax; ay = *pay; az = *paz;
      bx = *pbx; by = *pby; bz = *pbz;
      *pt = ay * bz - az * by;
      *(pt + 1) = az * bx - ax * bz;
      *(pt + 2) = ax * by - ay * bx;
      pax += 3; pay += 3; paz += 3;
      pbx += 3; pby += 3; pbz += 3;
      pt += 3;
    }
  }
  return *this;
}

// [[Rcpp::export]]
void Vector3__cross_vectors(const SEXP& self, const SEXP& a, const SEXP& b) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_a(a);
  Rcpp::XPtr<Vector3> ptr_b(b);
  ptr_self->crossVectors(*ptr_a, *ptr_b);
}


Vector3& Vector3::projectOnVector(Vector3& v) {

  if( v.getSize() != 1 ) {
    Rcpp::stop("C++ Vector3::projectOnVector - size of v must be 1.");
  }
  std::vector<double> denominator = v.lengthSq();

  if (denominator[0] == 0) {
    this->multiplyScalar(denominator);
    return *this;
  }

  double dinv = 1.0 / denominator[0];
  std::vector<double> d = this->dot(v);

  std::transform(
    d.begin(), d.end(), d.begin(),
    [dinv](double& x) { return x * dinv; }
  );

  this->setX(v.getX(0));
  this->setY(v.getY(0));
  this->setZ(v.getZ(0));

  this->multiplyScalar(d);

  return *this;
}

// [[Rcpp::export]]
void Vector3__project_on_vector(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  ptr_self->projectOnVector(*ptr_v);
}

Vector3& Vector3::projectOnPlane(Vector3& planeNormal) {

  Vector3 _vector;
  _vector.copy(*this, 0).projectOnVector(planeNormal);

  return this->sub(_vector);
}

// [[Rcpp::export]]
void Vector3__project_on_plane(const SEXP& self, const SEXP& planeNormal) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_planeNormal(planeNormal);
  ptr_self->projectOnPlane(*ptr_planeNormal);
}

Vector3& Vector3::reflect(Vector3& normal) {

  // reflect incident vector off plane orthogonal to normal
  // normal is assumed to have unit length
  if( normal.getSize() != 1 ) {
    Rcpp::stop("C++ Vector3::reflect - size of normal must be 1.");
  }

  Vector3 _vector;

  _vector.copy(normal, 1).normalize();

  double nx = _vector.data[0];
  double ny = _vector.data[1];
  double nz = _vector.data[2];

  std::vector<double>::iterator ptr = this->data.begin();
  double innerProd2;
  for(size_t i = 0; i < this->getSize(); i++) {
    innerProd2 = 2.0 * (ptr[0] * nx + ptr[1] * ny + ptr[2] * nz);
    *ptr = *ptr - innerProd2 * nx;
    ptr++;
    *ptr = *ptr - innerProd2 * ny;
    ptr++;
    *ptr = *ptr - innerProd2 * nz;
    ptr++;
  }

  // return this->sub(_vector.copy(normal).multiplyScalar(2 * this->dot(normal)));
  return *this;
}

// [[Rcpp::export]]
void Vector3__reflect(const SEXP& self, const SEXP& normal) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_normal(normal);
  ptr_self->reflect(*ptr_normal);
}

std::vector<double> Vector3::angleTo(Vector3& v) {

  const size_t vlen = v.getSize();
  const size_t len = this->getSize();

  if( vlen != 1 && vlen != len ) {
    Rcpp::stop("C++ Vector3::angleTo - size of v must be 1 or equal to self.");
  }

  std::vector<double> innerProduct = this->dot(v);
  if( innerProduct.empty() ) {
    return innerProduct;
  }

  std::vector<double> denominator = this->length();
  std::vector<double> vLength = v.length();

  if( vlen == 1 ) {
    double vLength_ = vLength[0];
    std::transform(
      denominator.begin(), denominator.end(), denominator.begin(),
      [vLength_](double& x) { return x * vLength_; }
    );
  } else {
    std::transform(
      denominator.begin(), denominator.end(), vLength.begin(), denominator.begin(),
      [](double& x, double& y) { return x * y; }
    );
  }

  std::transform(
    innerProduct.begin(), innerProduct.end(), denominator.begin(), innerProduct.begin(),
    [](double& x, double& y) {
      double theta = 0;
      if( y > 0 ) {
        theta = x / y;
      }
      // clamp, to handle numerical problems
      return std::acos(std::clamp(theta, -1.0, 1.0));
    }
  );

  return innerProduct;
}

// [[Rcpp::export]]
std::vector<double> Vector3__angle_to(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  return ptr_self->angleTo(*ptr_v);
}

std::vector<double> Vector3::distanceTo(Vector3& v) {

  std::vector<double> result = this->distanceToSquared(v);
  std::transform(
    result.begin(), result.end(), result.begin(),
    [](double& x) { return std::sqrt(x); }
  );
  return result;
}


// [[Rcpp::export]]
std::vector<double> Vector3__distance_to(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  return ptr_self->distanceTo(*ptr_v);
}

std::vector<double> Vector3::distanceToSquared(Vector3& v) {

  const size_t vlen = v.getSize();
  const size_t len = this->getSize();

  if( vlen != 1 && vlen != len ) {
    Rcpp::stop("C++ Vector3::distanceToSquared - size of v must be 1 or equal to self.");
  }

  std::vector<double> result(len);

  if( len == 0 ) { // empty vector
    return result;
  }

  if( vlen == 1 ) {
    double vx = v.data[0], vy = v.data[1], vz = v.data[2];
    std::vector<double>::iterator ptr = this->data.begin();
    for(size_t i = 0; i < len; i++, ptr += 3) {
      result[i] = std::pow(ptr[0] - vx, 2) + std::pow(ptr[1] - vy, 2) + std::pow(ptr[2] - vz, 2);
    }
  } else {
    std::vector<double>::iterator ptr1 = this->data.begin();
    std::vector<double>::iterator ptr2 = v.data.begin();
    for(size_t i = 0; i < len; i++, ptr1 += 3, ptr2 += 3) {
      result[i] = std::pow(ptr1[0] - ptr2[0], 2) + std::pow(ptr1[1] - ptr2[1], 2) + std::pow(ptr1[2] - ptr2[2], 2);
    }
  }

  return result;
}

// [[Rcpp::export]]
std::vector<double> Vector3__distance_to_squared(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  return ptr_self->distanceToSquared(*ptr_v);
}

std::vector<double> Vector3::manhattanDistanceTo(Vector3& v) {

  const size_t vlen = v.getSize();
  const size_t len = this->getSize();

  if( vlen != 1 && vlen != len ) {
    Rcpp::stop("C++ Vector3::manhattanDistanceTo - size of v must be 1 or equal to self.");
  }

  std::vector<double> result(len);

  if( len == 0 ) { // empty vector
    return result;
  }

  if( vlen == 1 ) {
    double vx = v.data[0], vy = v.data[1], vz = v.data[2];
    std::vector<double>::iterator ptr = this->data.begin();
    for(size_t i = 0; i < len; i++, ptr += 3) {
      result[i] = std::abs(ptr[0] - vx) + std::abs(ptr[1] - vy) + std::abs(ptr[2] - vz);
    }
  } else {
    std::vector<double>::iterator ptr1 = this->data.begin();
    std::vector<double>::iterator ptr2 = v.data.begin();
    for(size_t i = 0; i < len; i++, ptr1 += 3, ptr2 += 3) {
      result[i] = std::abs(ptr1[0] - ptr2[0]) + std::abs(ptr1[1] - ptr2[1]) + std::abs(ptr1[2] - ptr2[2]);
    }
  }

  return result;
}

// [[Rcpp::export]]
std::vector<double> Vector3__distance_to_manhattan(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  return ptr_self->manhattanDistanceTo(*ptr_v);
}


// Vector3& Vector3::setFromSpherical(const Spherical& s) {
//
//   return this->setFromSphericalCoords(s.radius, s.phi, s.theta);
// }

Vector3& Vector3::setFromSphericalCoords(const double& radius, const double& phi, const double& theta) {

  const double sinPhiRadius = std::sin(phi) * radius;
  this->resize(1);

  this->data[0] = sinPhiRadius * std::sin(theta);
  this->data[1] = std::cos(phi) * radius;
  this->data[2] = sinPhiRadius * std::cos(theta);

  return *this;
}

// [[Rcpp::export]]
void Vector3__set_from_spherical_coords(const SEXP& self, const double& radius, const double& phi, const double& theta) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  ptr_self->setFromSphericalCoords(radius, phi, theta);
}

Vector3& Vector3::setFromMatrixPosition(Matrix4& m) {

  std::vector<double>::iterator e = m.elements.begin();
  this->resize(1);

  this->data[0] = e[12];
  this->data[1] = e[13];
  this->data[2] = e[14];

  return *this;
}

// [[Rcpp::export]]
void Vector3__set_from_matrix_position(const SEXP& self, const SEXP& m) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Matrix4> ptr_m(m);
  ptr_self->setFromMatrixPosition(*ptr_m);
}

Vector3& Vector3::setFromMatrixScale(Matrix4& m) {
  std::vector<double>::iterator e = m.elements.begin();

  this->resize(1);

  this->data[0] = std::sqrt(e[0] * e[0] + e[1] * e[1] + e[2] * e[2]);
  this->data[1] = std::sqrt(e[4] * e[4] + e[5] * e[5] + e[6] * e[6]);
  this->data[2] = std::sqrt(e[8] * e[8] + e[9] * e[9] + e[10] * e[10]);
  return *this;
}

// [[Rcpp::export]]
void Vector3__set_from_matrix_scale(const SEXP& self, const SEXP& m) {
  Rcpp::XPtr<Vector3> ptr_self(self);
  Rcpp::XPtr<Matrix4> ptr_m(m);
  ptr_self->setFromMatrixScale(*ptr_m);
}

Vector3& Vector3::setFromMatrixColumn(Matrix4& m, unsigned int index) {
  const unsigned int nrows = m.nrows;
  return this->fromArray(m.elements, index * nrows, 1);
}
//
// Vector3& Vector3::setFromMatrix3Column(const Matrix3& m, unsigned int index) {
//
//   return this->fromArray(m.elements, index * 3);
// }

Vector3 Vector3::clone() {
  Vector3 re{};
  re.copy(*this, 0);
  return re;
}

// bool Vector3::equals(const Vector3& v) const {
//
//   return ((v.x == this->x) && (v.y == this->y) && (v.z == this->z));
// }
//
// bool Vector3::operator!=(const Vector3& other) const {
//
//   return !equals(other);
// }
// bool Vector3::operator==(const Vector3& other) const {
//
//   return equals(other);
// }
//
// bool Vector3::isNan() const {
//
//   return std::isnan(x) || std::isnan(y) || std::isnan(z);
// }
//
// Vector3& Vector3::makeNan() {
//
//   return set(NAN, NAN, NAN);
// }
//
// Vector3& Vector3::operator/=(double s) {
//
//   return divideScalar(s);
// }
//
// Vector3 Vector3::operator/(double s) const {
//
//   return clone().divideScalar(s);
// }
//
// Vector3& Vector3::operator/=(const Vector3& other) {
//
//   return divide(other);
// }
//
// Vector3 Vector3::operator/(const Vector3& other) const {
//
//   return clone().divide(other);
// }
//
// Vector3& Vector3::operator*=(double s) {
//
//   return multiplyScalar(s);
// }
//
// Vector3 Vector3::operator*(double s) const {
//
//   return clone().multiplyScalar(s);
// }
//
// Vector3& Vector3::operator*=(const Vector3& other) {
//
//   return multiply(other);
// }
//
// Vector3 Vector3::operator*(const Vector3& other) const {
//
//   return clone().multiply(other);
// }
//
// Vector3& Vector3::operator-=(double s) {
//
//   return subScalar(s);
// }
//
// Vector3 Vector3::operator-(double s) const {
//
//   return clone().subScalar(s);
// }
//
// Vector3& Vector3::operator-=(const Vector3& other) {
//
//   return sub(other);
// }
//
// Vector3 Vector3::operator-(const Vector3& other) const {
//
//   return clone().sub(other);
// }
// Vector3& Vector3::operator+=(double s) {
//
//   return addScalar(s);
// }
//
// Vector3 Vector3::operator+(double s) const {
//
//   return clone().addScalar(s);
// }
//
// Vector3& Vector3::operator+=(const Vector3& other) {
//
//   return add(other);
// }
//
// Vector3 Vector3::operator+(const Vector3& other) const {
//
//   return clone().add(other);
// }

