#include "glQuaternion.h"
#include "glVector3.h"
#include "glMatrix4.h"
//
// #include "threepp/math/Quaternion.hpp"
//
// #include "threepp/math/Euler.hpp"
// #include "threepp/math/Matrix4.hpp"
// #include "threepp/math/Vector3.hpp"
//
// #include <algorithm>
// #include <cmath>
// #include <string>

using namespace rave3d;


Quaternion::Quaternion() {
  this->x = 0.0;
  this->y = 0.0;
  this->z = 0.0;
  this->w = 1.0;
}

// [[Rcpp::export]]
SEXP Quaternion__new() {
  Rcpp::XPtr<Quaternion> ptr( new Quaternion(), true );
  // return the external pointer to the R side
  return ptr;
}

//
// double Quaternion::operator[](unsigned int index) const {
//   switch (index) {
//   case 0:
//     return x();
//   case 1:
//     return y();
//   case 2:
//     return z();
//   case 3:
//     return w();
//   default:
//     throw std::runtime_error("index out of bound: " + std::to_string(index));
//   }
// }

Quaternion& Quaternion::set(const double& x, const double& y, const double& z, const double& w) {

  this->x = x;
  this->y = y;
  this->z = z;
  this->w = w;

  return *this;
}

// [[Rcpp::export]]
void Quaternion__set(SEXP& self,
                     const double& x, const double& y, const double& z, const double& w) {
  Rcpp::XPtr<Quaternion> ptr(self);
  ptr->set(x, y, z, w);
}

Quaternion& Quaternion::copy(Quaternion& quaternion) {

  this->x = quaternion.x;
  this->y = quaternion.y;
  this->z = quaternion.z;
  this->w = quaternion.w;

  return *this;
}

// [[Rcpp::export]]
void Quaternion__copy(SEXP& self, SEXP& quaternion) {
  Rcpp::XPtr<Quaternion> ptr(self);
  Rcpp::XPtr<Quaternion> q(quaternion);
  ptr->copy(*q);
}

std::vector<double> Quaternion::toArray() {
  std::vector<double> arr(4);
  arr[0] = this->x;
  arr[1] = this->y;
  arr[2] = this->z;
  arr[3] = this->w;
  return arr;
}

// [[Rcpp::export]]
SEXP Quaternion__to_array(SEXP& self) {
  Rcpp::XPtr<Quaternion> ptr(self);
  SEXP re = PROTECT(Rf_allocVector(REALSXP, 4));
  double* ptr_re = REAL(re);
  *ptr_re++ = ptr->x;
  *ptr_re++ = ptr->y;
  *ptr_re++ = ptr->z;
  *ptr_re = ptr->w;
  UNPROTECT(1);
  return re;
}

// [[Rcpp::export]]
double Quaternion__getX(SEXP& self) {
  Rcpp::XPtr<Quaternion> ptr(self);
  return ptr->x;
}
// [[Rcpp::export]]
void Quaternion__setX(SEXP& self, const double& v) {
  Rcpp::XPtr<Quaternion> ptr(self);
  ptr->x = v;
}
// [[Rcpp::export]]
double Quaternion__getY(SEXP& self) {
  Rcpp::XPtr<Quaternion> ptr(self);
  return ptr->y;
}
// [[Rcpp::export]]
void Quaternion__setY(SEXP& self, const double& v) {
  Rcpp::XPtr<Quaternion> ptr(self);
  ptr->y = v;
}
// [[Rcpp::export]]
double Quaternion__getZ(SEXP& self) {
  Rcpp::XPtr<Quaternion> ptr(self);
  return ptr->z;
}
// [[Rcpp::export]]
void Quaternion__setZ(SEXP& self, const double& v) {
  Rcpp::XPtr<Quaternion> ptr(self);
  ptr->z = v;
}
// [[Rcpp::export]]
double Quaternion__getW(SEXP& self) {
  Rcpp::XPtr<Quaternion> ptr(self);
  return ptr->w;
}
// [[Rcpp::export]]
void Quaternion__setW(SEXP& self, const double& v) {
  Rcpp::XPtr<Quaternion> ptr(self);
  ptr->w = v;
}

// Quaternion& Quaternion::setFromEuler(const Euler& euler, bool update) {
//
//   const auto x = euler.x(), y = euler.y(), z = euler.z();
//   const auto order = euler.order_;
//
//   // http://www.mathworks.com/matlabcentral/fileexchange/
//   // 	20696-function-to-convert-between-dcm-euler-angles-quaternions-and-euler-vectors/
//   //	content/SpinCalc.m
//
//   const double c1 = std::cos(x / 2.0);
//   const double c2 = std::cos(y / 2.0);
//   const double c3 = std::cos(z / 2.0);
//
//   const double s1 = std::sin(x / 2.0);
//   const double s2 = std::sin(y / 2.0);
//   const double s3 = std::sin(z / 2.0);
//
//   switch (order) {
//
//   case Euler::RotationOrders::XYZ:
//     this->x = s1 * c2 * c3 + c1 * s2 * s3;
//     this->y = c1 * s2 * c3 - s1 * c2 * s3;
//     this->z = c1 * c2 * s3 + s1 * s2 * c3;
//     this->w = c1 * c2 * c3 - s1 * s2 * s3;
//     break;
//
//   case Euler::RotationOrders::YXZ:
//     this->x = s1 * c2 * c3 + c1 * s2 * s3;
//     this->y = c1 * s2 * c3 - s1 * c2 * s3;
//     this->z = c1 * c2 * s3 - s1 * s2 * c3;
//     this->w = c1 * c2 * c3 + s1 * s2 * s3;
//     break;
//
//   case Euler::RotationOrders::ZXY:
//     this->x = s1 * c2 * c3 - c1 * s2 * s3;
//     this->y = c1 * s2 * c3 + s1 * c2 * s3;
//     this->z = c1 * c2 * s3 + s1 * s2 * c3;
//     this->w = c1 * c2 * c3 - s1 * s2 * s3;
//     break;
//
//   case Euler::RotationOrders::ZYX:
//     this->x = s1 * c2 * c3 - c1 * s2 * s3;
//     this->y = c1 * s2 * c3 + s1 * c2 * s3;
//     this->z = c1 * c2 * s3 - s1 * s2 * c3;
//     this->w = c1 * c2 * c3 + s1 * s2 * s3;
//     break;
//
//   case Euler::RotationOrders::YZX:
//     this->x = s1 * c2 * c3 + c1 * s2 * s3;
//     this->y = c1 * s2 * c3 + s1 * c2 * s3;
//     this->z = c1 * c2 * s3 - s1 * s2 * c3;
//     this->w = c1 * c2 * c3 - s1 * s2 * s3;
//     break;
//
//   case Euler::RotationOrders::XZY:
//     this->x = s1 * c2 * c3 - c1 * s2 * s3;
//     this->y = c1 * s2 * c3 - s1 * c2 * s3;
//     this->z = c1 * c2 * s3 + s1 * s2 * c3;
//     this->w = c1 * c2 * c3 + s1 * s2 * s3;
//     break;
//   }
//
//   if (update) {
//     this->onChangeCallback_();
//   }
//
//   return *this;
// }
//
Quaternion& Quaternion::setFromAxisAngle(Vector3& axis, const double& angle) {

  // http://www.euclideanspace.com/maths/geometry/rotations/conversions/angleToQuaternion/index.htm

  if( axis.getSize() < 1 ) {
    Rcpp::stop("C++ Quaternion::setFromAxisAngle - `axis` must not be empty.");
  }
  // assumes axis is normalized

  const double halfAngle = angle / 2.0;
  const double s = std::sin(halfAngle);

  this->x = axis.getX(0) * s;
  this->y = axis.getY(0) * s;
  this->z = axis.getZ(0) * s;
  this->w = std::cos(halfAngle);

  return *this;
}

// [[Rcpp::export]]
void Quaternion__set_from_axis_angle(
    SEXP& self, SEXP& axis, const double& angle) {
  Rcpp::XPtr<Quaternion> ptr(self);
  Rcpp::XPtr<Vector3> ptr_axis(axis);
  ptr->setFromAxisAngle(*ptr_axis, angle);
}


Quaternion& Quaternion::setFromRotationMatrix(Matrix4& m) {

  // http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/index.htm

  // assumes the upper 3x3 of m is a pure rotation matrix (i.e, unscaled)

  std::vector<double>::iterator te = m.elements.begin();

  const double &m11 = te[0], &m12 = te[4], &m13 = te[8];
  const double &m21 = te[1], &m22 = te[5], &m23 = te[9];
  const double &m31 = te[2], &m32 = te[6], &m33 = te[10];
  const double trace = m11 + m22 + m33;

  double s;

  if (trace > 0) {

    s = 0.5 / std::sqrt(trace + 1.0);

    this->w = 0.25 / s;
    this->x = (m32 - m23) * s;
    this->y = (m13 - m31) * s;
    this->z = (m21 - m12) * s;

  } else if (m11 > m22 && m11 > m33) {

    s = 2.0 * std::sqrt(1.0 + m11 - m22 - m33);

    this->w = (m32 - m23) / s;
    this->x = 0.25 * s;
    this->y = (m12 + m21) / s;
    this->z = (m13 + m31) / s;

  } else if (m22 > m33) {

    s = 2.0 * std::sqrt(1.0 + m22 - m11 - m33);

    this->w = (m13 - m31) / s;
    this->x = (m12 + m21) / s;
    this->y = 0.25 * s;
    this->z = (m23 + m32) / s;

  } else {

    s = 2.0 * std::sqrt(1.0 + m33 - m11 - m22);

    this->w = (m21 - m12) / s;
    this->x = (m13 + m31) / s;
    this->y = (m23 + m32) / s;
    this->z = 0.25 * s;
  }

  return *this;
}

// [[Rcpp::export]]
void Quaternion__set_from_rotation_matrix(
    SEXP& self, SEXP& m) {
  Rcpp::XPtr<Quaternion> ptr(self);
  Rcpp::XPtr<Matrix4> ptr_m(m);
  ptr->setFromRotationMatrix(*ptr_m);
}



Quaternion& Quaternion::setFromUnitVectors(Vector3& vFrom, Vector3& vTo) {
  // assumes direction vectors vFrom and vTo are normalized

  const double EPS = 0.000001;

  double fx = vFrom.getX(0), fy = vFrom.getY(0), fz = vFrom.getZ(0);
  double tx = vTo.getX(0), ty = vTo.getY(0), tz = vTo.getZ(0);

  double flen = fx * fx + fy * fy + fz * fz;
  double tlen = tx * tx + ty * ty + tz * tz;

  if( flen > 0 && std::abs(flen - 1.0) > EPS ) {
    flen = std::sqrt(flen);
    fx /= flen;
    fy /= flen;
    fz /= flen;
  }
  if( tlen > 0 && std::abs(tlen - 1.0) > EPS ) {
    tlen = std::sqrt(tlen);
    tx /= tlen;
    ty /= tlen;
    tz /= tlen;
  }

  double r = fx * tx + fy * ty + fz * tz + 1.0;

  if (r < EPS) {

    // vFrom and vTo point in opposite directions

    r = 0.0;

    if (std::abs(fx) > std::abs(fz)) {

      this->x = -fy;
      this->y = fx;
      this->z = 0.0;
      this->w = r;

    } else {

      this->x = 0.0;
      this->y = -fz;
      this->z = fy;
      this->w = r;
    }

  } else {

    // crossVectors( vFrom, vTo ); // inlined to avoid cyclic dependency on Vector3

    this->x = fy * tz - fz * ty;
    this->y = fz * tx - fx * tz;
    this->z = fx * ty - fy * tx;
    this->w = r;
  }

  return this->normalize();
}

// [[Rcpp::export]]
void Quaternion__set_from_unit_vectors(
    SEXP& self, SEXP& v_from, SEXP& v_to) {
  Rcpp::XPtr<Quaternion> ptr(self);
  Rcpp::XPtr<Vector3> ptr_f(v_from);
  Rcpp::XPtr<Vector3> ptr_t(v_to);
  ptr->setFromUnitVectors(*ptr_f, *ptr_t);
}




double Quaternion::angleTo(Quaternion& q) {

  return 2 * std::acos(std::abs(std::clamp(this->dot(q), -1.0, 1.0)));
}

// [[Rcpp::export]]
double Quaternion__angle_to(SEXP& self, SEXP& q) {
  Rcpp::XPtr<Quaternion> ptr(self);
  Rcpp::XPtr<Quaternion> ptr_q(q);
  return ptr->angleTo(*ptr_q);
}


Quaternion& Quaternion::rotateTowards(Quaternion& q, const double& step) {

  const double angle = this->angleTo(q);

  if (angle == 0) return *this;

  const double t = std::min(1.0, step / angle);

  this->slerp(q, t);

  return *this;
}

// [[Rcpp::export]]
void Quaternion__rotate_towards(SEXP& self, SEXP& q, const double& step) {
  Rcpp::XPtr<Quaternion> ptr(self);
  Rcpp::XPtr<Quaternion> ptr_q(q);
  ptr->rotateTowards(*ptr_q, step);
}

Quaternion& Quaternion::slerp(Quaternion& qb, const double& t) {

  if (t == 0) return *this;
  if (t == 1) return this->copy(qb);

  const double x = this->x, y = this->y, z = this->z, w = this->w;

  // http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/slerp/

  double cosHalfTheta = w * qb.w + x * qb.x + y * qb.y + z * qb.z;

  if (cosHalfTheta < 0) {

    this->w = -qb.w;
    this->x = -qb.x;
    this->y = -qb.y;
    this->z = -qb.z;

    cosHalfTheta = -cosHalfTheta;

  } else {

    this->copy(qb);
  }

  if (cosHalfTheta >= 1.0) {

    this->w = w;
    this->x = x;
    this->y = y;
    this->z = z;

    return *this;
  }

  const double sqrSinHalfTheta = 1.0 - cosHalfTheta * cosHalfTheta;

  if (sqrSinHalfTheta <= std::numeric_limits<double>::epsilon()) {

    const double s = 1 - t;
    this->w = s * w + t * this->w;
    this->x = s * x + t * this->x;
    this->y = s * y + t * this->y;
    this->z = s * z + t * this->z;

    this->normalize();

    return *this;
  }

  const double sinHalfTheta = std::sqrt(sqrSinHalfTheta);
  const double halfTheta = std::atan2(sinHalfTheta, cosHalfTheta);
  const double ratioA = std::sin((1 - t) * halfTheta) / sinHalfTheta,
    ratioB = std::sin(t * halfTheta) / sinHalfTheta;

  this->w = (w * ratioA + this->w * ratioB);
  this->x = (x * ratioA + this->x * ratioB);
  this->y = (y * ratioA + this->y * ratioB);
  this->z = (z * ratioA + this->z * ratioB);

  return *this;
}

// [[Rcpp::export]]
void Quaternion__slerp(SEXP& self, SEXP& qb, const double& t) {
  Rcpp::XPtr<Quaternion> ptr(self);
  Rcpp::XPtr<Quaternion> ptr_qb(qb);
  ptr->slerp(*ptr_qb, t);
}

Quaternion& Quaternion::identity() {

  return this->set(0, 0, 0, 1);
}

// [[Rcpp::export]]
void Quaternion__identity(SEXP& self) {
  Rcpp::XPtr<Quaternion> ptr(self);
  ptr->identity();
}

Quaternion& Quaternion::invert() {
  // Quaternion is assumed to have unit length
  return this->conjugate();
}

// [[Rcpp::export]]
void Quaternion__invert(SEXP& self) {
  Rcpp::XPtr<Quaternion> ptr(self);
  ptr->invert();
}

Quaternion& Quaternion::conjugate() {
  this->x *= -1;
  this->y *= -1;
  this->z *= -1;
  return *this;
}

// [[Rcpp::export]]
void Quaternion__conjugate(SEXP& self) {
  Rcpp::XPtr<Quaternion> ptr(self);
  ptr->conjugate();
}

double Quaternion::dot(Quaternion& v) {

  return this->x * v.x + this->y * v.y + this->z * v.z + this->w * v.w;
}

// [[Rcpp::export]]
double Quaternion__dot(SEXP& self, SEXP& v) {
  Rcpp::XPtr<Quaternion> ptr(self);
  Rcpp::XPtr<Quaternion> ptr_v(v);
  return ptr->dot(*ptr_v);
}

double Quaternion::lengthSq() {

  return this->x * this->x + this->y * this->y + this->z * this->z + this->w * this->w;
}

// [[Rcpp::export]]
double Quaternion__length_squared(SEXP& self) {
  Rcpp::XPtr<Quaternion> ptr(self);
  return ptr->lengthSq();
}


double Quaternion::length() {

  return std::sqrt(this->x * this->x + this->y * this->y + this->z * this->z + this->w * this->w);
}

// [[Rcpp::export]]
double Quaternion__length(SEXP& self) {
  Rcpp::XPtr<Quaternion> ptr(self);
  return ptr->length();
}

Quaternion& Quaternion::normalize() {

  double l = this->length();

  if (l == 0.0) {
    this->x = 0.0;
    this->y = 0.0;
    this->z = 0.0;
    this->w = 1.0;
  } else {
    l = 1.0 / l;
    this->x *= l;
    this->y *= l;
    this->z *= l;
    this->w *= l;
  }

  return *this;
}

// [[Rcpp::export]]
void Quaternion__normalize(SEXP& self) {
  Rcpp::XPtr<Quaternion> ptr(self);
  ptr->normalize();
}

Quaternion& Quaternion::multiply(Quaternion& q) {

  return this->multiplyQuaternions(*this, q);
}

// [[Rcpp::export]]
void Quaternion__multiply(SEXP& self, SEXP& q) {
  Rcpp::XPtr<Quaternion> ptr(self);
  Rcpp::XPtr<Quaternion> ptr_q(q);
  ptr->multiply(*ptr_q);
}

Quaternion& Quaternion::premultiply(Quaternion& q) {

  return this->multiplyQuaternions(q, *this);
}

// [[Rcpp::export]]
void Quaternion__premultiply(SEXP& self, SEXP& q) {
  Rcpp::XPtr<Quaternion> ptr(self);
  Rcpp::XPtr<Quaternion> ptr_q(q);
  ptr->premultiply(*ptr_q);
}

Quaternion& Quaternion::multiplyQuaternions(Quaternion& a, Quaternion& b) {

  // from http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/code/index.htm

  const double qax = a.x, qay = a.y, qaz = a.z, qaw = a.w;
  const double qbx = b.x, qby = b.y, qbz = b.z, qbw = b.w;

  this->x = qax * qbw + qaw * qbx + qay * qbz - qaz * qby;
  this->y = qay * qbw + qaw * qby + qaz * qbx - qax * qbz;
  this->z = qaz * qbw + qaw * qbz + qax * qby - qay * qbx;
  this->w = qaw * qbw - qax * qbx - qay * qby - qaz * qbz;

  return *this;
}

// [[Rcpp::export]]
void Quaternion__multiply_quaternions(SEXP& self, SEXP& a, SEXP& b) {
  Rcpp::XPtr<Quaternion> ptr(self);
  Rcpp::XPtr<Quaternion> ptr_a(a);
  Rcpp::XPtr<Quaternion> ptr_b(b);
  ptr->multiplyQuaternions(*ptr_a, *ptr_b);
}

Quaternion Quaternion::clone() const {
  const Quaternion q = Quaternion().set(this->x, this->y, this->z, this->w);
  return q;
}

bool Quaternion::equals(const Quaternion& v) const {
  return ((v.x == this->x) && (v.y == this->y) && (v.z == this->z) && (v.w == this->w));
}

// Quaternion& Quaternion::_onChange(std::function<void()> callback) {
//
//   this->onChangeCallback_ = std::move(callback);
//   this->x.setCallback(this->onChangeCallback_);
//   this->y.setCallback(this->onChangeCallback_);
//   this->z.setCallback(this->onChangeCallback_);
//   this->w.setCallback(this->onChangeCallback_);
//
//   return *this;
// }
//
// bool Quaternion::operator==(const Quaternion& other) const {
//
//   return equals(other);
// }
//
// bool Quaternion::operator!=(const Quaternion& other) const {
//
//   return !equals(other);
// }
