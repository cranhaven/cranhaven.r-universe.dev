// https://github.com/mrdoob/three.js/blob/r129/src/math/Quaternion.js

#ifndef RAVETOOLS_QUATERNION_H
#define RAVETOOLS_QUATERNION_H

#include <Rcpp.h>

// #include <functional>

namespace rave3d {

class Vector3;
class Matrix4;
// class Euler;

class Quaternion {

public:
  double x;
  double y;
  double z;
  double w;

  Quaternion();

  // double operator[](unsigned int index) const;

  Quaternion& set(const double& x, const double& y, const double& z, const double& w);

  Quaternion& copy(Quaternion& quaternion);

  // Quaternion& setFromEuler(const Euler& euler, bool update = true);
  //
  Quaternion& setFromAxisAngle(Vector3& axis, const double& angle);

  Quaternion& setFromRotationMatrix(Matrix4& m);

  Quaternion& setFromUnitVectors(Vector3& vFrom, Vector3& vTo);

  double angleTo(Quaternion& q);

  Quaternion& rotateTowards(Quaternion& q, const double& step);

  Quaternion& identity();

  Quaternion& invert();

  Quaternion& conjugate();

  double dot(Quaternion& v);

  double lengthSq();

  double length();

  Quaternion& normalize();

  Quaternion& multiply(Quaternion& q);

  Quaternion& premultiply(Quaternion& q);

  Quaternion& multiplyQuaternions(Quaternion& a, Quaternion& b);

  Quaternion& slerp(Quaternion& qb, const double& t);

  Quaternion clone() const;

  bool equals(const Quaternion& v) const;

  // bool operator==(const Quaternion& other) const;
  //
  // bool operator!=(const Quaternion& other) const;
  //
  // Quaternion& _onChange(std::function<void()> callback);
  //
  // template<class ArrayLike>
  // Quaternion& fromArray(const ArrayLike& array, unsigned int offset = 0) {
  //
  //   this->x.value_ = array[offset];
  //   this->y.value_ = array[offset + 1];
  //   this->z.value_ = array[offset + 2];
  //   this->w.value_ = array[offset + 3];
  //
  //   this->onChangeCallback_();
  //
  //   return *this;
  // }
  //
  std::vector<double> toArray();
  //
  // friend std::ostream& operator<<(std::ostream& os, const Quaternion& v) {
  //   os << "Quaternion(x=" << v.x << ", y=" << v.y << ", z=" << v.z << ", w=" << v.w << ")";
  //   return os;
  // }

// private:
//   std::function<void()> onChangeCallback_ = [] {};
};

} // namespace rave3d

#endif // RAVETOOLS_QUATERNION_H
