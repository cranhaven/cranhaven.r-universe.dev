#ifndef RAVETOOLS_VECTOR3_H
#define RAVETOOLS_VECTOR3_H

#include <Rcpp.h>

namespace rave3d {

// class Euler;
// class Matrix3;
class Matrix4;
// class Spherical;
class Quaternion;
// class Camera;

class Vector3 {

public:
  std::vector<double> data;

  Vector3();
  ~Vector3();

  Vector3& fromArray(std::vector<double>& data, const int& offset = 0, const int& nElements = -1);
  Vector3& resize(const size_t& nElements);

  size_t getSize();

  // Set the x, y and z values of this vector both equal to scalar.
  Vector3& setScalar(double value);

  Vector3& setX(double value);
  Vector3& setY(double value);
  Vector3& setZ(double value);

  double getX(const size_t& nSkip);
  double getY(const size_t& nSkip);
  double getZ(const size_t& nSkip);
  SEXP getItem(const size_t& nSkip);

  SEXP toArray(const int& nSkip = 0, const int& maxElements = -1);

  Vector3& copy(Vector3& v, const size_t& nElem);

  // Adds v to this vector, if v.getSize() is shorter, then sub-elements will be added
  Vector3& add(Vector3& v);

  // Adds the scalar value s to this vector's x, y and z values.
  Vector3& addScalar(std::vector<double>& s);

  // Sets this vector to a + b.
  Vector3& addVectors(Vector3& a, Vector3& b);

  // Adds the multiple of v and s to this vector.
  Vector3& addScaledVector(Vector3& v, double s);

  // Subtracts v from this vector.
  Vector3& sub(Vector3& v);

  // Subtracts s from this vector's x, y and z components.
  Vector3& subScalar(std::vector<double>& s);

  // Sets this vector to a - b.
  Vector3& subVectors(Vector3& a, Vector3& b);

  // Multiplies this vector by v.
  Vector3& multiply(Vector3& v);

  // Multiplies this vector by scalar s.
  Vector3& multiplyScalar(std::vector<double>&);

  // Sets this vector equal to a * b, component-wise.
  Vector3& multiplyVectors(Vector3& a, Vector3& b);

  // Applies a rotation specified by an axis and an angle to this vector.
  Vector3& applyAxisAngle(Vector3& axis, const double& angle);

  // // Applies euler transform to this vector by converting the Euler object to a Quaternion and applying.
  // Vector3& applyEuler(const Euler& euler);

  // Multiplies this vector by m
  Vector3& applyMatrix3(const std::vector<double>& m);

  // // Multiplies this vector by normal matrix m and normalizes the result.
  // Vector3& applyNormalMatrix(const Matrix3& m);

  // Multiplies this vector (with an implicit 1 in the 4th dimension) by m, and divides by perspective.
  Vector3& applyMatrix4(const std::vector<double>& m);
  Vector3& applyMatrix4(Matrix4& m);

  // Applies a Quaternion transform to this vector.
  Vector3& applyQuaternion(const Quaternion& q);
  Vector3& applyQuaternion(const std::vector<double>& q);

  // // Projects this vector from world space into the camera's normalized device coordinate (NDC) space.
  // Vector3& project(const Camera& camera);
  //
  // // Projects this vector from the camera's normalized device coordinate (NDC) space into world space.
  // Vector3& unproject(const Camera& camera);

  // Transforms the direction of this vector by a matrix (the upper left 3 x 3 subset of a m) and then normalizes the result.
  Vector3& transformDirection(Matrix4& m);

  // Divides this vector by v.
  Vector3& divide(Vector3& v);

  // Divides this vector by scalar s.
  Vector3& divideScalar(std::vector<double>&);

  Vector3& min(Vector3& v);

  Vector3& max(Vector3& v);

  // If this vector's x, y or z value is greater than the max vector's x, y or z value, it is replaced by the corresponding value.
  //If this vector's x, y or z value is less than the min vector's x, y or z value, it is replaced by the corresponding value.
  Vector3& clamp(Vector3& min, Vector3& max);

  // The components of this vector are rounded down to the nearest integer value.
  Vector3& floor();

  // The x, y and z components of this vector are rounded up to the nearest integer value.
  Vector3& ceil();

  Vector3& round();

  Vector3& roundToZero();

  // Inverts this vector - i.e. sets x = -x, y = -y and z = -z.
  Vector3& negate();

  // Calculate the dot product of this vector and v.
  std::vector<double> dot(Vector3& v);

  // Computes the square of the Euclidean length (straight-line length) from (0, 0, 0) to (x, y, z). If you are comparing the lengths of vectors,
  // you should compare the length squared instead as it is slightly more efficient to calculate.
  std::vector<double> lengthSq();

  // Computes the Euclidean length (straight-line length) from (0, 0, 0) to (x, y, z).
  std::vector<double> length();

  // Computes the Manhattan length of this vector.
  std::vector<double> manhattanLength();

  // Convert this vector to a unit vector - that is, sets it equal to a vector with the same direction as this one, but length 1.
  Vector3& normalize();

  // Set this vector to a vector with the same direction as this one, but length l.
  Vector3& setLength(std::vector<double>& length);

  // Linearly interpolate between this vector and v, where alpha is the percent distance along the line - alpha = 0 will be this vector, and alpha = 1 will be v.
  Vector3& lerp(Vector3& v, std::vector<double>& alpha);

  // Sets this vector to be the vector linearly interpolated between v1 and v2 where alpha is the percent
  // distance along the line connecting the two vectors - alpha = 0 will be v1, and alpha = 1 will be v2.
  Vector3& lerpVectors(Vector3& v1, Vector3& v2, std::vector<double>& alpha);

  // Sets this vector to cross product of itself and v.
  Vector3& cross(Vector3& v);

  // Sets this vector to cross product of a and b.
  Vector3& crossVectors(Vector3& a, Vector3& b);

  Vector3& projectOnVector(Vector3& v);

  Vector3& projectOnPlane(Vector3& planeNormal);

  Vector3& reflect(Vector3& normal);

  // Returns the angle between this vector and vector v in radians.
  std::vector<double> angleTo(Vector3& v);

  // Computes the distance from this vector to v.
  std::vector<double> distanceTo(Vector3& v);

  // Computes the squared distance from this vector to v. If you are just comparing the distance with another distance,
  // you should compare the distance squared instead as it is slightly more efficient to calculate.
  std::vector<double> distanceToSquared(Vector3& v);

  // Computes the Manhattan distance from this vector to v.
  std::vector<double> manhattanDistanceTo(Vector3& v);

  // // Sets this vector from the spherical coordinates s.
  // Vector3& setFromSpherical(const Spherical& s);

  // Sets this vector from the spherical coordinates radius, phi and theta.
  Vector3& setFromSphericalCoords(const double& radius, const double& phi, const double& theta);

  // Sets this vector to the position elements of the transformation matrix m.
  Vector3& setFromMatrixPosition(Matrix4& m);

  // Sets this vector to the scale elements of the transformation matrix m.
  Vector3& setFromMatrixScale(Matrix4& m);

  // Sets this vector's x, y and z components from index column of matrix.
  Vector3& setFromMatrixColumn(Matrix4& m, unsigned int index);

  // // Sets this vector's x, y and z components from index column of matrix.
  // Vector3& setFromMatrix3Column(const Matrix3& m, unsigned int index);

  Vector3 clone();

  // [[nodiscard]] bool equals(const Vector3& v) const;
  //
  // bool operator==(const Vector3& other) const;
  //
  // bool operator!=(const Vector3& other) const;
  //
  // Vector3 operator+(const Vector3& other) const;
  //
  // Vector3& operator+=(const Vector3& other);
  //
  // Vector3 operator+(double s) const;
  //
  // Vector3& operator+=(double s);
  //
  // Vector3 operator-(const Vector3& other) const;
  //
  // Vector3& operator-=(const Vector3& other);
  //
  // Vector3 operator-(double s) const;
  //
  // Vector3& operator-=(double s);
  //
  // Vector3 operator*(const Vector3& other) const;
  //
  // Vector3& operator*=(const Vector3& other);
  //
  // Vector3 operator*(double s) const;
  //
  // Vector3& operator*=(double s);
  //
  // Vector3 operator/(const Vector3& other) const;
  //
  // Vector3& operator/=(const Vector3& other);
  //
  // Vector3 operator/(double s) const;
  //
  // Vector3& operator/=(double s);
  //
  // Vector3& makeNan();
  //
  // [[nodiscard]] bool isNan() const;
  //
  // template<class ArrayLike>
  // Vector3& fromArray(const ArrayLike& array, size_t offset = 0) {
  //
  //   this->x = array[offset];
  //   this->y = array[offset + 1];
  //   this->z = array[offset + 2];
  //
  //   return *this;
  // }
  //
  //
  // friend std::ostream& operator<<(std::ostream& os, const Vector3& v) {
  //   os << "Vector3(x=" << v.x << ", y=" << v.y << ", z=" << v.z << ")";
  //   return os;
  // }
  //
  // inline static Vector3 X() {
  //   return {1, 0, 0};
  // }
  //
  // inline static Vector3 Y() {
  //   return {0, 1, 0};
  // }
  //
  // inline static Vector3 Z() {
  //   return {0, 0, 1};
  // }
  //
  // inline static Vector3 ZEROS() {
  //   return {0, 0, 0};
  // }
  //
  // inline static Vector3 ONES() {
  //   return {1, 1, 1};
  // }
};


} // namespace rave3d

#endif // RAVETOOLS_VECTOR3_H
