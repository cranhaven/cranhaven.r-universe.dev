#ifndef RAVETOOLS_MATRIX4_H
#define RAVETOOLS_MATRIX4_H

#include <Rcpp.h>

namespace rave3d {

class Vector3;
// class Euler;
// class Quaternion;
// class Matrix3;

// A class representing a 4x4 matrix.
class Matrix4 {

public:
  // column-major!
  std::vector<double> elements{
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0};

  static const unsigned int nrows = 4;

  Matrix4() = default;

  double& operator[](unsigned int index);

  // Set the elements of this matrix to the supplied row-major values n11, n12, ... n44.
  Matrix4& set(const double& n11, const double& n12, const double& n13, const double& n14,
               const double& n21, const double& n22, const double& n23, const double& n24,
               const double& n31, const double& n32, const double& n33, const double& n34,
               const double& n41 = 0.0, const double& n42 = 0.0, const double& n43 = 0.0, const double& n44 = 1.0);

  // Resets this matrix to the identity matrix.
  Matrix4& identity();

  // Copies the elements of matrix m into this matrix.
  Matrix4& copy(Matrix4& m);

  // Copies the translation component of the supplied matrix m into this matrix's translation component.
  Matrix4& copyPosition(Matrix4& m);

  // // Set the upper 3x3 elements of this matrix to the values of the Matrix3 m.
  // Matrix4& setFromMatrix3(const Matrix3& m);

  Matrix4& extractBasis(Vector3& xAxis, Vector3& yAxis, Vector3& zAxis);

  Matrix4& makeBasis(const Vector3& xAxis, const Vector3& yAxis, const Vector3& zAxis);

  Matrix4& extractRotation(Matrix4& m);

  // // Matrix4& makeRotationFromEuler(const Euler& e);
  // //
  // // Matrix4& makeRotationFromQuaternion(const Quaternion& q);

  // Constructs a rotation matrix, looking from eye towards target oriented by the up vector.
  Matrix4& lookAt(Vector3& eye, Vector3& target, Vector3& up);

  // Post-multiplies this matrix by m.
  Matrix4& multiply(Matrix4& m);

  // Pre-multiplies this matrix by m.
  Matrix4& premultiply(Matrix4& m);

  // Sets this matrix to a x b.
  Matrix4& multiplyMatrices(Matrix4& a, Matrix4& b);

  // Multiplies every component of the matrix by a scalar value s.
  Matrix4& multiplyScalar(const double& s);

  // Computes and returns the determinant of this matrix.
  double determinant();

  Matrix4& transpose();

  // Sets the position component for this matrix from vector v, without affecting the rest of the matrix.
  Matrix4& setPosition(Vector3& v);

  // Sets the position component for this matrix from x, y, z, without affecting the rest of the matrix.
  Matrix4& setPosition(const double& x, const double& y, const double& z);

  // Inverts this matrix, using the analytic method. You can not invert with a determinant of zero. If you attempt this, the method produces a zero matrix instead.
  Matrix4& invert();

  // Multiplies the columns of this matrix by vector v.
  Matrix4& scale(Vector3& v);

  // Gets the maximum scale value of the 3 axes.
  double getMaxScaleOnAxis();

  // Sets this matrix as a translation transform from the numbers x, y and z,
  Matrix4& makeTranslation(const double& x, const double& y, const double& z);

  // Sets this matrix as a translation transform from vector the v,
  Matrix4& makeTranslation(Vector3& v);

  // Sets this matrix as a rotational transformation around the X axis by theta (θ) radians.
  Matrix4& makeRotationX(const double& theta);

  // Sets this matrix as a rotational transformation around the Y axis by theta (θ) radians.
  Matrix4& makeRotationY(const double& theta);

  // Sets this matrix as a rotational transformation around the Z axis by theta (θ) radians.
  Matrix4& makeRotationZ(const double& theta);

  Matrix4& makeRotationAxis(Vector3& axis, const double& angle);

  Matrix4& makeScale(const double& x, const double& y, const double& z);

  Matrix4& makeShear(const double& xy, const double& xz, const double& yx, const double& yz, const double& zx, const double& zy);

  // // Sets this matrix to the transformation composed of position, quaternion and scale.
  // Matrix4& compose(const Vector3& position, const Quaternion& quaternion, const Vector3& scale);
  //
  // Matrix4& decompose(Vector3& position, Quaternion& quaternion, Vector3& scale);

  Matrix4& makePerspective(const double& left, const double& right, const double& top, const double& bottom, const double& near, const double& far);

  Matrix4& makeOrthographic(const double& left, const double& right, const double& top, const double& bottom, const double& near, const double& far);

  Matrix4& fromArray(const std::vector<double>& array, size_t offset = 0);

};

}// namespace rave3d

#endif //RAVETOOLS_MATRIX4_H
