
#include "glMatrix4.h"
#include "glVector3.h"

// #include <algorithm>
// #include <cmath>
// #include <string>

using namespace rave3d;

// [[Rcpp::export]]
SEXP Matrix4__new() {
  Rcpp::XPtr<Matrix4> ptr( new Matrix4(), true );
  // return the external pointer to the R side
  return ptr;
}

double& Matrix4::operator[](unsigned int index) {

  if (index >= 16) {
    Rcpp::stop("index out of bounds: " + std::to_string(index));
  }
  return elements[index];
}

// [[Rcpp::export]]
SEXP Matrix4__to_array(const SEXP& self) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  SEXP re = PROTECT(Rf_allocVector(REALSXP, 16));
  SEXP dm = PROTECT(Rf_allocVector(INTSXP, 2));
  int* pdm = INTEGER(dm);
  pdm[0] = 4;
  pdm[1] = 4;
  std::copy(ptr_self->elements.begin(), ptr_self->elements.end(),
            REAL(re));
  Rf_setAttrib(re, R_DimSymbol, dm);
  UNPROTECT(2);
  return re;
}

Matrix4& Matrix4::set(
    const double& n11, const double& n12, const double& n13, const double& n14,
    const double& n21, const double& n22, const double& n23, const double& n24,
    const double& n31, const double& n32, const double& n33, const double& n34,
    const double& n41, const double& n42, const double& n43, const double& n44
) {
  std::vector<double>::iterator te = this->elements.begin();
  te[ 0 ] = n11; te[ 4 ] = n12; te[ 8 ] = n13; te[ 12 ] = n14;
  te[ 1 ] = n21; te[ 5 ] = n22; te[ 9 ] = n23; te[ 13 ] = n24;
  te[ 2 ] = n31; te[ 6 ] = n32; te[ 10 ] = n33; te[ 14 ] = n34;
  te[ 3 ] = n41; te[ 7 ] = n42; te[ 11 ] = n43; te[ 15 ] = n44;
  return *this;
}

Matrix4& Matrix4::fromArray(const std::vector<double>& array, size_t offset) {

  if( offset < 0 || array.size() < offset + 16 ) {
    Rcpp::stop("offset is out of bounds, not enough space in array");
  }

  std::copy(array.begin() + offset, array.begin() + offset + 16,
            this->elements.begin());

  return *this;
}

// [[Rcpp::export]]
void Matrix4__from_array(const SEXP& self, const std::vector<double>& array,
                                         size_t offset = 0) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  ptr_self->fromArray(array, offset);
}

Matrix4& Matrix4::identity() {
  this->set(
      1.0, 0.0, 0.0, 0.0,
      0.0, 1.0, 0.0, 0.0,
      0.0, 0.0, 1.0, 0.0,
      0.0, 0.0, 0.0, 1.0
  );
  return *this;
}

// [[Rcpp::export]]
void Matrix4__identity(const SEXP& self) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  ptr_self->identity();
}


Matrix4& Matrix4::copy(Matrix4& m) {

  std::vector<double>::iterator te = this->elements.begin();
  std::vector<double>::iterator me = m.elements.begin();

  // clang-format off
  te[ 0 ] = me[ 0 ]; te[ 1 ] = me[ 1 ]; te[ 2 ] = me[ 2 ]; te[ 3 ] = me[ 3 ];
  te[ 4 ] = me[ 4 ]; te[ 5 ] = me[ 5 ]; te[ 6 ] = me[ 6 ]; te[ 7 ] = me[ 7 ];
  te[ 8 ] = me[ 8 ]; te[ 9 ] = me[ 9 ]; te[ 10 ] = me[ 10 ]; te[ 11 ] = me[ 11 ];
  te[ 12 ] = me[ 12 ]; te[ 13 ] = me[ 13 ]; te[ 14 ] = me[ 14 ]; te[ 15 ] = me[ 15 ];
  // clang-format on

  return *this;
}

// [[Rcpp::export]]
void Matrix4__copy(const SEXP& self, const SEXP& m) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  Rcpp::XPtr<Matrix4> ptr_m(m);
  ptr_self->copy(*ptr_m);
}



Matrix4& Matrix4::copyPosition(Matrix4& m) {

  std::vector<double>::iterator te = this->elements.begin();
  std::vector<double>::iterator me = m.elements.begin();

  te[12] = me[12];
  te[13] = me[13];
  te[14] = me[14];

  return *this;
}

// [[Rcpp::export]]
void Matrix4__copy_position(const SEXP& self, const SEXP& m) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  Rcpp::XPtr<Matrix4> ptr_m(m);
  ptr_self->copyPosition(*ptr_m);
}


//
// Matrix4& Matrix4::setFromMatrix3(const Matrix3& m) {
//
//   const auto& me = m.elements;
//
//   this->set(
//
//       me[0], me[3], me[6], 0,
//       me[1], me[4], me[7], 0,
//       me[2], me[5], me[8], 0,
//       0, 0, 0, 1
//
//   );
//
//   return *this;
// }

Matrix4& Matrix4::extractBasis(Vector3& xAxis, Vector3& yAxis, Vector3& zAxis) {

  xAxis.setFromMatrixColumn(*this, 0);
  yAxis.setFromMatrixColumn(*this, 1);
  zAxis.setFromMatrixColumn(*this, 2);

  return *this;
}

// [[Rcpp::export]]
void Matrix4__extract_basis(const SEXP& self, const SEXP& x, const SEXP& y, const SEXP& z) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_x(x);
  Rcpp::XPtr<Vector3> ptr_y(y);
  Rcpp::XPtr<Vector3> ptr_z(z);
  ptr_self->extractBasis(*ptr_x, *ptr_y, *ptr_z);
}


Matrix4& Matrix4::makeBasis(const Vector3& xAxis, const Vector3& yAxis, const Vector3& zAxis) {

  if( xAxis.data.size() < 3 || yAxis.data.size() < 3 || zAxis.data.size() < 3 ) {
    Rcpp::stop("xAxis, yAxis, and zAxis must have non-zero size");
  }

  this->set(
      xAxis.data[0], yAxis.data[0], zAxis.data[0], 0,
      xAxis.data[1], yAxis.data[1], zAxis.data[1], 0,
      xAxis.data[2], yAxis.data[2], zAxis.data[2], 0,
      0, 0, 0, 1);

  return *this;
}

// [[Rcpp::export]]
void Matrix4__make_basis(const SEXP& self, const SEXP& x, const SEXP& y, const SEXP& z) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_x(x);
  Rcpp::XPtr<Vector3> ptr_y(y);
  Rcpp::XPtr<Vector3> ptr_z(z);
  ptr_self->makeBasis(*ptr_x, *ptr_y, *ptr_z);
}

Matrix4& Matrix4::extractRotation(Matrix4& m) {

  // this method does not support reflection matrices

  std::vector<double>::iterator te = this->elements.begin();
  std::vector<double>::iterator me = m.elements.begin();

  Vector3 _v1{};
  const double scaleX = 1.0 / _v1.setFromMatrixColumn(m, 0).length()[0];
  const double scaleY = 1.0 / _v1.setFromMatrixColumn(m, 1).length()[0];
  const double scaleZ = 1.0 / _v1.setFromMatrixColumn(m, 2).length()[0];

  te[0] = me[0] * scaleX;
  te[1] = me[1] * scaleX;
  te[2] = me[2] * scaleX;
  te[3] = 0;

  te[4] = me[4] * scaleY;
  te[5] = me[5] * scaleY;
  te[6] = me[6] * scaleY;
  te[7] = 0;

  te[8] = me[8] * scaleZ;
  te[9] = me[9] * scaleZ;
  te[10] = me[10] * scaleZ;
  te[11] = 0;

  te[12] = 0;
  te[13] = 0;
  te[14] = 0;
  te[15] = 1;

  return *this;
}

// [[Rcpp::export]]
void Matrix4__extract_rotation(const SEXP& self, const SEXP& m) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  Rcpp::XPtr<Matrix4> ptr_m(m);
  ptr_self->extractRotation(*ptr_m);
}

// Matrix4& Matrix4::makeRotationFromEuler(const Euler& ee) {
//
//   auto& te = this->elements;
//
//   const double x = ee.x(), y = ee.y(), z = ee.z();
//   const double a = std::cos(x), b = std::sin(x);
//   const double c = std::cos(y), d = std::sin(y);
//   const double e = std::cos(z), f = std::sin(z);
//
//   if (ee.getOrder() == Euler::XYZ) {
//
//     const auto ae = a * e, af = a * f, be = b * e, bf = b * f;
//
//     te[0] = c * e;
//     te[4] = -c * f;
//     te[8] = d;
//
//     te[1] = af + be * d;
//     te[5] = ae - bf * d;
//     te[9] = -b * c;
//
//     te[2] = bf - ae * d;
//     te[6] = be + af * d;
//     te[10] = a * c;
//
//   } else if (ee.getOrder() == Euler::YXZ) {
//
//     const auto ce = c * e, cf = c * f, de = d * e, df = d * f;
//
//     te[0] = ce + df * b;
//     te[4] = de * b - cf;
//     te[8] = a * d;
//
//     te[1] = a * f;
//     te[5] = a * e;
//     te[9] = -b;
//
//     te[2] = cf * b - de;
//     te[6] = df + ce * b;
//     te[10] = a * c;
//
//   } else if (ee.getOrder() == Euler::ZXY) {
//
//     const auto ce = c * e, cf = c * f, de = d * e, df = d * f;
//
//     te[0] = ce - df * b;
//     te[4] = -a * f;
//     te[8] = de + cf * b;
//
//     te[1] = cf + de * b;
//     te[5] = a * e;
//     te[9] = df - ce * b;
//
//     te[2] = -a * d;
//     te[6] = b;
//     te[10] = a * c;
//
//   } else if (ee.getOrder() == Euler::ZYX) {
//
//     const auto ae = a * e, af = a * f, be = b * e, bf = b * f;
//
//     te[0] = c * e;
//     te[4] = be * d - af;
//     te[8] = ae * d + bf;
//
//     te[1] = c * f;
//     te[5] = bf * d + ae;
//     te[9] = af * d - be;
//
//     te[2] = -d;
//     te[6] = b * c;
//     te[10] = a * c;
//
//   } else if (ee.getOrder() == Euler::YZX) {
//
//     const auto ac = a * c, ad = a * d, bc = b * c, bd = b * d;
//
//     te[0] = c * e;
//     te[4] = bd - ac * f;
//     te[8] = bc * f + ad;
//
//     te[1] = f;
//     te[5] = a * e;
//     te[9] = -b * e;
//
//     te[2] = -d * e;
//     te[6] = ad * f + bc;
//     te[10] = ac - bd * f;
//
//   } else if (ee.getOrder() == Euler::XZY) {
//
//     const auto ac = a * c, ad = a * d, bc = b * c, bd = b * d;
//
//     te[0] = c * e;
//     te[4] = -f;
//     te[8] = d * e;
//
//     te[1] = ac * f + bd;
//     te[5] = a * e;
//     te[9] = ad * f - bc;
//
//     te[2] = bc * f - ad;
//     te[6] = b * e;
//     te[10] = bd * f + ac;
//   }
//
//   // bottom row
//   te[3] = 0;
//   te[7] = 0;
//   te[11] = 0;
//
//   // last column
//   te[12] = 0;
//   te[13] = 0;
//   te[14] = 0;
//   te[15] = 1;
//
//   return *this;
// }
//
// Matrix4& Matrix4::makeRotationFromQuaternion(const Quaternion& q) {
//
//   return this->compose(Vector3::ZEROS(), q, Vector3::ONES());
// }
//
Matrix4& Matrix4::lookAt(Vector3& eye, Vector3& target, Vector3& up) {

  if( eye.getSize() < 1 || target.getSize() < 1 || up.getSize() < 1 ) {
    Rcpp::stop("C++ Matrix4::lookAt - eye, target, and up must have positive size.");
  }

  std::vector<double>::iterator te = this->elements.begin();

  Vector3 _x{};
  Vector3 _y{};
  Vector3 _z{};

  _z.copy(eye, 1).sub( _x.copy(target, 1) );

  if (_z.lengthSq()[0] == 0) {
    // eye and target are in the same position
    _z.setZ(1.0);
  }

  _z.normalize();

  _x.crossVectors(_y.copy(up, 1), _z);

  if (_x.lengthSq()[0] == 0) {

    // up and z are parallel
    if (std::abs(up.getZ(0)) == 1) {
      _z.setX(_z.getX(0) + 0.0001);
    } else {
      _z.setZ(_z.getZ(0) + 0.0001);
    }

    _z.normalize();
    _x.crossVectors(_y, _z);
  }

  _x.normalize();
  _y.crossVectors(_z, _x);

  // clang-format off
  te[ 0 ] = _x.getX(0); te[ 4 ] = _y.getX(0); te[ 8 ] = _z.getX(0);
  te[ 1 ] = _x.getY(0); te[ 5 ] = _y.getY(0); te[ 9 ] = _z.getY(0);
  te[ 2 ] = _x.getZ(0); te[ 6 ] = _y.getZ(0); te[ 10 ] = _z.getZ(0);
  // clang-format on

  return *this;
}

// [[Rcpp::export]]
void Matrix4__look_at(const SEXP& self, const SEXP& eye, const SEXP& target, const SEXP& up) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_eye(eye);
  Rcpp::XPtr<Vector3> ptr_target(target);
  Rcpp::XPtr<Vector3> ptr_up(up);
  ptr_self->lookAt(*ptr_eye, *ptr_target, *ptr_up);
}

Matrix4& Matrix4::multiply(Matrix4& m) {
  return this->multiplyMatrices(*this, m);
}

Matrix4& Matrix4::premultiply(Matrix4& m) {
  return this->multiplyMatrices(m, *this);
}

Matrix4& Matrix4::multiplyMatrices(Matrix4& a, Matrix4& b) {

  const std::vector<double>::iterator ae = a.elements.begin();
  const std::vector<double>::iterator be = b.elements.begin();
  std::vector<double>::iterator te = this->elements.begin();

  const double a11 = ae[0], a12 = ae[4], a13 = ae[8], a14 = ae[12];
  const double a21 = ae[1], a22 = ae[5], a23 = ae[9], a24 = ae[13];
  const double a31 = ae[2], a32 = ae[6], a33 = ae[10], a34 = ae[14];
  const double a41 = ae[3], a42 = ae[7], a43 = ae[11], a44 = ae[15];

  const double b11 = be[0], b12 = be[4], b13 = be[8], b14 = be[12];
  const double b21 = be[1], b22 = be[5], b23 = be[9], b24 = be[13];
  const double b31 = be[2], b32 = be[6], b33 = be[10], b34 = be[14];
  const double b41 = be[3], b42 = be[7], b43 = be[11], b44 = be[15];

  te[0] = a11 * b11 + a12 * b21 + a13 * b31 + a14 * b41;
  te[4] = a11 * b12 + a12 * b22 + a13 * b32 + a14 * b42;
  te[8] = a11 * b13 + a12 * b23 + a13 * b33 + a14 * b43;
  te[12] = a11 * b14 + a12 * b24 + a13 * b34 + a14 * b44;

  te[1] = a21 * b11 + a22 * b21 + a23 * b31 + a24 * b41;
  te[5] = a21 * b12 + a22 * b22 + a23 * b32 + a24 * b42;
  te[9] = a21 * b13 + a22 * b23 + a23 * b33 + a24 * b43;
  te[13] = a21 * b14 + a22 * b24 + a23 * b34 + a24 * b44;

  te[2] = a31 * b11 + a32 * b21 + a33 * b31 + a34 * b41;
  te[6] = a31 * b12 + a32 * b22 + a33 * b32 + a34 * b42;
  te[10] = a31 * b13 + a32 * b23 + a33 * b33 + a34 * b43;
  te[14] = a31 * b14 + a32 * b24 + a33 * b34 + a34 * b44;

  te[3] = a41 * b11 + a42 * b21 + a43 * b31 + a44 * b41;
  te[7] = a41 * b12 + a42 * b22 + a43 * b32 + a44 * b42;
  te[11] = a41 * b13 + a42 * b23 + a43 * b33 + a44 * b43;
  te[15] = a41 * b14 + a42 * b24 + a43 * b34 + a44 * b44;

  return *this;
}

// [[Rcpp::export]]
void Matrix4__multiply_matrices(const SEXP& self, const SEXP& a, const SEXP& b) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  Rcpp::XPtr<Matrix4> ptr_a(a);
  Rcpp::XPtr<Matrix4> ptr_b(b);
  ptr_self->multiplyMatrices(*ptr_a, *ptr_b);
}


Matrix4& Matrix4::multiplyScalar(const double& s) {

  std::transform(
    this->elements.begin(), this->elements.end(),
    this->elements.begin(),
    [s](double x) { return x * s; }
  );

  return *this;
}

// [[Rcpp::export]]
void Matrix4__multiply_scalar(const SEXP& self, const double& s) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  ptr_self->multiplyScalar(s);
}


double Matrix4::determinant() {

  const std::vector<double>::iterator te = this->elements.begin();

  const double &n11 = te[0], &n12 = te[4], &n13 = te[8], &n14 = te[12];
  const double &n21 = te[1], &n22 = te[5], &n23 = te[9], &n24 = te[13];
  const double &n31 = te[2], &n32 = te[6], &n33 = te[10], &n34 = te[14];
  const double &n41 = te[3], &n42 = te[7], &n43 = te[11], &n44 = te[15];

  return (
      n41 * (+n14 * n23 * n32 - n13 * n24 * n32 - n14 * n22 * n33 + n12 * n24 * n33 + n13 * n22 * n34 -
        n12 * n23 * n34) +
        n42 * (+n11 * n23 * n34 - n11 * n24 * n33 + n14 * n21 * n33 - n13 * n21 * n34 + n13 * n24 * n31 -
        n14 * n23 * n31) +
        n43 * (+n11 * n24 * n32 - n11 * n22 * n34 - n14 * n21 * n32 + n12 * n21 * n34 + n14 * n22 * n31 -
        n12 * n24 * n31) +
        n44 *
        (-n13 * n22 * n31 - n11 * n23 * n32 + n11 * n22 * n33 + n13 * n21 * n32 - n12 * n21 * n33 + n12 * n23 * n31)

  );
}

// [[Rcpp::export]]
double Matrix4__determinant(const SEXP& self) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  return ptr_self->determinant();
}


Matrix4& Matrix4::transpose() {

  const std::vector<double>::iterator te = this->elements.begin();
  double tmp;

  // clang-format off
  tmp = te[ 1 ]; te[ 1 ] = te[ 4 ]; te[ 4 ] = tmp;
  tmp = te[ 2 ]; te[ 2 ] = te[ 8 ]; te[ 8 ] = tmp;
  tmp = te[ 6 ]; te[ 6 ] = te[ 9 ]; te[ 9 ] = tmp;

  tmp = te[ 3 ]; te[ 3 ] = te[ 12 ]; te[ 12 ] = tmp;
  tmp = te[ 7 ]; te[ 7 ] = te[ 13 ]; te[ 13 ] = tmp;
  tmp = te[ 11 ]; te[ 11 ] = te[ 14 ]; te[ 14 ] = tmp;
  // clang-format on

  return *this;
}

// [[Rcpp::export]]
void Matrix4__transpose(const SEXP& self) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  ptr_self->transpose();
}


Matrix4& Matrix4::setPosition(Vector3& v) {

  this->setPosition(v.getX(0), v.getY(0), v.getZ(0));

  return *this;
}

Matrix4& Matrix4::setPosition(const double& x, const double& y, const double& z) {

  std::vector<double>::iterator te = this->elements.begin();

  te[12] = x;
  te[13] = y;
  te[14] = z;


  return *this;
}

// [[Rcpp::export]]
void Matrix4__set_position(const SEXP& self, const double& x, const double& y, const double& z) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  ptr_self->setPosition(x, y, z);
}


Matrix4& Matrix4::invert() {

  std::vector<double>::iterator te = this->elements.begin();

  const double n11 = te[0], n21 = te[1], n31 = te[2], n41 = te[3];
  const double n12 = te[4], n22 = te[5], n32 = te[6], n42 = te[7];
  const double n13 = te[8], n23 = te[9], n33 = te[10], n43 = te[11];
  const double n14 = te[12], n24 = te[13], n34 = te[14], n44 = te[15];
  const double t11 = n23 * n34 * n42 - n24 * n33 * n42 + n24 * n32 * n43 - n22 * n34 * n43 - n23 * n32 * n44 + n22 * n33 * n44;
  const double t12 = n14 * n33 * n42 - n13 * n34 * n42 - n14 * n32 * n43 + n12 * n34 * n43 + n13 * n32 * n44 - n12 * n33 * n44;
  const double t13 = n13 * n24 * n42 - n14 * n23 * n42 + n14 * n22 * n43 - n12 * n24 * n43 - n13 * n22 * n44 + n12 * n23 * n44;
  const double t14 = n14 * n23 * n32 - n13 * n24 * n32 - n14 * n22 * n33 + n12 * n24 * n33 + n13 * n22 * n34 - n12 * n23 * n34;

  const double det = n11 * t11 + n21 * t12 + n31 * t13 + n41 * t14;

  if (det == 0) return this->set(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

  const double detInv = 1.0 / det;

  te[0] = t11 * detInv;
  te[1] = (n24 * n33 * n41 - n23 * n34 * n41 - n24 * n31 * n43 + n21 * n34 * n43 + n23 * n31 * n44 - n21 * n33 * n44) * detInv;
  te[2] = (n22 * n34 * n41 - n24 * n32 * n41 + n24 * n31 * n42 - n21 * n34 * n42 - n22 * n31 * n44 + n21 * n32 * n44) * detInv;
  te[3] = (n23 * n32 * n41 - n22 * n33 * n41 - n23 * n31 * n42 + n21 * n33 * n42 + n22 * n31 * n43 - n21 * n32 * n43) * detInv;

  te[4] = t12 * detInv;
  te[5] = (n13 * n34 * n41 - n14 * n33 * n41 + n14 * n31 * n43 - n11 * n34 * n43 - n13 * n31 * n44 + n11 * n33 * n44) * detInv;
  te[6] = (n14 * n32 * n41 - n12 * n34 * n41 - n14 * n31 * n42 + n11 * n34 * n42 + n12 * n31 * n44 - n11 * n32 * n44) * detInv;
  te[7] = (n12 * n33 * n41 - n13 * n32 * n41 + n13 * n31 * n42 - n11 * n33 * n42 - n12 * n31 * n43 + n11 * n32 * n43) * detInv;

  te[8] = t13 * detInv;
  te[9] = (n14 * n23 * n41 - n13 * n24 * n41 - n14 * n21 * n43 + n11 * n24 * n43 + n13 * n21 * n44 - n11 * n23 * n44) * detInv;
  te[10] = (n12 * n24 * n41 - n14 * n22 * n41 + n14 * n21 * n42 - n11 * n24 * n42 - n12 * n21 * n44 + n11 * n22 * n44) * detInv;
  te[11] = (n13 * n22 * n41 - n12 * n23 * n41 - n13 * n21 * n42 + n11 * n23 * n42 + n12 * n21 * n43 - n11 * n22 * n43) * detInv;

  te[12] = t14 * detInv;
  te[13] = (n13 * n24 * n31 - n14 * n23 * n31 + n14 * n21 * n33 - n11 * n24 * n33 - n13 * n21 * n34 + n11 * n23 * n34) * detInv;
  te[14] = (n14 * n22 * n31 - n12 * n24 * n31 - n14 * n21 * n32 + n11 * n24 * n32 + n12 * n21 * n34 - n11 * n22 * n34) * detInv;
  te[15] = (n12 * n23 * n31 - n13 * n22 * n31 + n13 * n21 * n32 - n11 * n23 * n32 - n12 * n21 * n33 + n11 * n22 * n33) * detInv;

  return *this;
}

// [[Rcpp::export]]
void Matrix4__invert(const SEXP& self) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  ptr_self->invert();
}



Matrix4& Matrix4::scale(Vector3& v) {

  std::vector<double>::iterator te = this->elements.begin();
  const double &x = v.getX(0), &y = v.getY(0), &z = v.getZ(0);

  // clang-format off
  te[ 0 ] *= x; te[ 4 ] *= y; te[ 8 ] *= z;
  te[ 1 ] *= x; te[ 5 ] *= y; te[ 9 ] *= z;
  te[ 2 ] *= x; te[ 6 ] *= y; te[ 10 ] *= z;
  te[ 3 ] *= x; te[ 7 ] *= y; te[ 11 ] *= z;
  // clang-format on

  return *this;
}

// [[Rcpp::export]]
void Matrix4__scale(const SEXP& self, const SEXP& v) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_v(v);
  ptr_self->scale(*ptr_v);
}


double Matrix4::getMaxScaleOnAxis() {

  std::vector<double>::iterator te = this->elements.begin();

  const double scaleXSq = te[0] * te[0] + te[1] * te[1] + te[2] * te[2];
  const double scaleYSq = te[4] * te[4] + te[5] * te[5] + te[6] * te[6];
  const double scaleZSq = te[8] * te[8] + te[9] * te[9] + te[10] * te[10];

  return std::sqrt(std::max(scaleXSq, std::max(scaleYSq, scaleZSq)));
}

// [[Rcpp::export]]
double Matrix4__get_max_scale_on_axis(const SEXP& self) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  return ptr_self->getMaxScaleOnAxis();
}

Matrix4& Matrix4::makeTranslation(const double& x, const double& y, const double& z) {
  this->set(
      1, 0, 0, x,
      0, 1, 0, y,
      0, 0, 1, z,
      0, 0, 0, 1
  );
  return *this;
}

Matrix4& Matrix4::makeTranslation(Vector3& v) {
  return makeTranslation(v.getX(0), v.getY(0), v.getZ(0));
}

// [[Rcpp::export]]
void Matrix4__make_translation(const SEXP& self, const double& x, const double& y, const double& z) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  ptr_self->makeTranslation(x, y, z);
}

Matrix4& Matrix4::makeRotationX(const double& theta) {

  const double c = std::cos(theta), s = std::sin(theta);

  this->set(

      1, 0, 0, 0,
      0, c, -s, 0,
      0, s, c, 0,
      0, 0, 0, 1

  );

  return *this;
}

// [[Rcpp::export]]
void Matrix4__make_rotation_x(const SEXP& self, const double& theta) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  ptr_self->makeRotationX(theta);
}

Matrix4& Matrix4::makeRotationY(const double& theta) {

  const double c = std::cos(theta), s = std::sin(theta);

  this->set(

      c, 0, s, 0,
      0, 1, 0, 0,
      -s, 0, c, 0,
      0, 0, 0, 1

  );

  return *this;
}

// [[Rcpp::export]]
void Matrix4__make_rotation_y(const SEXP& self, const double& theta) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  ptr_self->makeRotationY(theta);
}

Matrix4& Matrix4::makeRotationZ(const double& theta) {

  const double c = std::cos(theta), s = std::sin(theta);

  this->set(

      c, -s, 0, 0,
      s, c, 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1

  );

  return *this;
}

// [[Rcpp::export]]
void Matrix4__make_rotation_z(const SEXP& self, const double& theta) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  ptr_self->makeRotationZ(theta);
}

Matrix4& Matrix4::makeRotationAxis(Vector3& axis, const double& angle) {

  // Based on http://www.gamedev.net/reference/articles/article1199.asp

  const double c = std::cos(angle);
  const double s = std::sin(angle);
  const double t = 1 - c;
  const double x = axis.getX(0), y = axis.getY(0), z = axis.getZ(0);
  const double tx = t * x, ty = t * y;

  this->set(
      tx * x + c, tx * y - s * z, tx * z + s * y, 0,
      tx * y + s * z, ty * y + c, ty * z - s * x, 0,
      tx * z - s * y, ty * z + s * x, t * z * z + c, 0,
      0, 0, 0, 1
  );

  return *this;
}

// [[Rcpp::export]]
void Matrix4__make_rotation_axis(const SEXP& self, const SEXP& axis, const double& angle) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  Rcpp::XPtr<Vector3> ptr_axis(axis);
  ptr_self->makeRotationAxis(*ptr_axis, angle);
}

Matrix4& Matrix4::makeScale(const double& x, const double& y, const double& z) {
  this->set(
      x, 0, 0, 0,
      0, y, 0, 0,
      0, 0, z, 0,
      0, 0, 0, 1
  );
  return *this;
}

// [[Rcpp::export]]
void Matrix4__make_scale(const SEXP& self, const double& x, const double& y, const double& z) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  ptr_self->makeScale(x, y, z);
}

Matrix4& Matrix4::makeShear(const double& xy, const double& xz, const double& yx,
                            const double& yz, const double& zx, const double& zy) {
  this->set(
      1, yx, zx, 0,
      xy, 1, zy, 0,
      xz, yz, 1, 0,
      0, 0, 0, 1
  );
  return *this;
}

// [[Rcpp::export]]
void Matrix4__make_shear(const SEXP& self, const double& xy, const double& xz, const double& yx,
                         const double& yz, const double& zx, const double& zy) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  ptr_self->makeShear(xy, xz, yx, yz, zx, zy);
}

//
// Matrix4& Matrix4::compose(const Vector3& position, const Quaternion& quaternion, const Vector3& scale) {
//
//   auto& te = this->elements;
//
//   const double x = quaternion.x(), y = quaternion.y(), z = quaternion.z(), w = quaternion.w();
//   const double x2 = x + x, y2 = y + y, z2 = z + z;
//   const double xx = x * x2, xy = x * y2, xz = x * z2;
//   const double yy = y * y2, yz = y * z2, zz = z * z2;
//   const double wx = w * x2, wy = w * y2, wz = w * z2;
//
//   const double sx = scale.x, sy = scale.y, sz = scale.z;
//
//   te[0] = (1 - (yy + zz)) * sx;
//   te[1] = (xy + wz) * sx;
//   te[2] = (xz - wy) * sx;
//   te[3] = 0;
//
//   te[4] = (xy - wz) * sy;
//   te[5] = (1 - (xx + zz)) * sy;
//   te[6] = (yz + wx) * sy;
//   te[7] = 0;
//
//   te[8] = (xz + wy) * sz;
//   te[9] = (yz - wx) * sz;
//   te[10] = (1 - (xx + yy)) * sz;
//   te[11] = 0;
//
//   te[12] = position.x;
//   te[13] = position.y;
//   te[14] = position.z;
//   te[15] = 1;
//
//   return *this;
// }
//
// Matrix4& Matrix4::decompose(Vector3& position, Quaternion& quaternion, Vector3& scale) {
//
//   const auto& te = this->elements;
//
//   Vector3 _v1{};
//   Matrix4 _m1{};
//
//   double sx = _v1.set(te[0], te[1], te[2]).length();
//   const double sy = _v1.set(te[4], te[5], te[6]).length();
//   const double sz = _v1.set(te[8], te[9], te[10]).length();
//
//   // if determine is negative, we need to invert one scale
//   const double det = this->determinant();
//   if (det < 0) sx = -sx;
//
//   position.x = te[12];
//   position.y = te[13];
//   position.z = te[14];
//
//   // scale the rotation part
//   _m1.copy(*this);
//
//   const double invSX = 1.0f / sx;
//   const double invSY = 1.0f / sy;
//   const double invSZ = 1.0f / sz;
//
//   _m1.elements[0] *= invSX;
//   _m1.elements[1] *= invSX;
//   _m1.elements[2] *= invSX;
//
//   _m1.elements[4] *= invSY;
//   _m1.elements[5] *= invSY;
//   _m1.elements[6] *= invSY;
//
//   _m1.elements[8] *= invSZ;
//   _m1.elements[9] *= invSZ;
//   _m1.elements[10] *= invSZ;
//
//   quaternion.setFromRotationMatrix(_m1);
//
//   scale.x = sx;
//   scale.y = sy;
//   scale.z = sz;
//
//   return *this;
// }
//
Matrix4& Matrix4::makePerspective(const double& left, const double& right, const double& top,
                                  const double& bottom, const double& near, const double& far) {

  std::vector<double>::iterator te = this->elements.begin();
  const double &x = 2 * near / (right - left);
  const double &y = 2 * near / (top - bottom);

  const double &a = (right + left) / (right - left);
  const double &b = (top + bottom) / (top - bottom);
  const double &c = -(far + near) / (far - near);
  const double &d = -2 * far * near / (far - near);

  // clang-format off
  te[ 0 ] = x;	te[ 4 ] = 0;	te[ 8 ] = a;	te[ 12 ] = 0;
  te[ 1 ] = 0;	te[ 5 ] = y;	te[ 9 ] = b;	te[ 13 ] = 0;
  te[ 2 ] = 0;	te[ 6 ] = 0;	te[ 10 ] = c;	te[ 14 ] = d;
  te[ 3 ] = 0;	te[ 7 ] = 0;	te[ 11 ] = - 1;	te[ 15 ] = 0;
  // clang-format on

  return *this;
}

// [[Rcpp::export]]
void Matrix4__make_perspective(const SEXP &self, const double& left, const double& right, const double& top,
                              const double& bottom, const double& near, const double& far) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  ptr_self->makePerspective(left, right, top, bottom, near, far);
}

Matrix4& Matrix4::makeOrthographic(const double& left, const double& right, const double& top,
                                   const double& bottom, const double& near, const double& far) {

  std::vector<double>::iterator te = this->elements.begin();
  const double &w = 1.0f / (right - left);
  const double &h = 1.0f / (top - bottom);
  const double &p = 1.0f / (far - near);

  const double &x = (right + left) * w;
  const double &y = (top + bottom) * h;
  const double &z = (far + near) * p;

  // clang-format off
  te[ 0 ] = 2 * w;	te[ 4 ] = 0;	    te[ 8 ] = 0;	    te[ 12 ] = - x;
  te[ 1 ] = 0;	    te[ 5 ] = 2 * h;	te[ 9 ] = 0;	    te[ 13 ] = - y;
  te[ 2 ] = 0;	    te[ 6 ] = 0;	    te[ 10 ] = - 2 * p;	te[ 14 ] = - z;
  te[ 3 ] = 0;	    te[ 7 ] = 0;	    te[ 11 ] = 0;	    te[ 15 ] = 1;
  // clang-format on

  return *this;
}

// [[Rcpp::export]]
void Matrix4__make_orthographic(const SEXP &self, const double& left, const double& right, const double& top,
                                const double& bottom, const double& near, const double& far) {
  Rcpp::XPtr<Matrix4> ptr_self(self);
  ptr_self->makeOrthographic(left, right, top, bottom, near, far);
}

//
// bool Matrix4::equals(const Matrix4& matrix) const {
//
//   const auto& te = this->elements;
//   const auto& me = matrix.elements;
//
//   for (int i = 0; i < 16; i++) {
//
//     if (te[i] != me[i]) return false;
//   }
//
//   return true;
// }
//
// bool Matrix4::operator==(const Matrix4& matrix) const {
//
//   return equals(matrix);
// }
//
// bool Matrix4::operator!=(const Matrix4& matrix) const {
//
//   return !equals(matrix);
// }
