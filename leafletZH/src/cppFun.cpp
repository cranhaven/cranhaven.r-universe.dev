#include <Rcpp.h>
using namespace Rcpp;

// Helper functions
bool outofChina(double lat, double lon) {
  if (lon < 72.004 || lon > 137.8347) return true;
  if (lat < 0.8293 || lat > 55.8271) return true;
  return false;
}

double transformLat(double x, double y) {
  double ret = -100.0 + 2.0 * x + 3.0 * y + 0.2 * y * y + 0.1 * x * y + 0.2 * sqrt(fabs(x));
  ret += (20.0 * sin(6.0 * x * M_PI) + 20.0 * sin(2.0 * x * M_PI)) * 2.0 / 3.0;
  ret += (20.0 * sin(y * M_PI) + 40.0 * sin(y / 3.0 * M_PI)) * 2.0 / 3.0;
  ret += (160.0 * sin(y / 12.0 * M_PI) + 320.0 * sin(y * M_PI / 30.0)) * 2.0 / 3.0;
  return ret;
}

double transformLon(double x, double y) {
  double ret = 300.0 + x + 2.0 * y + 0.1 * x * x + 0.1 * x * y + 0.1 * sqrt(fabs(x));
  ret += (20.0 * sin(6.0 * x * M_PI) + 20.0 * sin(2.0 * x * M_PI)) * 2.0 / 3.0;
  ret += (20.0 * sin(x * M_PI) + 40.0 * sin(x / 3.0 * M_PI)) * 2.0 / 3.0;
  ret += (150.0 * sin(x / 12.0 * M_PI) + 300.0 * sin(x * M_PI / 30.0)) * 2.0 / 3.0;
  return ret;
}

NumericVector wgs2gcj(double wgsLat, double wgsLon) {
  const double a = 6378245.0; // Semi-major axis
  const double f = 0.00335233; // Flattening
  const double b = a * (1 - f); // Semi-minor axis
  double ee = (a * a - b * b) / (a * a);

  if (outofChina(wgsLat, wgsLon)) {
    return NumericVector::create(wgsLat, wgsLon);
  }

  double dLat = transformLat(wgsLon - 105.0, wgsLat - 35.0);
  double dLon = transformLon(wgsLon - 105.0, wgsLat - 35.0);
  double radLat = wgsLat / 180.0 * M_PI;
  double magic = sin(radLat);
  magic = 1 - ee * magic * magic;
  double sqrtMagic = sqrt(magic);
  dLat = (dLat * 180.0) / ((a * (1 - ee)) / (magic * sqrtMagic) * M_PI);
  dLon = (dLon * 180.0) / (a / sqrtMagic * cos(radLat) * M_PI);
  return NumericVector::create(wgsLat + dLat, wgsLon + dLon);
}

NumericVector gcj2wgs(double gcjLat, double gcjLon) {
  double g0[2] = {gcjLat, gcjLon};
  double w0[2] = {gcjLat, gcjLon};
  NumericVector g1 = wgs2gcj(w0[0], w0[1]);
  double w1[2] = {w0[0] - (g1[0] - g0[0]), w0[1] - (g1[1] - g0[1])};

  while (fabs(w1[0] - w0[0]) >= 1e-6 || fabs(w1[1] - w0[1]) >= 1e-6) {
    w0[0] = w1[0];
    w0[1] = w1[1];
    g1 = wgs2gcj(w0[0], w0[1]);
    w1[0] = w0[0] - (g1[0] - g0[0]);
    w1[1] = w0[1] - (g1[1] - g0[1]);
  }

  return NumericVector::create(w1[0], w1[1]);
}

NumericVector gcj2bd(double gcjLat, double gcjLon) {
  double z = sqrt(gcjLon * gcjLon + gcjLat * gcjLat) + 0.00002 * sin(gcjLat * M_PI * 3000.0 / 180.0);
  double theta = atan2(gcjLat, gcjLon) + 0.000003 * cos(gcjLon * M_PI * 3000.0 / 180.0);
  double bdLon = z * cos(theta) + 0.0065;
  double bdLat = z * sin(theta) + 0.006;
  return NumericVector::create(bdLat, bdLon);
}

NumericVector bd2gcj(double bdLat, double bdLon) {
  double x = bdLon - 0.0065;
  double y = bdLat - 0.006;
  double z = sqrt(x * x + y * y) - 0.00002 * sin(y * M_PI * 3000.0 / 180.0);
  double theta = atan2(y, x) - 0.000003 * cos(x * M_PI * 3000.0 / 180.0);
  double gcjLon = z * cos(theta);
  double gcjLat = z * sin(theta);
  return NumericVector::create(gcjLat, gcjLon);
}

NumericVector wgs2bd(double wgsLat, double wgsLon) {
  NumericVector gcj = wgs2gcj(wgsLat, wgsLon);
  return gcj2bd(gcj[0], gcj[1]);
}

NumericVector bd2wgs(double bdLat, double bdLon) {
  NumericVector gcj = bd2gcj(bdLat, bdLon);
  return gcj2wgs(gcj[0], gcj[1]);
}

// [[Rcpp::export]]
NumericVector convertCoordinates(double latitude, double longitude, std::string from, std::string to) {
  if (from == "WGS-84") {
    if (to == "GCJ-02") {
      return wgs2gcj(latitude, longitude);
    } else if (to == "BD-09") {
      return wgs2bd(latitude, longitude);
    }
  } else if (from == "GCJ-02") {
    if (to == "WGS-84") {
      return gcj2wgs(latitude, longitude);
    } else if (to == "BD-09") {
      return gcj2bd(latitude, longitude);
    }
  } else if (from == "BD-09") {
    if (to == "WGS-84") {
      return bd2wgs(latitude, longitude);
    } else if (to == "GCJ-02") {
      return bd2gcj(latitude, longitude);
    }
  }

  throw std::invalid_argument("Invalid conversion parameters");
}
