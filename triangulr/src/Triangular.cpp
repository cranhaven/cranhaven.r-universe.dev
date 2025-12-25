#include "cpp11.hpp"
#include "Rmath.h"

namespace writable = cpp11::writable;
using cpp11::doubles;
using cpp11::warning;

double dtri_cpp_internal(
    double x, double min, double max, double mode, bool is_log)
{
  double d;
  if (x < min || x > max)
  {
    d = 0.0;
  }
  else if (min <= x && x < mode)
  {
    d = 2.0 * (x - min) / ((max - min) * (mode - min));
  }
  else if (x == mode)
  {
    d = 2.0 / (max - min);
  }
  else // if (mode < x && x <= max)
  {
    d = 2.0 * (max - x) / ((max - min) * (max - mode));
  }

  return is_log ? log(d) : d;
}

[[cpp11::register]]
doubles dtri_cpp(
    doubles x, doubles min, doubles max, doubles mode, bool is_log,
    bool is_scalar)
{
  int n = x.size();
  writable::doubles d(n);

  if (is_scalar)
  {
    if (min[0] >= max[0] || mode[0] > max[0] || min[0] > mode[0])
    {
      for (int i = 0; i < n; i++)
      {
        d[i] = NA_REAL;
      }

      warning("NaN(s) produced.");

      return d;
    }

    for (int i = 0; i < n; i++)
    {
      d[i] = dtri_cpp_internal(x[i], min[0], max[0], mode[0], is_log);
    }
  }
  else
  {
    bool has_nan = false;

    for (int i = 0; i < n; i++)
    {
      if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
      {
        d[i] = NA_REAL;
        has_nan = true;
      }
      else
      {
        d[i] = dtri_cpp_internal(x[i], min[i], max[i], mode[i], is_log);
      }
    }

    if (has_nan) warning("NaN(s) produced.");
  }

  return d;
}

double ptri_cpp_internal(
    double q, double min, double max, double mode, bool is_lower_tail,
    bool is_log_p)
{
  double p;

  if (q <= min)
  {
    p = 0.0;
  }
  else if (min < q && q <= mode)
  {
    p = pow((q - min), 2) / ((max - min) * (mode - min));
  }
  else if (mode < q && q < max)
  {
    p = 1.0 - pow((max - q), 2) / ((max - min) * (max - mode));
  }
  else // if (max <= q)
  {
    p = 1.0;
  }

  if (!is_lower_tail) p = 1.0 - p;

  return is_log_p ? log(p) : p;
}

[[cpp11::register]]
doubles ptri_cpp(
    doubles q, doubles min, doubles max, doubles mode, bool is_lower_tail,
    bool is_log_p, bool is_scalar)
{
  int n = q.size();
  writable::doubles p(n);

  if (is_scalar)
  {
    if (min[0] >= max[0] || mode[0] > max[0] || min[0] > mode[0])
    {
      for (int i = 0; i < n; i++)
      {
        p[i] = NA_REAL;
      }

      warning("NaN(s) produced.");

      return p;
    }

    for (int i = 0; i < n; i++)
    {
      p[i] = ptri_cpp_internal(
        q[i], min[0], max[0], mode[0], is_lower_tail, is_log_p);
    }
  }
  else
  {
    bool has_nan = false;

    for (int i = 0; i < n; i++)
    {
      if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
      {
        p[i] = NA_REAL;
        has_nan = true;
      }
      else
      {
        p[i] = ptri_cpp_internal(
          q[i], min[i], max[i], mode[i], is_lower_tail, is_log_p);
      }
    }

    if (has_nan) warning("NaN(s) produced.");
  }

  return p;
}

double qtri_cpp_internal(
    double p, double min, double max, double mode, bool is_lower_tail,
    bool is_log_p, double int_len, bool &has_nan)
{
  double q = is_log_p ? exp(p) : p;

  if (!is_lower_tail) q = 1.0 - q;

  if (q < 0.0 || q > 1.0)
  {
    has_nan = true;
    return NA_REAL;
  }
  else if (q < (mode - min) / int_len)
  {
    return min + sqrt(q * int_len * (mode - min));
  }
  else // if (q >= (mode - min) / int_len)
  {
    return max - sqrt((1.0 - q) * int_len * (max - mode));
  }
}

[[cpp11::register]]
doubles qtri_cpp(
    doubles p, doubles min, doubles max, doubles mode, bool is_lower_tail,
    bool is_log_p, bool is_scalar)
{
  int n = p.size();
  writable::doubles q(n);
  bool has_nan = false;

  if (is_scalar)
  {
    if (min[0] >= max[0] || mode[0] > max[0] || min[0] > mode[0])
    {
      for (int i = 0; i < n; i++)
      {
        q[i] = NA_REAL;
      }

      warning("NaN(s) produced.");

      return q;
    }

    double int_len = max[0] - min[0];

    for (int i = 0; i < n; i++)
    {
      q[i] = qtri_cpp_internal(
        p[i], min[0], max[0], mode[0], is_lower_tail, is_log_p, int_len,
        has_nan);
    }
  }
  else
  {
    for (int i = 0; i < n; i++)
    {
      if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
      {
        q[i] = NA_REAL;
        has_nan = true;
      }
      else
      {
        q[i] = qtri_cpp_internal(
          p[i], min[i], max[i], mode[i], is_lower_tail, is_log_p,
          max[i] - min[i], has_nan);
      }
    }
  }

  if (has_nan) warning("NaN(s) produced.");

  return q;
}

double rtri_cpp_internal(double min, double max, double mode, double int_len)
{
  double r = unif_rand();

  if (r < (mode - min) / int_len)
  {
    return min + sqrt(r * int_len * (mode - min));
  }
  else // if (r >= (mode - min) / int_len)
  {
    return max - sqrt((1.0 - r) * int_len * (max - mode));
  }
}

[[cpp11::register]]
doubles rtri_cpp(int n, doubles min, doubles max, doubles mode, bool is_scalar)
{
  writable::doubles r(n);

  if (is_scalar)
  {
    if (min[0] >= max[0] || mode[0] > max[0] || min[0] > mode[0])
    {
      for (int i = 0; i < n; i++)
      {
        r[i] = NA_REAL;
      }

      warning("NaN(s) produced.");

      return r;
    }

    double int_len = max[0] - min[0];

    for (int i = 0; i < n; i++)
    {
      r[i] = rtri_cpp_internal(min[0], max[0], mode[0], int_len);
    }
  }
  else
  {
    bool has_nan = false;

    for (int i = 0; i < n; i++)
    {
      if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
      {
        r[i] = NA_REAL;
        has_nan = true;
      }
      else
      {
        r[i] = rtri_cpp_internal(min[i], max[i], mode[i], max[i] - min[i]);
      }
    }

    if (has_nan) warning("NaN(s) produced.");
  }

  return r;
}

double mgtri_cpp_internal(
    double t, double min, double max, double mode, bool &has_nan)
{
  if (t == 0.0)
  {
    has_nan = true;
    return NA_REAL;
  }

  return 2.0 * ((max - mode) * exp(min * t) -
                (max - min) * exp(mode * t) +
                (mode - min) * exp(max * t)) /
                  ((max - min) * (mode - min) * (max - mode) * pow(t, 2));
}

[[cpp11::register]]
doubles mgtri_cpp(
    doubles t, doubles min, doubles max, doubles mode, bool is_scalar)
{
  int n = t.size();
  writable::doubles mg(n);
  bool has_nan = false;

  if (is_scalar)
  {
    if (min[0] >= max[0] || mode[0] >= max[0] || min[0] >= mode[0])
    {
      for (int i = 0; i < n; i++)
      {
        mg[i] = NA_REAL;
      }

      warning("NaN(s) produced.");

      return mg;
    }

    for (int i = 0; i < n; i++)
    {
      mg[i] = mgtri_cpp_internal(t[i], min[0], max[0], mode[0], has_nan);
    }
  }
  else
  {
    for (int i = 0; i < n; i++)
    {
      if (min[i] >= max[i] || mode[i] >= max[i] || min[i] >= mode[i])
      {
        mg[i] = NA_REAL;
        has_nan = true;
      }
      else
      {
        mg[i] = mgtri_cpp_internal(t[i], min[i], max[i], mode[i], has_nan);
      }
    }
  }

  if (has_nan) warning("NaN(s) produced.");

  return mg;
}

double estri_cpp_internal(
    double p, double min, double max, double mode, bool is_lower_tail,
    bool is_log_p, bool &has_nan)
{
  double es = is_log_p ? exp(p) : p;

  if (!is_lower_tail) es = 1.0 - es;

  if (es <= 0.0 || es > 1.0)
  {
    has_nan = true;
    return NA_REAL;
  }
  else if (es < (mode - min) / (max - min))
  {
    return ((es * min) +
            (2.0 / 3.0) * sqrt((max - min) * (mode - min)) * pow(es, 1.5)) /
              es;
  }

  double b = (mode - min) / (max - min);

  return ((b * min) +
          (2.0 / 3.0) * sqrt((max - min) * (mode - min)) * pow(b, 1.5) +
          (((es * max) + (2.0 / 3.0) * sqrt((max - min) * (max - mode)) * pow((1.0 - es), 1.5)) - ((b * max) + (2.0 / 3.0) * sqrt((max - min) * (max - mode)) * pow((1.0 - b), 1.5)))) /
            es;
}

[[cpp11::register]]
doubles estri_cpp(
    doubles p, doubles min, doubles max, doubles mode, bool is_lower_tail,
    bool is_log_p, bool is_scalar)
{
  int n = p.size();
  writable::doubles es(n);
  bool has_nan = false;

  if (is_scalar)
  {
    if (min[0] >= max[0] || mode[0] > max[0] || min[0] > mode[0])
    {
      for (int i = 0; i < n; i++)
      {
        es[i] = NA_REAL;
      }

      warning("NaN(s) produced.");

      return es;
    }

    for (int i = 0; i < n; i++)
    {
      es[i] = estri_cpp_internal(
        p[i], min[0], max[0], mode[0], is_lower_tail, is_log_p, has_nan);
    }
  }
  else
  {
    for (int i = 0; i < n; i++)
    {
      if (min[i] >= max[i] || mode[i] > max[i] || min[i] > mode[i])
      {
        es[i] = NA_REAL;
        has_nan = true;
      }
      else
      {
        es[i] = estri_cpp_internal(
          p[i], min[i], max[i], mode[i], is_lower_tail, is_log_p, has_nan);
      }
    }
  }

  if (has_nan) warning("NaN(s) produced.");

  return es;
}
