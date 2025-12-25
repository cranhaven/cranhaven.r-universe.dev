#include <cpp11.hpp>
#include "cpp11/as.hpp"
#include "hilbert.hpp"

using std::vector;
using cpp11::integers;
using cpp11::doubles;
using cpp11::data_frame;
using cpp11::literals::operator""_nm;

[[cpp11::register]]
integers HILBERT_index_(size_t n, integers x, integers y)
{   
    const size_t len = x.size();
    const vector<size_t> xx(x.begin(), x.end()), yy(y.begin(), y.end());
    vector<size_t> h(len);

    for (size_t i = 0; i < len; ++i) {
        if (i % 8130 == 0) {
            cpp11::check_user_interrupt();
        }

        hilbert::curve::positionToIndex(size_t(1) << n, xx[i], yy[i], &h[i]);
    }

    return cpp11::as_sexp(h);
}

[[cpp11::register]]
data_frame HILBERT_position_(size_t n, integers h)
{
    const size_t len = h.size();
    const vector<size_t> hh(h.begin(), h.end());
    vector<size_t> x(len), y(len);

    for (size_t i = 0; i < len; ++i) {
        if (i % 8130 == 0) {
            cpp11::check_user_interrupt();
        }

        hilbert::curve::indexToPosition(size_t(1) << n, hh[i], &x[i], &y[i]);
    }

    return cpp11::writable::data_frame{
        "x"_nm = x,
        "y"_nm = y
    };
}

[[cpp11::register]]
data_frame HILBERT_coords_to_xy_(size_t n, doubles x, doubles y, doubles extent)
{
    vector<double> xx(x.begin(), x.end()),
                   yy(y.begin(), y.end());

    return cpp11::writable::data_frame {
        "x"_nm = hilbert::grid::xToCol(size_t(1) << n, xx, extent["xmax"], extent["xmin"]),
        "y"_nm = hilbert::grid::yToRow(size_t(1) << n, yy, extent["ymax"], extent["ymin"])
    };
}

[[cpp11::register]]
data_frame HILBERT_xy_to_coords_(size_t n, integers x, integers y, doubles extent)
{
    vector<size_t> xx(x.begin(), x.end()),
                   yy(y.begin(), y.end());

    return cpp11::writable::data_frame {
        "x"_nm = hilbert::grid::colsToX(size_t(1) << n, xx, extent["xmax"], extent["xmin"]),
        "y"_nm = hilbert::grid::rowsToY(size_t(1) << n, yy, extent["ymax"], extent["ymin"])
    };
}
