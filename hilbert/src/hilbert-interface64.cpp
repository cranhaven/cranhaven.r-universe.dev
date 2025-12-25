#include <cpp11.hpp>
#include <cstring>
#include <bitset>
#include "hilbert.hpp"
#include "hilbert/grid.hpp"

using std::vector;
using cpp11::integers;
using cpp11::doubles;
using cpp11::strings;
using cpp11::data_frame;
using cpp11::literals::operator""_nm;

[[cpp11::register]]
strings HILBERT_index64_(size_t n, strings x, strings y) 
{
    const size_t len = x.size();
    vector<int64_t> h(len);

    for (size_t i = 0; i < len; ++i) {
        if (i % 8130 == 0) {
            cpp11::check_user_interrupt();
        }
        
        hilbert::curve::positionToIndex(
            int64_t(1) << n,
            static_cast<int64_t>(std::stoll(x[i], nullptr, 2)),
            static_cast<int64_t>(std::stoll(y[i], nullptr, 2)),
            &h[i]
        );
    }

    const cpp11::writable::strings indices(len);
    for (size_t i = 0; i < len; ++i) {
        indices[i] = std::bitset<64>(h[i]).to_string();
    }
    
    indices.attr("class") = "bitstring";
    return indices;
}

[[cpp11::register]]
data_frame HILBERT_position64_(size_t n, strings h)
{
    const size_t len = h.size();
    vector<int64_t> x(len), y(len);
    for (size_t i = 0; i < len; ++i) {
        hilbert::curve::indexToPosition(
            int64_t(1) << n,
            static_cast<int64_t>(std::stoll(h[i], nullptr, 2)),
            &x[i],
            &y[i]
        );
    }

    const cpp11::writable::strings xx(len), yy(len);
    for (size_t i = 0; i < len; ++i) {
        xx[i] = std::bitset<64>(x[i]).to_string();
        yy[i] = std::bitset<64>(y[i]).to_string();
    }

    xx.attr("class") = "bitstring";
    yy.attr("class") = "bitstring";

    return cpp11::writable::data_frame{
        "x"_nm = x,
        "y"_nm = y
    };
}

[[cpp11::register]]
data_frame HILBERT_coords_to_xy_64_(size_t n, doubles x, doubles y, doubles extent) {
    vector<double> xx(x.begin(), x.end()),
                   yy(y.begin(), y.end());

    vector<int64_t> xd = hilbert::grid::xToCol(int64_t(1) << n, xx, extent["xmax"], extent["xmin"]),
                    yd = hilbert::grid::yToRow(int64_t(1) << n, yy, extent["ymax"], extent["ymin"]);

    const size_t len = xd.size();

    cpp11::writable::strings xb(len), yb(len);

    for (size_t i = 0; i < len; ++i) {
        xb[i] = std::bitset<64>(xd[i]).to_string();
        yb[i] = std::bitset<64>(yd[i]).to_string();
    }

    xb.attr("class") = "bitstring";
    yb.attr("class") = "bitstring";

    return cpp11::writable::data_frame {
        "x"_nm = xb,
        "y"_nm = yb
    };
}

[[cpp11::register]]
data_frame HILBERT_xy_to_coords_64_(size_t n, strings x, strings y, doubles extent) {
    const size_t len = x.size();
    vector<int64_t> xx(len), yy(len);
    for (size_t i = 0; i < len; ++i) {
        xx[i] = static_cast<int64_t>(std::stoll(x[i], nullptr, 2));
        yy[i] = static_cast<int64_t>(std::stoll(y[i], nullptr, 2));
    }

    return cpp11::writable::data_frame {
        "x"_nm = hilbert::grid::colsToX(int64_t(1) << n, xx, extent["xmax"], extent["xmin"]),
        "y"_nm = hilbert::grid::rowsToY(int64_t(1) << n, yy, extent["ymax"], extent["ymin"])
    };
}
