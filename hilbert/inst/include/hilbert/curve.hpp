#pragma once
#ifndef __HILBERT_CURVE_H__
#define __HILBERT_CURVE_H__

#include "types.hpp"

namespace hilbert
{

namespace curve
{

// rotate/flip a quadrant appropriately
template <typename T>
inline void rotate(T n, T *x, T *y, T rx, T ry)
{
    if (ry == 0) {
        if (rx == 1) {
            *x = n - 1 - *x;
            *y = n - 1 - *y;
        }

        //Swap x and y
        T t  = *x;
        *x = *y;
        *y = t;
    }
}

// Convert (x,y) to index
template <typename T>
inline void positionToIndex(T n, T x, T y, T *h)
{
    T d = 0;
    for (T s = n / 2; s > 0; s /= 2) {
        T rx = (x & s) > 0;
        T ry = (y & s) > 0;
        d += s * s * ((3 * rx) ^ ry);
        rotate(n, &x, &y, rx, ry);
    }

    *h = d;
}

//convert index to (x,y)
template <typename T>
inline void indexToPosition(T n, T h, T *x, T *y)
{
    T t = h;
    *x = *y = 0;
    for (T s = 1; s < n; s *= 2) {
        T rx = 1 & (t / 2);
        T ry = 1 & (t ^ rx);
        rotate(s, x, y, rx, ry);
        *x += s * rx;
        *y += s * ry;
        t /= 4;
    }
}

} // namespace hilbert

} // namespace spress

#endif
