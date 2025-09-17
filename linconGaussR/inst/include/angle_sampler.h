#ifndef LINCONGAUSSR_ANGLE_SAMPLER_H
#define LINCONGAUSSR_ANGLE_SAMPLER_H
#include "active_intersections.h"

namespace linconGaussR{
class AngleSampler
{
public:
    ActiveIntersections active_intersections;
    double rotation_angle;
    arma::mat rotated_slices;
    AngleSampler() = default;
    AngleSampler(ActiveIntersections active_intersect)
    {
        active_intersections = active_intersect;
        rotationAngle temp = active_intersect.rotated_intersections();
        rotation_angle = temp.rotation_angle;
        arma::vec slices = temp.shifted_angle;
        rotated_slices = slices;
        rotated_slices.reshape( 2,slices.n_elem / 2);
    }

    inline arma::vec _get_slices_cumulative_length();
    inline double draw_angle();
};

inline arma::vec AngleSampler::_get_slices_cumulative_length()
{
    arma::vec cum_len(rotated_slices.n_cols + 1, fill::zeros);
    arma::vec lengths =trans( rotated_slices.row(1) - rotated_slices.row(0)); 
    cum_len.rows(1,rotated_slices.n_cols) = cumsum(lengths);
    return (cum_len);
}

inline double AngleSampler::draw_angle()
{
    arma::vec cum_len = this->_get_slices_cumulative_length();
    double l = cum_len(cum_len.n_elem - 1);
    double sample = l * arma::randu<double>();
    int idx = 1;
    for (idx = 1; idx < cum_len.n_elem; idx++)
    {
        if (cum_len(idx - 1) < sample && cum_len(idx) >= sample)
        {
            break;
        }
    }
    idx--;
    return (rotated_slices(0,idx) + sample - cum_len(idx) + rotation_angle);
}

}

#endif
