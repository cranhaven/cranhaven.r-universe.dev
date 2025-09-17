// base class for loop

#ifndef LINCONGAUSSR_LOOP_H
#define LINCONGAUSSR_LOOP_H

#include "linear_constraints.h"

namespace linconGaussR{

class Loop
{
public:
    LinearConstraints lincon;
    int n_skip;
    Loop() = default;
    Loop(LinearConstraints linear_con, int nskip)
    {
        lincon = linear_con;
        n_skip = nskip;
    }
    void run()
    {
        return;
    }
};

class SamplingLoop : public Loop
{
public:
    int n_iterations;
    SamplingLoop(int n_iter, LinearConstraints linear_con, int nskip) : Loop(linear_con, nskip)
    {
        n_iterations = n_iter;
    }
    inline arma::mat compute_next_point(arma::mat x0)
    {
        return x0;
    }
};


class SamplerState
{
public:
    arma::mat samples;
    int iteration;
    SamplerState() = default;
    SamplerState(arma::vec x_init)
    {
        samples = x_init.t();
        iteration = 0;
    }
    inline void update(arma::vec x_new)
    {
        iteration++;
        samples.insert_rows(iteration, x_new.t());
        
    }
};

}

#endif
