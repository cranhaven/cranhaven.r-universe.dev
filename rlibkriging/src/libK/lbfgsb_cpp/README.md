# L-BFGS-B C++ Wrapper


## Overview

This is a thin wrapper around the original [Fortran
L-BFGS-B routine v3.0](http://users.iems.northwestern.edu/~nocedal/lbfgsb.html)

- Lightweight & flexible abstraction.
- Provided interface follows SciPy convention.
- Require C++11.


## Install Using CMake


```bash
mkdir build
cd build
cmake ..
make install
```

- `-DCMAKE_INSTALL_PREFIX=/path/to/folder` - Install to `/path/to/folder` instead of `/usr/local`.
- `-DCMAKE_BUILD_TYPE=Release` - Fully optimized build.
- `-DBUILD_SHARED_LIBS=ON` - Build a shared library instead (default is static with `-fPIC`).
- `-DBUILD_EXAMPLE=ON` - Build example.
- The underlying fortran subroutine is compiled with `-frecursive` to ensure thread-safety.

## Quick Start

```c++
#include <lbfgsb_cpp/lbfgsb.hpp>
using namespace lbfgsb;

double get_objective(const std::array<double, 2>& x, std::array<double, 2>& grad) {
    grad[0] = 2 * (x[0] - 0.5);
    grad[1] = 2 * (x[1] - 1);
    return std::pow(x[0] - 0.5, 2) +  std::pow(x[1] - 1, 2);
}

int main() {
    const std::array<double, 2> lb{-2, -2};
    const std::array<double, 2> ub{2, 2};
    // 0 if unbounded,
    // 1 if only a lower bound,
    // 2 if both lower and upper bounds,
    // 3 if only an upper bound.
    const std::array<int, 2> bound_type{2, 2};

    Optimizer optimizer{lb.size()};
    // Can adjust many optimization configs.
    // E.g. `iprint`, `factr`, `pgtol`, `max_iter`, `max_fun`, `time_limit_sec`
    optimizer.iprint = 1;
    std::array<double, 2> x0{2, 3};
    auto result = optimizer.minimize(
        get_objective, x0, lb.data(), ub.data(), bound_type.data()
    );
    result.print();
    // (0.5, 1) => 0
    std::cout << "x0: (" << x0[0] << ", " << x0[1] << ")" << std::endl;

    return 0;
}
```


## API

- `minimize(F& func, T& x0, const double* lb, const double* ub, const int* bound_type)`
    - `x0` will be updated with the optimal parameters.

- `func(const T& x0, T& grad) -> double fval`
    - Aim to optimize the return value.

    - `grad` is required to be updated on each call.

- Requirement from `T`
    - `T grad(x0)` must be able to initialize `grad`.
  
    - `data(T)` must return a pointer to the data.
      
      You can override it to extend the support of your container (ex: `arma::vec`).
  
    - C++17 provides `std::data` for usual containers like `std::array` & `std::vector`.

## Differences with SciPy

- All defaults are from SciPy except for `max_iter`.
- Unlike SciPy, `max_iter` is defined by the fortran subroutine, not the number of times the subroutine is called.
    - The subroutine may be called multiple times for line searches in one iteration.
- `OptimizeResult.warn_flag` returns 3 for `ABNORMAL_TERMINATION_IN_LNSRCH` instead of 2.
- The Fortran subroutine SciPy uses has an additional parameter `maxls`.


## References

- Julia
    - <https://github.com/Gnimuc/LBFGSB.jl/>
- Rust
    - <https://github.com/noshu/lbfgsb-sys/>
    - <https://github.com/noshu/rustimization>
- C++
    - <https://github.com/constantino-garcia/lbfgsb_cpp_wrapper/>
- SciPy
    - <https://github.com/scipy/scipy/blob/master/scipy/optimize/lbfgsb.py>
