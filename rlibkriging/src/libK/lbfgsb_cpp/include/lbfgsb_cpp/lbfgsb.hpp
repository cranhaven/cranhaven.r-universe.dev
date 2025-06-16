#pragma once

#include <iostream>
#include <cassert>
#include <vector>
#include <cstring>
#include <string>
#include <chrono>


namespace lbfgsb {
    extern "C" {
        void setulb_(
            const int* n, const int* m, double* x,
            const double* l, const double* u, const int* nbd,
            double* f, double* g,
            const double* factr, const double* pgtol,
            double* wa, int* iwa, char* task,
            const int* iprint, char* csave,
            bool* lsave, int* isave, double* dsave,
            std::size_t len_task, std::size_t len_csave
        );
    }


    constexpr std::size_t N_TASK{60};
    constexpr std::size_t N_CSAVE{60};
    constexpr std::size_t N_LSAVE{4};
    constexpr std::size_t N_ISAVE{44};
    constexpr std::size_t N_DSAVE{29};


    inline void setulb_wrapper(
        int n, int m, double* x,
        const double* lb, const double* ub, const int* bound_type,
        double* fval, double* grad,
        double factr, double pgtol,
        double* wa, int* iwa, char* task,
        int iprint, char* csave,
        bool* lsave, int* isave, double* dsave
    ) {
        setulb_(
            &n, &m, x, lb, ub, bound_type, fval, grad, &factr, &pgtol,
            wa, iwa, task, &iprint, csave, lsave,
            isave, dsave, N_TASK, N_CSAVE
        );
    }


    /// Fortran doesn't like `\0` in the array.
    inline void set_char_array(char dest[], const char source[]) {
        memcpy(dest, source, strlen(source));
    }

    /// Convert a Fortran style `Char` array to a string.
    /// Search from `xs[n-1]` to left.
    inline std::string string_from_fortran(const char xs[], std::size_t n) {
        assert(n >= 1);
        if (xs[n - 1] != ' ') {
            std::string ret(xs, n);
            return ret;
        }
        int i{static_cast<int>(n) - 2};
        for (; i >= 0; i-=1) {
            if (xs[i] != ' ') {
                std::string ret(xs, i + 1);
                return ret;
            }
        }
        assert(i == -1);
        return "";
    }

    /// All members should be const
    struct OptimizeResult {
        double f_opt;
        int warn_flag;
        unsigned int num_iters;
        unsigned int num_fun_calls;
        double previous_fval;
        double f_tol;
        double time_on_cauchy_points;
        double time_on_subspace_minimization;
        double time_on_line_search;
        bool max_iter_exceeded;
        bool max_fun_exceeded;
        bool time_limit_exceeded;
        std::string task;
        double time_spent_sec;

        void print() const {
            std::cout << "f_opt: " << f_opt << '\n';
            std::cout << "task: " << task << '\n';
            std::cout << "warn_flag " << warn_flag << '\n';
            std::cout << "num_fun_calls " << num_fun_calls << '\n';
            std::cout << "num_iters " << num_iters << '\n';
            if (max_iter_exceeded) {
                std::cout << "stop due to max_iter_exceeded" << '\n';
            }
            if (max_fun_exceeded) {
                std::cout << "stop due to max_fun_exceeded" << '\n';
            }
            if (time_limit_exceeded) {
                std::cout << "stop due to time_limit_exceeded" << '\n';
            }
            std::cout << "total time spent " << time_spent_sec << " sec\n";
            std::cout << "time spent on searching for Cauchy points " << time_on_cauchy_points << '\n';
            std::cout << "time spent on subspace minimization " << time_on_subspace_minimization << '\n';
            std::cout << "time spent on line search " << time_on_line_search << '\n';
            std::cout << "f(x) in the previous iteration " << previous_fval << '\n';
            std::cout << "factr * epsilon " << f_tol << '\n';
            std::cout << std::endl;
        }

    };


    /// All defaults are from SciPy except for `max_iter`.
    class Optimizer {
        public:
            const unsigned int n;
            // The maximum number of variable metric corrections used to define the limited memory matrix. (recommended 3 <= m <= 20)
            const unsigned int m;
            // Unlike SciPy, It is defined by the fortran subroutine
            // Not the number of times the subroutine is called.
            // The subroutine may be called multiple times for line searches in one iteration.
            unsigned int max_iter{5000};
            unsigned int max_fun{15000};
            // If <= 0, no limit.
            double time_limit_sec{-1};
            // 1e12 for low accuracy; 1e7 for moderate accuracy; 10.0 for high accuracy
            double factr{1e7};
            // Stop when max{|proj g_i | i = 1, ..., n} <= pgtol
            double pgtol{1e-9};
            int iprint{-1};

            explicit Optimizer(unsigned int _n, unsigned int _m=10):
                n{_n},
                m{_m},
                wa(2*m*n + 5*n + 11*m*m + 8*m),
                iwa(3*n) {
            }
        private:
            // Reset in `init.
            std::vector<double> wa;
            std::vector<int> iwa;
            char task[N_TASK];
            char csave[N_CSAVE];
            bool lsave[N_LSAVE];
            int isave[N_ISAVE];
            double dsave[N_DSAVE];

            void init() {
                // Fill with 0.
                std::fill(wa.begin(), wa.end(), 0);
                std::fill(iwa.begin(), iwa.end(), 0);
                std::fill_n(lsave, N_LSAVE, 0);
                std::fill_n(isave, N_ISAVE, 0);
                std::fill_n(dsave, N_DSAVE, 0);
                std::fill_n(task, N_TASK, ' ');
                std::fill_n(csave, N_CSAVE, ' ');
            }

        public:
            const char* get_task() const {
                return task;
            }

            /// `x0` is modified in-place.
            /// func(const T& x0, T& grad) -> fval
            /// `grad` is modified in-place.
            /// `T grad(x0)` must be able to initialize grad.
            /// `data(T&)` must return a pointer to the data.
            template<typename T, typename F>
            OptimizeResult minimize(
                F&& func, T& x0,
                const double* lb, const double* ub, const int* bound_type
            ) {
                const auto start_time = std::chrono::steady_clock::now();
                init();
                // Reset fval;
                double fval{0};
                T grad(x0);

                bool max_iter_exceeded{false};
                bool max_fun_exceeded{false};
                bool time_limit_exceeded{false};
                int warn_flag{-1};

                set_char_array(task, "START");

                while (true) {
                    // std::cout << "task to do: " << string_from_fortran(task, N_TASK) << "--\n";
                    setulb_wrapper(
                        n, m, data(x0), lb, ub, bound_type,
                        &fval, data(grad), factr, pgtol,
                        wa.data(), iwa.data(), task, iprint,
                        csave, lsave, isave, dsave
                    );

                    if (strncmp(task, "FG", 2) == 0) {
                        // Reference: driver3.f
                        // Only terminate after at least 1 iter, otherwise x0 & fval is 0 somehow.
                        if (time_limit_sec > 0 && isave[29] >= 1) {
                            const auto stop_time = std::chrono::steady_clock::now();
                            const std::chrono::duration<double> time_spent_sec{stop_time - start_time};
                            if (time_spent_sec.count() > time_limit_sec) {
                                time_limit_exceeded = true;
                                warn_flag = 1;
                                std::fill_n(task, N_TASK, ' ');
                                set_char_array(task, "STOP: CPU EXCEEDING THE TIME LIMIT.");
                                continue;
                            }
                        }
                        // Should not use `std::forward` as it is called multiple times.
                        // Should not preserve rvalue.
                        fval = func(x0, grad);
                    } else if (strncmp(task, "NEW_X", 5) == 0) {
                        // Without `STOP`, fortran doesn't know we are going to stop.
                        // Hence no summary log.
                        // Custom termination condition
                        // Reference: SciPy, driver2.f
                        if (isave[29] >= static_cast<int>(max_iter)) {
                            max_iter_exceeded = true;
                            warn_flag = 1;
                            std::fill_n(task, N_TASK, ' ');
                            set_char_array(task, "STOP: TOTAL NO. of ITERATIONS REACHED LIMIT");
                        } else if (isave[33] >= static_cast<int>(max_fun)) {
                            max_fun_exceeded = true;
                            warn_flag = 1;
                            std::fill_n(task, N_TASK, ' ');
                            set_char_array(task, "STOP: TOTAL NO. of f AND g EVALUATIONS EXCEEDS LIMIT");
                        }
                    } else {
                        break;
                    }
                }

                if (warn_flag == -1) {
                    if (strncmp(task, "CONV", 2) == 0) {
                        warn_flag = 0;
                    } else if (strncmp(task, "ABNORMAL_TERMINATION_IN_LNSRCH", 30) == 0) {
                        warn_flag = 3;
                    } else {
                        warn_flag = 2;
                    }
                }

                const auto stop_time = std::chrono::steady_clock::now();
                const std::chrono::duration<double> time_spent_sec{stop_time - start_time};

                return {
                    fval, warn_flag, static_cast<unsigned int>(isave[29]), static_cast<unsigned int>(isave[33]),
                    dsave[1], dsave[2], dsave[6], dsave[7], dsave[8],
                    max_iter_exceeded, max_fun_exceeded, time_limit_exceeded,
                    string_from_fortran(task, N_TASK), time_spent_sec.count()
                };
            }
    };
}
