// src/solvers/solve_munkres.cpp
// Matrix-form Kuhn-Munkres LAP solver. Star/prime/cover bookkeeping with
// in-place reduced-cost adjustments. O(n^4) worst case; retained as a
// reference implementation. For production-speed assignment, prefer
// solve_hungarian (O(n^3)) or solve_jv.

#include "solve_munkres.h"
#include "../core/lap_error.h"
#include "../core/lap_utils.h"
#include <vector>
#include <limits>
#include <algorithm>
#include <cmath>

namespace lap {

LapResult solve_munkres(const CostMatrix& cost, bool maximize) {
    const int n = static_cast<int>(cost.nrow);
    const int m = static_cast<int>(cost.ncol);

    if (n == 0) {
        return LapResult({}, 0.0, "optimal");
    }
    lap::require_rows_fit_cols(n, m);

    CostMatrix work = prepare_for_solve(cost, maximize);
    ensure_each_row_has_option(work.mask, n, m);

    const double INF = std::numeric_limits<double>::infinity();

    // n*m computed in int64_t: as plain `int` this overflows the vector size
    // and every flat index below past a ~46,341-square matrix.
    const int64_t cell_count = static_cast<int64_t>(n) * static_cast<int64_t>(m);
    std::vector<double> C(static_cast<size_t>(cell_count));
    for (int64_t k = 0; k < cell_count; ++k) {
        C[static_cast<size_t>(k)] = work.mask[static_cast<size_t>(k)] ? work.data[static_cast<size_t>(k)] : INF;
    }

    // Magnitude-aware zero tolerance. The row/column reductions below subtract
    // running minima, so an intended-zero accumulates float error on the order
    // of (largest cost) * epsilon. A fixed absolute TOL (1e-12) then misses it
    // on large-magnitude inputs and a solvable matrix throws; scale by the
    // largest finite cost so ordinary small-cost inputs keep ~1e-12.
    double max_abs = 0.0;
    for (int64_t k = 0; k < cell_count; ++k) {
        if (std::isfinite(C[static_cast<size_t>(k)])) max_abs = std::max(max_abs, std::abs(C[static_cast<size_t>(k)]));
    }
    const double zero_tol = TOL * std::max(1.0, max_abs);

    auto row_min = [&](int i) {
        double mn = INF;
        for (int j = 0; j < m; ++j) {
            if (std::isfinite(C[static_cast<size_t>(flat_index(i, j, m))])) {
                mn = std::min(mn, C[static_cast<size_t>(flat_index(i, j, m))]);
            }
        }
        return mn;
    };

    for (int i = 0; i < n; ++i) {
        double mn = row_min(i);
        if (!std::isfinite(mn)) {
            LAP_THROW_INFEASIBLE("Row has no finite values (all forbidden)");
        }
        for (int j = 0; j < m; ++j) {
            if (std::isfinite(C[static_cast<size_t>(flat_index(i, j, m))])) {
                C[static_cast<size_t>(flat_index(i, j, m))] -= mn;
            }
        }
    }

    std::vector<int> star_row_of_col(m, -1);
    std::vector<int> star_col_of_row(n, -1);
    std::vector<int> prime_col_of_row(n, -1);
    std::vector<char> row_cov(n, 0);
    std::vector<char> col_cov(m, 0);

    auto is_zero = [&](int i, int j) {
        double x = C[static_cast<size_t>(flat_index(i, j, m))];
        return std::isfinite(x) && std::abs(x) <= zero_tol;
    };

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (is_zero(i, j) && col_cov[j] == 0) {
                star_row_of_col[j] = i;
                star_col_of_row[i] = j;
                col_cov[j] = 1;
                break;
            }
        }
    }

    std::fill(col_cov.begin(), col_cov.end(), 0);
    for (int j = 0; j < m; ++j) {
        if (star_row_of_col[j] != -1) {
            col_cov[j] = 1;
        }
    }

    auto find_uncovered_zero = [&]() -> std::pair<int, int> {
        for (int i = 0; i < n; ++i) {
            if (!row_cov[i]) {
                for (int j = 0; j < m; ++j) {
                    if (!col_cov[j] && is_zero(i, j)) {
                        return {i, j};
                    }
                }
            }
        }
        return {-1, -1};
    };

    auto smallest_uncovered = [&]() {
        double mn = INF;
        for (int i = 0; i < n; ++i) {
            if (!row_cov[i]) {
                for (int j = 0; j < m; ++j) {
                    if (!col_cov[j] && std::isfinite(C[static_cast<size_t>(flat_index(i, j, m))])) {
                        mn = std::min(mn, C[static_cast<size_t>(flat_index(i, j, m))]);
                    }
                }
            }
        }
        return mn;
    };

    const long long max_outer = static_cast<long long>(n) * m * 100;
    long long outer_iter = 0;

    while (true) {
        if (++outer_iter > max_outer) {
            LAP_THROW_CONVERGENCE("Exceeded maximum iterations");
        }

        int num_stars = 0;
        for (int j = 0; j < m; ++j) {
            if (star_row_of_col[j] >= 0) {
                num_stars++;
            }
        }
        if (num_stars >= n) {
            break;
        }

        std::fill(row_cov.begin(), row_cov.end(), 0);
        std::fill(col_cov.begin(), col_cov.end(), 0);
        for (int j = 0; j < m; ++j) {
            if (star_row_of_col[j] != -1) {
                col_cov[j] = 1;
            }
        }
        std::fill(prime_col_of_row.begin(), prime_col_of_row.end(), -1);

        const long long max_inner = static_cast<long long>(n) * m * 100;
        long long inner_iter = 0;

        while (true) {
            if (++inner_iter > max_inner) {
                LAP_THROW_CONVERGENCE("Inner loop exceeded maximum iterations");
            }

            auto z = find_uncovered_zero();
            if (z.first == -1) {
                double d = smallest_uncovered();
                if (!std::isfinite(d)) {
                    LAP_THROW_INFEASIBLE("No uncovered finite values");
                }

                for (int i = 0; i < n; ++i) {
                    for (int j = 0; j < m; ++j) {
                        if (row_cov[i] && std::isfinite(C[static_cast<size_t>(flat_index(i, j, m))])) {
                            C[static_cast<size_t>(flat_index(i, j, m))] += d;
                        }
                        if (!col_cov[j] && std::isfinite(C[static_cast<size_t>(flat_index(i, j, m))])) {
                            C[static_cast<size_t>(flat_index(i, j, m))] -= d;
                        }
                    }
                }
                continue;
            }

            int i = z.first;
            int j = z.second;
            prime_col_of_row[i] = j;

            if (star_col_of_row[i] == -1) {
                int ii = i;
                int jj = j;
                std::vector<std::pair<int, int>> path;
                path.emplace_back(ii, jj);

                while (true) {
                    int i_star = star_row_of_col[jj];
                    if (i_star == -1) {
                        break;
                    }
                    path.emplace_back(i_star, jj);
                    int j_prime = prime_col_of_row[i_star];
                    jj = j_prime;
                    path.emplace_back(i_star, jj);
                }

                for (size_t k = 0; k < path.size(); ++k) {
                    if (k % 2 == 1) {
                        int r = path[k].first;
                        int c = path[k].second;
                        star_row_of_col[c] = -1;
                        star_col_of_row[r] = -1;
                    }
                }
                for (size_t k = 0; k < path.size(); ++k) {
                    if (k % 2 == 0) {
                        int r = path[k].first;
                        int c = path[k].second;
                        star_row_of_col[c] = r;
                        star_col_of_row[r] = c;
                    }
                }

                break;
            } else {
                row_cov[i] = 1;
                col_cov[star_col_of_row[i]] = 0;
            }
        }
    }

    std::vector<int> assignment(n, -1);
    int num_matched = 0;
    for (int i = 0; i < n; ++i) {
        int j = star_col_of_row[i];
        if (j >= 0) {
            assignment[i] = j;
            num_matched++;
        }
    }

    if (num_matched < n) {
        LAP_THROW_INFEASIBLE("Could not find full matching");
    }

    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        int j = assignment[i];
        if (j < 0) {
            LAP_THROW_INFEASIBLE("Incomplete matching");
        }
        if (!cost.allowed(i, j)) {
            LAP_THROW_INFEASIBLE("Chosen forbidden edge");
        }
        double c = cost.at(i, j);
        if (!std::isfinite(c)) {
            LAP_THROW_INFEASIBLE("Chosen edge has non-finite cost");
        }
        total += c;
    }

    return LapResult(std::move(assignment), total, "optimal");
}

}  // namespace lap
