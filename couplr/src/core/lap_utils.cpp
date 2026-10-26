// src/core/lap_utils.cpp
// Pure C++ utility functions for LAP solvers - NO Rcpp dependencies

#include "lap_utils.h"
#include <sstream>
#include <cmath>
#include <algorithm>

namespace lap {

std::string match_to_key(const std::vector<int>& match) {
    std::ostringstream os;
    for (size_t i = 0; i < match.size(); ++i) {
        if (i) os << ',';
        os << match[i];
    }
    return os.str();
}

void build_allowed(const std::vector<int>& mask, int64_t n, int64_t m,
                   std::vector<int64_t>& row_ptr, std::vector<int>& cols) {
    row_ptr.assign(static_cast<size_t>(n + 1), 0);

    // Count allowed entries per row
    // Note: mask uses 0=forbidden, nonzero=allowed
    for (int64_t i = 0; i < n; ++i) {
        for (int64_t j = 0; j < m; ++j) {
            if (mask[static_cast<size_t>(flat_index(i, j, m))] != 0) ++row_ptr[static_cast<size_t>(i + 1)];
        }
    }

    // Prefix sum
    for (int64_t i = 1; i <= n; ++i) {
        row_ptr[static_cast<size_t>(i)] += row_ptr[static_cast<size_t>(i - 1)];
    }

    // Fill column indices
    cols.assign(static_cast<size_t>(row_ptr.back()), -1);
    std::vector<int64_t> fill = row_ptr;
    for (int64_t i = 0; i < n; ++i) {
        for (int64_t j = 0; j < m; ++j) {
            if (mask[static_cast<size_t>(flat_index(i, j, m))] != 0) {
                cols[static_cast<size_t>(fill[static_cast<size_t>(i)]++)] = static_cast<int>(j);
            }
        }
    }
}

void ensure_each_row_has_option(const std::vector<int>& mask, int64_t n, int64_t m) {
    for (int64_t i = 0; i < n; ++i) {
        bool has_option = false;
        for (int64_t j = 0; j < m; ++j) {
            if (mask[static_cast<size_t>(flat_index(i, j, m))] != 0) {
                has_option = true;
                break;
            }
        }
        if (!has_option) {
            throw InfeasibleException("Infeasible: row " + std::to_string(i + 1) +
                                     " has no allowed edges");
        }
    }
}

bool is_feasible(const CostMatrix& cost) {
    if (cost.empty()) return false;
    if (cost.nrow > cost.ncol) return false;

    for (int64_t i = 0; i < cost.nrow; ++i) {
        bool has_finite = false;
        for (int64_t j = 0; j < cost.ncol; ++j) {
            if (cost.allowed(i, j) && std::isfinite(cost.at(i, j))) {
                has_finite = true;
                break;
            }
        }
        if (!has_finite) return false;
    }
    return true;
}

bool is_valid_matching(const CostMatrix& cost, const std::vector<int>& match) {
    for (int64_t i = 0; i < cost.nrow && i < static_cast<int64_t>(match.size()); ++i) {
        int j = match[static_cast<size_t>(i)];  // 0-based
        if (j < 0 || j >= cost.ncol) continue;  // Skip unmatched
        if (!cost.allowed(i, j) || !std::isfinite(cost.at(i, j))) {
            return false;  // Forbidden edge was chosen
        }
    }
    return true;
}

bool has_valid_matching(const CostMatrix& cost) {
    return has_valid_matching_view(cost);
}

double compute_total_cost(const CostMatrix& cost, const std::vector<int>& match) {
    double total = 0.0;

    for (int64_t i = 0; i < static_cast<int64_t>(match.size()) && i < cost.nrow; ++i) {
        int j = match[static_cast<size_t>(i)];  // 0-based
        if (j < 0 || j >= cost.ncol) continue;  // Skip unmatched or dummy

        double c = cost.at(i, j);
        if (std::isfinite(c)) {
            total += c;
        }
    }

    return total;
}

double compute_total_cost(const CostMatrix& original_cost,
                          const CostMatrix& /*work_cost*/,
                          const std::vector<int>& match) {
    // Always use original cost for reporting
    return compute_total_cost(original_cost, match);
}

CostMatrix negate_costs(const CostMatrix& cost) {
    CostMatrix result = cost;
    for (size_t i = 0; i < result.data.size(); ++i) {
        if (std::isfinite(result.data[i])) {
            result.data[i] = -result.data[i];
        }
    }
    return result;
}

void forbid_sentinel_costs(CostMatrix& cost) {
    for (int64_t i = 0; i < cost.nrow; ++i) {
        for (int64_t j = 0; j < cost.ncol; ++j) {
            if (cost.allowed(i, j) && cost.at(i, j) >= BIG) cost.forbid(i, j);
        }
    }
}

CostMatrix prepare_for_solve(const CostMatrix& cost, bool maximize) {
    CostMatrix result = maximize ? negate_costs(cost) : cost;

    // Ensure forbidden entries are BIG
    for (int64_t i = 0; i < result.nrow; ++i) {
        for (int64_t j = 0; j < result.ncol; ++j) {
            if (!result.allowed(i, j)) {
                result.at(i, j) = BIG;
            }
        }
    }

    return result;
}

std::vector<int> to_one_based(const std::vector<int>& match) {
    std::vector<int> result(match.size());
    for (size_t i = 0; i < match.size(); ++i) {
        result[i] = (match[i] >= 0) ? (match[i] + 1) : 0;
    }
    return result;
}

std::vector<int> to_zero_based(const std::vector<int>& match) {
    std::vector<int> result(match.size());
    for (size_t i = 0; i < match.size(); ++i) {
        result[i] = (match[i] > 0) ? (match[i] - 1) : -1;
    }
    return result;
}

}  // namespace lap
