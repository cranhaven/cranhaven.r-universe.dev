// src/flow/flow_push_relabel.cpp
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.

#include "flow_push_relabel.h"

#include "../core/lap_certify.h"
#include "../core/lap_error.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace lap {

namespace {

// One direction of a residual arc, laid out as flow_solve.cpp lays it out so
// the two solvers read the same graph.
struct PRArc {
    int32_t to   = -1;
    int32_t rev  = -1;
    int64_t cap  = 0;
    double  cost = 0.0;
};

struct PRGraph {
    std::vector<int32_t> start;
    std::vector<PRArc>   arcs;

    int32_t begin_of(int32_t v) const {
        return start[static_cast<std::size_t>(v)];
    }
    int32_t end_of(int32_t v) const {
        return start[static_cast<std::size_t>(v) + 1];
    }
    PRArc& at(int32_t e) { return arcs[static_cast<std::size_t>(e)]; }
    const PRArc& at(int32_t e) const {
        return arcs[static_cast<std::size_t>(e)];
    }
};

std::string derive_status(int64_t flow_sent, int64_t flow_required,
                          bool stalled) {
    if (flow_sent == flow_required) return "optimal";
    if (!stalled)                   return "iteration_limit";
    if (flow_sent == 0)             return "infeasible";
    return "partial";
}

}  // namespace

FlowResult solve_min_cost_flow_push_relabel(FlowProblem&       prob,
                                            const FlowOptions& opts,
                                            PRTrace*           trace) {
    validate(prob);
    expand_blocks(prob);

    const int64_t n_arcs  = static_cast<int64_t>(prob.arcs.size());
    const int32_t n_nodes = prob.n_nodes;

    if (n_arcs > static_cast<int64_t>(std::numeric_limits<int32_t>::max()) / 2) {
        LAP_THROW_DIMENSION("FlowProblem: " + std::to_string(n_arcs) +
                            " expanded arcs is more than the residual graph "
                            "can index");
    }

    FlowResult out;
    out.flow.assign(static_cast<std::size_t>(n_arcs), 0);
    if (n_nodes <= 0) {
        out.status = derive_status(0, 0, true);
        return out;
    }

    const int32_t N = n_nodes;

    // ---- residual graph, starting from the lower bounds ----

    PRGraph g;
    g.start.assign(static_cast<std::size_t>(N) + 1, 0);
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        ++g.start[static_cast<std::size_t>(arc.tail) + 1];
        ++g.start[static_cast<std::size_t>(arc.head) + 1];
    }
    for (int32_t v = 0; v < N; ++v) {
        g.start[static_cast<std::size_t>(v) + 1] +=
            g.start[static_cast<std::size_t>(v)];
    }
    g.arcs.assign(static_cast<std::size_t>(2 * n_arcs), PRArc{});

    std::vector<int32_t> cursor(g.start.begin(), g.start.end() - 1);
    std::vector<int32_t> arc_slot(static_cast<std::size_t>(n_arcs), -1);
    double max_abs_cost = 0.0;
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        const int32_t iu = cursor[static_cast<std::size_t>(arc.tail)]++;
        const int32_t iv = cursor[static_cast<std::size_t>(arc.head)]++;
        g.at(iu) = PRArc{arc.head, iv, arc.upper - arc.lower, arc.cost};
        g.at(iv) = PRArc{arc.tail, iu, 0, -arc.cost};
        arc_slot[static_cast<std::size_t>(a)] = iu;
        max_abs_cost = std::max(max_abs_cost, std::abs(arc.cost));
    }

    // ---- excess: the supply, minus what the lower bounds already move ----

    std::vector<int64_t> excess(static_cast<std::size_t>(N), 0);
    for (int32_t v = 0; v < N; ++v) {
        excess[static_cast<std::size_t>(v)] =
            prob.supply[static_cast<std::size_t>(v)];
    }
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        excess[static_cast<std::size_t>(arc.tail)] -= arc.lower;
        excess[static_cast<std::size_t>(arc.head)] += arc.lower;
    }

    int64_t required = 0;
    for (int32_t v = 0; v < N; ++v) {
        if (excess[static_cast<std::size_t>(v)] > 0) {
            required += excess[static_cast<std::size_t>(v)];
        }
    }
    out.flow_required = required;

    std::vector<double> pi(static_cast<std::size_t>(N), 0.0);

    auto cbar = [&](int32_t u, int32_t e) -> double {
        const PRArc& a = g.at(e);
        return a.cost + pi[static_cast<std::size_t>(u)] -
               pi[static_cast<std::size_t>(a.to)];
    };

    // ---- cost scaling ----

    double eps = std::max(1.0, max_abs_cost);

    // Distinct integer totals differ by at least 1, and an eps-optimal flow is
    // off by at most n * eps, so below this every eps-optimal flow is optimal.
    // A caller with non-integer costs has scaled them to integers before
    // handing the problem over; there is no bound short of that.
    const double eps_final = 1.0 / (static_cast<double>(N) + 1.0);

    // Both are bounded on a run that keeps eps-optimality. Past them the
    // invariant has been lost and going round again would not recover it.
    const int64_t max_ops =
        static_cast<int64_t>(g.arcs.size()) * static_cast<int64_t>(N) + 1000;

    // A node's price only ever falls, and it cannot fall past the point where
    // every arc out of it is admissible and still no deficit is reachable. That
    // is the shape of an infeasible instance, and this is where it is noticed.
    const double price_floor =
        -(static_cast<double>(N) + 1.0) * (max_abs_cost + eps) - 1.0;

    if (trace != nullptr) {
        trace->eps_start = eps;
        trace->phases.clear();
    }

    // The flow on the caller's arcs as it stands, which is what a phase records
    // and what the answer is read back from at the end.
    auto read_flow = [&](std::vector<int64_t>& into) {
        into.assign(static_cast<std::size_t>(n_arcs), 0);
        for (int64_t a = 0; a < n_arcs; ++a) {
            const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
            const PRArc& fwd = g.at(arc_slot[static_cast<std::size_t>(a)]);
            into[static_cast<std::size_t>(a)] = arc.lower + g.at(fwd.rev).cap;
        }
    };

    std::vector<int32_t> active;
    std::vector<char>    is_active(static_cast<std::size_t>(N), 0);
    active.reserve(static_cast<std::size_t>(N));

    auto activate = [&](int32_t v) {
        if (is_active[static_cast<std::size_t>(v)]) return;
        if (excess[static_cast<std::size_t>(v)] <= 0) return;
        is_active[static_cast<std::size_t>(v)] = 1;
        active.push_back(v);
    };

    bool stalled = false;

    while (true) {
        int64_t n_saturated = 0;
        int64_t n_pushes    = 0;
        int64_t n_relabels  = 0;

        // ---- refine: saturate what the new eps no longer admits ----
        //
        // An arc priced below zero is one the smaller eps would allow to carry
        // negative reduced cost, so it goes to its upper bound and leaves the
        // residual graph. What that breaks is conservation, and restoring
        // conservation is what the discharge loop below does.
        for (int32_t u = 0; u < N; ++u) {
            const int32_t lo = g.begin_of(u);
            const int32_t hi = g.end_of(u);
            for (int32_t e = lo; e < hi; ++e) {
                PRArc& a = g.at(e);
                if (a.cap <= 0) continue;
                if (cbar(u, e) >= 0.0) continue;
                const int64_t delta = a.cap;
                excess[static_cast<std::size_t>(u)] -= delta;
                excess[static_cast<std::size_t>(a.to)] += delta;
                g.at(a.rev).cap += delta;
                a.cap = 0;
                ++n_saturated;
            }
        }

        active.clear();
        std::fill(is_active.begin(), is_active.end(), 0);
        for (int32_t v = 0; v < N; ++v) activate(v);

        int64_t ops = 0;
        stalled = false;
        while (!active.empty()) {
            if (++ops > max_ops) {
                out.status = "iteration_limit";
                break;
            }

            const int32_t u = active.back();
            active.pop_back();
            is_active[static_cast<std::size_t>(u)] = 0;
            if (excess[static_cast<std::size_t>(u)] <= 0) continue;

            const int32_t lo = g.begin_of(u);
            const int32_t hi = g.end_of(u);

            // Push along every admissible arc out of u until its excess is
            // gone. A push is the whole of what an arc can take or the whole of
            // what u is holding, whichever is smaller.
            for (int32_t e = lo; e < hi && excess[static_cast<std::size_t>(u)] > 0;
                 ++e) {
                PRArc& a = g.at(e);
                if (a.cap <= 0) continue;
                if (cbar(u, e) >= 0.0) continue;
                const int64_t delta =
                    std::min(excess[static_cast<std::size_t>(u)], a.cap);
                a.cap -= delta;
                g.at(a.rev).cap += delta;
                excess[static_cast<std::size_t>(u)] -= delta;
                excess[static_cast<std::size_t>(a.to)] += delta;
                activate(a.to);
                ++n_pushes;
            }

            if (excess[static_cast<std::size_t>(u)] <= 0) continue;

            // Relabel: u still holds excess and no arc out of it is admissible,
            // so lower pi[u] by the least amount that admits one.
            //
            // Writing q(e) = pi[head(e)] - cost(e), an arc's reduced cost is
            // pi[u] - q(e). Admitting one arc needs pi[u] < max q(e); keeping
            // every arc at or above -eps needs pi[u] >= max q(e) - eps. The two
            // meet at exactly one value, and there the widest arc sits at -eps
            // and every other one above it. Taking the minimum instead would
            // drop every arc below -eps at once, which is the eps-optimality
            // invariant gone rather than maintained.
            double best = -std::numeric_limits<double>::infinity();
            for (int32_t e = lo; e < hi; ++e) {
                const PRArc& a = g.at(e);
                if (a.cap <= 0) continue;
                const double q = pi[static_cast<std::size_t>(a.to)] - a.cost;
                if (q > best) best = q;
            }

            if (!std::isfinite(best) || best - eps < price_floor) {
                // Nothing left to push into, or the price has fallen past what
                // any reachable deficit could justify: this excess cannot be
                // placed, and neither can anything that would reach it.
                stalled = true;
                continue;
            }

            pi[static_cast<std::size_t>(u)] = best - eps;
            activate(u);
            ++n_relabels;
        }

        if (trace != nullptr) {
            PRPhase rec;
            rec.eps         = eps;
            rec.n_saturated = n_saturated;
            rec.n_pushes    = n_pushes;
            rec.n_relabels  = n_relabels;
            rec.potential.assign(static_cast<std::size_t>(N), 0.0);
            const double gauge = pi[0];
            for (int32_t v = 0; v < N; ++v) {
                rec.potential[static_cast<std::size_t>(v)] =
                    pi[static_cast<std::size_t>(v)] - gauge;
            }
            read_flow(rec.flow);
            trace->phases.push_back(std::move(rec));
        }

        if (out.status == "iteration_limit") break;
        if (eps <= eps_final) break;
        eps /= 8.0;
        if (eps < eps_final) eps = eps_final;
    }

    // ---- read the answer back ----

    detail::CompensatedSum total;
    int64_t placed_total = 0;
    for (int64_t a = 0; a < n_arcs; ++a) {
        const FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        const PRArc& fwd = g.at(arc_slot[static_cast<std::size_t>(a)]);
        const int64_t placed = g.at(fwd.rev).cap;
        const int64_t fa = arc.lower + placed;
        out.flow[static_cast<std::size_t>(a)] = fa;
        total.add(arc.cost * static_cast<double>(fa));
    }
    out.total_cost = total.value();

    // What reached its destination is the required flow less what is still
    // sitting on a node that was meant to send it.
    int64_t left = 0;
    for (int32_t v = 0; v < N; ++v) {
        if (excess[static_cast<std::size_t>(v)] > 0) {
            left += excess[static_cast<std::size_t>(v)];
        }
    }
    out.flow_sent = required - left;
    (void)placed_total;

    if (opts.return_potentials) {
        out.potential.assign(static_cast<std::size_t>(n_nodes), 0.0);
        const double gauge = pi[0];
        for (int32_t v = 0; v < n_nodes; ++v) {
            out.potential[static_cast<std::size_t>(v)] =
                pi[static_cast<std::size_t>(v)] - gauge;
        }
    }

    if (out.status != "iteration_limit") {
        out.status = derive_status(out.flow_sent, out.flow_required, stalled);
    }
    return out;
}

}  // namespace lap
