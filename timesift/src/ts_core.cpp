#include "ts_core.h"

#include <algorithm>
#include <cmath>
#include <limits>

namespace timesift {

namespace {

constexpr seconds kDay = 86400;

// Beyond this the slot table stops being the cheap way round and the readings are binned one by
// one instead. A record has to span some 1,900 years hourly, or 45,000 years daily, to reach it.
constexpr std::int64_t kMaxSlots = 1 << 24;

constexpr double kInf = std::numeric_limits<double>::infinity();

struct Grouping {
  std::vector<seconds> bins;          // sorted distinct bin starts
  std::vector<std::int32_t> bin_of;   // one per reading
};

// The order both reductions walk a record in: by unit, then by the local clock, then by the
// instant. The lookback searches a unit's readings by local second, so that is the second key; the
// instant is what tells the two readings of an hour a zone repeated apart, so the order is a
// total one and a sum accumulated in it is the same bytes wherever it is taken.
struct ReadingOrder {
  const std::int32_t* unit;
  const seconds* local;
  const seconds* when;
  bool operator()(std::size_t a, std::size_t b) const {
    if (unit[a] != unit[b]) return unit[a] < unit[b];
    if (local[a] != local[b]) return local[a] < local[b];
    return when[a] < when[b];
  }
};

// The permutation that puts the record in that order, empty where it already is in it. Addition is
// not associative, so a sum accumulated in the order the caller happened to write its rows in
// depends on that order in its last bits, and a digest is a statement about the representation
// rather than about the table it was read from. A record given by unit and then by time is the
// common case and pays the scan that finds that out.
std::vector<std::size_t> reading_order(const ReadingOrder& before, std::size_t n) {
  for (std::size_t i = 1; i < n; ++i) {
    if (!before(i - 1, i)) {
      std::vector<std::size_t> order(n);
      for (std::size_t k = 0; k < n; ++k) order[k] = k;
      std::sort(order.begin(), order.end(), before);
      return order;
    }
  }
  return {};
}

// What a guard calls a unit or a target: the name the caller gave it, or its position where the
// caller gave none.
std::string label_of(const char* const* names, std::size_t i) {
  if (names != nullptr && names[i] != nullptr) {
    return std::string(names[i]);
  }
  return std::to_string(i + 1);
}

std::string plural(std::size_t n, const char* word) {
  return std::to_string(n) + " " + word + (n == 1 ? "" : "s");
}

// A reading has to be a finite number, and this is the one place that is said. A missing one
// propagates through a sum and is skipped by a comparison, so a bin's mean and its minimum would
// disagree about which readings were in it; an infinity makes the mean of a bin holding both signs
// a NaN. Neither has a value to put in front of a model, and neither has one spelling across the
// languages the digest is read in.
void check_finite(const double* value, std::size_t n, const std::int32_t* unit,
                  const char* const* unit_name, const seconds* local) {
  std::size_t bad = 0;
  std::size_t first = 0;
  for (std::size_t i = 0; i < n; ++i) {
    if (std::isfinite(value[i])) continue;
    if (bad == 0) first = i;
    ++bad;
  }
  if (bad == 0) return;
  throw Error(plural(bad, "reading") + (bad == 1 ? " is" : " are") +
              " not a finite number, first: unit " +
              label_of(unit_name, static_cast<std::size_t>(unit[first])) + " at " +
              iso8601(local[first]) +
              ". Fill or drop them before building a representation.");
}

// Which of the three per-day quantities the requested statistics need. A day-level statistic
// reduces each calendar day first and reduces again over the days of a bin; nothing else reads a
// day at all.
struct DayNeed {
  bool mean = false;
  bool min = false;
  bool max = false;
  bool any() const { return mean || min || max; }
};

DayNeed day_need(const std::vector<Stat>& stats) {
  DayNeed need;
  for (Stat s : stats) {
    switch (s) {
      case Stat::cold_day: case Stat::warm_day: need.mean = true; break;
      case Stat::mean_daily_min: need.min = true; break;
      case Stat::mean_daily_max: need.max = true; break;
      default: break;
    }
  }
  return need;
}

// The day-level statistics a guard names, with the verb agreeing with how many there are.
std::string day_level_named(const std::vector<Stat>& stats) {
  std::string which;
  std::size_t named = 0;
  for (Stat s : stats) {
    if (!is_day_level(s)) continue;
    if (!which.empty()) which += " and ";
    which += stat_name(s);
    ++named;
  }
  return which + (named == 1 ? " needs" : " need");
}

// The first stage of a day-level statistic: every (unit, calendar day) the record holds reduced to
// its own mean, its own minimum and its own maximum. Both reductions read a day this way; what
// they differ in is which bin owns a day, which each decides for itself.
struct DayTable {
  std::vector<std::int32_t> count;
  std::vector<double> sum;
  std::vector<double> low;
  std::vector<double> high;

  DayTable(std::size_t n_dcell, DayNeed need)
      : count(n_dcell, 0), sum(n_dcell, 0.0),
        low(need.min ? n_dcell : 0, kInf), high(need.max ? n_dcell : 0, -kInf) {}

  void read(std::size_t c, double v, DayNeed need) {
    count[c] += 1;
    sum[c] += v;
    if (need.min && v < low[c]) low[c] = v;
    if (need.max && v > high[c]) high[c] = v;
  }
};

// The second stage: the days of one bin reduced again, which is what keeps an extreme day distinct
// from an extreme reading. Days are folded in oldest first, so the mean of the daily extremes
// accumulates in the same order wherever it is read.
struct DayFold {
  std::vector<double> low, high, min_sum, max_sum;
  std::vector<std::int32_t> n_day;

  DayFold(std::size_t n_cell, DayNeed need)
      : low(need.mean ? n_cell : 0, kInf), high(need.mean ? n_cell : 0, -kInf),
        min_sum(need.min ? n_cell : 0, 0.0), max_sum(need.max ? n_cell : 0, 0.0),
        n_day(n_cell, 0) {}

  void add(const DayTable& day, std::size_t dcell, std::size_t cell, DayNeed need) {
    n_day[cell] += 1;
    if (need.mean) {
      const double m = day.sum[dcell] / day.count[dcell];
      if (m < low[cell]) low[cell] = m;
      if (m > high[cell]) high[cell] = m;
    }
    if (need.min) min_sum[cell] += day.low[dcell];
    if (need.max) max_sum[cell] += day.high[dcell];
  }
};

// One channel of the output, read off the accumulators both reductions fill.
void write_channel(Stat s, double* channel, std::size_t n_cell,
                   const std::vector<std::int32_t>& count, const std::vector<double>& sum,
                   const std::vector<double>& low, const std::vector<double>& high,
                   const DayFold& day) {
  switch (s) {
    case Stat::mean:
      for (std::size_t c = 0; c < n_cell; ++c) channel[c] = sum[c] / count[c];
      break;
    case Stat::min:
      for (std::size_t c = 0; c < n_cell; ++c) channel[c] = low[c];
      break;
    case Stat::max:
      for (std::size_t c = 0; c < n_cell; ++c) channel[c] = high[c];
      break;
    case Stat::cold_day:
      for (std::size_t c = 0; c < n_cell; ++c) channel[c] = day.low[c];
      break;
    case Stat::warm_day:
      for (std::size_t c = 0; c < n_cell; ++c) channel[c] = day.high[c];
      break;
    case Stat::mean_daily_min:
      for (std::size_t c = 0; c < n_cell; ++c) channel[c] = day.min_sum[c] / day.n_day[c];
      break;
    case Stat::mean_daily_max:
      for (std::size_t c = 0; c < n_cell; ++c) channel[c] = day.max_sum[c] / day.n_day[c];
      break;
  }
}

// Bins whose starts the caller supplied, or whose slots span more of the calendar than a table can
// hold: sort the distinct starts and look each reading up in them.
Grouping group_by_search(const seconds* start, std::size_t n) {
  Grouping out;
  out.bins.assign(start, start + n);
  std::sort(out.bins.begin(), out.bins.end());
  out.bins.erase(std::unique(out.bins.begin(), out.bins.end()), out.bins.end());
  out.bin_of.resize(n);
  for (std::size_t i = 0; i < n; ++i) {
    out.bin_of[i] = static_cast<std::int32_t>(
        std::lower_bound(out.bins.begin(), out.bins.end(), start[i]) - out.bins.begin());
  }
  return out;
}

// What a guard calls one reading of the record. The unit index is read here, so it is checked
// here too rather than trusted from a wrapper.
std::string reading_label(const Request& req, std::size_t i) {
  const std::int32_t u = req.unit[i];
  if (u < 0 || static_cast<std::size_t>(u) >= req.n_unit) {
    throw Error("a reading carries a unit index outside the units given.");
  }
  return "unit " + label_of(req.unit_name, static_cast<std::size_t>(u)) + " at " +
         iso8601(req.local[i]);
}

// A supplied calendar is a function of the reading instants, and two things have to hold of what
// it returns before anything below reads its bins as bins. A bin cannot begin after a reading it
// holds: that is what a calendar shifted by one boundary returns, and what an interval lookup
// wraps to below its first boundary, where NumPy's `searchsorted() - 1` gives the last edge of
// all. And a bin's readings have to be a stretch of the record: a calendar sending alternate
// readings to two bins interleaves them, and the array then carries two bins the record never
// had. Neither is visible to the empty-cell or the contiguity guard, which read the bins the
// calendar declared and can only ask whether every unit reaches each of them.
void check_supplied(const Request& req, const Grouping& grid) {
  for (std::size_t i = 0; i < req.n; ++i) {
    if (req.custom[i] > req.local[i]) {
      throw Error("the supplied calendar puts a reading in a bin beginning after it: " +
                  reading_label(req, i) + " is given the bin beginning " +
                  iso8601(req.custom[i]) +
                  ". A bin begins at or before every reading it holds; a calendar read with an "
                  "interval lookup has to say where the readings below its first boundary go.");
    }
  }

  const std::size_t n_bin = grid.bins.size();
  if (n_bin < 2) return;
  std::vector<seconds> first(n_bin, std::numeric_limits<seconds>::max());
  std::vector<seconds> last(n_bin, std::numeric_limits<seconds>::min());
  std::vector<std::size_t> opens(n_bin, 0);
  for (std::size_t i = 0; i < req.n; ++i) {
    const std::size_t b = static_cast<std::size_t>(grid.bin_of[i]);
    if (req.local[i] < first[b]) {
      first[b] = req.local[i];
      opens[b] = i;
    }
    if (req.local[i] > last[b]) last[b] = req.local[i];
  }
  for (std::size_t k = 0; k + 1 < n_bin; ++k) {
    if (last[k] < first[k + 1]) continue;
    throw Error("the supplied calendar interleaves two of its bins: " +
                reading_label(req, opens[k + 1]) + " opens the bin beginning " +
                iso8601(grid.bins[k + 1]) + ", and the bin beginning " + iso8601(grid.bins[k]) +
                " reaches to " + iso8601(last[k]) +
                ". A bin holds a stretch of the record, not readings taken from within another.");
  }
}

// Bin membership is a function of the slot alone, so the calendar is read once per slot the record
// touches and every reading is then an array lookup.
Grouping group_by_grain(const seconds* local, std::size_t n, Grain w, YearStart ys) {
  const seconds g = grain_granularity(w);
  std::int64_t lo = floor_div(local[0], g);
  std::int64_t hi = lo;
  for (std::size_t i = 1; i < n; ++i) {
    const std::int64_t s = floor_div(local[i], g);
    if (s < lo) lo = s;
    if (s > hi) hi = s;
  }
  const std::int64_t span = hi - lo + 1;
  if (span > kMaxSlots) {
    std::vector<seconds> start(n);
    bin_starts(local, n, w, ys, start.data());
    return group_by_search(start.data(), n);
  }

  std::vector<std::int32_t> slot_bin(static_cast<std::size_t>(span), -1);
  for (std::size_t i = 0; i < n; ++i) {
    slot_bin[static_cast<std::size_t>(floor_div(local[i], g) - lo)] = -2;
  }

  Grouping out;
  seconds last = 0;
  std::int32_t index = -1;
  for (std::int64_t s = 0; s < span; ++s) {
    if (slot_bin[static_cast<std::size_t>(s)] != -2) continue;
    const seconds bs = slot_bin_start(lo + s, w, ys);
    if (index < 0 || bs != last) {
      out.bins.push_back(bs);
      last = bs;
      ++index;
    }
    slot_bin[static_cast<std::size_t>(s)] = index;
  }

  out.bin_of.resize(n);
  for (std::size_t i = 0; i < n; ++i) {
    out.bin_of[i] = slot_bin[static_cast<std::size_t>(floor_div(local[i], g) - lo)];
  }
  return out;
}

// Every unit must reach every bin. A cell that no reading falls in is a record that stops early or
// starts late, and padding it would put an invented value in front of a model.
void check_full_grid(const std::vector<std::int32_t>& count, const std::vector<seconds>& bins,
                     const Request& req) {
  const std::size_t n_unit = req.n_unit;
  std::size_t empty = 0;
  std::size_t first = 0;
  for (std::size_t c = 0; c < count.size(); ++c) {
    if (count[c] == 0) {
      if (empty == 0) first = c;
      ++empty;
    }
  }
  if (empty == 0) return;
  throw Error(plural(empty, "(unit, bin) cell") + " hold no readings, first: unit " +
              label_of(req.unit_name, first % n_unit) + " at " + iso8601(bins[first / n_unit]) +
              ". Every unit must span every bin; gaps are not padded. coverage() lists them.");
}

// The clock a grain's bins are read on. The `native` grain's bin is the reading itself, and a
// reading is its instant: the two readings of an hour a zone repeats share a local second and are
// two bins. Every other grain is a stretch of the local calendar.
const seconds* bin_clock(const Request& req) {
  return req.grain == Grain::native ? req.when : req.local;
}

// The distinct bins of the record and the bin each reading falls in, whichever way the request
// says the bins are made. A supplied calendar declares its bins, and the `native` grain is the
// record unreduced, so in both the bin start is already in hand and the distinct ones are read off
// it directly. Every other grain is a function of the slot an instant falls in.
Grouping group(const Request& req) {
  if (req.n == 0) throw Error("no readings to reduce.");
  if (req.n_unit == 0) throw Error("no units to reduce.");
  if (req.grain == Grain::custom && req.custom == nullptr) {
    throw Error("a supplied calendar must give a bin start for every reading.");
  }
  if (req.grain == Grain::custom) {
    const Grouping grid = group_by_search(req.custom, req.n);
    check_supplied(req, grid);
    return grid;
  }
  return req.grain == Grain::native
             ? group_by_search(bin_clock(req), req.n)
             : group_by_grain(req.local, req.n, req.grain, req.year_start);
}

// How many readings each unit has in each bin of a grouping.
std::vector<std::int32_t> cell_counts(const Request& req, const Grouping& grid) {
  const std::size_t n_unit = req.n_unit;
  std::vector<std::int32_t> count(n_unit * grid.bins.size(), 0);
  for (std::size_t i = 0; i < req.n; ++i) {
    const std::int32_t u = req.unit[i];
    if (u < 0 || static_cast<std::size_t>(u) >= n_unit) {
      throw Error("a reading carries a unit index outside the units given.");
    }
    count[static_cast<std::size_t>(grid.bin_of[i]) * n_unit + static_cast<std::size_t>(u)] += 1;
  }
  return count;
}

// The bins have to tile the record. What the empty-cell guard cannot see is a bin the whole record
// skips, because a bin no unit reaches is never built: a February missing from every logger gives
// four "adjacent" monthly bins with February simply gone, and a convolution then reads January and
// March as neighbours.
void check_contiguous(const std::vector<seconds>& bins, const Request& req) {
  if (bins.size() < 2) return;
  std::vector<seconds> next(bins.size());
  bin_nexts(bins.data(), bins.size(), req.grain, req.year_start, next.data());
  for (std::size_t k = 0; k + 1 < bins.size(); ++k) {
    if (next[k] == bins[k + 1]) continue;
    throw Error(std::string("the ") + grain_name(req.grain) +
                " bins are not contiguous: nothing falls in the one beginning " +
                iso8601(next[k]) + ", between " + iso8601(bins[k]) + " and " +
                iso8601(bins[k + 1]) + ". Bins must tile the record; a gap is not closed up.");
  }
}

}  // namespace

Coverage coverage(const Request& req) {
  const Grouping grid = group(req);
  const std::vector<std::int32_t> held = cell_counts(req, grid);
  const std::size_t n_unit = req.n_unit;

  Coverage out;
  if (req.grain == Grain::custom || req.grain == Grain::native) {
    out.bin_start = grid.bins;
    out.count = held;
    return out;
  }
  // The calendar tiles the record from its first bin to its last, so a bin no unit reaches is a
  // column of zeros in its place rather than a bin the grouping never built.
  std::vector<std::size_t> column(grid.bins.size());
  for (std::size_t k = 0; k < grid.bins.size(); ++k) {
    if (k > 0) {
      seconds next;
      bin_nexts(&out.bin_start.back(), 1, req.grain, req.year_start, &next);
      while (next < grid.bins[k]) {
        out.bin_start.push_back(next);
        bin_nexts(&next, 1, req.grain, req.year_start, &next);
      }
    }
    out.bin_start.push_back(grid.bins[k]);
    column[k] = out.bin_start.size() - 1;
  }
  out.count.assign(n_unit * out.bin_start.size(), 0);
  for (std::size_t k = 0; k < grid.bins.size(); ++k) {
    for (std::size_t u = 0; u < n_unit; ++u) {
      out.count[column[k] * n_unit + u] = held[k * n_unit + u];
    }
  }
  return out;
}

Result reduce(const Request& req) {
  const std::size_t n = req.n;
  const std::size_t n_unit = req.n_unit;
  if (req.stats.empty()) throw Error("no statistic to compute.");

  const Grouping grid = group(req);
  const std::vector<seconds>& bins = grid.bins;
  const std::vector<std::int32_t>& bin_of = grid.bin_of;
  const std::size_t n_bin = bins.size();
  const std::size_t n_cell = n_unit * n_bin;

  // cell_counts is what checks a reading's unit index, so the readings can be named from here on.
  std::vector<std::int32_t> count = cell_counts(req, grid);
  check_finite(req.value, n, req.unit, req.unit_name, req.local);
  check_full_grid(count, bins, req);

  // A supplied calendar owns its own bin lengths, and the `native` grain's bin is the reading
  // itself, so in neither case can the calendar say what a bin between two others would have been.
  if (req.grain != Grain::custom && req.grain != Grain::native) {
    check_contiguous(bins, req);
  }

  bool need_sum = false, need_min = false, need_max = false;
  for (Stat s : req.stats) {
    switch (s) {
      case Stat::mean: need_sum = true; break;
      case Stat::min: need_min = true; break;
      case Stat::max: need_max = true; break;
      default: break;
    }
  }
  const DayNeed need = day_need(req.stats);

  std::vector<double> sum, low, high;
  if (need_sum) sum.assign(n_cell, 0.0);
  if (need_min) low.assign(n_cell, kInf);
  if (need_max) high.assign(n_cell, -kInf);

  const ReadingOrder before{req.unit, req.local, req.when};
  const std::vector<std::size_t> order = reading_order(before, n);
  const auto walk = [&](std::size_t k) { return order.empty() ? k : order[k]; };

  for (std::size_t k = 0; k < n; ++k) {
    const std::size_t i = walk(k);
    const std::size_t c = static_cast<std::size_t>(bin_of[i]) * n_unit +
                          static_cast<std::size_t>(req.unit[i]);
    const double v = req.value[i];
    if (need_sum) sum[c] += v;
    if (need_min && v < low[c]) low[c] = v;
    if (need_max && v > high[c]) high[c] = v;
  }

  // The day-level stage: reduce every (unit, calendar day) to its own mean, minimum and maximum,
  // then reduce again over the days of a bin. That second reduction is what keeps an extreme day
  // distinct from an extreme reading.
  DayFold day(need.any() ? n_cell : 0, need);
  if (need.any()) {
    const Grouping calendar = group_by_grain(req.local, n, Grain::day, req.year_start);
    const std::size_t n_days = calendar.bins.size();

    std::vector<std::int32_t> day_bin(n_days, -1);
    const std::size_t n_dcell = n_unit * n_days;
    DayTable table(n_dcell, need);

    for (std::size_t k = 0; k < n; ++k) {
      const std::size_t i = walk(k);
      const std::int32_t d = calendar.bin_of[i];
      if (day_bin[d] < 0) {
        day_bin[d] = bin_of[i];
      } else if (day_bin[d] != bin_of[i]) {
        throw Error(day_level_named(req.stats) +
                    " bins of a calendar day or coarser: the day beginning " +
                    iso8601(calendar.bins[d]) + " is split between the bins beginning " +
                    iso8601(bins[day_bin[d]]) + " and " + iso8601(bins[bin_of[i]]) + ".");
      }
      table.read(static_cast<std::size_t>(d) * n_unit + static_cast<std::size_t>(req.unit[i]),
                 req.value[i], need);
    }

    // Walked in day-cell order, so the days of a bin are folded in oldest first.
    for (std::size_t c = 0; c < n_dcell; ++c) {
      if (table.count[c] == 0) continue;
      day.add(table, c,
              static_cast<std::size_t>(day_bin[c / n_unit]) * n_unit + (c % n_unit), need);
    }
  }

  Result out;
  out.values.assign(n_cell * req.stats.size(), 0.0);
  for (std::size_t k = 0; k < req.stats.size(); ++k) {
    write_channel(req.stats[k], out.values.data() + k * n_cell, n_cell, count, sum, low, high, day);
  }

  out.bin_start = bins;
  out.bin_n.assign(count.begin(), count.end());
  out.bin_end.assign(n_bin, std::numeric_limits<seconds>::min());
  for (std::size_t i = 0; i < n; ++i) {
    const std::size_t b = static_cast<std::size_t>(bin_of[i]);
    if (req.when[i] > out.bin_end[b]) out.bin_end[b] = req.when[i];
  }

  // Which bins the record does not cover for their whole calendar span. The record covers from its
  // first reading to its last plus one sampling interval, and a bin is partial when its own span
  // reaches outside that. Only a bin at an end of the record can, because every unit has already
  // been required to hold readings in every bin between them. A supplied calendar declares where
  // its bins begin but not where the last one was meant to end, so that one is taken to end with
  // the record and is never partial. The record's ends are read on the clock the bins are.
  const seconds* clock = bin_clock(req);
  seconds covered_start = clock[0];
  seconds covered_end = clock[0];
  for (std::size_t i = 1; i < n; ++i) {
    if (clock[i] < covered_start) covered_start = clock[i];
    if (clock[i] > covered_end) covered_end = clock[i];
  }
  covered_end += req.sampling_step;

  std::vector<seconds> next(n_bin);
  if (req.grain == Grain::custom || req.grain == Grain::native) {
    for (std::size_t k = 0; k + 1 < n_bin; ++k) next[k] = bins[k + 1];
    if (n_bin > 0) next[n_bin - 1] = covered_end;
  } else {
    bin_nexts(bins.data(), n_bin, req.grain, req.year_start, next.data());
  }
  out.bin_partial.assign(n_bin, 0);
  for (std::size_t k = 0; k < n_bin; ++k) {
    out.bin_partial[k] = (bins[k] < covered_start || next[k] > covered_end) ? 1 : 0;
  }

  return out;
}

LookbackResult reduce_lookbacks(const LookbackRequest& req) {
  const std::size_t n = req.n;
  const std::size_t n_target = req.n_target;
  if (n == 0) throw Error("no readings to reduce.");
  if (n_target == 0) throw Error("no targets to reduce to.");
  if (req.n_unit == 0) throw Error("no units to reduce.");
  if (req.stats.empty()) throw Error("no statistic to compute.");
  if (req.span <= 0) throw Error("a lookback spans a positive number of seconds.");
  if (req.lag < 0) throw Error("a lookback's lag cannot be negative.");
  if (req.n_bin < 1) throw Error("a lookback holds at least one bin.");
  if (req.span % req.n_bin != 0) {
    throw Error("a span of " + std::to_string(req.span) + " seconds does not divide into " +
                std::to_string(req.n_bin) + " bins.");
  }
  const seconds step = req.span / req.n_bin;
  const std::size_t n_bin = static_cast<std::size_t>(req.n_bin);
  const std::size_t n_cell = n_target * n_bin;

  for (std::size_t i = 0; i < n; ++i) {
    if (req.unit[i] < 0 || static_cast<std::size_t>(req.unit[i]) >= req.n_unit) {
      throw Error("a reading carries a unit index outside the units given.");
    }
  }
  for (std::size_t i = 0; i < n_target; ++i) {
    if (req.target_unit[i] < 0 ||
        static_cast<std::size_t>(req.target_unit[i]) >= req.n_unit) {
      throw Error("a target carries a unit index outside the units given.");
    }
  }
  check_finite(req.value, n, req.unit, req.unit_name, req.local);

  // A day-level statistic is a state a calendar day was in, so it is defined only where each day
  // lies whole inside one bin. The lookback's bins are a fixed length from a fixed instant rather
  // than a calendar, so that is two conditions: the step is a whole number of days, and the lookback
  // opens on a day boundary. The first holds for every target or for none.
  const DayNeed need = day_need(req.stats);
  if (need.any() && floor_mod(step, kDay) != 0) {
    throw Error(day_level_named(req.stats) + " bins of a calendar day or coarser: target " +
                label_of(req.target_name, 0) + "'s bins are " + std::to_string(step) +
                " seconds long.");
  }

  // Put in the same order the calendar reduction walks, so each target's readings are a range found
  // by binary search rather than a pass over the record. This one needs the permutation itself.
  std::vector<std::size_t> order = reading_order(ReadingOrder{req.unit, req.local, req.when}, n);
  if (order.empty()) {
    order.resize(n);
    for (std::size_t i = 0; i < n; ++i) order[i] = i;
  }

  std::vector<seconds> when(n);
  std::vector<double> reading(n);
  std::vector<std::int32_t> holder(n);
  std::vector<std::size_t> start(req.n_unit + 1, 0);
  for (std::size_t k = 0; k < n; ++k) {
    when[k] = req.local[order[k]];
    reading[k] = req.value[order[k]];
    holder[k] = req.unit[order[k]];
    start[static_cast<std::size_t>(holder[k]) + 1] += 1;
  }
  for (std::size_t u = 0; u < req.n_unit; ++u) start[u + 1] += start[u];

  // The calendar days of the whole record, reduced once, whatever lookbacks read them afterwards.
  std::int64_t day_lo = 0;
  std::vector<std::int32_t> day_ix;
  DayTable table(0, need);
  if (need.any()) {
    day_lo = floor_div(when[0], kDay);
    std::int64_t day_hi = day_lo;
    for (std::size_t k = 1; k < n; ++k) {
      const std::int64_t s = floor_div(when[k], kDay);
      if (s < day_lo) day_lo = s;
      if (s > day_hi) day_hi = s;
    }
    const std::int64_t n_slot = day_hi - day_lo + 1;
    if (n_slot > kMaxSlots) {
      throw Error("the record spans too many days for a day-level statistic.");
    }
    day_ix.resize(n);
    table = DayTable(req.n_unit * static_cast<std::size_t>(n_slot), need);
    for (std::size_t k = 0; k < n; ++k) {
      day_ix[k] = static_cast<std::int32_t>(floor_div(when[k], kDay) - day_lo);
      table.read(static_cast<std::size_t>(day_ix[k]) * req.n_unit +
                     static_cast<std::size_t>(holder[k]),
                 reading[k], need);
    }
  }

  bool need_sum = false, need_min = false, need_max = false;
  for (Stat s : req.stats) {
    switch (s) {
      case Stat::mean: need_sum = true; break;
      case Stat::min: need_min = true; break;
      case Stat::max: need_max = true; break;
      default: break;
    }
  }
  std::vector<std::int32_t> count(n_cell, 0);
  std::vector<double> sum, low, high;
  if (need_sum) sum.assign(n_cell, 0.0);
  if (need_min) low.assign(n_cell, kInf);
  if (need_max) high.assign(n_cell, -kInf);
  DayFold day(need.any() ? n_cell : 0, need);

  for (std::size_t i = 0; i < n_target; ++i) {
    const std::size_t u = static_cast<std::size_t>(req.target_unit[i]);
    const seconds open = req.target_at[i] - req.lag - req.span;
    if (need.any() && floor_mod(open, kDay) != 0) {
      throw Error(day_level_named(req.stats) + " bins that open on a day boundary: target " +
                  label_of(req.target_name, i) + "'s lookback opens at " + iso8601(open) +
                  " and a calendar day falls in two of its bins.");
    }
    const auto first = when.begin() + static_cast<std::ptrdiff_t>(start[u]);
    const auto last = when.begin() + static_cast<std::ptrdiff_t>(start[u + 1]);
    const std::size_t lo =
        static_cast<std::size_t>(std::lower_bound(first, last, open) - when.begin());
    const std::size_t hi =
        static_cast<std::size_t>(std::lower_bound(first, last, open + req.span) - when.begin());

    for (std::size_t k = lo; k < hi; ++k) {
      const std::size_t c = static_cast<std::size_t>((when[k] - open) / step) * n_target + i;
      const double v = reading[k];
      count[c] += 1;
      if (need_sum) sum[c] += v;
      if (need_min && v < low[c]) low[c] = v;
      if (need_max && v > high[c]) high[c] = v;
    }

    // The readings of a unit are in time order, so its days arrive oldest first and each is folded
    // into the one bin that holds it whole.
    if (need.any()) {
      std::int32_t seen = -1;
      for (std::size_t k = lo; k < hi; ++k) {
        if (day_ix[k] == seen) continue;
        seen = day_ix[k];
        const seconds day_start = (day_lo + seen) * kDay;
        day.add(table, static_cast<std::size_t>(seen) * req.n_unit + u,
                static_cast<std::size_t>((day_start - open) / step) * n_target + i, need);
      }
    }
  }

  // A lookback reaching past the record is a target the record cannot answer for, and padding it
  // would put an invented value in front of a model.
  std::size_t empty = 0;
  std::size_t missing = 0;
  for (std::size_t c = 0; c < n_cell; ++c) {
    if (count[c] == 0) {
      if (empty == 0) missing = c;
      ++empty;
    }
  }
  if (empty > 0) {
    const std::size_t i = missing % n_target;
    const seconds from = req.target_at[i] - req.lag - req.span +
                         static_cast<seconds>(missing / n_target) * step;
    throw Error(plural(empty, "(target, bin) cell") + " hold no readings, first: target " +
                label_of(req.target_name, i) + " over [" + iso8601(from) + ", " +
                iso8601(from + step) + "). A lookback reaching past the record is not padded.");
  }

  LookbackResult out;
  out.values.assign(n_cell * req.stats.size(), 0.0);
  for (std::size_t k = 0; k < req.stats.size(); ++k) {
    write_channel(req.stats[k], out.values.data() + k * n_cell, n_cell, count, sum, low, high, day);
  }
  out.bin_n.assign(count.begin(), count.end());
  return out;
}

}  // namespace timesift
