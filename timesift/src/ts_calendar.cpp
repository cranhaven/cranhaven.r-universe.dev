#include "ts_core.h"

#include <algorithm>
#include <cmath>
#include <cstdio>

namespace timesift {

namespace {

constexpr seconds kDay = 86400;

struct NameMap {
  const char* name;
  Grain grain;
};

const NameMap kGrains[] = {
  {"native", Grain::native}, {"halfday", Grain::halfday}, {"day", Grain::day},
  {"week", Grain::week}, {"month", Grain::month}, {"season", Grain::season},
  {"year", Grain::year}, {"custom", Grain::custom}
};

struct StatMap {
  const char* name;
  Stat stat;
};

const StatMap kStats[] = {
  {"mean", Stat::mean}, {"min", Stat::min}, {"max", Stat::max},
  {"cold_day", Stat::cold_day}, {"warm_day", Stat::warm_day},
  {"mean_daily_min", Stat::mean_daily_min}, {"mean_daily_max", Stat::mean_daily_max}
};

// How many whole months a day sits past the year_start anniversary, counted from year 0. Flooring
// this by three or by twelve is what puts a seasonal or a hydrological-year bin in phase with the
// anniversary rather than with January.
std::int64_t offset_months(std::int64_t day_number, YearStart ys) noexcept {
  std::int64_t y;
  unsigned m, d;
  civil_from_days(day_number, y, m, d);
  return y * 12 + (static_cast<std::int64_t>(m) - 1) - (ys.month - 1) -
         (static_cast<int>(d) < ys.day ? 1 : 0);
}

seconds anniversary(std::int64_t offset, YearStart ys) noexcept {
  const std::int64_t absolute = offset + (ys.month - 1);
  const std::int64_t y = floor_div(absolute, 12);
  const unsigned m = static_cast<unsigned>(floor_mod(absolute, 12)) + 1;
  return days_from_civil(y, m, static_cast<unsigned>(ys.day)) * kDay;
}

}  // namespace

std::int64_t days_from_civil(std::int64_t y, unsigned m, unsigned d) noexcept {
  y -= m <= 2;
  const std::int64_t era = (y >= 0 ? y : y - 399) / 400;
  const unsigned yoe = static_cast<unsigned>(y - era * 400);
  const unsigned doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;
  const unsigned doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
  return era * 146097 + static_cast<std::int64_t>(doe) - 719468;
}

void civil_from_days(std::int64_t z, std::int64_t& y, unsigned& m, unsigned& d) noexcept {
  z += 719468;
  const std::int64_t era = (z >= 0 ? z : z - 146096) / 146097;
  const unsigned doe = static_cast<unsigned>(z - era * 146097);
  const unsigned yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
  const std::int64_t yy = static_cast<std::int64_t>(yoe) + era * 400;
  const unsigned doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
  const unsigned mp = (5 * doy + 2) / 153;
  d = doy - (153 * mp + 2) / 5 + 1;
  m = mp + (mp < 10 ? 3 : -9);
  y = yy + (m <= 2);
}

std::string iso8601(seconds t) {
  const std::int64_t day_number = floor_div(t, kDay);
  const std::int64_t rest = t - day_number * kDay;
  std::int64_t y;
  unsigned m, d;
  civil_from_days(day_number, y, m, d);
  char buffer[64];
  std::snprintf(buffer, sizeof(buffer), "%04lld-%02u-%02uT%02lld:%02lld:%02lld",
                static_cast<long long>(y), m, d,
                static_cast<long long>(rest / 3600),
                static_cast<long long>((rest / 60) % 60),
                static_cast<long long>(rest % 60));
  return std::string(buffer);
}

Grain grain_from_name(const std::string& name) {
  for (const NameMap& entry : kGrains) {
    if (name == entry.name) return entry.grain;
  }
  throw Error("unknown grain: " + name + ".");
}

const char* grain_name(Grain w) {
  for (const NameMap& entry : kGrains) {
    if (w == entry.grain) return entry.name;
  }
  return "unknown";
}

Stat stat_from_name(const std::string& name) {
  for (const StatMap& entry : kStats) {
    if (name == entry.name) return entry.stat;
  }
  throw Error("unknown statistic: " + name + ".");
}

const char* stat_name(Stat s) {
  for (const StatMap& entry : kStats) {
    if (s == entry.stat) return entry.name;
  }
  return "unknown";
}

bool is_day_level(Stat s) {
  return s == Stat::cold_day || s == Stat::warm_day ||
         s == Stat::mean_daily_min || s == Stat::mean_daily_max;
}

// The granularity a grain's bin boundaries land on. Bin membership is a function of this slot
// alone, which is what lets the calendar be read once per slot of the record rather than once per
// reading: three years hourly is 26,304 slots against 23.5 million readings.
seconds grain_granularity(Grain w) {
  switch (w) {
    case Grain::native: return 1;
    case Grain::halfday: return 43200;
    default: return kDay;
  }
}

// The start of the bin holding a whole slot, given the slot index at the grain's granularity.
seconds slot_bin_start(std::int64_t slot, Grain w, YearStart ys) noexcept {
  switch (w) {
    case Grain::native:
      // The `native` grain is the record unreduced: a reading is its own bin.
      return slot;
    case Grain::halfday:
      return slot * 43200;
    case Grain::day:
      return slot * kDay;
    case Grain::week:
      // 1970-01-01 was a Thursday, so the slot index plus three is zero on a Monday.
      return (slot - floor_mod(slot + 3, 7)) * kDay;
    case Grain::month: {
      std::int64_t y;
      unsigned m, d;
      civil_from_days(slot, y, m, d);
      return days_from_civil(y, m, 1) * kDay;
    }
    case Grain::season:
      return anniversary(floor_div(offset_months(slot, ys), 3) * 3, ys);
    case Grain::year:
      return anniversary(floor_div(offset_months(slot, ys), 12) * 12, ys);
    default:
      return slot * kDay;
  }
}

void bin_starts(const seconds* when, std::size_t n, Grain w, YearStart ys, seconds* out) {
  if (w == Grain::custom) {
    throw Error("a supplied calendar declares its own bins; bin_starts() does not apply.");
  }
  const seconds g = grain_granularity(w);
  for (std::size_t i = 0; i < n; ++i) {
    out[i] = slot_bin_start(floor_div(when[i], g), w, ys);
  }
}

void bin_nexts(const seconds* bin_start, std::size_t n, Grain w, YearStart ys, seconds* out) {
  for (std::size_t i = 0; i < n; ++i) {
    const seconds t = bin_start[i];
    switch (w) {
      case Grain::native:
        throw Error("the native grain's bin is the reading itself; "
                    "bin_nexts() does not apply.");
      case Grain::halfday: out[i] = t + 43200; break;
      case Grain::day: out[i] = t + kDay; break;
      case Grain::week: out[i] = t + 7 * kDay; break;
      case Grain::month: {
        std::int64_t y;
        unsigned m, d;
        civil_from_days(floor_div(t, kDay), y, m, d);
        out[i] = days_from_civil(m == 12 ? y + 1 : y, m == 12 ? 1 : m + 1, 1) * kDay;
        break;
      }
      case Grain::season:
        out[i] = anniversary(offset_months(floor_div(t, kDay), ys) + 3, ys);
        break;
      case Grain::year:
        out[i] = anniversary(offset_months(floor_div(t, kDay), ys) + 12, ys);
        break;
      default:
        throw Error("a supplied calendar declares its own bins; bin_nexts() does not apply.");
    }
  }
}

// Where in a cycle a bin sits. The midpoint of the record the bin holds is read in UTC, and its
// position in the year or the day that midpoint falls in becomes an angle: the turn of the cycle
// is where the fraction itself jumps and the sine and the cosine do not.
//
// The instants are read in UTC rather than on the clock the bins were placed on, because a phase
// is a place on the orbit or the rotation rather than a reading of a clock. A zone moves it by its
// offset, the same shift for every bin of the record; on the day cycle that is also what a site's
// longitude does to the solar day, and a clock's summer time would move it by an hour twice a year.
namespace {

// A bin spanning an odd number of seconds has its midpoint on a half second, and the second it
// began is the one it is read at. Every calendar grain spans whole hours; a supplied calendar need
// not.
seconds bin_midpoint(seconds bin_start, seconds bin_end) noexcept {
  return bin_start + floor_div(bin_end - bin_start, 2);
}

void year_fraction(const seconds* bin_start, const seconds* bin_end, std::size_t n, double* out) {
  for (std::size_t i = 0; i < n; ++i) {
    const seconds mid = bin_midpoint(bin_start[i], bin_end[i]);
    std::int64_t y;
    unsigned m, d;
    civil_from_days(floor_div(mid, kDay), y, m, d);
    const seconds opens = days_from_civil(y, 1, 1) * kDay;
    const seconds length = days_from_civil(y + 1, 1, 1) * kDay - opens;
    out[i] = static_cast<double>(mid - opens) / static_cast<double>(length);
  }
}

// The UTC day has no leap second in these instants, so its length is a constant and the fraction
// is the midpoint's second of the day over it.
void day_fraction(const seconds* bin_start, const seconds* bin_end, std::size_t n, double* out) {
  seconds closest = kDay;
  for (std::size_t i = 1; i < n; ++i) {
    closest = std::min(closest, bin_start[i] - bin_start[i - 1]);
  }
  if (n < 2 || closest >= kDay) {
    throw Error("the day cycle places a bin in the day, and these bins sit a day or more apart, "
                "so every one would sit at the same place. It reads a grain finer than a day.");
  }
  for (std::size_t i = 0; i < n; ++i) {
    const seconds mid = bin_midpoint(bin_start[i], bin_end[i]);
    out[i] = static_cast<double>(floor_mod(mid, kDay)) / static_cast<double>(kDay);
  }
}

struct CycleMap {
  const char* name;
  void (*fraction)(const seconds*, const seconds*, std::size_t, double*);
};

const CycleMap kCycles[] = {{"year", year_fraction}, {"day", day_fraction}};

}  // namespace

void cycle_fraction(const std::string& cycle, const seconds* bin_start, const seconds* bin_end,
                    std::size_t n, double* out) {
  for (const CycleMap& c : kCycles) {
    if (cycle == c.name) {
      c.fraction(bin_start, bin_end, n, out);
      return;
    }
  }
  throw Error("unknown cycle: " + cycle + ". The cycles are year and day.");
}

void cycle_phase(const std::string& cycle, const seconds* bin_start, const seconds* bin_end,
                 std::size_t n, double* out_sin, double* out_cos) {
  constexpr double kTwoPi = 6.283185307179586476925286766559;
  std::vector<double> frac(n);
  cycle_fraction(cycle, bin_start, bin_end, n, frac.data());
  for (std::size_t i = 0; i < n; ++i) {
    out_sin[i] = std::sin(kTwoPi * frac[i]);
    out_cos[i] = std::cos(kTwoPi * frac[i]);
  }
}

}  // namespace timesift
