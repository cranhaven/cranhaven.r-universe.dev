#ifndef TIMESIFT_TS_CORE_H
#define TIMESIFT_TS_CORE_H

#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <vector>

// The binning and the reduction, once, for both languages.
//
// Everything below works in naive local seconds: the count of seconds from 1970-01-01T00:00:00 in
// the calendar the caller wants binned, with the zone already resolved away. A day is 86400 of
// them whatever the zone did that night, a month is what the Gregorian calendar says, and no
// tzdata, no locale and no parse is reachable from here. The caller resolves the zone on the way
// in and puts it back on the way out.
namespace timesift {

using seconds = std::int64_t;

enum class Grain { native, halfday, day, week, month, season, year, custom };
enum class Stat { mean, min, max, cold_day, warm_day, mean_daily_min, mean_daily_max };

struct YearStart {
  int month;
  int day;
};

struct Error : std::runtime_error {
  using std::runtime_error::runtime_error;
};

Grain grain_from_name(const std::string& name);
Stat stat_from_name(const std::string& name);
const char* grain_name(Grain w);
const char* stat_name(Stat s);
bool is_day_level(Stat s);

inline std::int64_t floor_div(std::int64_t a, std::int64_t b) noexcept {
  std::int64_t q = a / b;
  if ((a % b != 0) && ((a < 0) != (b < 0))) --q;
  return q;
}

inline std::int64_t floor_mod(std::int64_t a, std::int64_t b) noexcept {
  return a - floor_div(a, b) * b;
}

// Proleptic Gregorian civil arithmetic, exact over the whole int64 range and free of any parse.
// This is what replaces as.POSIXct(paste0(monday, " 00:00:00"), tz).
std::int64_t days_from_civil(std::int64_t y, unsigned m, unsigned d) noexcept;
void civil_from_days(std::int64_t z, std::int64_t& y, unsigned& m, unsigned& d) noexcept;

// "YYYY-MM-DDTHH:MM:SS", for the messages the guards raise.
std::string iso8601(seconds t);

// The start of the bin each instant falls in, and the start of the bin that follows a given one on
// the same calendar. Neither is defined for Grain::custom, whose bins the caller declares.
void bin_starts(const seconds* when, std::size_t n, Grain w, YearStart ys, seconds* out);
void bin_nexts(const seconds* bin_start, std::size_t n, Grain w, YearStart ys, seconds* out);

// Bin membership is a function of the slot an instant falls in at the grain's own granularity:
// the reading for `native`, the half day for `halfday`, the calendar day for everything coarser. The
// reduction reads the calendar once per slot of the record rather than once per reading.
seconds grain_granularity(Grain w);
seconds slot_bin_start(std::int64_t slot, Grain w, YearStart ys) noexcept;

// Where in a cycle each bin sits, read at the midpoint of the record the bin holds. A cycle is
// named: `year`, the place on the orbit, or `day`, the place in the Earth's rotation. Instants
// here, not naive local seconds: a phase is a place on the orbit or the rotation, and it is read
// in UTC whatever clock the bins were placed on. The day cycle is refused on bins that sit a day
// or more apart, where every bin would sit at one place in the day.
//
// The fraction is exact arithmetic on the calendar and the same bits on every platform; the sine
// and the cosine of it are the platform's. The two are separate so the contract can pin the first
// and state a tolerance on the second.
void cycle_fraction(const std::string& cycle, const seconds* bin_start, const seconds* bin_end,
                    std::size_t n, double* out);
void cycle_phase(const std::string& cycle, const seconds* bin_start, const seconds* bin_end,
                 std::size_t n, double* out_sin, double* out_cos);

struct Request {
  const std::int32_t* unit = nullptr;   // 0-based unit index, one per reading
  const double* value = nullptr;        // one per reading
  const seconds* when = nullptr;        // the true instant, one per reading; `native` bins on it
  const seconds* local = nullptr;       // naive local seconds, one per reading
  const seconds* custom = nullptr;      // a supplied calendar's bin start per reading, else null
  const char* const* unit_name = nullptr;  // n_unit names, for the guards; may be null
  std::size_t n = 0;
  std::size_t n_unit = 0;
  Grain grain = Grain::day;
  YearStart year_start{9, 1};
  seconds sampling_step = 0;
  std::vector<Stat> stats;
};

// The bins of every grain but `native` are read on the naive local clock, and their starts come
// back on it. The `native` grain's bin is the reading itself, and a reading is its instant: the two
// readings of an hour a zone repeats share a local second and are two bins, so that grain is read
// on the instant and its bin starts come back as instants.
struct Result {
  std::vector<double> values;            // [unit, bin, channel], unit fastest, as the digest reads it
  std::vector<seconds> bin_start;        // sorted distinct, on the clock the grain is read on
  std::vector<seconds> bin_end;          // the last reading instant assigned to each bin
  std::vector<std::int32_t> bin_n;       // [unit, bin], unit fastest
  std::vector<std::uint8_t> bin_partial; // one per bin
};

// Throws Error for a (unit, bin) cell holding no readings, for a bin sequence with a hole in it,
// and for a bin shorter than a calendar day under a day-level statistic.
Result reduce(const Request& req);

// How many readings each unit has in each bin, over every bin the calendar tiles the record with.
// It is what `reduce` refuses a gap on, laid out so the gaps can be read: a bin no unit reaches is
// a column of zeros rather than a bin left out. `stats` and `value` are not read.
struct Coverage {
  std::vector<seconds> bin_start;   // every bin from the first to the last the record touches, on
                                    // the clock the grain is read on, as Result::bin_start is
  std::vector<std::int32_t> count;  // [unit, bin], unit fastest
};

Coverage coverage(const Request& req);

// The second reduction: a lookback anchored on each target, which no calendar expresses.
// A target is a thing to predict, carrying the unit whose record it reads and the instant it is
// anchored at, and the lookback is a fixed length of time ending a fixed lag before that instant.
// Naive local seconds here as everywhere below the wrappers: the length is measured on the local
// clock, which is what keeps a calendar day whole inside a bin for the day-level statistics, so a
// lookback spanning a clock change holds an hour more or less of record than one that does not.
struct LookbackRequest {
  const std::int32_t* unit = nullptr;         // series unit index, one per reading
  const double* value = nullptr;              // one per reading
  const seconds* when = nullptr;              // the true instant, one per reading
  const seconds* local = nullptr;             // naive local seconds, one per reading
  const char* const* unit_name = nullptr;     // n_unit names, for the guards; may be null
  std::size_t n = 0;
  std::size_t n_unit = 0;
  const std::int32_t* target_unit = nullptr;  // unit index, one per target
  const seconds* target_at = nullptr;         // anchor instant in naive local seconds, per target
  const char* const* target_name = nullptr;   // for the guards; may be null
  std::size_t n_target = 0;
  seconds span = 0;                           // the lookback's length
  seconds lag = 0;                            // the gap between the anchor and the lookback's end
  std::int32_t n_bin = 1;                     // sub-bins inside the lookback, oldest first
  std::vector<Stat> stats;
};

struct LookbackResult {
  std::vector<double> values;       // [target, bin, channel], target fastest
  std::vector<std::int32_t> bin_n;  // [target, bin], target fastest
};

// Throws Error for a span that does not divide into the bins asked for, for a (target, bin) cell
// holding no readings, and for a day-level statistic over bins that do not each hold whole
// calendar days.
LookbackResult reduce_lookbacks(const LookbackRequest& req);

}  // namespace timesift

#endif  // TIMESIFT_TS_CORE_H
