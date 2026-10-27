# The representation contract

Normative for both implementations. R and Python must produce the same numbers from the same
input; where this document and either implementation disagree, this document is right.

## Input

A long table of readings with three columns of interest:

| column | type | meaning |
|---|---|---|
| id | character, factor, or a whole number | the unit carrying the sensor (a plot, a site, a device) |
| time | POSIXct | the instant of the reading, read on the clock **The time zone** names |
| value | numeric | the reading |

An identifier is a name, and the two languages have to write the same name for the same value.
A character id is itself and a factor is its label. A whole number is its digits, with no exponent
and no decimal point, so a plot read as `100000` from a file is `100000` in both, rather than R's
`1e+05` beside Python's `100000.0`. A number that is not whole has no such writing and is refused,
naming the column, as is a column of any other type. This holds wherever an identifier names
something: the id column of the readings, of the targets, of the target table a lookback is
anchored by, and of a response.

Requirements, each checked and each an error rather than a warning:

- No missing `id` or `time`.
- Every `value` is a finite number. A missing one propagates through a sum and is skipped by a
  comparison, so a bin's mean and its minimum would disagree about which readings were in it, and
  an infinity makes the mean of a bin holding both signs a not-a-number. The reading columns are
  read by the shared core, so this is one guard rather than one per language, and its message names
  the unit and the instant of the first reading it refuses.
- No duplicate `(id, time)` pair.
- Every id spans the same set of bins once binned. A record that stops early is not silently
  padded; it is reported with the ids and the bins concerned.

Ordering of the input rows carries no meaning, exactly and not approximately: both reductions walk
the record by unit, then by the local clock, then by the instant, rather than in the order the
caller wrote it, so a record and any permutation of its rows reduce to the same bytes and reach the
same digest. Addition is not associative, and a reduction that accumulated in the caller's order
would move in its last bits under a permutation that changed nothing about the record. The instant
is the third key because two readings can share a local second, on the night a zone sets its clock
back, and an order that left them tied would leave their sum to whatever the sort did with the tie.
A record already in that order is the common case and pays the scan that establishes it. The
output is ordered by sorted unique id and by bin start.

## Ordering identifiers

Every place an identifier decides a position -- the ids naming the first dimension of the
representation, and the variable names naming the cells of the scorable mask -- they are sorted by
**C collation**: the byte order of the UTF-8 encoding, which for every code point is also the code
point order. It does not depend on the session's locale, on the machine, or on the language.

That has to be stated because the two languages' defaults disagree and so do two R sessions in
different locales. R's `sort()` follows `LC_COLLATE`, which in an English locale orders `_z` before
`a` before `A`, and NumPy's `np.unique()` orders by code point, which puts `A` first. Both
implementations therefore name the rule rather than take a default: R passes
`method = "radix"`, which sorts characters in the C locale whatever `LC_COLLATE` says, and NumPy
already sorts this way.

The failure this prevents is silent rather than loud. Two orderings hold the same numbers in
different rows, so a response matrix built in one language and a representation built in the other
line up row for row while naming different units, and nothing errors. `series_order.csv` carries
ids the two rules order differently, so the fixtures fail rather than the user.

The channel names are never sorted: the channel order is the order the caller named the statistics
in.

## The time zone

Bin membership is decided from the calendar the series is carried in, so the zone has to be named
before anything is binned. It is named once, at the edge of each implementation, and nothing below
that edge knows a zone exists: the binning and the reduction see only *naive local seconds*, the
count of seconds from 1970-01-01T00:00:00 on that calendar. A day there is 86400 of them whatever
the night did, and a month is what the proleptic Gregorian calendar says.

| | how the zone is named |
|---|---|
| R | the `tzone` attribute of the `POSIXct` column; unset means UTC |
| Python | the zone the time column carries, where it carries one, else the `tz` argument; `None` and no zone on the column mean the instants already read as the calendar to bin by, which is what a zone-free `datetime64` says |

The same instants and the same zone give the same answer in both languages, and the fixtures pin
that rather than leaving it assumed. On the Python side reading the column as instants drops the
zone it was written on, so it is taken off the column before that; a `tz` naming a different zone
beside one the column carries is an error, because two zones are two answers. A zone name the
database does not know is an error on both sides, never a warning and a calendar in UTC.

The `native` grain is the one grain not read on that clock. Its bin is the reading itself, and a
reading is its instant: the two readings of the hour a zone repeats when it sets its clock back
share a local second and are two bins, not one bin holding two readings, so the record read at
`native` is the same array whichever zone it is carried in, and its bin starts are the instants
themselves.

Reading an instant as a clock is defined for every instant in every zone. The reverse is not: on
the night a zone moves its clock forward a local time exists on no instant, and on the night it
moves back a local time exists on two. A bin start is a local time, so reporting it as an instant
needs a rule, and the rule is that **a local time the clock skipped resolves to the instant the
clock jumped to, and a local time the clock repeated resolves to the first of the two**. In
`America/Sao_Paulo`, whose clock moved at midnight until 2019, the day beginning 4 November 2018
therefore opens at 01:00 local rather than at a midnight that never happened, and holds 23 readings
rather than 24.

Instants are read at whole seconds. Two readings a fraction of a second apart are the same reading
twice, and are reported as a duplicated `(id, time)` pair.

## Grains

Bin membership is read from the calendar rather than from a running count of hours, so a bin is a
real week or a real month rather than a drifting block of 168 or 730 hours.

| grain | bin |
|---|---|
| `native` | the reading itself, no reduction |
| `halfday` | 00:00-11:59 and 12:00-23:59 of each calendar day |
| `day` | the calendar day |
| `week` | ISO week, Monday to Sunday |
| `month` | the calendar month |
| `season` | three calendar months, counted from `year_start` |
| `year` | the year running from `year_start` |

`year_start` is a `"MM-DD"` string, default `"09-01"`. It sets the boundary of the hydrological
year and therefore also the phase of the seasonal bins. A seasonal bin is three calendar months
counted from that anniversary, so a record of three hydrological years beginning on it holds
twelve of them and no partial one. Cutting seasons anywhere else, at the equinoxes and solstices
for instance, is a different calendar and is passed as a function; see Custom bins.

Bins are contiguous and cover the record with no gap and no overlap, and both halves of that are
asserted:

- every `(id, bin)` cell holds at least one reading, which is what makes a bin the same span for
  every id and a record that stops early an error rather than a padded row;
- consecutive bin starts are one bin apart on the grain's own calendar. A bin no id reaches is
  never built, so a February missing from every logger would otherwise give four "adjacent" monthly
  bins with February simply gone, and a convolution would read January and March as neighbours.

The second is not asserted for `native`, where the bin is the reading itself and the bin sequence is
the record's own sampling grid rather than a calendar, nor for a supplied calendar, which declares
its own bin lengths and is contiguous by construction. It is asserted in local time, so a sequence
stepping across a clock change is contiguous: the civil day a zone shortened is still one bin of
one calendar day.

## Partial bins

A bin is **partial** when the record does not cover its whole calendar span. The record covers
from its first reading to its last plus one sampling interval, taken as the smallest gap between
consecutive distinct reading instants, and a bin is partial when its own span reaches outside
that. Only a bin at an end of the record can, because every id is required to hold readings in
every bin between them.

Which bins those are follows from where the record starts and stops against the calendar, not from
the grain alone. Three years of hourly readings from 1 September on a `"09-01"` boundary carry no
partial month, season or year, and a partial week at each end, because 1 September 2021 is a
Wednesday. A record from an arbitrary deployment date carries one at each end of almost every
grain.

`grain_matrix()` reports the verdict as `bin_partial` and takes a `partial` argument saying what
becomes of such a bin: `"keep"`, the default, returns it alongside the full bins; `"drop"` removes
it, and errors rather than returning an empty representation if that leaves no bin. Dropping is a
choice about the record, not about the implementation: it discards up to three months of readings
at each end of a seasonal grain, while keeping gives a bin whose mean is taken over fewer readings
and whose `cold_day` and `warm_day` are drawn from fewer days, so they sit closer to that bin's
mean than a full bin's would. `bin_n` gives the count each bin was reduced from.

Both implementations obey the same rule and the fixtures pin both settings.

## Statistics

Each named statistic becomes one channel of the output. A grain may carry any subset, and the
channel order in the output is the order given by the caller.

| name | definition | defined for |
|---|---|---|
| `mean` | arithmetic mean of the readings in the bin | every grain |
| `min` | smallest single reading in the bin | every grain |
| `max` | largest single reading in the bin | every grain |
| `cold_day` | smallest daily mean among the days in the bin | `day` and coarser |
| `warm_day` | largest daily mean among the days in the bin | `day` and coarser |
| `mean_daily_min` | mean over the bin's days of each day's smallest reading | `day` and coarser |
| `mean_daily_max` | mean over the bin's days of each day's largest reading | `day` and coarser |

"A day or coarser" is decided from the bins rather than from the grain's name: a day-level
statistic requires every calendar day of the record to lie entirely inside one bin. Naming `native`
or `halfday` is refused before any data is read; a supplied calendar that cuts inside a day is
refused once the bins are in hand, naming the day it splits and the two bins it splits it between.

The four day-level statistics reduce each calendar day first and then reduce again over the days of
the bin. `cold_day` and `warm_day` take the extreme of the daily means; `mean_daily_min` and
`mean_daily_max` take the mean of the daily extremes. They are not `min` and `max`, which act on
single readings, and the difference is the point: an extreme day is a state the site was in, an
extreme reading can be one hour, and an average daily extreme is the exposure a typical day of the
bin brought.

Two orderings follow from the definitions, and both test suites assert them on every bin of
every grain, on the core's output and on the oracle's alike:
`min <= mean_daily_min <= mean <= mean_daily_max <= max` and
`min <= cold_day <= mean <= warm_day <= max`. The two day-level pairs are not ordered against each
other, and a bin whose days differ widely in level is where they part: a bin of one day at 0 and
one at 10 has `cold_day` 0 and `mean_daily_min` 5.

At the `day` grain `cold_day`, `warm_day` and `mean` coincide by construction, as do
`mean_daily_min` with `min` and `mean_daily_max` with `max`. Requesting them there is allowed and
returns the identical channels.

## Custom bins

A caller may supply the binning instead of naming a grain, as a function of the reading instants
returning the start of each reading's bin. Everything downstream is unchanged: the bins are still
required to tile the record, and the output still carries the bin starts as its second dimension's
names. This is how a calendar the package does not carry is used, such as seasons cut at the
equinoxes and solstices rather than on the first of a month.

Such a calendar owns its own bin lengths. A record beginning inside one of its seasons gives a
leading bin of a few weeks beside neighbours of three months, and that is the calendar the caller
asked for rather than a bin the record failed to fill: it is what makes three years cut at the
equinoxes thirteen bins where three calendar months from `"09-01"` are twelve. The function
declares where its bins begin, so the package cannot know where the last one was meant to end and
takes the record's end as its end; that final bin is never reported partial, and a leading bin is
judged as any other.

The function must return a bin start for every reading, including any that precede its first
boundary. Deciding that with an interval lookup is the natural way to write one and the two
languages disagree below the first boundary, where R's `findInterval()` gives 0 and NumPy's
`searchsorted() - 1` gives -1: the first silently shortens the result, and the second silently
wraps to the last boundary. Either put the first boundary at or before the record's first reading,
as the fixtures do, or handle the readings below it explicitly. A reading the function returns no
bin start for, an `NA` in R or a `NaT` in Python, is refused before the bins are read, naming how
many there are and the unit and instant of the first: below the boundary a missing time is not
distinguishable from one at the beginning of time, and the bin it would open there would hold the
reading its real bin then lacks.

Two things have to hold of what the function returns, and both are checked before its bins are
read as bins, naming the first reading that breaks them:

- A bin begins at or before every reading it holds. A calendar shifted by one boundary breaks it,
  and so does the wrap above, whose bin start is the last edge of all and therefore later than the
  reading it was asked about.
- A bin's readings are a stretch of the record. A calendar sending alternate readings to two bins
  breaks it, and the two bins the array then carries are not bins the record ever had.

Neither is visible to the empty-cell guard or to the contiguity rule, which read the bins the
calendar declared and can only ask whether every unit reaches each of them.

## Output

A numeric array of shape `[n_id, n_bin, n_channel]`.

- Dimension 1 is named by the sorted unique ids.
- Dimension 2 is named by the ISO-8601 timestamp of each bin's start, in UTC.
- Dimension 3 is named by the statistic.

Attributes carried on the array:

| attribute | content |
|---|---|
| `grain` | the grain name |
| `stats` | the statistic names, in channel order |
| `year_start` | the `"MM-DD"` boundary used |
| `bin_start` | the bin start instants, resolved from local time as **The time zone** describes |
| `bin_end` | the last reading instant assigned to each bin |
| `bin_n` | a `[id, bin]` matrix of how many readings fell in each cell |
| `bin_partial` | a logical vector marking the bins the record does not cover for their whole calendar span |

No standardisation, centring or scaling happens here. Scaling is a property of a fit and belongs
to the fold it is computed on, never to the representation, because computing it over all ids
would leak the held-out units into the training input.

## The lookback

The second reduction, and the one no calendar expresses. Its input is a table of **targets** beside
the readings: one row per thing to predict, carrying the unit whose record it reads and the instant
it is anchored at. Two targets on the same unit a fortnight apart read two different stretches of
that unit's series, which is why the bins are placed relative to the target rather than to a month
or a week.

| column | type | meaning |
|---|---|---|
| id | as the readings' id | the unit, which the readings must carry |
| at | POSIXct | the instant the target is anchored at |

Both columns are read **by name**, as every alignment in the package is, and a table missing either
is refused. A table carrying more columns than these two is read for these two.

A target's identity is its position in that table. A unit may carry any number of targets, so the
unit cannot name a row, and the output is in the table's own row order rather than in a sorted one.

### The bin arithmetic

Three numbers describe a lookback: `span`, its length; `lag`, the gap between the anchor and the end
of the lookback; and `bins`, how many sub-bins it is cut into, oldest first. With `a` the target's
anchor and `step = span / bins`, bin `b`, counted from zero, covers naive local

    [a - lag - span + b * step,  a - lag - span + (b + 1) * step)

closed at the left and open at the right, so a reading on a boundary belongs to the later bin.
`span` must divide by `bins` exactly; one that does not is an error rather than a rounded step.
Only the readings whose unit is the target's are read: a lookback is a stretch of one unit's record.

Nothing else changes. The statistics are the seven **Statistics** defines, computed over the
readings of a cell exactly as they are over the readings of a grain's cell, and the day-level pair
reduces the days of a bin oldest first.

### The two guards

- Every `(target, bin)` cell holds at least one reading. A lookback reaching past either end of the
  record is an error naming the target and the interval, never a padded row, for the reason a
  grain's empty cell is one: an invented value in front of a model is worse than a target the
  record cannot answer for.
- The four day-level statistics reduce each calendar day first, so they are defined only where
  every calendar day lies whole inside one bin. A calendar settles that by itself; a lookback has to
  be asked, and the answer is two conditions rather than one. `step` must be a whole number of
  days, which holds for every target or for none, and `a - lag - span` must fall on a day boundary,
  which is a property of the anchor: a table of anchors on the hour rules the four statistics out
  where the same anchors at midnight allow them. Either failing is an error naming the target.

### Durations

`span` and `lag` are a count and a unit, or a bare count of seconds. **A year is 365 days and a
month is 30 days.** A lookback of a fixed length is a fixed length rather than a calendar step:
comparing two targets' representations means each read the same amount of record, and a February
or a leap year takes that away. Where the calendar is what matters, a grain is the reduction that
follows it.

The length is measured on the local clock, as everything below the zone boundary is. In a zone
that moves its clock, a lookback of one day ending at a local midnight holds the whole local day
before it, which is 25 hours of record on the night the clock is set back and 23 on the night it
is set forward; a lookback that spans neither night holds 24. That is what keeps a calendar day
whole inside a bin for the four day-level statistics, which a length fixed in instants could not:
a day-level lookback anchored within `span` after a transition would then have to be refused. The
difference is one hour twice a year, against the whole day a February differs by.

| unit | seconds |
|---|---|
| `second`, `seconds` | 1 |
| `minute`, `minutes` | 60 |
| `hour`, `hours` | 3600 |
| `day`, `days` | 86400 |
| `week`, `weeks` | 604800 |
| `month`, `months` | 2592000 |
| `year`, `years` | 31536000 |

A string is a count, optional space, and one of those names, upper or lower case: `"30 days"`,
`"12 hours"`, `"1 year"`. A string of digits alone is seconds, and so is a number. Nothing else
parses, and a duration that does not parse is an error naming the argument.

### The output of a lookback

A numeric array of shape `[n_target, n_bin, n_channel]`, traversed and digested exactly as a
grain's is, target fastest.

- Dimension 1 is named by the target's own row label where the target table carries one, and by its
  position, from 1, where it does not. R takes the row names of the `at` data frame; Python has no
  row names and always names by position.
- Dimension 2 is named by where the bin opens relative to the anchor, oldest first, written as a
  signed count and the coarsest of `day`, `hour`, `minute` and `second` that divides it exactly,
  singular at one: `-30 days`, `-1 day`, `-12 hours`. In a grain an instant names a bin; here no
  two targets share one.
- Dimension 3 is named by the statistic.

Attributes carried on the array:

| attribute | content |
|---|---|
| `grain` | `"lookback"` |
| `span`, `lag` | the durations, resolved to seconds |
| `bins` | how many bins the lookback was cut into |
| `stats` | the statistic names, in channel order |
| `bin_n` | a `[target, bin]` matrix of how many readings fell in each cell |

There is no `bin_start`, no `bin_end` and no `bin_partial`. A bin is a position relative to an
anchor rather than a span of the calendar, and a cell the record does not cover is an error rather
than a verdict. R carries no such attribute and Python carries them as `None`; a column of
not-a-time beside a column of `FALSE` would read as an answer to a question the reduction does not
ask.

## The channels

An array of readings is not the only thing a learner is handed. Two more reductions put a channel
beside those readings, and both return an array a model reads, so both are normative here.

### Where a bin sits in the year

`calendar_channels()` reads a representation and returns an array of the same units and bins with
two channels, `year_sin` and `year_cos`, identical across units. An encoder that ends in global
pooling discards when in the record a thermal event happened, so the position of a bin in the year
is given to it as an input or it is not used at all; it is the time index of each bin rather than a
summary of the readings, and a sine and a cosine rather than the fraction itself because the two
are continuous across the turn of the year where the fraction jumps.

For each bin, with `bin_start` and `bin_end` the instants the representation carries:

    mid  = bin_start + floor((bin_end - bin_start) / 2)
    y    = the calendar year mid falls in, on the proleptic Gregorian calendar in UTC
    frac = (mid - first instant of y) / (length of y in seconds)

    year_sin = sin(2 pi frac)
    year_cos = cos(2 pi frac)

Four things in that are decisions rather than consequences.

The position is read at the **midpoint of the record the bin holds** -- `bin_end` is the last
reading assigned to the bin, not the end of its calendar span -- so a bin the record only partly
covers sits at the phase it was actually measured over rather than at the phase of a whole one.

A bin spanning an **odd number of seconds** has its midpoint on a half second, and the second it
began on is the one it is read at. Every named grain spans a whole number of hours, so nothing
reaches that today; a supplied calendar need not, and the rule is written down rather than left to
whichever language happens to round.

The year is read **in UTC**, on the instants, whatever clock the bins were placed on. The phase is
a place on the orbit rather than a reading of a clock, and a zone moves it by its offset: under a
day on a cycle of a year, the same shift for every bin of the record.

`frac` is **exact arithmetic on the calendar** and the same bits on every platform. The sine and
the cosine of it are the platform's library, accurate to about an ulp and no further, so the
contract pins `frac` and states a tolerance of **1e-12** on the two channels. Both suites read the
fraction back and assert the channels against it.

The result carries every attribute of its input, with `stats` replaced by `year_sin, year_cos`, no
`static` channel, and `position` naming every channel it holds. A
lookback is refused: its bins are placed relative to a target rather than on the calendar, so they
have no position in the year, and it carries no `bin_start` to read one from.

### Where a bin sits in the day

`calendar_channels()` takes `cycles`, the cycles to place each bin in, `year` by default. Naming
`day` adds two channels, `day_sin` and `day_cos`, read at the same midpoint:

    mid  = bin_start + floor((bin_end - bin_start) / 2)
    frac = (mid mod 86400) / 86400

    day_sin = sin(2 pi frac)
    day_cos = cos(2 pi frac)

with `mod` the floored remainder, so an instant before 1970 sits at its place in its own day. The
channels come two per cycle in the order `cycles` names them, and `stats` names them the same way.

The day is read **in UTC**, as the year is. A zone moves the phase by its offset and a site's
longitude moves the solar day against UTC by a fixed amount; both are the same shift for every bin
of a record, which a model absorbs. A local clock's summer time would instead move the phase by an
hour twice a year, which is a change in the input rather than a rotation of it.

The day cycle is **refused on bins that sit a day or more apart**, read as the smallest gap between
consecutive `bin_start`s, and on a representation of fewer than two bins, where no gap can be read.
At a day or coarser every bin would sit at the same place in the day, and a constant channel is
not a position. The half-daily grain passes and alternates between the two halves of the day,
which is what it records. An unknown cycle name is refused. The fraction is pinned exactly and the
two channels to the same **1e-12** as the year's.

### Putting channels side by side

`bind_channels()` takes two or more representations of the same units and bins and returns one
array carrying every channel, in the order the arguments are given and, inside each argument, in
its own channel order. It is how a temperature reading, an external product such as snow cover, and
the calendar position of each bin reach a model as one input.

The result carries the **first argument's** attributes, with `stats` the joined channel names.
Every other argument is read for its channels alone; nothing of its own binning survives, which is
why the units and the bins have to agree in the first place. The two attributes that name channels
rather than bins, `static` and `position`, name those of every argument, in the order the channels
come.

Four inputs are refused, and the three that concern one argument name its position from one:

| what | the message both raise |
|---|---|
| fewer than two arguments | `` `bind_channels()` needs at least two representations `` |
| an argument that is not a representation | `argument 2 is a ..., not a representation` |
| an argument covering other units or bins | `argument 2 covers different units or bins from the first` |
| two arguments carrying a channel of one name | `two representations carry a channel of the same name: mean` |

## What crosses the language boundary, and what does not

The binning and the reduction are one implementation: `src/ts_core.cpp` and `src/ts_calendar.cpp`,
compiled into the R package by R itself and into the Python extension by CMake. So is the
penalised fit, `src/ts_penalised.cpp`, for the same reason: it is the arm the networks are
measured against, and a baseline that moved between the languages would make the tool the
confound. What each language holds above them is the boundary, which resolves the columns,
resolves the zone, deals the folds and wraps the result. The two agree by construction rather
than by two implementations being checked against each other after the fact.

The digests did not stop meaning anything when that happened. The implementations they used to
compare are kept as test oracles, `tests/testthat/helper-oracle.R` and `python/tests/oracle.py`,
reachable from neither package at runtime and exercised only against the core on the fixtures and
on random series. The NumPy one was written from this document rather than from the R source, which
is what makes it evidence that the document is complete. One implementation in production, two in
evidence.

The representation is normative and is checked byte-exactly. Three things beside it are artifacts
that both sides read rather than each side computing: the response matrix, the fold map, and the
mask of scorable cells that follows from those two. A fold map built from a seed in R and a fold
map built from the same seed in Python are different maps, because the two languages draw on
different random streams; the fix is not to align the streams but to build the map once and read
it in the other language. **The file format below is what makes that possible**, and both sides
carry the reader and the writer for all three.

A model fitted in one language and a model fitted in the other cannot be byte-identical and are
not required to be. The penalised fit is the one model that is: it goes through the shared core,
so the two sides return the same coefficients from the same design, and what is stated below is
how far either sits from glmnet rather than from the other.

## The file format of the three artifacts

One format for all three: CSV, UTF-8, a header row, `,` as the separator, no quoting, and LF line
endings on every platform. Written the same way in either language, the same artifact gives the
same bytes, so a round trip through a file is checkable and is checked.

A number is written with `%.12g`: twelve significant digits, which renders `0` and `1` as `0` and
`1` and carries any measurement a response holds. A logical is written `TRUE` or `FALSE`, and is
read from either that or `1`/`0`.

### The fold map: `id,fold`

| column | type | |
|---|---|---|
| `id` | character | the unit |
| `fold` | integer | the fold it is held out in, from 1 |

One row per unit, ordered by the id under **Ordering identifiers**. A unit may appear once.

### The response matrix: `id` and one column per variable

| column | type | |
|---|---|---|
| `id` | character | the unit |
| each remaining column | numeric | that variable's value at that unit |

The columns after `id` are the variables, **in the file's own order**, which is the order the
response carries them in; they are not sorted, because a response's column order is the caller's.
Rows are ordered by the id. A presence-absence response holds `0` and `1` and nothing else, and is
checked for that when it is prepared rather than when it is read.

### The scorable mask: `variable,fold,n_occ,pres_train,abs_train,pres_test,abs_test,scorable`

One row per `(variable, fold)` cell, ordered by variable under **Ordering identifiers** and then
by fold ascending. The seven columns after `variable` are integers except `scorable`, which is a
logical. The mask is a pure function of the response and the fold map, so it can be recomputed
rather than carried; it is written because reading it is how a language that did not build it gets
the exact cells the other one scored on.

### A unit the file does not carry

Aligning any of the three to a representation's units is by name, never by position. A unit in the
representation that the file has no row for is an error, reporting how many are missing and naming
the first of them in the representation's own order. A unit the file carries that the
representation does not is dropped without comment: a fold map covering a whole study is a normal
thing to read a subset of.

## Fixtures

Four series, because a record that starts on a bin boundary cannot tell two binning rules apart,
a record in UTC cannot tell two readings of a zone apart, and identifiers that agree under every
collation rule cannot tell two row orders apart.
`spec/fixtures/series.csv` is a synthetic three-unit, 400-day hourly series beginning at midnight
on the default anniversary, so every coarse grain is in phase with it from the first reading.
`series_offset.csv` is a two-unit, 200-day series beginning at 05:00 on 17 October, which is what
a logger deployed when someone could walk to it gives, and puts every grain out of phase.
`series_zoned.csv` is a two-unit, 10-day series across 4 November 2018, the night
`America/Sao_Paulo` moved its clock at midnight, which is the record that tells a calendar read by
arithmetic apart from one read by writing a local midnight and parsing it back.
`series_order.csv` is a five-unit, 30-day series whose identifiers C collation and an English
locale order differently, `A1 P10 P9 _x a1` against `_x a1 A1 P10 P9`, and which arrive in a third
order again. `seasons.csv` holds
the equinox and solstice boundaries that make each series a caller-supplied calendar, which is the
only path the manuscript's seasonal rung ever took.

The digests are the core's own output: `inst/spec/make_fixtures.R` loads the R package and calls
the same public functions a user calls. They pin a regression, not an agreement between two
implementations. The evidence that the two agree is the oracles, checked against the core
separately, on these fixtures and on random series.

`digests.csv` holds one row per series, grain, time zone, `year_start`, `partial` setting and
statistic, covering every grain-by-statistic combination, each of the three-channel schemes
(`min+mean+max`, `mean_daily_min+mean+mean_daily_max`, `cold_day+mean+warm_day`), the coarse
grains at anniversaries other than the default, both `partial` settings, the supplied calendar over
the same grid of statistics a named grain is read at,
and the zone: every grain of the aligned series read as a `Europe/Vienna` clock, which moves twice
inside that record, and the short series read as an `America/Sao_Paulo` clock, which moves at
midnight inside it, including a `year_start` landing on the night it moves. Each row carries `n_unit`, `n_bin`, the first and last bin start, how many bins are
partial, and the digest.

`coverage.csv` holds what `coverage()` reports, which is the same binning laid out as a count of
readings per `(unit, bin)` and the one reduction no digest above reaches: every series here is
complete, and a gap is what that table exists to show. Each case names the readings it takes out,
by unit and by span, so both suites build the same record; a case takes none, a case takes a
month from one unit, a case takes a span from every unit so that the calendar tiles over a bin no
unit reaches at all, and a case reads a supplied calendar. Each row carries the shape, the first
and last bin, how many cells are empty, how many units have a gap, how many bins no unit reaches,
and the digest of the count matrix.

`grain_guards.csv` holds one case per guard on a supplied calendar, each naming the series and the
calendar that breaks it beside the substring of the message both implementations must raise. The
calendars are named rather than written out, so both suites build the same function: `late` gives
every reading the midnight after it, `(floor(t / 86400) + 1) * 86400`; `alternate` sends
consecutive readings to two bins an hour apart, `t0 + 3600 * (((t - t0) / 3600) mod 2)` with `t0`
the record's first reading; and `missing` bins by the calendar day but returns no bin start,
`NA` or `NaT`, for the reading exactly one day after the record's first.

The lookback reads the same series and three files of its own. `lookback_targets.csv`
holds the anchors, in named sets rather than one set per series, because an anchor that is a local
midnight in one zone is not one in another and an anchor on the hour rules out the day-level
statistics that a midnight allows: `aligned` and `offset` sit on day boundaries in UTC, `hourly`
sits on the hour, and `zoned` sits on local midnights in `America/Sao_Paulo`. `lookback_digests.csv`
holds one row per target set, zone, span, lag, bin count and statistic, covering every statistic
and each of the three-channel schemes, lags of none and of a day and of half a day, one and three
and four and seven bins, a step that is a whole number of days and one that is not, a span written
as a bare count of seconds, and the zoned set read both as UTC and as the clock that moves inside
it. Each row carries `n_target`, `n_bin`, the first and last bin's offset, the unit of the first
and last target, and the digest. `lookback_guards.csv` holds one case per guard -- a lookback reaching
past the record, a day-level statistic over bins shorter than a day, and one over bins that do not
open on a day boundary -- with the substring of the message the two implementations must both
raise.

`channels_digests.csv` and `channels_guards.csv` pin the two channel functions the way the
reductions are pinned rather than each side testing itself. A `calendar` row is a representation's
two channels and a `bound` row is its readings with those channels beside them, over the grains
whose bins are a day, a week, a month, a season, a year and a supplied calendar's, a record out of
phase with all of them, both anniversaries of the two grains that count from one, both `partial`
settings, the two zones, and the two bin widths no other row reaches: the `native` bin, whose
start and end are one instant, and the `halfday` bin, which is narrower than a day. What
each row digests is the year fraction, not the sine and the cosine of it, for the reason **The
channels** gives; the row carries the tolerance the two channels are then asserted against.
`channels_guards.csv` names each refused input and the substring of the message both
implementations must raise, and both suites build the input from the case name: `x` at the weekly
grain of the aligned series, `other` at its monthly one, and `back` a 30-day lookback anchored on
its last reading.

Both test suites read the series, rebuild every row and assert all of it. The shape is asserted
before the digest, so two implementations that put the record into a different number of bins are
reported as that rather than as an unexplained hash mismatch. A contract that carried one series
starting on the anniversary, at the default anniversary, with no supplied calendar and no bin
count beside the hash would pass while the two sides disagreed on how many seasons three years
hold, which is the whole thing it exists to prevent.

The digest is defined byte-exactly, because a scheme that varies by platform pins nothing:

1. Traverse the array in its own order: unit fastest, then bin, then channel.
2. Format each value with `%.12f`.
3. Join with a line feed, terminate with one, encode UTF-8.
4. MD5 of those bytes.

The line ending is LF on every platform. R's `writeLines()` emits CRLF on Windows, so the bytes
are written explicitly; a digest generated on Windows and checked on Linux must agree.

The array has to be finite, and an array holding an infinity or a missing value is refused rather
than digested. `%.12f` writes an infinity as `Inf` in R and as `inf` in Python, and R tells `NA`
apart from `NaN` where Python has one spelling for both, so any pinned rendering would either
compare the language or conflate two values R keeps distinct. Nothing built here reaches that case:
a reading that is not a finite number is refused where the record is read, which is the guard
below.

Twelve places is far below any difference that could change a fitted model and far above the
noise from the two languages accumulating a mean in different orders. Should a combination ever
straddle a rounding boundary at the twelfth place, the fix is to record that combination's
tolerance in this document, not to loosen the scheme for everything.

A digest that moves without a matching change to this document is a bug in whichever
implementation moved. Regenerating the fixtures is a deliberate act with its own commit.

## What else is pinned

The representation is not the only deterministic thing the two languages share, and a contract
that pinned it alone would let everything above it drift. Four more fixtures, generated by the same
script and read by both suites:

`response.csv`, `folds.csv` and `cells.csv` hold one response of 40 units by 6 variables, a fold
map of five folds over it, and the mask that follows. The variable names order differently under
C collation and under an English locale, and the prevalences are chosen so the mask is not all
`TRUE`: one variable is present nowhere and one everywhere, so neither has a scorable cell at all,
and a rare one is scorable in some folds and not others. Both suites read the response and the
fold map, recompute the mask, and assert it cell by cell. Both also write all three back and
assert the bytes, which is what makes the file format a contract rather than a convention.

`metric_cases.csv` and `metrics.csv` hold ten `(y, p)` cases and the value of every threshold
metric on each: `tss`, `roc_auc`, `average_precision`, `kappa` under both rules, and
`decision_threshold` under all three. The cases are where the tie rule is the whole answer -- every prediction tied, ties within
a class, ties across the classes, one presence, one absence, all presences, all absences, a
perfect separation and a reversed one. A metric a case defines no value on is written `NA` rather
than left out, so a suite that quietly skipped it fails rather than passes.

`contrast_cells.csv` and `contrast.csv` hold a fixed table of per-cell scores for two arms, with
cells one arm scored and the other did not, and the paired contrast read off it. No model is
involved: the pairing, the per-variable mean, the interval on Student's t with one degree of
freedom fewer than there are variables, and the signed-rank p-value with the method it was read by
are what the two languages own, and a fitted model is what they are not required to share. The
p-value is exact below fifty per-variable differences holding no zero and no tie, and the normal
approximation with continuity and tie corrections otherwise.

### How exactly

Everything named above is **byte-exact**, at the twelve significant digits the format writes. It
is arithmetic on the same finite inputs in both languages, so anything less would be a difference
worth finding rather than a tolerance worth allowing. The one exception is the signed-rank
p-value: it is exact where the exact distribution applies, which is fewer than fifty values with
no tie, and the Python side reaches the normal approximation beyond that through a Chebyshev fit
to the complementary error function accurate to about 1.2e-7 relative. The fixture stays on the
exact branch; where a caller lands on the other, the two agree to 1e-6 and no closer.

`tss_inflation()` cannot be pinned as a digest, because it draws replicates from each language's
own random stream, and aligning those streams would be the wrong fix for the same reason it is the
wrong fix for a fold map. Its inflation figure is a headline claim of the package, so what is
required of it is stated rather than left to a hand check: **at 200 replicates the two
implementations agree on the inflation to within 0.02 at each planted skill**, which is well
inside the Monte Carlo error of either one alone and far below the +0.110 the claim rests on. A
disagreement beyond that is a bug in one of them, not sampling.

## The penalised fit

`elasticnet()` is one elastic net per response, over `src/ts_penalised.cpp`, which both languages
compile. Its conventions are glmnet's, because that is what the arm has always been and the
acceptance criterion for replacing it was agreement with it rather than an elastic net of our own.

- Case weights are normalised to sum to one, and the objective is the mean deviance halved for a
  Gaussian family and the mean negative log likelihood for a binomial one, plus
  `lambda * (alpha * sum |b| + (1 - alpha) / 2 * sum b^2)`.
- Every column is centred on its weighted mean and divided by its weighted standard deviation,
  taken with those weights and no correction for degrees of freedom. A Gaussian response is
  centred and scaled the same way, which is what puts the reported penalties on the response's
  own scale. A column holding one value has no spread to divide by, is left out of the descent,
  and is reported at zero.
- A penalty factor, where one is given, is rescaled to sum to the number of columns, so a factor
  of one everywhere gives the path no factor gives.
- The path is `n_lambda` penalties, geometric from the smallest that leaves every coefficient at
  zero down by a ratio of `1e-4` where there are more units than columns and `1e-2` otherwise.
  The largest is `max_j |g_j| / (vp_j * max(alpha, 1e-3))` on the standardised scale, with the
  floor under the mixing that gives a ridge a finite start.
- The path ends early where a step explains almost nothing more: a Gaussian family reads that
  share against the deviance explained so far and a binomial one reads it outright, which is the
  difference glmnet's two solvers carry. It also ends where a fit explains more than `0.999` of
  the null deviance. Neither rule is read before the fifth point, and neither applies to a path
  of supplied penalties.
- The fit is iteratively reweighted least squares with a cyclic coordinate descent inside it,
  warm-started along the path, restricted by Tibshirani's sequential strong rule and checked
  against the optimality condition on every column the rule discarded. A reweighted least squares
  step is judged on its own move, the coefficients it started from against the ones it reached,
  over the columns that have left zero and over the intercept, which is glmnet's test.
- The cycle over the columns that have left zero is Anderson-extrapolated every five passes
  (Bertrand and Massias, 2021): the affine combination of the last six iterates whose successive
  moves come closest to cancelling is taken where the penalised quadratic is lower there than at
  the cycle's own iterate. The objective only falls, and the descent still stops only on a pass
  that moved nothing.
- A Gaussian fit folds the root of each case weight into the standardised columns and into the
  intercept's column, so its quadratic has unit weight.
- A fit that does not settle at a penalty, inside `max_irls` reweightings or inside what is left
  of the path's `max_pass` passes, ends the path there: the points before it are the path, and
  `stalled` records the 1-based position of the penalty it did not settle at, `0` where the path
  ran to its end. That is glmnet's `jerr = -m`. A fit that does not settle at the first penalty
  has nothing to return and is an error. A cross-validation records each fold's `stalled` in fold
  order as `fold_stalled`, and reads a fold whose path ended early at its last point for every
  penalty below it, as glmnet's `predict` reads a truncated path.
- A cross-validated penalty fits each fold along a path of its own and reads it at the
  whole-unit path's penalties, interpolating between the two points around each, which is what
  `cv.glmnet` aligns on. The held-out deviance is averaged within a fold and then across the
  folds, weighted by each fold's weight, and its standard error is the spread across the folds
  over `nfolds - 1`. `lambda.min` is the largest penalty of least held-out deviance and
  `lambda.1se` the largest within one standard error of it.
- The folds are dealt by the caller and handed over as one 0-based index per unit, so a grouping
  the outer map keeps whole stays whole where the penalty is chosen. Nothing inside the core
  draws.
- The fit on every unit and the fit of each fold are independent of one another, so `threads`
  runs them at once. What comes back is the same numbers either way, to the bit: the fits share
  the design they read and nothing else, and the held-out deviance is summarised after all of
  them have finished, in fold order. Both suites assert it.

### The fixtures

`penalised_input.csv` holds the design both suites fit: a weekly representation of eighty
simulated records, flattened, with the square of every column beside it, which is what
`elasticnet()` penalises over. The squares are the scale case the standardisation exists for, a
reading of ten degrees and its square of a hundred, and the weekly bins of one record are
collinear the way adjacent bins are. Beside it are a binomial response, the continuous driver it
was generated from, a case weight per unit and a five-fold map.

`penalised_cases.csv` names twelve cases, each family at each of three mixings with and without
the weights, and carries the convergence threshold and the pass budget the reference was read at
and the tolerances a suite is allowed. `penalised_path.csv` holds glmnet's coefficients, its
intercept and the objective at every tenth point of each case's path, and `penalised_cv.csv` holds
the path's length and the penalty the cross-validation chose.

### How exactly

Not byte-exactly, and not by digest. Two implementations of a coordinate descent settle at the
same point to the tolerance they are run at and no closer, so what is required is a distance:

- **The path's length and its penalties are exact**, to `1e-10` relative. They follow from the
  gradient at the null model and a geometric ratio, so a difference there is a difference in the
  conventions rather than in the descent.
- **The objective is what is pinned tightly.** A suite's fit may sit no more than `1e-6` above
  glmnet's at the same penalty. Sitting below it is the fit being closer to the optimum than the
  reference, which is the direction the arm is allowed to move in; sitting above it is the arm
  being weakened, which is the thing this replacement was not allowed to do. Measured at a
  matched threshold of `1e-14`, the core sits between `3e-13` and `2e-7` above glmnet across the
  twelve cases.
- **A coefficient is allowed `1e-4`**, against the largest coefficient of the case. It is looser
  than the objective on purpose: two nearly identical columns split one coefficient between them
  differently in any two descents, and that split is not determined to the precision the fit is.
  On a design without collinear columns the two agree far closer, to `2e-6` at a matched
  threshold of `1e-14`.
- **The cross-validated penalty is exact.** `lambda.min` and `lambda.1se` are the same point of
  the same path in all twelve cases, and the held-out deviance agrees to `1e-4` relative.

One point of the path is different by construction, and only for a ridge. glmnet fits its first
point at a penalty of `9.9e35` and reports it under the largest penalty that would have left every
coefficient at zero. Above a mixing of zero those are the same fit, because the threshold holds
every coefficient down at that penalty; at a mixing of zero there is no threshold, and the core
solves the point it reports while glmnet reports a fit made at an infinite penalty. The fixtures
therefore start at the second point, and the difference at the first is asserted to be small
rather than absent.

The reference is generated with glmnet's own pass budget raised well above its default. At the
threshold the reference is read at, a collinear design runs past that default, and glmnet then
warns, truncates the path and returns the penalties it did reach; the generator refuses any
reference glmnet warned about, so a fixture can never encode a failure both implementations would
have to reproduce to match.

## What each language carries

The representation and the three artifacts are the contract. Everything built over them is meant to
match too, and where the two sides differ the difference is recorded here rather than found at a
call site.

### One name per concept

| concept | the name, on both sides |
|---|---|
| the whole run | `timesift()`, from a table of targets and a table of series to a scored comparison and a nested estimate of choosing among it |
| what a representation is | `native()`, `grain()`, `multigrain()`, `lookback()`, and the sets `grains()` and `lookbacks()` |
| coercing to a set of representations | `as_sift()`, from a representation, a list of them or a vector of grain names |
| the calendar-binned array | `grain_matrix()` |
| the target-anchored array | `lookback_matrix()` |
| an already-reduced feature table | `feature_matrix()`, a one-channel array with no time axis, so a published set of aggregates can be an arm beside a grain |
| which units reach which bins | `coverage()`, the count of readings per unit and bin over every bin the calendar tiles the record with, which is where a refused record's gaps are read off |
| building one representation | `build_representation()` |
| a channel added to an array | `bind_channels()`, and `calendar_channels()` for the sine and cosine of each bin's position in the year and, finer than a day, in the day, both as **The channels** defines them |
| the penalised learner | `elasticnet()` |
| the forward selector | `stepwise()` |
| the forest | `forest()` |
| the encoders | `mlp()`, `cnn()`, `rescnn()` |
| how an encoder is trained | `train_control()` |
| fitting one learner on one representation | `fit_learner()` |
| the resampling | `cv()` and `grouped_cv()` |
| the fold map and the mask | `fold_map()` and `scorable_cells()` |
| fitting across a set of grains | `grain_ladder()`, and `select_grain()` for the nested selection |
| the combiner | `ensemble()`, `ensemble_fit()`, `ensemble_combine()` and `ensemble_weights()` |
| scoring held-out predictions | `score_predictions()`, on the cells the mask allows |
| the metrics | `tss()`, `roc_auc()`, `average_precision()` and `kappa_score()`, with `decision_threshold()` and `model_agreement()` beside them |
| two arms on matched cells | `paired_contrast()` |
| the inflation of a self-selected threshold | `tss_inflation()`, and `implied_skill()` for the level it implies |
| a set of representations | `timesift_set()`, which reads as a mapping of grain name to representation |
| folds of the inner cross-validation | `n_inner` |
| the three artifacts | `write_folds()` and `read_folds()`, `write_response()` and `read_response()`, `write_cells()` and `read_cells()` |
| the digest | `digest_array()`, exported |
| the three registries | `register_learner()` and `learners()`, `register_metric()` and `metrics()`, `register_response()` and `responses()` |
| what a rare response weighs | `positive_weights()`, the case weights the shipped presence-absence head carries as its weights function and every learner that ships reads through the head |

### The same call does the same thing

- Naming one grain returns the representation and naming two or more returns a set, whether the
  one is named as a string or as a sequence of one.
- A guard above the core names what it refuses the same way: a missing identifier or instant
  names its column, a duplicated reading and a supplied calendar returning no bin start name the
  unit and the instant in UTC to the second, and the noun agrees with the count. `bins` given as a
  whole number in floating point is that whole number, as R reads `3` and Python reads `3.0`.
- A representation refuses a statistic its grain has no definition for, and a `year_start` that is
  not a month and a day, when it is constructed rather than when it is built. `"auto"` is the whole
  set the record supports and is refused beside a named grain, and the `stats` and `year_start`
  given to `grains()` reach every member of the set it returns.
- A representation anchored on the target and a run without `target_time` are refused against each
  other in both directions, over the members of the sift and the representations learners pinned
  themselves to alike. A learner's `data` is a representation or nothing; the name of a grain is
  not one.
- `grain_ladder()` and `select_grain()` left without a fold map build one with the defaults of
  `fold_map()`. The two languages draw different maps from the same seed, so where both must see
  one split, write it and read it back as the section above describes.
- `grain_ladder()` and `select_grain()` take a `control` as a run does, and hand it to every
  learner that declares one. A selection hands the same one to the inner search and to the refit.
- Held-out predictions are placed by unit and by variable, never by position.
- A learner is handed the whole response matrix and returns one column per response, whether it
  declares `joint` or `separate`. `multi` is what the learner says it does with that matrix and
  what a report says of the candidate; a learner fitting one model per response does that inside
  its own fit, so the block of predictors is built once for the fit rather than once for every
  response of it.
- A prediction that is not a number on a scorable cell is refused where it is scored, naming the
  arm, the cell and how many there are, rather than scored as the `NA` a one-class cell gives. The
  combiner is fitted on every scorable cell and refuses to drop one for the same reason.
- A setting given at fit time overrides the one the learner carries, and a setting the learner does
  not have is refused rather than ignored.
- The response head and the metric are registry entries. `metric` takes a registered name or a
  function of `(y, p)`, and left unset it is the one the response head carries, which for the
  shipped presence-absence head is `roc_auc`. Both travel with
  the fit: the function is what scores, and the name is what the report prints. A function has no
  name to print and reads as `<function>` on both sides rather than as whatever each language
  calls an anonymous one. `select_grain()` is the one door that takes a name only, because it
  reports the estimate under every registered metric and the one it selects on has to be a row of
  that table.
- An occlusion profile left without a `metric` is read by the one the fit was scored under, so a
  weight is a fall in the number the summary reports rather than in a second one. It reaches the
  response through the head the fit was made under, as everything else does, so a head that is
  not presence-absence is occluded like any other.
- An occlusion profile is read on the scorable cells alone, the mask of the response and the fold
  map, as every score is. A bin is held back whole: one permutation of the held-out units per draw
  moves every channel of the bin together, so no unit is shown a coldest day from one unit beside
  a warmest day from another. A channel identical across units, as the calendar channels are, is
  left in place when a bin is held back, since it says where the bin sits rather than what a unit
  read there; holding such a channel back across the record with `over = "channel"` is still
  asked for by name. The `fold_mean` and `unit_mean` substitutes draw nothing and are defined the
  same way on both sides; the `permute` substitute draws on each language's own random stream, so
  its weights are the same in distribution and not draw for draw.
- The encoders take `swa` and `swa_start`: the schedule anneals until the averaging begins and is
  then held flat, the averaged weights get their own pass to rebuild the batch-normalisation
  statistics from a reset, and the default is off, so a default recipe is the same recipe on both
  sides. `swa_start` is at least 0 and under 1 on both.
- What a rare response weighs is the response head's and not a training setting. The head's
  weights function returns one case weight per cell of the response, and the encoders, the
  penalised fit, the forest and the forward search all fit under it: the encoders as an
  elementwise weight on the loss, the penalised fit and the forward search as case weights, and
  the forest as the probability a unit is drawn into a tree's bootstrap, which is what ranger's
  case weights are and what the Python side's forest does with its own draw, because
  scikit-learn's forest grows the same pure-leaved tree under any sample weight. The weights
  function takes the response and a mask of the rows the model is fitted on, reads whatever it
  reads off those rows alone, and weights every row. The shipped presence-absence head weights
  each presence by the ratio of absences to presences among the fitting units, capped at 50, and
  each absence by one; a head without a weights function fits unweighted. On both sides.
- The encoders standardise every channel by its own centre and sample standard deviation over
  every unit and bin of the fitting units, except a channel `position` names, which they read at
  its own amplitude (centre 0, scale 1); the inner validation set is a plain random draw of the
  fitting units, and the loss read on it, which the early stopping watches, is weighted by the
  head as the fitting loss is, with the fitting units alone in the count the weights are made
  from; the fitting units are cut into as
  few batches of at most `batch_size` rows as they divide into, of as equal a length as they can
  be; the snapshot early stopping restores, and the running average `swa` keeps, are copies of
  the weights and never the storage the optimiser updates.
- The default training control holds no inner validation set back, so an encoder trains every
  fitting unit for the whole budget and keeps the last epoch; the patience is read only where
  `val_frac` holds a set back, and a patience that never runs out still restores the epoch of
  lowest validation loss. On both sides.
- A `static` column enters the array as a channel holding the same number in every bin, which is
  the constant an encoder reads beside the readings. Flattening the bins into a block of features
  reads such a channel once, so a static predictor is one column of the design however many bins
  the grain has.
- A learner is fitted toward the registered response head and holds no response of its own: the
  encoders train under the head's `loss` and predict through its `activation`, and the three
  learners fitting one model per response take the family the loss names, logistic under
  `binary_cross_entropy` and Gaussian under `squared_error`. A fit that declares a `head` argument
  is handed the head, as one that declares `control` is handed the control.
- A fitted encoder holds its weights as arrays and the device *setting* rather than the device it
  resolved to, and rebuilds the network when it predicts, so a fit written with `saveRDS()` or
  `pickle` predicts in a fresh session and on another machine.
- A fit refers to the code that made it rather than carrying a copy of it: an encoder stores the
  name of its module builder, and a fit stores the name and the settings of its learner wherever
  the registry can rebuild it. A fit read back therefore predicts through the code the package
  holds now, and one naming a builder or a learner the session does not carry says so by name. A
  learner defined outside any registry has no name to be rebuilt from and travels whole.
- `select_grain()` searches the candidates in the order the grains and the learners were declared
  in, so which candidate an exact tie on the inner score falls to does not depend on how the names
  sort. Its `rule` is `"argmax"` by default. `"coarsest_adequate"` takes, among the candidates
  whose inner score is at least the highest minus that candidate's standard error (the standard
  deviation over the inner folds of each fold's mean over its scored variables, over the square
  root of the fold count), the one with the fewest bins, then the fewest channels, then the higher
  score, then the one declared first; a standard error that cannot be computed is zero. Each
  outer fold reports the chosen score, the highest score and that standard error. With a
  `threshold` rule, each outer fold learns one cut per variable by `decision_threshold()` on the
  selected candidate's inner out-of-fold predictions of the outer training units, and the test
  fold is read at it by `tss(threshold =)`, presence at `p >= threshold`; the estimate row is
  `tss_inner_cut`.
- Every estimate and every contrast names the interval it carries. `"variables"` is the spread
  across the response variables of the dataset, on Student's t with one degree of freedom fewer
  than there are variables. `"nested_cv"` is the nested cross-validation interval of Bates, Hastie
  and Tibshirani (2024): `select_grain()` and `grain_ladder()` take it as `interval` with
  `repeats` fold maps, the first being the map already cross-validated on, and each repetition
  fits the procedure or the arm once per unordered pair and once per unordered triple of outer
  folds, which needs at least four; `paired_contrast()` reads it on the difference of two arms of
  a ladder fitted with it. A fold's score is the mean over the variables scorable in it, the inner
  estimate is averaged as the reported estimate is, the variance of a fold's score is its
  delete-one jackknife variance over that fold's units, the root mean squared error is rescaled by
  `(K - 1) / K` and held between the jackknife standard error of the estimate and `sqrt(K)` times
  it, and the centre carries the paper's bias correction, equation (15) at `K` folds. The mean
  squared error the width is read off is that of the bias-corrected estimate: inside every outer
  training set the same nested cross-validation runs once more, from the triple fits, at `K - 1`
  folds, and term (a) is the squared gap between the bias-corrected estimate that training set
  reports and the held-out fold's score, and its root is held between the corrected centre's own
  jackknife standard error, every prediction held fixed, and `sqrt(K)` times it; the paper's
  width, read off the plain inner estimate and held by the plain estimate's jackknife standard
  error, is reported beside it as `se_bates`. A fit of the nested cross-validation draws its seed as
  `seed + 10007 r + 101 a + b + 3001 c` from its tag `(r, a, b, c)`, the repetition and the folds
  it leaves out, zero where it leaves out fewer than three. Two
  tables whose contrast is read must carry the same response, maps, `repeats` and `seed`.
- `models` takes one learner, a set or list of them, or the name of a registered one, and
  `learners` on a ladder takes the same three forms.
- Predicting rebuilds each member's representation for the new targets from the settings its own
  arm was built with, so a new target frame has to carry the identifier, the anchor and the static
  columns the fit was made with, and is refused by name where it does not. It carries no response
  column, because a target being predicted has none.
- A learner left without a `data =` runs across every representation of the run, and one given a
  representation there runs at that one alone. A pairing the learner cannot read is skipped and
  reported by name inside a set, and is an error where the caller named it.
- A candidate is reported as `learner / representation`, and every candidate emits an out-of-fold
  prediction for every scorable cell over the same folds. The combiner is handed those predictions,
  the response, the mask and the fold map, and never a model.
- `timesift()` evaluates the procedure nested. Within each outer fold it draws an inner map of
  `inner` folds on the training targets, as `select_grain()` draws one, cross-validates every
  candidate on it, chooses one by `rule` on the inner scores, and fits the stack's weights on the
  inner out-of-fold predictions over the inner mask; every candidate is then refitted on the outer
  training targets and predicts the test fold, which gives each candidate's outer out-of-fold
  prediction, the selected candidate's and the stack's under that fold's weights. The estimate is
  both of those held-out predictions scored under every registered metric, and the run's own where
  it is a function, with the interval across variables; one outer fold's choice, weights and
  held-out predictions do not move when that fold's responses change. `inner` left unset is 5, and
  `inner = NULL` / `inner=None` runs no search and makes no estimate. With one candidate there is
  no search and the candidate is its own choice. `choice` is the rule applied to the outer scores,
  with the outer folds as its split, and the stack a prediction goes through is fitted on the outer
  out-of-fold predictions, so neither is the one the estimate was read under.
- The combiner minimises the loss of the head the run was fitted under. `ensemble()` left without
  a `response` takes the run's, and one naming a different head is refused before anything is
  fitted; `ensemble_fit()` called on its own reads an unnamed head as `presence_absence`.

### The same thing, shaped differently

| concept | in R | in Python |
|---|---|---|
| a learner of your own | `learner()`, a constructor taking the fit and the predict | `Learner`, the dataclass, built directly with the same fields |
| a learner's own training settings | a `control` field holding a partly specified `train_control()` | its `params`, beside the architecture |
| the occlusion profile | `occlusion()`, an S3 generic with methods on a run and on a ladder | `occlusion()`, one function taking either |
| the report on a run | `summary()`, a method on the base generic, printing the candidates and the procedure | `summary()`, one function returning the text, with `candidate_table()` and `procedure_table()` for the two tables it prints |
| predicting new targets | `predict()`, a method on the base generic | `.predict()`, a method on the fit |
| a set of learners, or of representations | `c()`, an S3 method on each spec class | a `list`, and `+` between two of them |

All of these are shapes rather than behaviours: a setting given to a learner beats the run's
control on both sides, the profile is one implementation on both sides, and a set is the same set.
`c()` is a method rather than a second constructor because R's own way to combine things of one
kind is `c()`; a Python list already concatenates, so nothing is added there. `summary()` and
`predict()` are methods in R because R has the generics to add them to, and Python's `summary()`
is a function because it has none.

### Present in one language only

| in R only | why |
|---|---|
| `grain_contrasts()` | fits a mixed model over the whole ladder and reads Dunnett's comparisons off it, on lme4, lmerTest and emmeans. The Python twin would need a mixed-model fitter of its own or a scientific stack the wheel does not depend on, and nothing in the contract reads it. |
| `simulate_records()` | generates a record with a planted grain, for the vignette and the recovery tests. The Python suite builds its records in its own fixtures. |
| `plot()` on a ladder and on a selection | the wheel depends on numpy alone, and every number a plot draws is on the object it is called on. |
| `starts_with()`, `ends_with()`, `contains()`, `matches()`, `all_of()`, `any_of()`, `everything()` and `where()` | tidyselect's verbs, re-exported so that `y = starts_with("sp_")` is written the way R writes a selection. Python has no non-standard evaluation, so a selection there is a name, a list of names, a glob such as `"sp_*"` or a predicate on the name, resolved by `select_columns()`. |

| in Python only | what it is |
|---|---|
| `flatten`, `align_folds`, `as_response`, `as_resampling`, `get_learner`, `resolve_metric`, `cohen_kappa`, `auto_grains`, `expand_sift`, `resolve_folds`, `n_targets`, `target_labels`, `select_columns`, `column_names` | the helpers R keeps unexported: `.flatten()`, `.as_folds()`, `.as_response()`, `.as_learner()`, `.as_metric()`, `.kappa_table()`, `.auto_grains()` and `.select_columns()` do the same work by the same name, and `.as_fold_map()`, `.sift_specs()` and `.target_frame()` do what the last five do. A Python module namespace is flat, and anyone writing a learner or reading an artifact against this side reaches them. |
| `Representation`, `Sift`, `Resampling`, `TimesiftSpec`, `Learner`, `TrainControl`, `TimesiftMatrix`, `TimesiftSet`, `Coverage`, `Response`, `Folds`, `Cells`, `Fit`, `Ladder`, `Selection`, `Timesift`, `Stack`, `EnsembleSpec` | the types. R attaches a class attribute to a list or an array and the constructor is the only door to it; a Python dataclass is the type itself, and a user annotating a function or building one by hand reaches it by name. |
| `GRAINS`, `STATS`, `DAY_LEVEL_STATS`, `PRESENCE_ABSENCE` | the grain and statistic vocabularies as tuples, and the shipped head as the mapping `register_response()` takes. R holds the vocabularies unexported and prints them in the error that refuses a name; the head is reached through `responses()` on both sides. |

Models are the one thing neither side promises. A fit in torch and a fit in libtorch cannot be
byte-identical, and the encoders match module for module rather than number for number.
