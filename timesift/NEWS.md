# timesift 0.3.1

## New

* `calendar_channels()` gains `cycles`. `"day"` adds `day_sin` and `day_cos`, the place of each
  bin's midpoint in the UTC day, beside or instead of the year's pair, in the order named. It is
  read in UTC for the reason the year is: a zone's offset and a site's longitude are the same
  shift for every bin, where a clock's summer time would move the phase by an hour twice a year.
  It is refused on bins a day or more apart, where every bin would sit at one place in the day.
  One definition in the shared core serves both languages, and the fixtures pin it as they pin
  the year's.

## Fixed

* The encoders read the channels of `calendar_channels()` at their own amplitude, where they used
  to standardise them like a reading. The channels lie on the unit circle already, and being
  identical across units they reach a network as a bias, whose step under AdamW grows with the
  scale of the input carrying it; standardising multiplied them by 1.41. At the hourly rung they
  are four of the fully connected encoder's five channels, and on the Schrankogel record they cost
  it 0.044 TSS against the study, which fed them unscaled. `calendar_channels()` records its
  channels in a `position` attribute (a field in Python), and `bind_channels()` carries
  `position` and `static` from every argument; R's used to drop `static` and Python's to keep the
  first argument's alone (#81).
* `plot()` of a `select_grain()` result drew empty axes. The inner scores were matched to their
  candidates on a label joined by a space where the candidates' was joined by a bar, so every
  line and every circle was placed at no position. One helper now writes the label wherever a
  selection's tables are matched, the bottom margin is sized to the longest candidate name, and
  the table returned says where each score was drawn and which one its fold selected (#79).
* `occlusion()` with `substitute = "permute"` permuted each channel of a held-back bin
  separately, so a unit could be shown the coldest day of one unit beside the warmest day of
  another, a reading no unit made. A bin is now held back whole, by one permutation per draw. A
  channel that is the same for every unit, as the calendar channels are, is left in place when a
  bin is held back; `unit_mean` used to replace it too, which took the bin's place in the year
  away along with its reading (#79).
* The Python `occlusion()` scored every cell, including the ones the scorable-cell mask leaves
  out, so its weights could rest on cells R's did not. It now reads the mask as every score does
  (#79).
* `grain_contrasts()` read each grain's name back out of the contrast label emmeans writes, which
  puts parentheses around a level holding a character it treats as an operator: a grain called
  `month-mean` came back as `(month-mean)`. The name is now taken from the factor's levels (#79).
* `grain_contrasts()` stopped with "undefined columns selected" on a ladder of more than 3000
  scored cells, which the Schrankogel grid is at 7070 per architecture: emmeans stops computing
  Satterthwaite's degrees of freedom past that size and names its interval columns differently.
  The limit is now the ladder's own size (#79).
* Recovery tests for `grain_contrasts()`: a planted effect per grain is recovered and its
  intervals cover it at their stated rate over twenty seeds, and with no effect the contrast finds
  none (#79).

## The reproduction

* `inst/reproduce/schrankogel.R` compares the stepwise arm and the network grid with the paper,
  where it used to write them without a reference (#80). The stepwise arm is fitted unweighted,
  as the study's forward selection was, under a head that differs from the shipped one in that
  alone. The eleven-member ensemble is the study's: each member at its own window, channels,
  kernel, dropout and seed, where the driver used to run one generic set at every window. The
  hourly rung reads the five channels the study's hourly networks read. The grain contrast is
  read in AUC against the window the study took as each architecture's best, as its table is.

# timesift 0.3.0

## Changed

* `elasticnet()` is one implementation in both languages. The elastic net is now
  `src/ts_penalised.cpp`, compiled into the R package and into the Python extension beside the
  binning, in place of `glmnet::cv.glmnet()` on one side and scikit-learn's
  `LogisticRegressionCV` on the other. Those were different algorithms: a different optimiser, a
  different parametrisation of the penalty, ten penalty values against a hundred, and different
  conventions for standardisation, so the same call returned different coefficients in the two
  languages. The penalised arm is what the networks are measured against, which made it the
  worst place for the twin to diverge. `glmnet` and `scikit-learn` drop out of the learner's
  `needs`; `scikit-learn` is still what `forest()` fits on (#76).
* The core's conventions are glmnet's, and the replacement was held to reproducing it rather than
  to being an elastic net of its own. At a matched convergence threshold of `1e-14` the path's
  length and its penalties agree exactly, the cross-validated `lambda.min` and `lambda.1se` are
  the same point of the same path in all twelve fixture cases, and the penalised objective sits
  between `3e-13` and `2e-7` of glmnet's. Coefficients agree to `2e-6` where no two columns are
  collinear and to `1e-4` on the collinear fixture design, where the split between two nearly
  identical columns is not determined to the precision the fit is.
  `inst/spec/representation.md` states all of it, and
  `inst/spec/fixtures/penalised_*.csv` hold glmnet's own numbers for both suites to assert
  against.
* `elasticnet()` gains `threads`, how many fits of one response's inner cross-validation run at
  once. The path on every fitting unit and the path of each inner fold are one independent fit
  each, reading the design and sharing nothing, so they parallelise without any of them seeing
  another: the result is bit-identical to the serial one, which both suites assert. The default
  is serial.
* `elasticnet()` gains `n_lambda` and `thresh`, and `s` now reads on the Python side too: the
  whole penalty path is kept on both, so `"lambda.1se"` or a penalty of a caller's own naming a
  point between two of the path's is read the way `predict(s =)` reads one.
* A fitted penalised model is arrays rather than a fitter's object, so it round trips through
  `saveRDS()` and `pickle` and predicts on a machine that has neither `glmnet` nor
  `scikit-learn`.
* The reproduction's elastic-net row reads the same under the shared core as it did under glmnet:
  `inst/reproduce/schrankogel.R --stages=baseline` against the deposit gives 0.68605 on the 188
  aggregates, 0.00095 from the published 0.687 and inside the 0.002 the driver compares at.
* The penalised core's binomial fit is at glmnet's speed on one thread, at the shipped
  `thresh = 1e-8`, one notch tighter than glmnet's default. The path reads the columns once per
  penalty rather than twice, a binomial fit ends each penalty on the one reweighting the next
  penalty opens with, and a reweighted least squares step is judged on its own move, which is
  glmnet's test (#77). A Gaussian fit folds the root of each case weight into the design, so a
  coordinate's step reads two streams where it read three.
* The descent's inner cycle is Anderson-extrapolated every five passes (Bertrand and Massias,
  2021), and the extrapolated point is taken only where it lowers the penalised objective. That
  is what an ill-conditioned design needed: a tighter threshold there multiplied the passes, and
  now costs 2.8 to 6.9 times fewer of them.
* Timings, one cross-validated path, installed builds, against `cv.glmnet` at its default on the
  same run: 0.47 s against 0.47 to 0.51 s for a binomial fit on the Schrankogel shape (894 units,
  942 columns, five inner folds), 264 ms against 228 ms for a Gaussian one there (190 ms at
  glmnet's own threshold of `1e-7`), and 0.12 s against 0.25 s for the four-response learner on a
  small ill-conditioned design (60 units, 18 columns). The reproduction's elastic-net row reads
  0.68606 and runs in 21 minutes.
* A penalised fit that does not settle at a penalty ends its path there and returns the points
  before it, which is what glmnet does with the same event. A fit that does not settle at the
  first penalty is an error. The path records where it stopped in `stalled`, a cross-validation
  records each fold's in `fold_stalled`, and `elasticnet()` names every response whose path ended
  that way in `stopped`, beside `unfitted` (#78).

# timesift 0.2.0

## Changed

* `train_control()` holds no inner validation set back by default (`val_frac = 0`, was 0.15): an
  encoder trains every fitting target for the whole epoch budget and keeps the last epoch, where
  the cosine schedule has annealed the learning rate to zero. On the Schrankogel weekly
  three-channel arm that epoch reads 0.8803 AUC against 0.8729 for early stopping on a 15 percent
  split and 0.8813 for the best epoch read on the test folds. `early_stopping = Inf` with a split
  held back still restores the epoch of lowest validation loss (0.8760 there). The study's rule is
  `train_control(val_frac = 0.15, early_stopping = 10)`, which the reproduction driver passes. On
  both sides.
* The encoders read the inner validation loss that early stopping watches under the head's case
  weights, as they read the fitting loss, so the epoch kept is the one the fit's own objective
  prefers. The weights are still read off the fitting units alone: a response head's `weights`
  now takes `(y, fitting)`, with `fitting` marking the rows the model is fitted on, and
  `positive_weights()` gains the same argument. Read unweighted, the validation loss of a
  presence-absence fit is dominated by the absences of the rare responses the fit was told to
  weigh up and stops at an earlier epoch: on the Schrankogel weekly three-channel arm the change
  is worth about 0.003 AUC, which is the gap to the analysis pipeline's encoder (#75). On both
  sides.
* The inner validation set `val_frac` holds back is now drawn by a plain random permutation, not
  by one unit from each of `n_val` equal-count strata of the response total. Five seeds each of
  the fixed Schrankogel weekly three-channel arm, package recipe otherwise unchanged: the plain
  draw reads 0.8721 AUC against the stratified draw's 0.8674 (+0.0047, Welch p = 0.019), the same
  margin the validation-loss weighting is worth. The stratified draw guaranteed a rare response a
  presence in every validation set; the plain draw can still miss one, and no longer guarantees
  it. `train_control(val_frac = 0.15, early_stopping = 10)` is unaffected by name, only by what the
  split under it now does (#75). On both sides.
* `timesift()` now estimates the procedure it runs. Inside each outer fold of `resampling` the
  training targets are split again into `inner` folds (5 by default), every candidate is
  cross-validated there, `rule` chooses one on its inner score and the stack's weights are fitted
  on the inner out-of-fold predictions; the choice and the weights then predict the outer test fold
  once. `estimate` holds the held-out score of the selected candidate and of the stack under every
  registered metric, `selected` and `inner` the choice in every outer fold, `fold_weights` the
  weights, and `predictions` both held-out prediction matrices. The candidates' scores on the
  outer folds are unchanged and are reported apart from the estimate, because the best of them was
  picked out on the folds it is scored on. `inner = NULL` compares the candidates without an
  estimate. `timesift()` and `select_grain()` share one inner search and one refit, so on the same
  candidates, fold map, inner count and seed the selected arm is `select_grain()`'s selection.
  On both sides.
* The ensemble row of the report is the stack's held-out score under per-fold weights. Before,
  the weights were fitted on the outer out-of-fold predictions and the combination was then scored
  against the responses they had been fitted to. `summary()` prints the candidates and the
  procedure as two tables; the Python side's `ensemble_row()` is replaced by `procedure_table()`.
* `predict(candidate = "selected")` predicts with `choice`, the candidate the rule takes on every
  target.
* The presence-absence head is scored by `roc_auc` unless a metric is named, and
  `score_predictions()` defaults to it too. TSS read at its best cut is inflated by an amount that
  depends on how a model's predictions are distributed, so it stays beside AUC rather than ahead of
  it. On both sides.

## New

* `average_precision()`, registered as a metric and pinned in the metric fixtures. On both sides.
* The reproduction driver compares the demonstration species by species (#74).
  `inst/reproduce/reference.csv` carries, per species, the analysis pipeline's five-inner-fold
  selection and its weekly series elastic net under AUC and TSS, and the spread of a single
  fitted encoder at that species over the pipeline's eleven runs of the fixed weekly arm;
  `reference_runs.csv` carries those runs' levels. Every tolerance is derived from those two
  files rather than written into the script: a level or a margin of the encoder is two fitted
  runs against each other, so its tolerance is three times sqrt(2) times the spread of the
  level, and a species is held to the same rule at its own spread, with the check being how many
  of the 101 sit inside. Run on the deposit with the study's fold map and inner partition on a
  graphics processor (`dev_notes/repro-lisc/` holds the run's files): the series elastic net
  reproduces to +0.001 TSS and +0.0002 AUC with every species inside; the selection lands on a
  weekly candidate in nine of the ten outer folds and on a monthly one in the tenth; the
  procedure's level is 0.0065 AUC and 0.0086 TSS below the pipeline's, inside the tolerances of
  0.0087 and 0.0145, with 100 and 99 of 101 species inside their own bands; and its margin over
  the series elastic net is +0.002 AUC against the pipeline's +0.009, inside the tolerance but,
  unlike the pipeline's, not separated from zero across species.

## Documentation

* The pairing and inflation text no longer says a paired TSS difference cancels the inflation of a
  self-selected threshold, or that a level is an upper bound: the inflation is an expectation, and
  two arms of equal skill can carry different amounts of it.
* The width of `interval = "nested_cv"` is the mean squared error of the bias-corrected estimate
  it is centred on, from a nested cross-validation one level further down (#73). The paper's width
  is the mean squared error of the plain cross-validation estimate and adds its bias correction
  to the centre as a shift with no spread of its own; in the package's benchmark that correction
  moved from sample to sample by more than the estimate it corrects where the sample was small
  or the signal absent, and the paper's interval covered a nominal 95% at 0.86 to 0.93 on the two
  penalised cells rerun at ten repetitions (300 units, no signal and the weekly lag; 174 and 183
  replicates) and at 0.84 to 0.96 across the twelve cells at one repetition. Inside every outer
  training set the same nested cross-validation now runs once more, over the unordered triples of
  outer folds, which gives the bias-corrected estimate that training set alone would report, and
  the squared gap between that and the held-out fold's score is what the width is read off; its
  root is held between the corrected centre's own jackknife standard error and sqrt(K) times it,
  where the paper holds the plain estimate's root between the plain estimate's. On the package's
  cheap recovery design (150 units, five outer folds, one repetition, 200 replicates) the interval
  covers a nominal 95% at 0.96 with a signal and 0.94 without, against 0.955 and 0.895 for the
  paper's width around the same centre. The paper's width is kept beside it as `se_bates` in
  `nested_cv`, and the benchmark writes both intervals and reports both coverages. Nested
  cross-validation needs four outer folds now rather than three. On both sides.

# timesift 0.1.1

## New

* `select_grain(rule = "coarsest_adequate")` takes, inside each outer fold, the coarsest
  candidate whose inner score lies within one standard error of the highest, where `"argmax"`,
  still the default, takes the highest. Coarser is fewer bins, then fewer channels. The standard
  error is the spread over the inner folds of each fold's score, so the rule reads nothing the
  selection did not already compute. Where the inner profile is flat it returns the least storage
  the record can be kept at without a measured loss inside the training data; where one candidate
  separates by more than a standard error the two rules agree. Every outer fold now reports the
  highest inner score (`inner_best`) and its standard error (`inner_se`) beside the chosen one,
  and `inner` carries each candidate's standard error. On both sides.
* `select_grain(threshold = "youden")` learns a presence-absence cut inside the training data.
  Each outer fold takes one cut per variable from the inner out-of-fold predictions of the
  candidate it selected, which the inner search already made for every outer training unit and
  for no other, by `decision_threshold()` under the rule named, and reads its test fold at that cut,
  frozen. The estimate gains a row `tss_inner_cut`, and the selection carries `thresholds` and the
  per-cell `cut_scores`. `tss()` takes the cut it is read at as `threshold`, left `NULL` for the
  maximum over cuts as before. On a binormal design with a planted skill of 0.60 the learned cut
  reads it back within Monte Carlo error, where the maximum over cuts on the same cells does not.
  On both sides.
* An interval for the procedure's risk, by the nested cross-validation of Bates, Hastie and
  Tibshirani (2024). `select_grain(interval = "nested_cv")` cross-validates each outer training
  set again over the remaining folds of the same map, over `repeats` maps, and reports the
  estimate's mean squared error the way the paper's Algorithm 1 does, rescaled and bounded as its
  section 4.3.2 states and centred on its bias correction; `final` is then the procedure fitted on
  every unit, whose risk the interval is for. `grain_ladder(interval = "nested_cv")` does the same
  for every arm, and `paired_contrast(interval = "nested_cv")` reads it on the difference between
  two. The paper's error is a mean of per-unit losses, so a fold's score here is the mean over the
  variables scorable in it and the variance of that score is its delete-one jackknife variance,
  which for a mean of per-unit losses is exactly the paper's `var(e) / |I_k|`.
* The estimate and the contrast now name what their interval is for. The across-variable interval
  is still reported and now carries `interval = "variables"`, `lower` and `upper`: it is the spread
  across the response variables of one dataset, all of them fitted and scored on the same units and
  folds, and not an interval for a new sample. On a simulated design with a measured truth
  (150 replicates, five outer folds, AUC) it covered 0.925 of the time with an error-SD to
  standard-error ratio of 1.23, where the nested interval covered 0.955 at one repetition (#73).
* `inst/reproduce/schrankogel.R` runs the demonstration's own procedure through the public
  interface: a `selection` stage searching the study's 33 candidates, a window and a summary each,
  with `select_grain()` on the convolutional encoder, over the study's outer fold map and its own
  inner partition, which ships beside the script as `inner_folds.csv`. Beside it a `series` arm
  fits the penalised model on the weekly coldest-day, mean and warmest-day reading of the record
  rather than on summaries of it, which is the comparison the demonstration is read against. Every
  comparison with a published number states its tolerance before anything is fitted and lands in
  `checks.csv`; `run.meta` now records the torch and libtorch versions and the device; and
  `--smoke=<folds>,<species>` runs the stages on a few of each, names its output `smoke_` and
  compares nothing (#74).
* `grain_matrix()` documents what a day is in a zone that keeps daylight saving time: a
  wall-clock day, 23 hours on the day the clock goes forward and 25 on the day it goes back, with
  the week and the month holding them an hour shorter or longer, and 24-hour days on a record kept
  on a fixed offset or carried in one such as `"Etc/GMT-1"`. Nothing in the binning changed; a
  test on both sides pins the Europe/Vienna transitions of 2021 at both offsets.

* `coverage()` lays the binning out as a count of readings per `(unit, bin)`, over every bin the
  calendar tiles the record with. `grain_matrix()` refuses a record where a unit misses a bin
  rather than padding it, and this is the table that decision is made on: a logger that started
  late, stopped early or lost a month is a row with zeros in it, and a bin the whole record skips
  is a column of zeros. Nothing here fills a cell. On both sides, and pinned by
  `inst/spec/fixtures/coverage.csv`.

## Paired contrasts

* `paired_contrast()` takes each arm whole, as `"grain|learner"`. A learner named alone used to
  take its best grain, chosen on the held-out scores the contrast was then read off, so the
  difference was one between two maxima and its p-value optimistic by an amount nothing in the
  output recorded. It is refused now, with a message naming the whole arm; `select_grain()`
  chooses a grain on inner folds and contrasts the selection through `compare`. `occlusion()`
  still reads a learner named alone at its best grain, since it describes a fitted model rather
  than testing a difference. On both sides (#67).
* The interval on a contrast is Student's t on one degree of freedom fewer than there are variables,
  in place of the normal quantile, which made it about a quarter too narrow at six variables and
  more than six times too narrow at two. The ladder plot draws its interval the same way (#68).
* The signed-rank p-value is read by a method chosen rather than fallen back on, and the row says
  which: `p_method` is `"exact"` below fifty per-variable differences holding no zero and no tie,
  and `"normal"`, the approximation with continuity and tie corrections, otherwise. The Python side
  took the exact distribution where the differences held a zero and R the approximation; both
  take the approximation now. `inst/spec/fixtures/contrast.csv` pins the interval and the method,
  and the Python side carries its own Student's t quantile, which agrees with `qt()` to within
  about 1e-14 (#68).

## The response head

* A head carries a `loss` and an `activation`, and they are what every shipped learner fits
  toward. The encoders train under the loss and predict through the activation, the per-response
  learners take the family the loss names, and the combiner minimises the same loss. A registered
  head other than presence-absence is therefore fitted as itself rather than as clamped binary
  cross-entropy under another name.
* A `fit` that declares a `head` argument is handed the head, as one that declares `control` is
  handed the control. Both are read from the fit's own signature, at the one point every fold loop
  fits through, so a learner whose fit takes `(x, y)` is called with `(x, y)`.
* `elasticnet()`, `stepwise()` and `forest()` fit the Gaussian family where the head's loss is
  squared error.
* `ensemble()` left without a response takes the run's head; one naming a different head is
  refused before a candidate is fitted.

## Fitting

* `elasticnet()` deals its inner folds for each response and stratified on it, so a rare outcome
  is spread over them as evenly as its count allows rather than wherever a plain deal dropped it. A
  presence-absence response whose inner training sets cannot each hold two of each outcome, the
  fewest a logistic path is fitted to, is predicted its share among the fitting units, as a
  response holding one outcome already was, and the fit names it in `unfitted`. Such a response
  used to stop the whole run from inside `cv.glmnet()`, which cost a benchmark cell a replicate it
  could not recover by rerunning. The inner fold draw is not the one before, so a penalised fit's
  chosen penalty can differ from an earlier run's. On both sides (#72).
* On the Python side the elastic net chooses its penalty on the held-out log loss, weighted by the
  head's case weights, as `cv.glmnet()` chooses on the weighted deviance. It was choosing on
  accuracy, scikit-learn's default, a step function of the penalty whose choice moved by orders of
  magnitude with the fold draw.
* Predicting from a fit whose new targets are missing a static predictor names the column that is
  missing. An absent column read as one that is not numeric, and the message sent the reader to
  encode something their table does not hold.
* On the Python side `models` takes one learner as well as a list of them, which is the form R has
  always taken; a single learner was reaching the fitting layer as a sequence of nothing.
* A learner is handed the whole response matrix whether it declares `joint` or `separate`, and
  the three that fit one model per response already did that inside their own fit. The fitting
  layer was splitting the responses as well, so the flattened block of predictors was rebuilt once
  for every response: on the published grid, 101 copies of a 47 MB matrix per fold and per arm.
  `multi` is now what the learner says it does with the matrix and what a report says of the
  candidate. The wrapper that reassembled the columns is gone from both sides, and with it the
  Python `CandidateFit`.
* One fold loop on the Python side, which the run, the ladder and the inner search of a selection
  all call, in place of three copies; one `_check_bins` and one `_check_grain`; and one baseline
  prediction per fold in the occlusion, which was rebuilding an encoder from its arrays once per
  response.
* A fit refers to the code that made it rather than carrying a copy of it. R writes a closure's
  body out beside the closure, so `saveRDS()` on a fit copied every function that produced it and a
  fit read back after an upgrade rebuilt the old encoder and loaded the new weights into it. A
  fitted encoder now stores the name of its module builder, and a fit stores the name and the
  settings of its learner wherever the registry can rebuild it; a learner defined outside any
  registry still travels whole, because there is nowhere else for it to come from. The Python side
  stores the encoder's name for the same reason.
* A candidate that holds no number on a scorable cell stops the run and names itself. A threshold
  metric returns `NA` on a cell whose predictions are `NaN` or infinite, which is the same `NA` a
  one-class cell returns, so a network whose training diverged was reported as an unscorable cell
  and dropped from the combiner without a word; the stack was then fitted on fewer cells than the
  mask said and nothing showed it. The combiner is now fitted on every scorable cell and refuses to
  drop one. `score_predictions()` refuses the same predictions on both sides.
* `grain_ladder()` and `select_grain()` take a `control`, so the training settings of a ladder are
  given once for the run rather than restated on every neural learner in it. A selection hands the
  same control to the inner search and to the refit, and a learner carrying settings of its own
  still overrides it on the ones it names.
* A response is fitted under a seed of its own, taken from its name. A learner that covers the
  responses one at a time is handed one column per call, so a seed spent from one shared stream
  made a response's fit depend on which responses were fitted before it, and a seed read off the
  column's position inside its own call gave every response the same one. Either way the model of
  a response depended on how the columns were batched, and `elasticnet()` and `forest()` both did.
  Fitting a response alone, or beside others, or in a different order now gives the same model,
  which `tests/testthat/test-variable-seeds.R` holds to. This changes the numbers those two
  produce; `stepwise()` spends no randomness and the encoders cover the responses jointly, so
  neither moves.
* `metric` takes a registered name or a function of `(y, p)` everywhere the contract says it does.
  What scores and what a report prints travel with the fit, so a function reads as `<function>` in
  a report rather than as whatever each language calls an anonymous one, and the occlusion profile
  rescores under the metric the fit was scored with. `select_grain()` is the one door that takes a
  name only, because it reports its estimate under every registered metric and selects on a row of
  that table.
* A static predictor enters the design once rather than once per bin. It reaches the array as a
  channel that does not move across the bins, which is what an encoder reads; flattening the bins
  was emitting it once per bin, so a penalised fit saw it as often as the grain had bins and a
  forward search could pick it repeatedly.
* A resampling given as an unnamed vector follows the targets into the order they are fitted in.
  `timesift()` sorts the targets by identifier before it builds the fold map, so such a vector was
  landing on whichever unit had taken that position.
* `elasticnet()` takes `s`, the point of the penalty path to predict at. It also raises what
  `cv.glmnet()` raises rather than turning any failure into the response's own mean, and the
  forward search leaves out a column holding a single value rather than reaching a fitter's error
  on it.
* On the Python side `elasticnet()` honours its mixing -- the scikit-learn floor is 1.8, where
  `l1_ratios` alone selects the elastic net -- and standardises its design before penalising it,
  as glmnet does. The scaler travels with the fit.

## The representation boundary

* `calendar_channels()` reads the shared core. The year fraction, the midpoint of the record a bin
  holds and the calendar the year is read on used to be written once per language, and the two
  were written differently at the two points that decide the number: R took the phase through
  `sinpi()` and the midpoint as a double, Python took `sin(2 * pi * x)` and truncated the midpoint
  on `datetime64`. `inst/spec/representation.md` now defines all of it, and `channels_digests.csv`
  and `channels_guards.csv` pin it: what is digested is the year fraction, which is arithmetic on
  the calendar, with a stated tolerance of 1e-12 on the sine and the cosine, which are the
  platform's library. Both suites read the fixtures.
* `calendar_channels()` refuses a lookback, whose bins are placed relative to a target rather than
  on the calendar and have no position in the year. It used to reach for a `bin_start` that is not
  there.
* `bind_channels()` reads every argument as a representation rather than only the first, and both
  languages number the arguments from one and raise the same message. A bare array reached the
  R side and came back carrying attributes describing a binning it had never been through.
* A supplied calendar is checked before its bins are read as bins: a bin begins at or before every
  reading it holds, and a bin's readings are a stretch of the record. A calendar shifted by one
  boundary, and one interleaving consecutive readings, used to produce an array that looked like
  any other.
* An identifier is written by one rule in both languages. A whole number is its digits, a
  character id is itself, a factor is its label, and anything else is refused naming the column.
* A time column carrying a zone names the calendar to bin by, in `grain_matrix()`, `coverage()`
  and `lookback_matrix()` alike; a `tz` naming a different zone beside it is refused rather than
  silently preferred.
* The lookback's target table is read by name, `id` and `at`, as every other alignment in the
  package is, and a lookback reaching the fitting layer without an anchor is refused with a
  message that says so.
* A reading that is not a finite number is refused by the shared core, naming the unit and the
  instant of the first one, so the rule is written once rather than once per wrapper.
  `digest_array()` refuses an array that is not finite for the same reason the contract gives.
* A record and any permutation of its rows now reduce to the same bytes. Both reductions walk the
  record by unit and then by instant; the calendar reduction accumulated in the caller's order,
  which moved a mean in its last bits under a shuffle that changed nothing about the record.
* The `native` grain is read on the instant rather than on the local clock, so the two readings
  of the hour a zone repeats when it sets its clock back are two bins. They used to share a local
  second and were merged into one bin holding their mean, on a grain the contract defines as the
  record unreduced; the `Europe/Vienna` native fixture pinned 9599 bins for 9600 readings, and
  is regenerated. The walk order both reductions accumulate in carries the instant as a third
  key, so those two readings are no longer a tie the sort resolved as it liked, and a zoned
  record already in order is no longer sorted again.
* A supplied calendar that returns no bin start for a reading, `NA` in R or `NaT` in Python, is
  refused naming the first such reading. It used to reach the core as a bin at the beginning of
  time, holding the reading its real bin then lacked, with nothing raised; the guard fixture
  carries the case as `missing`.
* A time column carried in a zone the database does not know is an error in R, as it was in
  Python. R's zone resolution used to warn and read the clock in UTC.
* The contract now says that a lookback's length is measured on the local clock, as everything
  below the zone boundary is: a one-day lookback ending at a local midnight holds 25 hours of
  record on the night a zone sets its clock back and 23 on the night it sets it forward. That is
  what keeps a calendar day whole inside a bin for the day-level statistics; both suites pin it.
* The guards above the core name what they refuse the same way on both sides: a missing
  identifier or instant names its column in Python as it did in R, a duplicated reading is named
  by its instant in UTC in R as it was in Python and the noun agrees with the count on both, and
  Python reads `bins = 3.0` as R reads `bins = 3`. A zoned record from before 1970 no longer
  raises `OSError` on Windows in Python. A lookback's rows are named by the row names of `at`
  whatever kind of row names the frame carries, which is what the contract already said.
* The zoned digests have a witness that is not the core. Each oracle reads a zoned series as a
  clock in its zone and bins that clock with its own calendar, and both suites assert every
  `Europe/Vienna` and `America/Sao_Paulo` digest against it. The core's negative-lag guard, its
  out-of-range unit index and its bound on the day table are exercised by both suites as well.

* The two orderings among the seven statistics, `min <= mean_daily_min <= mean <= mean_daily_max
  <= max` and `min <= cold_day <= mean <= warm_day <= max`, are asserted by both test suites on
  the core's output and the oracle's. They used to sit in a block of the core that neither build
  compiled.
* `grain_matrix()` and `lookback_matrix()` on the Python side refuse a call that names no `value`
  column before reaching the compiled core, and a lookback's label reads a zero lag as zero however
  it is spelled.

## Names

* `as_sift()` is the coercer to a set of representations on both sides, in place of R's
  `timesift_sift()`; the class keeps its name.
* The last section of the contract records every public name of both languages, on the side it
  is on, and each suite reads that section against its own exports, so a name added to one side
  without a line there fails the suite. Python exports `resolve_metric` beside `get_learner`, as
  the contract had said it did.

## What a rare response weighs

* The response head owns it. A head's `weights(y)` returns one case weight per cell of the
  response, and the encoders, the penalised fit, the forest and the forward search all fit
  under it: the encoders as an elementwise weight on the loss, the penalised fit and the forward
  search as case weights, the forest as the probability a unit is drawn into a tree's bootstrap.
  The shipped presence-absence head carries `positive_weights()`, exported on both sides: each
  presence weighs the ratio of absences to presences among the fitting units, capped at 50, and
  each absence weighs one. A head registered with `weights = function(y) positive_weights(y,
  cap = 20)` weights every learner by that cap, and a head without `weights` fits unweighted.
* Three learners used to decide this for themselves, three ways. The encoders capped the weight
  at `pos_weight_cap` from `train_control()`, the elastic net and the forest weighted uncapped
  under `weight_positives`, and the forward search did not weight at all, so a response present
  in one target of a hundred weighed 99 in two learners, 50 in a third and 1 in the fourth, in
  the rare regime the network-against-aggregates comparison is about. `pos_weight_cap` and
  `weight_positives` are gone. On the study's data, whose rarest species has 26 presences in
  894 plots, the cap never binds, so the reproduced elastic net is the same number.
* Python's forest draws each tree's bootstrap by the weights itself, as ranger's `case.weights`
  does. scikit-learn's forest takes a sample weight into its impurities and leaf values, and a
  tree grown to pure leaves is the same tree under any weight, so the Python forest was fitting
  a rare response unweighted while the R forest was not.
* The forward search fits the quasi-binomial family and reads its criterion off the deviance,
  which is the same number the binomial family reports for a 0/1 response and takes a case
  weight that is not a whole number without a warning. The Python forward search runs its
  iteratively reweighted least squares under the same case weights.

## Fitting under a grouping

* A fit is checked once, on the `timesift_fit`, against the bins and the channels it was made
  on, before any learner sees the representation it is asked to predict. A calendar grain's bins
  are named by their instants, so a record from another period with the same number of bins is
  refused by the first bin that differs rather than read by position, as `cnn()` fitted on
  September to November used to read March to May. A lookback's bins are relative to each target
  and predict any period. The check that each learner carried is gone; every learner, including
  one of your own, is held to this one. On both sides.
* The grouping an outer fold map keeps whole reaches every split drawn inside a fit. A
  [fold_map()] built with `group` carries it, a `fit` that declares a `group` argument is handed
  the grouping of the units it is fitted on, and the encoders' inner validation set, the elastic
  net's inner folds and `select_grain()`'s inner map are all dealt by it. Under `grouped_cv()`
  with `target_time` a unit kept whole across the outer folds used to be split across the inner
  ones, so the epoch and the penalty were chosen on rows the fit had already seen a near-copy
  of. On both sides. The elastic net deals its own inner folds now, through `fold_map()`, rather
  than leaving the draw to `cv.glmnet()` or to scikit-learn.

## Fixes

* `fold_map()` deals with one round-robin counter that runs on from each stratum into the next,
  on both sides, so the folds are equal in size to within one unit whatever `v` is and
  leave-one-out is every unit in a fold of its own. A counter restarted in every stratum used
  the first labels once more than the last in every stratum, and a stratum smaller than `v`
  never reached the last labels at all: ten folds over thirty units came back holding two to
  five units each. The fold-map fixture, and the mask and the inflation built on it, are
  regenerated.
* A fold map given as `resampling` that holds one fold is refused, and one built by
  `fold_map()` keeps its `grouped`, `seed` and `strata` when it reaches the run, so a grouped
  design reports as one. A run under which no `(response, fold)` cell is scorable stops before
  anything is fitted, and `ensemble_fit()` handed no scorable cell says so rather than fitting a
  combiner on nothing.
* The threshold metrics check that the response is 0/1 before coercing it, on both sides. They
  used to coerce first, which read 0.6 as 0 and scored a response that was never binary.
* A torch fit puts torch's generators back where it found them, as it does R's.
* `.simplex_weights()` stops at a relative gain of 1e-14 rather than 1e-12, on both sides. Near
  the minimum the loss is flat to second order, so the stop settles the gradient to about the
  square root of the tolerance, and the looser one left the carried members' gradients apart by
  more than a millionth on some boards.
* The within-variable-then-across-variables mean is one function, `.cell_means()`, read by the
  ladder, the report, the stack's member scores and the selection's estimate; the five copies
  had four spellings.
* Small edges: `models` and `learners` take a character vector of registered names, as the docs
  said; a fit read back through its reference keeps a setting held at `NULL`, so a forest refits
  with its default `mtry`; `train_control(early_stopping = Inf)` is a patience that never runs
  out; `predict()` on a `timesift` fit holds the new targets to one row per identifier, as the
  fit's own were held; the elastic net refuses a design of one column in its own words; and
  `grain_contrasts()` names the reference it cannot find rather than failing on a length-zero
  test.
* Python: a learner pinned to a representation that shares a sift member's label but not its
  definition is refused, as R refuses it, rather than fitted on the sift's array.
* Python: `auto_grains()` counts the bins in the zone the time column carries, the clock the
  representation is then binned by. It used to probe in UTC, so a zoned record could be offered a
  grain it does not support or denied one it does.
* Python: `grain()` takes a supplied calendar as R does, reported as `custom`; `multigrain()`
  refuses one on both sides.
* Four differences between the languages are gone rather than recorded. `grouped_cv()` deals the
  groups unstratified on both sides. A lookback is labelled `<span> x<bins> lag <lag>` on both,
  and a numeric span as a duration. `lookback()` refuses a span that is not positive when it is
  built. `forest()` and `elasticnet()` derive one seed per response from the response's name in
  Python as in R, so two species no longer share their bootstrap draws.
* Python: `feature_matrix()` names unnamed rows from 1, as every other unit label does, and a
  fold map prints every fold level it holds.
* A response given as a data frame with two or more non-numeric columns is refused, naming them,
  rather than read through the first and silently stripped of the rest.
* A presence-absence response holding a missing value says so on the Python side, rather than
  reporting it as a value that is not 0 or 1.
* An arm is matched whole against the ladder's labels rather than split at the `|`, so a learner
  or a sift whose own name holds one is still found, on both sides.
* `select_grain()`'s `inner` is checked before it is coerced, so a value that is not a count is
  named in the error rather than printed as `NA`.

## The selection benchmark

* The oracle and the single-loop truth are averaged over the outer training sets, as the
  procedure's truth always was. Every candidate is scored on the deployment sample through the
  ladder's own per-fold fits, and the three arms are read off that one grid, so the regret compares
  the procedure with the best fixed candidate under the same training draws. The oracle used to be
  fitted on the first outer training set alone, so the regret carried a maximum over twelve
  one-draw estimates, and the single-loop bias a truth read on one fold against a score read on
  five. `inst/benchmark/design.R` states the definition and what of the selection term remains.
  The interval whose coverage is read is Student's t, as the package's are (#65).

## Packaging

* `inst/CITATION` cites the package, and `citation("timesift")` prints it; the methods article's
  entry is added once it has a DOI. `codemeta.json` is generated from `DESCRIPTION` with
  `codemetar` and regenerated at a release. `DESCRIPTION` declares `Language: en-GB`, and
  `inst/WORDLIST` holds the proper nouns and API names the spell check would otherwise flag.
  `tests/spelling.R` runs the check against it and fails on a word it does not know, and the
  `R-CMD-check` workflow sets `NOT_CRAN` so it runs there; it is skipped on CRAN, whose
  dictionaries are not this machine's (#70).
* The Python side is checked as a distribution. The `python-dist` workflow builds the source
  distribution, installs it into a clean environment, and runs the suite it ships from the
  unpacked tarball, which is what holds the manifest in `pyproject.toml` to the sources. It builds
  wheels for Python 3.11 to 3.13 on Linux, Windows and macOS with `cibuildwheel`, each tested on an
  interpreter that did not build it, and passes every artefact through `twine check`. A published
  GitHub release uploads them to PyPI by trusted publishing (#69).
* The committed site is held to its sources. `build_site.R` writes a digest over everything the
  site is rendered from into `docs/site-digest.txt`, and the `site` workflow recomputes it from
  the checkout on every push, so a push that changes the reference, the news or an article
  without rebuilding the site fails there rather than serving last week's pages under this
  week's sources. The site stays built locally, because the build post-processes its figures in
  a way a plain build in CI would not.
* The Python floor is 3.11. The `sklearn` extra pins the scikit-learn release where the mixing
  parameter alone selects the elastic net, and that release has no build for 3.10, so the 3.10
  job had failed at install on every push. The `test` extra carries pandas, so the suite runs
  `timesift()` on a data frame with a categorical identifier, a nullable-integer response and a
  zone-aware time column rather than skipping that test on every runner.
* `DESCRIPTION` is where the version is written, and `pyproject.toml` reads it from there.
* `.gitattributes` holds the repository to one line ending.
* The wheel ships `py.typed`.

# timesift 0.1.0

`timesift()` is the whole entry point. A table of targets, a table of time-stamped series belonging
to them, and one call builds every candidate representation, fits the learners that can read each
one, scores them all on one set of held-out folds, and stacks the out-of-fold predictions.

```r
fit <- timesift(plots, logger, y = starts_with("sp_"), id = plot_id, time = datetime,
                models = c(elasticnet(), forest(), cnn()),
                sift = grains("day", "week", "month"))
summary(fit)
```

The version reads 0.1.0 because this is a first release: the package fits any time-varying record
against any prediction target, where its predecessors fitted a climate record at a climate grain.
Species distribution modelling from microclimate loggers is the application it ships defaults for,
and the Schrankogel grid it was built on still reproduces from `inst/reproduce/schrankogel.R`.

## The four concepts

* A **target** is one row to predict, a **series** is the long record belonging to those rows, a
  **representation** is how that record becomes an array, and a **learner** is a fit and a predict
  pair. A candidate is one (representation, learner) pair, and every candidate emits an out-of-fold
  prediction for every scorable cell over the same folds. Comparison, ensembling and importance
  read only those predictions.
* `y`, `x` and `static` take tidyselect expressions over their own table, and `starts_with()`,
  `ends_with()`, `contains()`, `matches()`, `all_of()`, `any_of()`, `everything()` and `where()`
  are re-exported rather than redefined. A column of `targets` that is neither the response nor the
  identifier nor the anchor is a predictor only where `static` names it.
* `predict()` on a fit rebuilds each member's representation for the new targets from the settings
  its own arm was built with, and combines them through the ensemble.

## Representations

* `native()`, `grain()`, `multigrain()` and `lookback()` are what a representation is before any
  record has been read; `grains()` and `lookbacks()` are sets of them, and `grains("auto")` reads
  off the record every named grain it gives at least two bins.
* `c()` combines learners, or representations, into a set: `c(elasticnet(), forest())` and
  `c(grains("day", "week"), lookback("30 days"))`. A set handed back to `c()` splices, so a set can
  be added to rather than rewritten. `models` and `sift` take that, a bare spec, or a list.
* `lookback()` is a fixed span of record ending a fixed lag before each target's own instant, which
  is what a unit carrying several targets through time needs. It is one entry point in `src/`
  beside the calendar reduction, so both languages read it from the same implementation, and
  `lookback_matrix()` exposes it directly.
* `build_representation()` is the one place the fitting layer turns a representation and the two
  tables into an array, so a run and a prediction on new targets reach a record the same way.
* A learner given `data = grain("week")` runs at that representation alone; left open it runs
  across the whole sift. A learner that reads a block of features and one that reads a sequence say
  so, and a pairing neither can carry is reported by name rather than fitted.

## Learners and training

* `elasticnet()`, `stepwise()`, `mlp()`, `cnn()` and `rescnn()` drop the `_learner` suffix and
  gain `data`. `forest()` joins them, a probability forest on `ranger` in R and on scikit-learn in
  Python. `reads` and `multi` are `learner()`'s, where a learner of your own declares what it reads
  and whether one fitted model covers every response.
* `train_control()` is the one place a training setting is defaulted. The architecture constructors
  carry architecture, a run gives one control to every neural learner, and a learner given its own
  control overrides that on the settings it names.
* A learner declares whether one fitted model covers every response or one is fitted per response.
  Either way a candidate emits one `[target, response]` matrix, so nothing above the learner layer
  has to know which it was.

## Resampling and the ensemble

* `cv()` and `grouped_cv()`; `resampling` also takes a fold vector or a `fold_map()` result, which
  is how a split the package has no constructor for reaches the same fitting path.
* `ensemble()` fits non-negative weights summing to one on the out-of-fold predictions alone,
  minimising the response head's own loss over the scorable cells, solved by an exponentiated-
  gradient loop in the package. `"mean"`, `"median"` and `"weighted"` combine without fitting.
  `ensemble_fit()` is handed the predictions, the response, the mask and the fold map, and never a
  model.
* `summary()` reports each candidate's mean, how many responses it scored highest on, whether one
  model covered them, and the level the combination reached under the weights it reached it with.

## Names

* `native()` rather than `raw()` and `lookback()` rather than `window()`, which would have masked
  `base::raw()` and `stats::window()`. `occlusion()` is one generic over a run and a ladder, and
  `bin_occlusion()` is gone. `ensemble_learner()` is gone with it: it fitted its members and
  averaged them, which the stack does over any candidates at all and with the weights fitted rather
  than assumed.
* The bin's name is the grain throughout, in both languages.

# climgrain 0.3.0

The package is renamed from `timegrain` to `timesift`. With it go the C++ prefix (`tg_` to `ts_`
and the four files that carried it), the S3 classes (`timegrain_matrix` and its siblings to
`timesift_matrix`), `timegrain_set()` to `timesift_set()`, and the Python module. No function
signature, default or return type changed, and the fixture digests are untouched.

What the user chooses is the temporal grain of a climate representation, and the name now carries
the thing whose grain it is. The title reads "Temporal Climate Resolution for Ecological
Prediction".

# climgrain 0.2.0

The binning and the reduction are now one implementation, `src/ts_core.cpp` and
`src/ts_calendar.cpp`, compiled into the R package by R itself and into the Python extension by
CMake. The two languages agree by construction rather than by two implementations being checked
against each other after the fact. What each side keeps above it is the boundary: resolving the
columns, resolving the zone, and wrapping the result.

Four bugs that existed twice, once per language, are closed by that (#1, #2, #4, #5), and a fifth
on the Python side with them (#3).

## The calendar

* Bin starts are computed by proleptic Gregorian arithmetic on local time rather than by writing a
  local midnight and parsing it back. A zone that moves its clock at midnight, such as
  `America/Sao_Paulo` before 2019, no longer produces `NA` bin starts and a failure from inside the
  reduction, and a `year_start` landing on such a night is an argument rather than an error (#1).
* A bin start is a local time, so reporting it as an instant now has a stated rule: one the clock
  skipped resolves to the instant the clock jumped to, one the clock repeated to the first of the
  two.
* `grain_matrix()` in Python takes a `tz` argument. The same instants and the same zone now give
  the same answer in both languages, and the fixtures pin it rather than leaving it assumed (#5).
* Instants are read at whole seconds in both languages, so two readings a fraction of a second
  apart are the same reading twice.

## Guards

* Consecutive bin starts must be one bin apart on the grain's own calendar. A bin no unit reaches
  is never built, so a month missing from the whole record used to pass as four adjacent monthly
  bins with one gone, in both languages (#4). Not asserted for `native`, whose bin is the reading
  itself, nor for a supplied calendar, which declares its own bin lengths.
* A day-level statistic requires every calendar day to lie inside one bin, decided from the bins
  rather than from the grain's name. A supplied calendar cutting inside a day used to give a
  mostly-`NA` array in R and a numpy `IndexError` in Python (#2).
* Python no longer rejects a unit called `nan` and no longer misses a genuinely missing id (#3).

## Evidence

* The pure-R and pure-NumPy implementations are kept as test oracles,
  `tests/testthat/helper-oracle.R` and `python/tests/oracle.py`. Neither package reaches them at
  runtime; both suites check them against the core on the fixtures and on random series. The NumPy
  one was written from `inst/spec/representation.md` rather than from the R source, which is what
  makes it evidence that the document is complete.
* A third fixture series and a `tz` column in `digests.csv`: every grain of the aligned series
  read as a `Europe/Vienna` clock, and a short series across the night `America/Sao_Paulo` moved
  its clock at midnight.

## The two languages

The names, the defaults and the extension points had drifted, so a script moved from one side to
the other met them one at a time (#8). One name per concept now, and where the two still differ the
difference is a row in `inst/spec/representation.md` rather than something to be discovered at a
call site.

* `glmnet_learner()` is `elasticnet_learner()`, and its `nfolds` is `n_inner`. The name says which
  model is fitted rather than which package fits it, which is also what the Python side already
  called it. It is registered as `"elasticnet"`.
* Python carries the response and metric registries, so on both sides the response head and the
  metric are registry entries and the fitting path holds no list of names. `learners()`,
  `metrics()` and `responses()` list what a session has, in C collation.
* `stepwise_learner()` and `select_grain()` are on the Python side. The selector's orthogonal
  polynomial basis and its logistic fits agree with R's `poly()` and `glm()` to twelve decimals on
  the same design; the selection procedure is the same nested one, reporting its estimate under
  every registered metric.
* Python's `grain_matrix()` returns one representation when one grain is named, whether as a
  string or as a sequence of one, and a `timesift_set()` for two or more. Its fold map carries the
  units it was drawn for, so aligning one is by name there as it already was in R.
* The torch encoders take `swa` and `swa_start` on both sides, and both refuse a setting the
  learner does not have rather than ignoring it. A misspelled argument used to be dropped in
  silence.
* `select_grain()` searches its candidates in the order they were declared. It read them off a join
  on the names before, which is the collation the rest of the package stopped depending on in this
  version, so an exact tie on the inner score could fall to a different candidate on a different
  machine.

## Build

* `LinkingTo: cpp11`, and flat `.cpp` under `src/`, so R compiles the core with no `Makevars`.
* `pyproject.toml` and `CMakeLists.txt` sit at the repository root rather than under `python/`,
  because a Python source distribution cannot reach above its own project directory and the shared
  sources must not be vendored into a second copy. The Python build is scikit-build-core and
  nanobind; the wheel carries `python/timesift` as `timesift`.
* The wheel depends on `tzdata` on Windows, which ships no IANA database of its own, so a
  zoned record bins there as it does everywhere else.
* `R-CMD-check`, `pytest` and `contract` run on push and on pull requests, the last of them
  running both fixture suites against one `inst/spec/fixtures/` in a single job.

## Documentation

* One site for both languages at <https://gillescolling.com/timesift/>: the R reference from the
  Rd files, the Python reference written from the Python sources by `tools/python_reference.py`,
  and `inst/spec/representation.md` rendered as a page of its own, so the document the two answer
  to is read where the calls are. The `pytest` workflow rewrites the Python pages and fails if what
  is on disk differs, so a docstring cannot change without the page changing with it.
* Every public class, method and property of the Python package carries a docstring.

# climgrain 0.1.0

First release. The package builds the representation, fits at every grain, and reports where
predictive skill saturates.

## Representation

* `grain_matrix()`: reduces a long table of sensor readings to a `[unit, bin, channel]` array at
  one of seven temporal grains, from the unreduced record to a single value per hydrological year.
  Naming several grains returns one representation per grain; passing a function bins by a
  calendar the package does not carry, such as seasons cut at the equinoxes.
* Bins follow the calendar, so a month is 28, 30 or 31 days and a week starts on a Monday, and
  every `(unit, bin)` cell is asserted to hold readings.
* A bin the record does not cover for its whole calendar span is reported on `bin_partial` and
  kept or removed by the `partial` argument, so a record that begins away from a bin boundary
  says so rather than carrying a short bin that looks like any other.
* Seven statistics: `mean`, `min`, `max`, the day-level `cold_day` and `warm_day`, which reduce
  each day to its own mean before taking the extreme over days, and `mean_daily_min` and
  `mean_daily_max`, which take the mean of the daily extremes.
* `calendar_channels()` and `bind_channels()` supply the position of each bin in the year to an
  encoder that would otherwise pool it away.
* `feature_matrix()` brings an already-reduced feature table in as an arm of the same ladder.
* Gaps, duplicated `(unit, time)` pairs and missing values are errors rather than silent padding.

## Fitting and scoring

* `fold_map()` and `scorable_cells()`: one split read by everything that scores, and the mask of
  cells a score is defined on, computed from the response and the fold map with no model involved,
  so every arm shares one denominator and every paired difference runs on matched cells.
* `grain_ladder()` fits every learner at every grain, `plot()` draws the curve, and
  `paired_contrast()` compares two arms inside each cell both scored.
* `bin_occlusion()` holds each bin of the record back and rescores, without refitting, so a fitted
  model says which part of the year its skill rests on.
* `grain_contrasts()` fits `score ~ grain + (1 | variable) + (1 | fold)` and compares every
  grain against a learner's best by Dunnett's procedure. Needs `lme4`, `lmerTest` and `emmeans`.
* `tss()`, `roc_auc()`, `kappa_score()`, `model_agreement()` and `decision_threshold()`.
* `tss_inflation()` measures how much a self-selected threshold inflates a reported level at the
  user's own presence counts, and `implied_skill()` inverts that map to say what population skill
  a level actually read is consistent with.

## Learners

* `elasticnet_learner()` and `stepwise_learner()` on the flattened representation, both redoing their
  selection inside whichever units they are handed.
* `mlp_learner()`, `cnn_learner()` and `rescnn_learner()`, joint multi-label encoders on `torch`,
  sharing one training recipe.
* `ensemble_learner()` averages several members' predicted probabilities before the threshold is
  chosen, and the torch learners take `swa = TRUE` to average the weights of their tail epochs.
* `learner()` takes a fit and a predict pair of your own; `register_learner()`,
  `register_response()` and `register_metric()` extend the three registries the fitting path reads.

## Reproducing the study

* `inst/reproduce/schrankogel.R` runs the published grid from the Zenodo deposit it was built on,
  asserting the plot count, the species count, the cell count and the bin count of every grain
  before fitting anything. See `vignette("reproducing-schrankogel")`.

## The cross-language contract

* `inst/spec/representation.md` is normative for both the R and the Python implementation.
* `inst/spec/fixtures/` carries a synthetic series and the digest of every grain-by-statistic
  combination; both test suites assert against the same digests.
* The Python side implements the representation, the folds, the mask, the metrics, the ladder and
  the same three encoders.
