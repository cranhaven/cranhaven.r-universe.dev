# Reproducing the Schrankogel grid

`schrankogel.R` runs the published grid from the deposit it was built on.

```
Rscript schrankogel.R <deposit_dir> <out_dir> [--name=value ...]
```

`<deposit_dir>` is the unpacked `data` directory of the Chytrý et al. deposit
(doi:10.5281/zenodo.17047026, CC BY 4.0). Four of its files are read: `spe_wide.csv`,
`logger_data.csv`, `seasons.csv` and `output_temperature_variables_scaled.csv`. Each is checked
against the size and the MD5 sum of the deposit's copy before it is read, so a file that is not
the deposit's stops the run rather than producing a number of the right shape. `run.meta` in
`<out_dir>` records the deposit, the package version, the fold map and the sum of every file
written.

`folds.csv` beside the script is the study's own fold map, 894 loggers in ten folds. It is the
map every number below was checked against, and the default.

## Stages

| stage | what it does | writes |
|---|---|---|
| `contract` | the species filter, the fold map, the mask of scorable cells | `cells.csv` |
| `representation` | the record at all seven grains, with every bin count asserted | `representation.csv` |
| `baseline` | the aggregated-feature arms on the deposit's 188 variables, and the penalised fit on the weekly three-channel series | `baseline.csv`, `baseline_series.csv` |
| `selection` | the study's own procedure through `select_grain()`: 33 candidates, ten outer folds, the study's five inner folds, the convolutional encoder | `selection.csv`, `selection_inner.csv`, `selection_cells_auc.csv`, `selection_cells_tss.csv`, `selection_contrast.csv` |
| `networks` | the encoders across the ladder, and the study's eleven-member ensemble, each under TSS and AUC | `networks_mean.csv`, `networks_extremeday.csv`, and `_auc` beside each |
| `contrasts` | every pair of arms, paired inside each cell both scored | `contrasts.csv` |
| `grains` | each window against the window the study took as its architecture's best, in AUC on the window mean | `grain_contrasts.csv` |
| `inflation` | what the reported levels are upper bounds on, and the level each reported score implies | `inflation.csv`, `implied_skill.csv` |

The contract stage always runs, since everything downstream reads its response and its folds. The
default is every stage but `networks` and `selection`, the two that fit encoders.

## The selection stage

The demonstration in the paper chooses one of 33 candidates inside each outer training set, on five
inner folds, by the area under the curve, and refits the winner on the whole training set. That is
what `select_grain()` does, so this stage is the demonstration rather than a description of it:

```
Rscript schrankogel.R <deposit_dir> <out_dir> --stages=contract,baseline,selection \
  --baseline=series --epochs=60
```

A candidate is a window and a summary. The mean is defined at all seven windows, the reading-level
minimum and maximum and their three-channel pair from the half-daily window up, and the two
day-level pairs from the weekly window up, since they reduce to whole days first: 7 + 6 + 6 + 6 + 4
+ 4 = 33. Each is given the two calendar channels the encoders of the study read.

The inner partition is the study's own, `inner_folds.csv` beside the script, which holds one inner
fold per (outer fold, training plot). `--inner=build` deals five inner folds per outer training set
with `fold_map()` instead, which is a different partition of the same design.

It is 1,660 encoder fits at the full candidate set: ten outer folds by five inner folds by 33
candidates, plus one refit per outer fold. The two finest windows are 26,304 and 2,192 steps per
plot and want a graphics processor. `--grains` narrows the windows, and a narrowed run says how
many candidates it searched rather than asserting 33.

## Smoke runs

`--smoke=<outer folds>,<species>` runs the stages on a few folds and the most frequent species, to
see them run before an overnight one. It names every file it writes `smoke_`, records itself as a
smoke run in `run.meta`, and compares nothing with the study. Its output is not the reproduction.

## What each comparison is made at

Every comparison with a published number states its tolerance in the script before anything is
fitted, and `checks.csv` holds the table: the quantity, what this run got, what the study reported,
the difference, the tolerance and whether it is inside. Nothing stops the run, so a number outside
its tolerance is a finding rather than a crash.

The selected window is compared exactly: the study's five-inner-fold selection chose a weekly
candidate in all ten outer folds. Which weekly summary won is reported and not compared: in five of
the ten folds the best and second-best inner scores differ by less than 0.0013, and in one by
0.00001, which is inside the spread of a single encoder fit.

The levels, the margin and every species are compared at tolerances derived from `reference.csv`
and `reference_runs.csv` beside the script, which carry the pipeline's numbers per species and the
spread of its fixed weekly encoder over eleven runs; the derivation is in the script, above the
reference, and its result for the one run made so far is in the section below.

## The environment on the output

`run.meta` records the R version and platform, the package version, and the torch and libtorch
versions and the device, since a network fitted under another libtorch or on another device does
not return the same weights.

## Options

- `--stages` comma-separated stage names.
- `--grains` which grains the network grid covers. Default `day,week,month,season,year`, the
  five coarse grains; `native` and `halfday` are 26,304 and 2,192 steps per plot and want a
  graphics processor, as they had in the study.
- `--learners` which encoders, plus `ensemble` for the eleven-member set. Default `cnn`.
- `--baseline` which arms: `elastic_net` and `stepwise` on the deposit's 188 aggregated variables,
  `series` for the penalised fit on the weekly coldest-day, mean and warmest-day reading of the
  record itself, or several. Default `elastic_net`. Forward selection over 188 columns is one fit
  per candidate per step per species per fold and takes many hours single-threaded.
- `--inner` a CSV of `outer_fold`, `logger_ID` and `inner_fold`. Default `inner_folds.csv` beside
  the script, the study's own inner partition; `--inner=build` deals its own.
- `--smoke` `<outer folds>,<species>`, see Smoke runs.
- `--folds` a CSV of `logger_ID` and `fold`. Default `folds.csv` beside the script, the study's
  own map. `--folds=build` draws a map with `fold_map()` instead, which is a different partition
  of the same design: `fold_map()` draws on R's random stream and the study's map came from
  `rsample`.
- `--epochs` epoch budget per network fit. Default 60, the budget the study used.

## The network grid

The encoders are the study's: the fully connected network at batches of 64, the two convolutional
ones at 32, 60 epochs, early stopping on 15 percent of the fitting plots after ten epochs without
an improvement. Every window gives them the reading with the two channels of its place in the
year; the hourly rung adds the two of its place in the day, the five channels the study's hourly
networks read. The coldest-day reading's arms are named `<window>.extremeday`, so the two readings'
files can be read together without one window's arm standing in for the other's.

Each network is compared with the paper's grid table, level by level, at the tolerance the fixed
weekly encoder's spread over eleven refits gives, which is the one spread that was measured; the
other windows and architectures are assumed to spread as it does.

## The ensemble arm

Asked for with `--learners=...,ensemble`, the study's eleven members run as arms of their own,
each at its own window, channels, kernel, dropout and seed, as the paper's table of members lists
them, and one further arm is the mean of their held-out predictions, scored on the same cells by
the same metric. On the window mean the members read daily, weekly and half-daily windows; on the
coldest-day reading the half-daily and daily members move to weekly and the weekly ones to
monthly. A member's out-of-fold prediction on a fold is its held-out prediction there, so averaging
the eleven and choosing a threshold afterwards is the set scored as one model rather than as a
vote between eleven decisions. All twelve arms are written, so a member's own level is readable
beside the level the set reached.

## The stepwise arm

The study's forward selection fitted each species unweighted, where its elastic net and its
encoders weighted each presence by the ratio of absences to presences. `--baseline=stepwise` fits
under a head registered in the script that differs from the shipped one in that alone. It draws
nothing at random, so it is held to the rounding of the published 0.662.

## What it asserts before fitting anything

The size and the MD5 sum of every deposit file it reads, then the plot count, the species count
after the contract filter, the rarest retained species, the cell count, the reading count, the
readings per plot, and the bin count of every grain. A mismatch stops the run rather than
producing a number nobody can trace to an input.

Every stage reports the fold it is on as it goes, so a run measured in hours is distinguishable
from one that has hung.

## What it costs

Measured on one Windows processor, R 4.6.1:

| stage | time |
|---|---|
| contract | seconds |
| representation, all seven grains | about 2 minutes, 75 s of it reading the 1.2 GB CSV |
| baseline, elastic net, 101 species by 10 folds | tens of minutes |
| inflation, 2000 replicates at three planted levels | minutes |
| networks, coarse grains, one encoder | hours |
| networks, the hourly rung | wants a graphics processor, as it had in the study |

Nothing here caps the data. A stage runs over all 894 plots and all 101 species or it does not run.

## What has been checked against the paper

Run on 2026-09-02 with the study's own fold map, the `folds.csv` shipped beside the script.

| quantity | paper | this run |
|---|---|---|
| plots, species, rarest species | 894, 101, 26 | 894, 101, 26 |
| scorable cells | 1003 of 1010 (99.3%) | 1003 of 1010 (99.3%) |
| species with a scorable fold | 101 of 101 | 101 of 101 |
| bins per grain | 26304, 2192, 1096, 157, 36, 13, 3 | same |
| numbers per plot, weekly three-channel | 471 | 471 |
| numbers per plot, daily three-channel | 3288 | 3288 |
| inflation at truth 0.60, 0.70, 0.90 | +0.110, +0.095, +0.051 | +0.110, +0.095, +0.051 |
| elastic net on the 188 aggregates | 0.687 | 0.686 |

The elastic net sits 0.001 below the published figure. Its penalty is chosen by an inner
cross-validation whose folds are drawn at random, and the two runs seed that stream differently:
the study seeded one stream across its parallel workers, this one seeds once per fitted fold. The
counts and the representation carry no such randomness and reproduce exactly.

That row was read when `elasticnet()` called glmnet. It was read again on 2026-09-20 under the
shared penalised core, on the same deposit and the same fold map, and gives 0.68605: the same
figure, 0.00095 from the published one and inside the 0.002 the script compares at. On
2026-09-21, at `197fd5a`, with the inner cycle Anderson-extrapolated, it gives 0.68606.

## The stepwise arm and the network grid

Run on 2026-09-22 on LiSC, the stepwise arm on one processor node and each architecture of the
grid on one NVIDIA L40S (R 4.5.2, torch 0.17.0, CUDA 12.8), package commit `7f03a3e`, the study's
fold map, 60 epochs and the study's stopping rule; the contrasts across them were read at the same
commit.

| quantity | paper | this run | tolerance | inside |
|---|---|---|---|---|
| stepwise on the 188 aggregates, TSS | 0.662 | 0.6619 | 0.001 | yes |
| stepwise on the 188 aggregates, AUC | 0.844 | 0.8437 | 0.001 | yes |
| elastic net on the 188 aggregates, TSS | 0.687 | 0.6861 | 0.002 | yes |
| grid levels, convolutional network, seven windows | table S12 | -0.005 to +0.010 | 0.0087 AUC, 0.0145 TSS | 14 of 14 |
| grid levels, residual network, seven windows | table S12 | -0.007 to +0.004 | same | 14 of 14 |
| grid levels, fully connected network, six coarse windows | table S12 | -0.002 to +0.002 | same | 12 of 12 |
| fully connected network, hourly, TSS | 0.600 | 0.588 | 0.0145 | yes |
| fully connected network, hourly, AUC | 0.789 | 0.778 | 0.0087 | no, see below |
| fully connected network, hourly, TSS over four seeds | 0.585 (sd 0.011) | 0.582 (sd 0.006) | | level |
| weekly coldest-day network, AUC and TSS | 0.878, 0.712 | 0.875, 0.707 | 0.0087, 0.0145 | yes |
| eleven-member ensemble, window mean, TSS | 0.720 | 0.710 | 0.0145 | yes |
| eleven-member ensemble, coldest-day reading, AUC and TSS | 0.887, 0.727 | 0.880, 0.714 | 0.0087, 0.0145 | yes |
| each window against its architecture's best, AUC | table S12 | 18 of 18 inside | 0.0124 | yes |
| the convolutional network's best window | weekly | weekly | exact | yes |

The stepwise arm draws nothing at random and reproduces to the rounding of the published figures.
Every grid level is inside the spread of one refit against another, and every window's distance
from its architecture's best is inside that tolerance too. The ensemble sits below the paper by
0.010 and 0.013 TSS on its two readings, inside the tolerance and in the same direction on both;
its tolerance is the single encoder's, and an average of eleven fits spreads less, so that is a
looser comparison than the others. The fully connected network and the residual network take
their best window at half-daily and weekly here, where the paper's single seed took daily and
half-daily; the paper's own four seeds move both within the sub-monthly plateau (S5).

The fully connected network at the hourly rung, 131,520 inputs of which 105,216 are the four
calendar channels, is the one cell read against the study's own seeds. Its four seeds here read
0.5875, 0.5856, 0.5831 and 0.5731 TSS, mean 0.582, against the study's four-seed 0.585 (sd 0.011,
`tables_ci_grid/seed_sweep.csv` of the study code), whose published seed is the highest of its
four at 0.600. The AUC of the published seed, 0.789, is 0.011 above this run's seed 1 and 0.013
above the mean of its four (0.776); the study stores no four-seed AUC to set it against.

That cell sat 0.047 TSS below the study until `7f03a3e` (#81). The package standardised the
calendar channels like a reading, which multiplied them by 1.41, where the study's DataLoader
hands them over at their own amplitude. Being identical across units, they reach a network as a
bias, and under AdamW the step that bias takes grows with the scale of the input carrying it. With
that alone changed on the earlier commit `780a91d`, the four seeds moved from 0.5347, 0.5392,
0.5667 and 0.5109 to the four above (`dev_notes/repro81`). Under either scaling about 99 percent
of the first layer's 512 units are zero on every fitting plot, and the fits stop between epochs 12
and 60, where the study's stored checkpoints stopped between 27 and 60.

## The selection stage against the analysis pipeline

The selection stage was run on 2026-09-12 on one NVIDIA L40S (R 4.5.2, torch 0.17.0 with
libtorch 2.8.0 and CUDA 12.8, package commit `5f0a5a6`), with the study's fold map and inner
partition, all 33 candidates at 60 epochs, in 19.7 hours; the series arm ran beside it. The
files of that run are under `dev_notes/repro-lisc/selection33/` in the repository. The
pipeline's side is `review/round2/nested_inner5.py` of the paper's repository, which ranks the
same 33 candidates on the same inner partition and reads the winner's stored held-out
predictions, and its weekly series elastic net from `baseline/08_regularized_glm.R` at a mixing
parameter of 0.5.

The tolerances come from `reference.csv` and `reference_runs.csv`, not from the script: the
pipeline ran its fixed weekly coldest-day, mean, warmest-day encoder eleven times, on the study
map and on ten repeated partitions, and the spread of the level over those runs is 0.0021 AUC and
0.0034 TSS. A level of the encoder here is one fitted run against another, each under its own
seed, so the two differ by sqrt(2) times that spread, and the tolerance is three of those: 0.0087
AUC and 0.0145 TSS. Each species is held to the same rule at its own spread, and the check is
how many of the 101 sit inside their band, with five allowed outside.

The stage was run a second time on 2026-09-15, on another L40S under the same R, torch, libtorch
and CUDA, with the package at `a212771`, which weights the
validation loss early stopping watches (below), from the same deposit, fold map and inner
partition, in 17.4 hours; its files are under `dev_notes/repro-lisc/selection33_fixed/`.

| quantity | pipeline | 2026-09-12, `5f0a5a6` | 2026-09-15, `a212771` | tolerance | inside |
|---|---|---|---|---|---|
| outer folds selecting a weekly candidate | 10 of 10 | 9 of 10, fold 6 monthly | 10 of 10 | exact | no, yes |
| the selected procedure, AUC | 0.8770 | 0.8704 | 0.8717 | 0.0087 | yes |
| the selected procedure, TSS | 0.7098 | 0.7012 | 0.7022 | 0.0145 | yes |
| species inside their own band, AUC | 101 | 100 | 101 | 96 | yes |
| species inside their own band, TSS | 101 | 99 | 101 | 96 | yes |
| the weekly series elastic net, TSS | 0.6962 | 0.6973 | 0.6973 | 0.002 | yes |
| the weekly series elastic net, AUC | 0.8680 | 0.8682 | 0.8682 | 0.002 | yes |
| its species inside their own band, TSS | 101 | 101 | 101 | 96 | yes |
| the procedure over the series elastic net, AUC | +0.0090 (0.0055 to 0.0124) | +0.0022 (-0.0018 to 0.0062) | +0.0034 (-0.0003 to 0.0072) | 0.0087 | yes |

Which weekly summary won differs between the two in most folds, as the script expects: the top
inner scores sit within a thousandth of each other. In the first run the package's inner scores
were level with the pipeline's, fold for fold, to within 0.006, and its held-out level sat 0.0065
AUC below the pipeline's single run, in the same direction in 83 of the 101 species. Under the
weighted validation loss every outer fold selects a weekly candidate, the inner scores sit above
the pipeline's in all ten folds by 0.0007 to 0.0047, and the held-out level sits 0.0053 AUC below
the pipeline's single run (per species mean -0.0053, standard deviation 0.0097), below it in 71
of the 101 species; the pipeline's run sits at the high end of its own eleven, 0.8777 for the fixed
weekly encoder against their mean of 0.8735. In both runs the margin over the series elastic net is
inside the tolerance and, unlike the pipeline's, does not separate from zero across species. What this measures is the two
implementations of one procedure on one dataset; it does not measure agreement between a fitted
R network and a fitted Python network, which the seed spread above says is not a thing to
measure.

### Where the package's encoder sat below the pipeline's, and why

The direction was consistent enough to chase. The package's `cnn()` on the fixed weekly
coldest-day, mean, warmest-day arm, on the study's fold map, 60 epochs, batches of 32, on an RTX
5080 (`dev_notes/repro-lisc/weekly_seeds.R`), reads 0.8684, 0.8708, 0.8695, 0.8673 and 0.8674
AUC under seeds 1 to 5, a mean of 0.8687 with a standard deviation of 0.0015, against the
pipeline's 0.8777 on the study map and 0.8735 over its eleven runs. The inner scores agree fold
for fold, so the difference was in the refit on the outer training set, and the two training
recipes differed in four places: the pipeline standardises the three thermal channels by one
scalar and leaves the calendar channels alone, draws the early-stopping validation plots by a
plain permutation rather than by strata of the response total, reads the validation loss that
early stopping watches under the same positive-class weights as the fitting loss, and cuts
batches with a remainder.

Rather than a seed experiment per variant, each fold was trained once for the whole budget and
the validation loss read both ways after every epoch beside the held-out level of that epoch
(`dev_notes/repro-lisc/stop_rule_diag.R`), so the two stopping rules are compared on one
trajectory. Twelve trajectories, three seeds under each of the package recipe, the shared
standardisation, the random split and the two together, give at the species level:

| rule | mean AUC | range |
|---|---|---|
| the package's, validation loss unweighted | 0.8715 | 0.8678 to 0.8755 |
| the pipeline's, validation loss weighted | 0.8729 | 0.8699 to 0.8753 |
| the lowest weighted validation loss over the whole budget | 0.8760 | 0.8726 to 0.8781 |
| the last epoch of the schedule | 0.8803 | 0.8787 to 0.8820 |
| the best epoch, read on the test fold | 0.8813 | 0.8798 to 0.8829 |

The weighted rule keeps an epoch 0.0014 AUC better on average (paired standard deviation
0.0021, nine of twelve trajectories in that direction), and the package reads the validation
loss that way from version 0.2.0: a response head's `weights` takes the rows the model is fitted
on, reads its counts off those alone and weights every row. On these processor trajectories the
standardisation and the split move the unweighted level by less than the seed spread (0.8696 to
0.8755 under the package's own recipe, 0.8678 to 0.8725 under the others). The last row is the
larger finding: on this data the cosine schedule's final epoch beats either stopping rule by
0.007 to 0.009 AUC and sits within 0.001 of the best epoch, so early stopping on 121 validation
plots costs more than any recipe difference. Training the whole budget and keeping the epoch of
lowest validation loss, which is what `early_stopping = Inf` does while a validation set is held
back, recovers less than half of that. Neither implementation of the study trains to the last
epoch. The package's default does from this version on: `train_control()` holds no validation set
back, trains every fitting plot for the whole budget and keeps the last epoch, and `schrankogel.R`
hands every encoder the study's rule as `train_control(val_frac = 0.15, early_stopping = 10)`.

Five seeds of each variant were then fitted on one NVIDIA L40S
(`dev_notes/repro-lisc/weekly_variants.sh`, `weekly_seeds_fixed.sh`), the variants under the
build the selection stage ran on (`5f0a5a6`, whose training code is the recipe before the fix)
and the last row under `a212771`, which weights the validation loss and keeps the study's stopping
rule. Welch tests against the package's recipe:

| recipe | mean AUC | sd | difference | p |
|---|---|---|---|---|
| the package's | 0.8674 | 0.0029 | | |
| shared standardisation | 0.8689 | 0.0046 | +0.0015 | 0.56 |
| random validation split | 0.8721 | 0.0019 | +0.0047 | 0.019 |
| weighted validation loss | 0.8729 | 0.0016 | +0.0055 | 0.009 |
| all three | 0.8755 | 0.0017 | +0.0081 | 0.001 |
| the package, validation loss weighted | 0.8738 | 0.0019 | +0.0064 | 0.004 |

On the card the random split moves the level as far as the weighting does, and all three
together sit 0.0026 above the weighting alone (p = 0.039), where on the processor trajectories
the split moved nothing detectable. The weighted package reads 0.0003 from the pipeline's
eleven-run mean. The selection stage rerun under `a212771` is in the table above.

The package draws the inner validation set by a plain random permutation from this version on,
where it drew one unit from each of `n_val` equal-count strata of the response total before: the
margin is the same as the weighting's, and it is real on the card even where the standardisation
change is not. The stratified draw's reason still stands for a single rare response fitted alone,
which is the case the docstring's own test covers: it guaranteed the response a presence in every
validation set, and the plain draw can miss it. On the Schrankogel weekly arm that is 101
responses sharing one draw, and the level moved the direction the evidence pointed.
