library(lubridate)
library(lmomco)
library(SPEI)

# work
(res <- tenterfield |>
  init(id = id, time = ym, indicators = prcp:tavg) |>
  aggregate(.var = prcp, .scale = 12) |>
  # need to load package lubridate and lmomco
  dist_fit(.dist = gamma(), .method = "lmoms", .var = .agg))

# work
(res <- tenterfield |>
  init(id = id, time = ym, indicators = prcp:tavg) |>
  aggregate(.var = prcp, .scale = c(6, 12)) |>
  dist_fit(.dist = list(gamma(), loglogistic()), .method = "lmoms", .var = .agg))

(res <- tenterfield |>
  init(id = id, time = ym, indicators = prcp:tavg) |>
  aggregate(.var = prcp, .scale = 12) |>
  dist_fit(.dist = gamma(), .method = "lmoms", .var = .agg) |>
  augment(.var = .agg))

# calculating SPEI using different methods on PET
# there is also the penman method, which requires monthly mean daily wind speed at 2m height
library(SPEI)
res2 <- tenterfield |>
  init(id = id, time = ym, indicators = prcp:tavg) |>
  var_trans(.method = thornthwaite, .vars = tavg, lat = lat, .new_name = "pet") |>
  dim_red(diff = prcp - pet) |>
  aggregate(.var = diff, .scale = 12) |>
  dist_fit(.dist = list(gamma(), loglogistic()), .method = "lmoms", .var = .agg) |>
  augment(.var = .agg)

res3 <- tenterfield |>
  init(id = id, time = ym, indicators = prcp:tavg) |>
  var_trans(.method = hargreaves, Tmin = tmin, Tmax = tmax, lat = -29.0479, .new_name = "pet") |>
  dim_red(diff = prcp - pet) |>
  aggregate(.var = diff, .scale = 12) |>
  dist_fit(.dist = loglogistic(), .method = "lmoms", .var = .agg) |>
  augment(var = .agg)

res |>
  ggplot(aes(x = ym, y = .index, id = id)) +
  geom_line() +
  geom_line(data = res3, color = "red") +
  theme_benchmark()

#######################################################################################
# Reconnaissance Drought Index (RDI)
res3 <- tenterfield |>
  init(id = id, time = ym, indicators = prcp:tavg) |>
  var_trans(method = thornthwaite, Tave = tavg, lat = -29.0479, new_name = "pet") |>
  dim_red(expr = prcp/ pet, new_name = "r") |>
  aggregate(.var = r, .scale = 12) |>
  var_trans(y = log10(.agg),
            index = rescale_zscore(y)) # currently rescaling in also implemented under var_trans()

# two ways to write it - they are equivalent:
tenterfield |>
  init(id = id, time = ym, indicators = prcp:tavg) |>
  var_trans(.var = rescale_zscore(prcp))
  #var_trans(method = rescale_zscore, var = prcp)

# Effective Drought Index (EDI) - for daily data
#w <- purrr::map_dbl(1: 12, ~digamma(.x + 1) - digamma(1)) |> rev()
out <- tenterfield |>
  init(id = id, time = ym, indicators = prcp:tavg) |>
  var_trans(w = rev(digamma(dplyr::row_number() + 1) - digamma(1)),
            mult = prcp * w) |>
  aggregate(.var = mult, .scale = 12, sum, .new_name = "ep") |>
  var_trans(ep_norm = rescale_zscore(ep)) # this is rescaling

out$data |>
  ggplot(aes(x = ym, y = ep_norm)) +
  geom_line()
