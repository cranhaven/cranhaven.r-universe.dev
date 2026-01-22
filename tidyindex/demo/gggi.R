dt <- gggi |>
  na.omit() |>
  init(id = country) |>
  add_meta(gggi_weights |> mutate(weight = weight * 0.25), var_col = variable)

dt2 <- dt |>
  dimension_reduction(
    index1 = aggregate_linear(
      ~labour_force_participation:years_with_female_head_of_state, weight = weight)
    )

dt2$data |>
  ggplot(aes(x = index, y = index1)) +
  geom_point() +
  theme(aspect.ratio = 1)

###############################################################################
#how a small changes in weights will causes the index to change.
w_proj2idx <- function(w){w/sum(w)}
w_idx2proj <- function(w){w/(sqrt(sum(w^2))) |> matrix(nrow = length(w))}
#animate_dist(dt$data[, 4:7], local_tour(matrix(w_idx2rpoj(rep(0.25, 4))), angle = pi/10))
t1 <- save_history(dt$data[,4:7], local_tour(w_idx2proj(rep(0.25, 4)), angle = pi/20))
#animate_idx(dt$data[,4:7], planned_tour(t1))
a <-  apply(t1, 3, identity, simplify = FALSE)
local_tour_weights <- a[map_lgl(1:100, ~all(a[[.x]] > 0))]|>
  purrr::map(w_proj2idx) |>
  purrr::reduce(cbind)
colnames(local_tour_weights) <- paste0("V", 1:ncol(local_tour_weights))
new_weights <- gggi_weights |>
  filter(!is.na(pillar_weight)) |>
  dplyr::select(-c(std:weight)) |>
  dplyr::bind_cols(local_tour_weights)

dt <- gggi |>
  dplyr::select(country:political_empowerment) |>
  na.omit() |>
  init(id = country) |>
  add_meta(new_weights, var_col = variable)

dt2 <- dt |>
  dimension_reduction(
    index_new = aggregate_linear(
      ~economic_participation_and_opportunity:political_empowerment,
      weight = pillar_weight))

dt2$data |>
  ggplot(aes(x = index, y = index_new)) +
  geom_point() +
  theme(aspect.ratio = 1)

dt3 <- dt2 |>
  swap_values(.id = 1, .param = weight,
              .value = list(pillar_weight, paste0("V", 2:100)),
              .raw_data = dt)

idx_wide <- dt3$data |> select(country, dplyr::contains("index_new"))
idx_wide2 <- idx_wide |> mutate(index_avg = rowMeans(idx_wide[,2:(ncol(idx_wide))]))
idx_long2 <- idx_wide2 |> pivot_longer(-country, names_to = "index", values_to = "value")

idx_long2 |>
  ggplot(aes(x = value, y = fct_reorder(country, value), group = country)) +
  geom_point(color = "grey80", size = 0.5) +
  geom_point(data = idx_long2 |> filter(index == "index_new1"), color = "red")

###############################################################################
set.seed(123)
w_idx <- stats::runif(4, min = 0, max = 1)
w_idx <- w_idx/sum(w_idx)
w_proj <- normalise(w_idx/sum(w_idx))
sum(w_idx) == 1
sum(w_proj^2) == 1

w_proj <- basis_random(n = 4, d= 1)
w_idx <- w_proj/sum(w_proj)
sum(w_idx) == 1
sum(w_proj^2) == 1

set.seed(123)
w_idx <- map(1:100, ~{w_idx <- stats::runif(4, min = 0, max = 1);
                      w_idx <- w_idx/sum(w_idx);
                      w_idx |> as.matrix(nrow = 4, ncol = 1)})
w_proj <- map(w_idx, ~normalise(.x/sum(.x)))
# res <- unlist(w_proj) |> array(dim = c(4, 1, 100))
# interpolate(res)
animate_dist(dt$data[, 4:7], planned_tour(w_proj)) # this needs a better display

v <- rep(0.25, 4)
w <- v/(sqrt(sum(v^2))) |> matrix(nrow = 4)
animate_dist(dt$data[, 4:7], local_tour(w, angle = pi/10))


###############################################################################

dt |> dimension_reduction(index2 = pca(~., ))

# this now creates an aggregation specification for aggregation
a <- aggregate(~labour_force_participation:years_with_female_head_of_state, weight = weight, method = "arithmetic")
b <- aggregate_manual(~(exp_sch + avg_sch)/2)
