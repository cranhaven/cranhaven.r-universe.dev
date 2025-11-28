Vicinities <- function(data, groups, num=NULL, centers=NULL, method.c="median", method.d="manhattan") { # Manhattan distance is absolute deviation, analogous to median
data <- as.data.frame(data, row.names=seq_len(nrow(data))) # to avoid custom row names, and also because split() treats matrices as vectors
.distances <- function(method.d, x) as.matrix(dist(x, method=method.d))
if (is.null(centers)) {
res <- lapply(split(data, groups), function(.x)
 as.numeric(names( # because split.data.frame() keeps row names
  sort(
   .distances(
     method.d,
     rbind(.x, # data first to prevent rbind() change row names
      sapply(.x, get(method.c), na.rm=TRUE)) # naive (univariate) centroid computation
    )[nrow(.x)+1, -(nrow(.x)+1)] # keep only distances from center (last row) to all observations except itself
 ))))
} else {
splitted <- split(data, groups)
if (!is.factor(groups)) groups <- as.factor(groups) # split() converts to factor automatically but here we need levels()
indices <- setNames(1:nlevels(groups), levels(groups)) # to keep group names in the result
res <- lapply(indices, function(.i) # lapply() on indices to use them as counter merging 'centers' and 'data'
 as.numeric(names(
  sort(
   .distances(
     method.d,
     rbind(splitted[[.i]],
      centers[.i, ])
    )[nrow(splitted[[.i]])+1, -(nrow(splitted[[.i]])+1)]
))))
}
if (!is.null(num)) res <- lapply(res, `[`, 1:num)
res
}
