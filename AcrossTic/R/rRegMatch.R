rRegMatch <- function(X, r, y = NULL, dister = "daisy", dist.args = list(), 
         keep.X = nrow (X) < 100, keep.D = (dister == "treeClust.dist"),
         relax = (N >= 100), thresh = 1e-6) {
#
# rRegMatch 2.1: attempted efficiency gains; lpSolve
#
# Arguments: X, matrix of data or object inheriting from "dist"
#            r: integer, degree
#            y: factor, group assignment indicator
#       dister: distance computation function (default, daisy())
#    dist.args: optional list of arguments to dister
#       keep.X: Should we keep data X in the output object?
#       keep.D: Should we keep distances D in the output object?
#        relax: Solve relaxed version of problem? Default FALSE unless N > 100
#       thresh: Elements of solution > this are declared non-zero
# 
# 1. Set up interpoint distance. If X is already a thing of class "dist,"
# just use that. Otherwise, use dister and dist.args to compute distances.
#
if (inherits (X, "dist"))  {
    X.supplied <- FALSE
    D <- X
    keep.X <- FALSE
    N <- attributes(D)$Size 
} else {
    X.supplied <- TRUE
    dist.args$x <- X
    N <- nrow (X)
    D <- do.call (dister, dist.args)
}
# Check that r's value makes sense. It needs to be an integer between
# 1 and N-1. If N is odd, it will be increased by one temporarily (below),
# but that doesn't concern us here.
#
if ((r != as.integer (r)) || r < 1 || r >= N) {
    stop ("r must be an integer between 1 and (size - 1)")
}

#
# N is the # of rows or size of D. It must be bigger than the degree r.
# Moreover, if it's not even we need to take special steps. Add a dummy
# entry whose distance to all other entries is zero. The easy way is to
# turn D back into a matrix, add a row of zeros at the bottom and a column
# of zeros at the right, then turn it back into a dist. This feels expensive.
#
N.WAS.ODD <- FALSE; N.WAS.EVEN <- TRUE
if (N %% 2 != 0) {
    attr.save <- attributes (D)
    D <- as.dist (cbind (rbind (as.matrix (D), 0), 0)) # inefficient?
    attr.save$Size <- attr.save$Size + 1
    if (any (names (attr.save) == "Labels"))
        attr.save$Labels <- c(attr.save$Labels, attr.save$Size)
    attributes (D) <- attr.save
    N.WAS.EVEN <- FALSE; N.WAS.ODD <- TRUE
    N <- attr (D, "Size")
}

#
# Build a two-column matrix with all combinations (i, j) of two values from 
# 1, ..., N with i < j. const.txt is a text version with entries like "5.10".
# So nrow (const.rows) = length (const.txt) = choose (N, 2).
#
const.rows <- cbind (rep (1:(N-1), (N-1):1), 
                    unlist (sapply (2:N, seq, to = N)))
const.txt <- paste0 (const.rows[,1], ".", const.rows[,2])
#
# This constructs a "dense" matrix suitable for lpSolve, but the
# first two columns will also be useful in the call to Rsymphony.
# This thing has first column = "constraint's row," second column is
# "variable associated with this constraint" and the third is the 
# value -- which is 1.
#
const.mat.dense <- cbind (rep (1:N, each = N-1),
             c(sapply (1:N, function (x) match (const.txt[const.rows[,1] == x | const.rows[,2] == x], const.txt))),
             rep (1, N * (N-1)))
#
# Set up constraint directions, right-hand sides, and all.bin
#
const.dir <- rep ("=", N)
const.rhs <- rep (r, N)
if (relax) all.bin <- FALSE else all.bin <- TRUE
#
# For lpSolve, upper bounds need to be entered into the constraint
# matrix. There are N-choose 2 constraints so far; we need to add that
# many more. The ith new one says that variable i <= 1. 
# The first column is just the constraint number. 
#
nc2 <- N * (N-1) / 2
uppers <- cbind ((N+1):(N + nc2), 1:nc2, 1)
const.mat.dense <- rbind (const.mat.dense, uppers)
const.dir <- c(const.dir, rep ("<", nc2))
const.rhs <- c(const.rhs, rep (1, nc2))
#
# Call lpSolve solver
#
this.took <- system.time (
RsOut <- lp ("min", objective.in = c(D), dense.const = const.mat.dense, 
             const.dir = const.dir, const.rhs = const.rhs, all.bin = all.bin)
)
#
# These are the matches, in text form.
#
assigns <- const.txt[RsOut$solution > thresh]
#
# Convert that to a two-column matrix of pairs.
#
S <- matrix (as.numeric (unlist (strsplit (assigns, "\\."))), ncol = 2, byrow=2)
#
# Build result. If this started with odd N, omit any mention of the
# N-th observation, which is the dummy one.
#
if (N.WAS.ODD) {
    S <- S[S[,1] != N & S[,2] != N,]
    N <- N - 1
    D <- as.dist ((as.matrix (D)[1:N,1:N]))
}
result <- list(matches = S, total.dist = RsOut$objval, status = RsOut$status,
               time.required = this.took)
#
# "Bug": if r = N-1, our problem is degenerate. The solution is correct,
# but the status is TM_UNBOUNDED. Let's make it TM_OPTIMAL_SOLUTION_FOUND.
#
if (r == N-1) result$status <- c("TM_OPTIMAL_SOLUTION_FOUND" = 0)

result$call <- match.call ()
result$r <- r
result$dister <- dister
result$dist.args <- dist.args
result$X.supplied <- X.supplied
result$relax <- relax
if (X.supplied && keep.X) result$X <- X
if (keep.D) result$D <- D
if (!missing (y) && length (y) != N)
    warning ("Length of y was not equal to N")
result$y <- y

# Save the costs. Should this be a third column of S?
result$edge.weights <- RsOut$solution[RsOut$solution > thresh]

#
# If y was supplied, compute two (for now) cross-count statistics.
# One is the simple count of the number of cross-group matchings -- 
# only we have to account for weights if relax=TRUE. In that case,
# extract only the weights > thresh. If that vector isn't the same
# length as nrow(S), we have trouble. The second is the sum of the
# distances associated with cross-group matching. That weird formula
# in S.num comes from help(dist).
#

if (!is.null (y)) {
    S.num <- N * (S[,1] - 1) - (S[,1] * (S[,1] - 1)/2) + S[,2] - 1
    result$cross.sum <- sum (D[S.num][y[S[,1]] != y[S[,2]]])
    result$cross.count <- sum (result$edge.weights[y[S[,1]] != y[S[,2]]])
}

if (X.supplied) {
    result$nrow.X <- nrow (X)
    result$ncol.X <- ncol (X)
} else {
    result$nrow.X <- N
}

class (result) <- "AcrossTic"

return (result)
}

