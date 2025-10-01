library(parallel)
library(data.table)

# Given a path to a *.tar.gz source package, try to parse its
# DESCRIPTION. Returns either a 'packageDescription2' object, or (if
# failed) a named character vector containing the results of read.dcf(),
# of (if failed) an unnamed character vector containing the contents of
# DESCRIPTION, or NULL (if there was no such file).
extract_one <- function(f) {
	message(basename(f))
	f <- normalizePath(f)
	d <- tempfile(paste0('untar', Sys.getpid()))

	stopifnot(dir.create(d))
	on.exit(unlink(d, TRUE), add = TRUE)

	owd <- setwd(d)
	on.exit(setwd(owd), add = TRUE)

	file.path(basename(f) |> sub('_.*', '', x = _), 'DESCRIPTION') |>
		untar(f, files = _) -> ret
	if (ret) return(NULL)

	file <- Sys.glob('*/DESCRIPTION')
	if (!length(file)) return(file)

	file |>
		tools:::.read_description() |>
		tools:::.split_description() |> try() -> ret
	if (!inherits(ret, 'try-error')) return(ret)

	file |> read.dcf() |> try() -> ret
	if (!inherits(ret, 'try-error')) return(ret)

	readLines(file)
}

# Given a directory containing source packages, apply extract_one() to
# every *.tar.gz file and return a list with basename() as names.
# 'cl' can be a 'parallel' cluster object or the number of processes.
extract_all <- function(d, cl = NULL) {
	if (is.numeric(cl)) {
		cl <- makeCluster(cl)
		on.exit(stopCluster(cl))
	}

	parLapplyLB(
		cl,
		# FIXME: ideally should only look at $d/*.tar.gz and
		# $d/Archive/*/*.tar.gz
		list.files(
			d, pattern = '\\.tar\\.gz$',
			recursive = TRUE, full.names = TRUE
		) |> (\(.) setNames(., basename(.)))(),
		extract_one
	)
}

# Given a list returned by extract_all(), produce a data.table of
# packages and their versions and dependencies.
to_table <- function(l) {
	l |> lapply(\(x)
		# some DESCRIPTIONs fail to parse altogether;
		# others are for Bundles, not Packages, and so on
		if (is.list(x) && !is.na(x$DESCRIPTION['Package']))
			with(x, data.table(
				Package = DESCRIPTION['Package'],
				Version = package_version(DESCRIPTION['Version'], strict = FALSE),
				Depends = list(Depends),
				Imports = list(Imports),
				LinkingTo = list(LinkingTo)
			))
	) |> rbindlist() -> ret
	# duplicates are Recommended packages themselves
	# package_version is a list, so can't use forder()/key/index
	ret[!duplicated(paste(Package, Version)) & !is.na(Version)][base::order(Package, Version)]
}

base_or_recommended <- c("R", "base", "tools", "utils", "grDevices",
"graphics", "stats", "datasets", "methods", "grid", "splines", "stats4",
"tcltk", "compiler", "parallel", "MASS", "lattice", "Matrix", "nlme",
"survival", "boot", "cluster", "codetools", "foreign", "KernSmooth",
"rpart", "class", "nnet", "spatial", "mgcv")

# Which updates lose a strong, non-base-or-Recommended dependency?
incremental_dependency_loss <- \(x) x[,
	# combine all strong dependencies together
	strong :=
		Map(c, Depends, Imports, LinkingTo) |>
		lapply(names) |>
		lapply(setdiff, base_or_recommended)
][
	# filter out packages with only one version
	x[, .(N = .N), by=Package][N > 1],
	on = 'Package',
	nomatch = NULL
][,
	# find out which strong dependencies were lost on updates
	.(
		Package = Package,
		# otherwise data.table's runlock() spends more time walking these than merging
		Version.old = as.character(Version)[-.N],
		Version.new = as.character(Version[-1]),
		dependency_decrease = Map(setdiff, strong[-.N], strong[-1])
	),
	by = Package
]

# Which package versions don't have strong, non-base-or-Recommended
# dependencies missing from the latest version?
cumulative_dependency_loss <- \(x) x[,
	# combine all strong dependencies together
	strong :=
		Map(c, Depends, Imports, LinkingTo) |>
		lapply(names) |>
		lapply(setdiff, base_or_recommended)
][
	# filter out packages with only one version
	x[, .(N = .N), by=Package][N > 1],
	on = 'Package',
	nomatch = NULL
][,
	# find out which strong dependencies were lost on updates
	.(
		Package = Package,
		# otherwise data.table's runlock() spends more time walking these than merging
		Version.old = as.character(Version)[-.N],
		Version.new = as.character(Version[-1]),
		dependency_decrease = Map(setdiff, strong[-.N], MoreArgs = tail(strong,1))
	),
	by = Package
]
