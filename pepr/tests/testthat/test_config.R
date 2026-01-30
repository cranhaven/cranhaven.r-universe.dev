# get data ----------------------------------------------------------------


branch = "master"

cfg = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_basic",
    "project_config.yaml",
    package = "pepr"
)

f = yaml::yaml.load_file(system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_amendments2",
    "project_config.yaml",
    package = "pepr"
))

cfgSubproj = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_amendments2",
    "project_config.yaml",
    package = "pepr"
)

cfgSubtable = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_subtable1",
    "project_config.yaml",
    package = "pepr"
)

cfgBioc = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_BiocProject",
    "project_config.yaml",
    package = "pepr"
)

configConst = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_append",
    "project_config.yaml",
    package = "pepr"
)

#' @keywords internal
.isAbsolute = function(path) {
    if (!is.character(path)) stop("The path must be character")
    return(grepl("^(/|[A-Za-z]:|\\\\|~)", path))
}

# tests -------------------------------------------------------------------


test_that("loadConfig returns correct object type", {
    expect_is(Config(
        system.file(
            "extdata",
            paste0("example_peps-",branch),
            "example_basic",
            "project_config.yaml",
            package = "pepr"
        )
    ), 'Config')
    expect_is(Config(
        system.file(
            "extdata",
            paste0("example_peps-",branch),
            "example_basic",
            "project_config.yaml",
            package = "pepr"
        )
    ), 'list')
})

test_that("loadConfig throws errors", {
    expect_error(Config("a"))
    expect_error(Config(Project(cfg)@config$sample_table))
})

test_that("paths are automatically expanded", {
    cfg=config(Project(cfgSubproj))
    expect_true(.isAbsolute(cfg$output_dir))
    expect_true(.isAbsolute(cfg[["output_dir"]]))
    expect_true(.isAbsolute(cfg["output_dir"][[1]]))
})
