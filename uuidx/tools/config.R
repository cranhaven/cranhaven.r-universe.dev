# Note: Any variables prefixed with `.` are used for text
# replacement in the Makevars.in and Makevars.win.in

# Check the package MSRV first.
source("tools/msrv.R")

# Check DEBUG and NOT_CRAN environment variables.
env_debug = Sys.getenv("DEBUG")
env_not_cran = Sys.getenv("NOT_CRAN")

# Check whether the vendored archive exists.
vendor_exists = file.exists("src/rust/vendor.tar.xz")

is_not_cran = env_not_cran != ""
is_debug = env_debug != ""

if (is_debug) {
    # DEBUG implies a non-CRAN build.
    is_not_cran = TRUE
    message("Creating DEBUG build.")
}

if (!is_not_cran) {
    message("Building for CRAN.")
}

# Set CRAN flags only when NOT_CRAN is empty and vendored crates exist.
.cran_flags = ifelse(!is_not_cran && vendor_exists, "-j 2 --offline", "")

# Use a release build unless DEBUG is set.
.profile = ifelse(is_debug, "", "--release")
.clean_targets = ifelse(is_debug, "", "$(TARGET_DIR)")

# Specify this target when building for webR.
webr_target = "wasm32-unknown-emscripten"
is_wasm = identical(R.version$platform, webr_target)

if (is_wasm) {
    message("Building for WebR")
}

# In webR builds, prepend the target triple to the library path.
target_libpath = if (is_wasm) "wasm32-unknown-emscripten" else NULL
cfg = if (is_debug) "debug" else "release"

.libdir = paste(c(target_libpath, cfg), collapse = "/")
.target = ifelse(is_wasm, paste0("--target=", webr_target), "")

is_windows = .Platform[["OS.type"]] == "windows"

mv_fp = ifelse(is_windows, "src/Makevars.win.in", "src/Makevars.in")
mv_ofp = ifelse(is_windows, "src/Makevars.win", "src/Makevars")

if (file.exists(mv_ofp)) {
    message("Cleaning previous `", mv_ofp, "`.")
    invisible(file.remove(mv_ofp))
}

mv_txt = readLines(mv_fp)

new_txt = gsub("@CRAN_FLAGS@", .cran_flags, mv_txt)
new_txt = gsub("@PROFILE@", .profile, new_txt)
new_txt = gsub("@CLEAN_TARGET@", .clean_targets, new_txt)
new_txt = gsub("@LIBDIR@", .libdir, new_txt)
new_txt = gsub("@TARGET@", .target, new_txt)

message("Writing `", mv_ofp, "`.")

con = file(mv_ofp, open = "wb")
writeLines(new_txt, con, sep = "\n")
close(con)

message("`tools/config.R` has finished.")
