# Read the DESCRIPTION file.
desc = read.dcf("DESCRIPTION")

if (!"SystemRequirements" %in% colnames(desc)) {
    fmt = c(
        "`SystemRequirements` not found in `DESCRIPTION`.",
        "Please specify `SystemRequirements: Cargo (Rust's package manager), rustc`"
    )
    stop(paste(fmt, collapse = "\n"))
}

# Extract system requirements.
sysreqs = desc[, "SystemRequirements"]

if (!grepl("cargo", sysreqs, ignore.case = TRUE)) {
    stop("You must specify `Cargo (Rust's package manager)` in your `SystemRequirements`")
}

if (!grepl("rustc", sysreqs, ignore.case = TRUE)) {
    stop("You must specify `Cargo (Rust's package manager), rustc` in your `SystemRequirements`")
}

parts = strsplit(sysreqs, ", ")[[1]]
rustc_ver = parts[grepl("rustc", parts)]

no_cargo_msg = c(
    "----------------------- [CARGO NOT FOUND]--------------------------",
    "The 'cargo' command was not found on the PATH. Please install Cargo",
    "from: https://www.rust-lang.org/tools/install",
    "",
    "Alternatively, you may install Cargo from your OS package manager:",
    " - Debian/Ubuntu: apt-get install cargo",
    " - Fedora/CentOS: dnf install cargo",
    " - macOS: brew install rust",
    "-------------------------------------------------------------------"
)

no_rustc_msg = c(
    "----------------------- [RUST NOT FOUND]---------------------------",
    "The 'rustc' compiler was not found on the PATH. Please install",
    paste(rustc_ver, "or higher from:"),
    "https://www.rust-lang.org/tools/install",
    "",
    "Alternatively, you may install Rust from your OS package manager:",
    " - Debian/Ubuntu: apt-get install rustc",
    " - Fedora/CentOS: dnf install rustc",
    " - macOS: brew install rust",
    "-------------------------------------------------------------------"
)

# Add {user}/.cargo/bin to PATH before checking.
new_path = paste0(
    Sys.getenv("PATH"),
    ":",
    paste0(Sys.getenv("HOME"), "/.cargo/bin")
)

Sys.setenv("PATH" = new_path)

rustc_version = tryCatch(
    system("rustc --version", intern = TRUE),
    error = function(
        e
    ) {
        stop(paste(no_rustc_msg, collapse = "\n"))
    }
)

cargo_version = tryCatch(
    system("cargo --version", intern = TRUE),
    error = function(
        e
    ) {
        stop(paste(no_cargo_msg, collapse = "\n"))
    }
)

extract_semver = function(
    ver
) {
    if (grepl("\\d+\\.\\d+(\\.\\d+)?", ver)) {
        return(sub(".*?(\\d+\\.\\d+(\\.\\d+)?).*", "\\1", ver))
    }

    NA
}

msrv = extract_semver(rustc_ver)
current_rust_version = extract_semver(rustc_version)

if (!is.na(msrv)) {
    # compareVersion():
    #   -1 means current is newer
    #    0 means equal
    #    1 means MSRV is newer than current
    is_msrv = utils::compareVersion(msrv, current_rust_version)

    if (is_msrv == 1) {
        fmt = paste0(
            "\n------------------ [UNSUPPORTED RUST VERSION]------------------\n",
            "- Minimum supported Rust version is %s.\n",
            "- Installed Rust version is %s.\n",
            "---------------------------------------------------------------"
        )
        stop(sprintf(fmt, msrv, current_rust_version))
    }
}

versions_fmt = "Using %s\nUsing %s"
message(sprintf(versions_fmt, cargo_version, rustc_version))
