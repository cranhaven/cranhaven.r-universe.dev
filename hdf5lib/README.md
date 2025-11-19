
# **hdf5lib: Standalone HDF5 C Library for R** <img src="man/figures/logo.png" align="right" width="172" height="200" alt="hdf5lib logo" />

[![cran](https://www.r-pkg.org/badges/version/hdf5lib)](https://CRAN.R-project.org/package=hdf5lib)

`hdf5lib` is an R package that provides a self-contained, static build of the [HDF5 C library](https://www.hdfgroup.org/solutions/hdf5/) ([release 1.14.6](https://github.com/HDFGroup/hdf5)). Its **sole purpose** is to allow other R packages to easily link against HDF5 without requiring users to install system-level dependencies, thereby ensuring a consistent and reliable build process across all major platforms.

This package provides **no R functions** and is intended for R package developers to use in the `LinkingTo` field of their `DESCRIPTION` file.


## Features

-   **Self-contained:** Builds the HDF5 library from source using only R and a standard C compiler (like Rtools on Windows, Xcode Command Line Tools on macOS, or `build-essential` on Linux).

-   **No System Dependencies:** Users and dependent packages can be installed without needing system administration rights to install HDF5 via `apt-get`, `brew`, etc.

-   **Compression Support:** Includes built-in support for reading and writing HDF5 files using standard `gzip/deflate` compression via the bundled zlib library.

-   **Includes High-Level API:** Provides the convenient HDF5 High-Level (HL) APIs, including H5LT (Lite), H5IM (Image), and H5TB (Table), alongside the core low-level API.

-   **Safe for Parallel Code:** Compiled with thread-safety enabled. This prevents data corruption and crashes by ensuring that library calls from multiple threads (e.g., via `RcppParallel`) are safely serialized..

-   **Extensible Filter Support:** Enables the HDF5 library to dynamically load external filter plugins (e.g., for Blosc, LZ4, Bzip2) at runtime via `H5Pset_filter_path()`, provided the user has installed those plugins separately.


## **Installation**

You can install the released version of `hdf5lib` from CRAN with:

``` r
install.packages("hdf5lib")
```

Alternatively, you can install the development version from GitHub:

``` r
# install.packages("devtools")  
devtools::install_github("cmmr/hdf5lib")
```

**Note:** As this package builds the HDF5 library from source, the one-time installation may take several minutes. ⏳


## **Usage (For Developers)**

To use this library in your own R package, you need to add `hdf5lib` to `LinkingTo`, create a `src/Makevars` file to link against its static library, and then include the HDF5 headers in your C/C++ code.


### **1. Update your `DESCRIPTION` file**

Add `hdf5lib` to the `LinkingTo` field.

``` yaml
Package: myrpackage  
Version: 0.1.0  
...  
LinkingTo: hdf5lib
```

This step ensures the R build system can find the HDF5 header files in `hdf5lib`.


### **2. Create `src/Makevars`**

Create a file named `Makevars` inside your package's `src/` directory. This tells the build system how to find and link your package against the static HDF5 library.

Add the following lines to `src/Makevars`:

``` makefile
PKG_CPPFLAGS = `$(R_HOME)/bin/Rscript -e "cat(hdf5lib::c_flags())"`
PKG_LIBS     = `$(R_HOME)/bin/Rscript -e "cat(hdf5lib::ld_flags())"`
```

*(Note: You only need this one `src/Makevars` file. The R build system on Windows will use `src/Makevars.win` if it exists, but will fall back to using `src/Makevars` if it's not found. Since these commands are platform-independent, this single file works for all operating systems.)*


### **3. Include Headers in Your C/C++ Code**

You can now include the HDF5 headers directly in your package's `src` files.

``` c
#include <R.h>  
#include <Rinternals.h>

// Include the main HDF5 header  
#include <hdf5.h>

// Optionally include the High-Level header for H5LT etc.  
#include <hdf5_hl.h>

SEXP read_my_hdf5_data(SEXP filename) {  
    hid_t file_id;  
    const char *fname = CHAR(STRING_ELT(filename, 0));

    // Call HDF5 functions directly  
    file_id = H5Fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT);

    // ... your code using HDF5 APIs ...

    H5Fclose(file_id);  
    return R_NilValue;  
}
```


## **Included HDF5 APIs**

This package provides access to the HDF5 C API, including:

### **High-Level (HL) APIs (Recommended for simplicity)**

-   **H5LT (Lite):** Simplified functions for common dataset and attribute operations.
    -   `H5LTmake_dataset_int()`, `H5LTmake_dataset_double()`, etc.
    -   `H5LTread_dataset_int()`, `H5LTread_dataset_double()`, etc.
    -   `H5LTset_attribute_string()`, `H5LTget_attribute_int()`, etc.
    -   `H5LTget_dataset_info()`
-   **H5IM (Image):** Functions for working with image data.
    -   `H5IMmake_image_24bit()`, `H5IMread_image()`
-   **H5TB (Table):** Functions for working with table structures.
    -   `H5TBmake_table()`, `H5TBappend_records()`, `H5TBread_records()`

### **Low-Level APIs (Core functionality for fine-grained control)**

-   **H5F (File):** `H5Fcreate()`, `H5Fopen()`, `H5Fclose()`
-   **H5G (Group):** `H5Gcreate2()`, `H5Gopen2()`, `H5Gclose()`
-   **H5D (Dataset):** `H5Dcreate2()`, `H5Dopen2()`, `H5Dread()`, `H5Dwrite()`, `H5Dclose()`
-   **H5S (Dataspace):** `H5Screate_simple()`, `H5Sselect_hyperslab()`, `H5Sclose()`
-   **H5T (Datatype):** `H5Tcopy()`, `H5Tset_size()`, `H5Tinsert()`, `H5Tclose()` (and predefined types like `H5T_NATIVE_INT`, `H5T_NATIVE_DOUBLE`)
-   **H5A (Attribute):** `H5Acreate2()`, `H5Aopen()`, `H5Aread()`, `H5Awrite()`, `H5Aclose()`
-   **H5P (Property List):** `H5Pcreate()`, `H5Pset_chunk()`, `H5Pset_deflate()`, `H5Pclose()`

For complete documentation, see the official [HDF5 Reference Manual](https://support.hdfgroup.org/documentation/hdf5/latest/_r_m.html).


## **Relationship to `Rhdf5lib`**

The [`Rhdf5lib`](https://doi.org/doi:10.18129/B9.bioc.Rhdf5lib) package also provides the HDF5 C library. `hdf5lib` was created to provide a general-purpose, standalone HDF5 library provider that offers several key distinctions:

-   **Zero Configuration Installation:** `hdf5lib` is designed for simplicity. Installation via `install.packages()` requires no user configuration and reliably provides a modern HDF5 build with important features enabled by default. `Rhdf5lib`, while flexible, requires users to manage compile-time configuration options for a customized build.

-   **Modern HDF5 Version:** `hdf5lib` bundles HDF5 v1.14.6, providing access to more recent features and fixes compared to the version typically bundled in `Rhdf5lib` (v1.12.2 as of Bioconductor 3.19).

-   **Thread-Safety Enabled:** `hdf5lib` builds HDF5 with thread-safety enabled, ensuring safe use with parallel R packages (like `RcppParallel`). `Rhdf5lib` does not support building with this feature.

`hdf5lib` is intended to be a simple and reliable provider of the HDF5 C library for any R package.

## **License**

The `hdf5lib` package itself is available under the MIT license. The bundled HDF5 and zlib libraries are available under their own permissive licenses, as detailed in [inst/COPYRIGHTS](https://github.com/cmmr/hdf5lib/blob/main/inst/COPYRIGHTS).

*(Note: The zlib library is bundled internally but its headers are not exposed).*
