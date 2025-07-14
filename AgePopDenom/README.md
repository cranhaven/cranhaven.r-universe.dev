# AgePopDenom <img src="man/figures/logo.png" align="right" height="138"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/truenomad/AgePopDenom/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/truenomad/AgePopDenom/actions/workflows/R-CMD-check.yaml) [![CodeFactor](https://www.codefactor.io/repository/github/truenomad/agepopdenom/badge)](https://www.codefactor.io/repository/github/truenomad/agepopdenom) [![codecov](https://codecov.io/gh/truenomad/AgePopDenom/graph/badge.svg?token=UL9XKIIXTQ)](https://app.codecov.io/gh/truenomad/AgePopDenom) 
<!-- badges: end -->

## What is AgePopDenom?

**`AgePopDenom`** is an R package designed to facilitate the generation of fine-scale, age-structured population denominators for public health decision-making and service delivery. By combining census and household survey data with a novel parameter-based geostatistical modeling approach, the package produces high-resolution (5km x 5km) population estimates disaggregated by age.

------------------------------------------------------------------------

## Installation

### System Requirements

Before installing **AgePopDenom**, ensure your system meets the following requirements:

1.  **R version**: \>= 4.1.0
2.  **C++ compiler**: C++17 compatible
3.  **TMB** (Template Model Builder)

#### Platform-Specific Setup

**Windows**

1.  Install Rtools (matches your R version):

``` r
# Check if Rtools is installed and properly configured
pkgbuild::has_build_tools()
```

If FALSE, download and install Rtools from: [CRAN Rtools](https://cran.r-project.org/bin/windows/Rtools/)


2. After installation, add Rtools to the system PATH:

```
echo 'export PATH="C:/rtools43/usr/bin;C:/rtools43/mingw64/bin:$PATH"' >> ~/.Renviron
```

3. Restart R and verify the correct compiler setup:

```
Sys.getenv("PATH")
```

It should include `C:/rtools43/usr/bin` and `C:/rtools43/mingw64/bin`.

4. Ensure the correct compiler is available:

```
g++ --version
```

It should output GCC version 10 or later.

5. Set up the Makevars.win file to use the correct compiler:


```
mkdir -p ~/.R
nano ~/.R/Makevars.win
```

Add the following lines:

```
CXX14=C:/rtools43/mingw64/bin/g++
CXX17=C:/rtools43/mingw64/bin/g++
CXX20=C:/rtools43/mingw64/bin/g++
```

Save and exit (`CTRL+X`, then `Y`, then `Enter`).

**macOS**

1.  Install Command Line Tools:

```         
xcode-select --install
```

2.  Alternatively, install gcc via Homebrew:

```         
brew install gcc
```

3. Install LLVM via Homebrew:

```         
brew install llvm
```

4. Set up compiler paths:

For Zsh (default on macOS):

```
echo 'export PATH="/opt/homebrew/opt/llvm/bin:$PATH"' >> ~/.zshrc
echo 'export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"' >> ~/.zshrc
echo 'export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"' >> ~/.zshrc
source ~/.zshrc
```

For Bash:

```
echo 'export PATH="/opt/homebrew/opt/llvm/bin:$PATH"' >> ~/.bashrc
echo 'export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"' >> ~/.bashrc
echo 'export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"' >> ~/.bashrc
source ~/.bashrc
```

5. Verify the correct compiler is now being used:

```
clang++ --version
```

It should output Homebrew Clang (e.g., `Homebrew clang version XXXXX`).

6. Configure R to use LLVM: Modify your `~/.R/Makevars` file:

```

nano ~/.R/Makevars
```

Add the following lines:

```
CXX=/opt/homebrew/opt/llvm/bin/clang++
CXX11=/opt/homebrew/opt/llvm/bin/clang++
CXX14=/opt/homebrew/opt/llvm/bin/clang++
CXX17=/opt/homebrew/opt/llvm/bin/clang++
CXX20=/opt/homebrew/opt/llvm/bin/clang++
```

Save and exit (`CTRL+X`, then `Y`, then `Enter`).

**Linux (Ubuntu/Debian)**

1. Update your system and install necessary packages:

```
sudo apt-get update
sudo apt-get install build-essential libxml2-dev
```

2. Ensure you have GCC installed:

```
sudo apt-get install g++
```

3. Install Clang (optional, if required for TMB compilation):

```
sudo apt-get install clang
```
4. Verify compiler setup:

```
g++ --version
clang++ --version
```

#### Install and Compile TMB

Restart R and install TMB:

```
install.packages("TMB", type = "source")
```

### AgePopDenom installation

Once the setup is complete, follow the instructions below to download **AgePopDenom**

Note: **AgePopDenom** is currently under development. Once it is available on CRAN, you will be able to install it using the following command:

``` r
# install.packages("AgePopDenom")
```

To get the development version from GitHub, use:

``` r
# install.packages("devtools")
devtools::install_github("truenomad/AgePopDenom")
```

Then load it in R:

``` r
library(AgePopDenom)
```

------------------------------------------------------------------------

### Core Functions

1.  Initialize project structure:

``` r
init()
```

2.  Download required data:

``` r
# Example for Kenya and Uganda
countries <- c("KEN", "UGA")

# Get DHS data
download_dhs_datasets(countries, 
                      email = "my_email@example.com",
                      project = "Population denominator project")

# Process DHS data
process_dhs_data()

# Download shapefiles
download_shapefile(countries)

# Download population rasters from worldpop
download_pop_rasters(countries)

# Extract urban extent raster
extract_afurextent()
```

3.  Run full analysis:

``` r
run_full_workflow(countries)
```

## Documentation

For detailed documentation and examples, visit our [package website](https://truenomad.github.io/AgePopDenom/).

## Support and Contributions

For support, bug reports, or feature requests, please contact:

-   **Mo Yusuf** (Package Developer)\
    **Email**: [moyusuf\@who.int](mailto:moyusuf@who.int)\
    **Affiliation**: World Health Organization Regional Office for Africa, P.O. Box 06, Cite du Djoue, Brazzaville, Congo

Alternatively, open an issue on the [GitHub repository](https://github.com/truenomad/AgePopDenom).

We welcome contributions from the community to improve `AgePopDenom`.
