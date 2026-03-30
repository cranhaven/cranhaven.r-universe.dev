# dialvalidator <a href="https://pcstrategyandopsco.github.io/dialvalidator/"><img src="man/figures/logo.png" align="right" height="139" alt="dialvalidator logo" /></a>

<!-- badges: start -->
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-blue)](https://pcstrategyandopsco.github.io/dialvalidator/)
<!-- badges: end -->

Parse, validate, format, and classify international phone numbers in R
-- powered by Google's [libphonenumber](https://github.com/google/libphonenumber)
metadata. No Java required.

## Overview

Phone number validation is surprisingly hard. A 10-digit number that's valid
in the US is invalid in New Zealand. Country code +1 is shared by 25
territories. Local formatting rules vary wildly. Google's libphonenumber
project has spent over a decade cataloguing these rules across 240+
territories, and is the validation engine behind Android, WhatsApp, and most
major telecoms.

**dialvalidator** brings this metadata to R by parsing libphonenumber's
`PhoneNumberMetadata.xml` directly into a native R object -- no Java, no
rJava, no JDK configuration. Install it like any other R package and start
validating immediately.

### What it does

- **Validates** phone numbers against per-territory patterns and length rules
- **Formats** numbers as E.164, national, or international
- **Classifies** numbers as mobile, fixed-line, toll-free, premium-rate, VoIP, and more
- **Resolves** country from number (including NANPA: +1 US vs +1 CA vs +1 JM)
- **Handles** national-format input when you specify the default region

All functions are vectorised and return simple R types (`logical`, `character`,
`data.frame`).

## Installation

```r
# install.packages("pak")
pak::pak("pcstrategyandopsco/dialvalidator")
```

## Examples

```r
library(dialvalidator)
```

### Validation

```r
# International format -- region is inferred from the number
phone_valid("+64211234567")
#> TRUE

phone_valid("+6421")
#> FALSE (too short)

# National format -- supply the region
phone_valid("021 123 4567", default_region = "NZ")
#> TRUE

# Vectorised -- validate a whole column at once
phone_valid(c("+64211234567", "+12125551234", "not a number", "+61412345678"))
#> TRUE TRUE FALSE TRUE
```

### Formatting

Three output formats, matching libphonenumber's conventions:

```r
phone_format("+64211234567", "E164")
#> "+64211234567"

phone_format("+64211234567", "NATIONAL")
#> "021 123 4567"

phone_format("+64211234567", "INTERNATIONAL")
#> "+64 21 123 4567"

# US number
phone_format("+12125551234", "NATIONAL")
#> "(212) 555-1234"
```

### Type detection

Classifies numbers by matching against per-territory patterns for each number
type. Patterns are defined by Google's libphonenumber team and updated
regularly.

```r
phone_type("+64211234567")
#> "mobile"

phone_type("+6493001234")
#> "fixed_line"

phone_type("+64800123456")
#> "toll_free"

phone_type("+18005551234")
#> "toll_free"
```

Possible types: `mobile`, `fixed_line`, `fixed_line_or_mobile`, `toll_free`,
`premium_rate`, `shared_cost`, `personal_number`, `voip`, `pager`, `uan`,
`voicemail`, `unknown`.

### Country resolution

```r
phone_country("+64211234567")
#> "NZ"

phone_country("+12125551234")
#> "US"

# NANPA resolution -- +1 416 is Toronto, not New York
phone_country("+14165551234")
#> "CA"
```

### All-in-one with phone_info()

For batch processing or exploratory analysis, `phone_info()` returns
everything in a single data frame:

```r
phone_info(c("+64211234567", "+12125551234", "+61412345678"))
#>            raw         e164       national   international region country_code   type valid
#> 1 +64211234567 +64211234567   021 123 4567 +64 21 123 4567     NZ           64 mobile  TRUE
#> 2 +12125551234 +12125551234 (212) 555-1234 +1 212-555-1234     US            1  ...   TRUE
#> 3 +61412345678 +61412345678  0412 345 678  +61 412 345 678     AU           61 mobile  TRUE
```

## API reference

| Function | Returns | Description |
|----------|---------|-------------|
| `phone_parse()` | list | Parse into components (country code, national number, region) |
| `phone_valid()` | logical | Is the number valid? |
| `phone_format()` | character | Format as E164, NATIONAL, or INTERNATIONAL |
| `phone_type()` | character | Classify number type (mobile, fixed_line, toll_free, ...) |
| `phone_country()` | character | ISO 3166-1 alpha-2 region code |
| `phone_info()` | data.frame | All of the above in one call |
| `dv_metadata()` | list | Access the full metadata object |
| `dv_territory()` | list | Metadata for a specific territory |
| `dv_update_metadata()` | (side effect) | Download latest metadata from upstream |

## Performance

Benchmarked on Apple M1, R 4.5.2:

| Operation | Single call | Batch (per number) | Throughput |
|-----------|-------------|--------------------|-----------:|
| `phone_valid()` | ~170 µs | ~25 µs | ~4,000/sec |
| `phone_type()` | ~160 µs | ~25 µs | ~4,000/sec |
| `phone_format()` | ~500 µs | ~50 µs | ~2,000/sec |
| `phone_info()` | ~1 ms | ~90 µs | ~1,100/sec |

Single-call overhead is dominated by R function dispatch. In batch mode
(vectorised input), throughput scales well -- 10,000 numbers validate in
under 3 seconds.

## Keeping metadata current

The package ships with pre-parsed metadata covering 240+ territories. No
internet access is needed at install time or during normal use.

To pull the latest upstream metadata (new area codes, format changes):

```r
dv_update_metadata()
```

Updated metadata is cached in `tools::R_user_dir("dialvalidator", "cache")`
and used for the remainder of the session.

## Acknowledgements

This package would not exist without
[libphonenumber](https://github.com/google/libphonenumber), Google's
open-source phone number handling library. Originally developed for Android
and now maintained by a dedicated team of contributors, libphonenumber is
the world's most comprehensive and rigorously maintained database of phone
number validation rules, formatting patterns, and metadata -- covering 240+
territories, hundreds of number types, and countless edge cases accumulated
over more than a decade of real-world use.

The `PhoneNumberMetadata.xml` file that powers this package represents an
extraordinary body of work: every territory's numbering plan, every carrier's
allocation pattern, every formatting convention -- painstakingly documented
and continuously updated. We are deeply grateful to the libphonenumber authors
and contributors for making this resource available under the Apache 2.0
license.

We also acknowledge the [dialr](https://github.com/socialresearchcentre/dialr)
package by Danny Smith, which first brought libphonenumber to R via rJava
and demonstrated the value of proper phone number handling in the R ecosystem.

## Comparison to dialr

| | dialvalidator | dialr |
|---|---|---|
| Java required | No | Yes (rJava + JDK) |
| Dependencies | stringr, cli, rlang | rJava, libphonenumber JAR |
| Install | `pak::pak()` | JDK install + rJava configuration |
| Metadata updates | `dv_update_metadata()` | Update JAR file |
| Coverage | 240+ territories | 240+ territories |
| API style | Character vectors in/out | Custom S3 class (`phone`) |

## License

Apache License 2.0. See the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) for details.
