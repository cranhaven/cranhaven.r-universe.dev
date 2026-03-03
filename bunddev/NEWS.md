# bunddev 0.2.0

## Bug Fixes

- Fixed response cache collision for path-based API calls (dip_bundestag): `path` was not included in the cache key, causing all list endpoints to return identical data.
- Fixed dip_bundestag detail endpoints crashing on responses with mixed-length list fields by wrapping list/vector values as list-columns.
- Fixed handelsregister search returning 0 results due to missing JSF submit button field (`form:btnSuche`).
- Fixed handelsregister false-positive error detection (the error-message div is always present in the HTML template).

## Improvements

- Expanded dip_bundestag test coverage to all 16 exported functions.
- Updated active adapter count from 39 to 33.

## Dormant Adapters

Moved 6 adapters to `inst/dormant/` due to upstream issues:

- **interpol**: Akamai JavaScript-based bot detection blocks non-browser clients.
- **zoll**: Endpoints removed (site redesigned) and Radware bot protection.
- **berufssprachkurssuche**: Public OAuth2 credentials revoked by Bundesagentur fuer Arbeit.
- **coachingangebote**: Public OAuth2 credentials revoked by Bundesagentur fuer Arbeit.
- **entgeltatlas**: Public OAuth2 credentials revoked by Bundesagentur fuer Arbeit; confirmed no official public API.
- **weiterbildungssuche**: Endpoint returns HTTP 403; undocumented internal endpoint with revoked credentials.

# bunddev 0.1.0

## Initial Release

First public release of bunddev - a comprehensive R interface to German government and public sector APIs available through bund.dev.

### Package Structure

- **Registry System**: Centralized API registry with automatic endpoint discovery
- **Core OpenAPI Client**: Generic client layer for exploring specs and making requests
- **39 API Adapters**: Ready-to-use functions returning tidy tibbles for:
  - Government data (Bundestag, Bundesrat, Bundeshaushalt)
  - Environmental data (DWD weather, Luftqualität, SMARD energy, Pegel water levels)
  - Public services (Jobsuche, Weiterbildungssuche, Ausbildungssuche)
  - Transportation (Autobahn, Ladestationen)
  - Cultural data (Deutsche Digitale Bibliothek, Tagesschau)
  - And many more (see full list in README)

### Key Features

- **Authentication Support**: Flexible authentication system supporting API keys, OAuth2, and custom schemes
- **Response Caching**: Built-in caching to reduce API load and improve performance
- **Rate Limiting**: Configurable rate limiting to respect API quotas
- **Tidy Output**: All adapters return tibbles with consistent structure
- **Time Handling**: Automatic parsing of timestamps to POSIXct (Europe/Berlin timezone)
- **Parameter Discovery**: Helper functions to explore available parameters for each API
- **Comprehensive Documentation**: Vignettes, function documentation, and examples included

### Bug Fixes

- Fixed R CMD check errors and warnings
- Fixed DDB authentication to use correct OAuth format
- Exported time conversion utilities (`bunddev_ms_to_posix()`, `bunddev_timestamp_to_ms()`)
- Corrected package build configuration

# bunddev 0.0.0.9000 (Development)

- Added shared timestamp helpers and parsed POSIXct time columns (Europe/Berlin) across SMARD, DWD, Autobahn, and Tagesschau adapters.
- SMARD helpers now accept POSIXct/Date timestamps and return parsed `time` columns for series and table outputs.
- DWD helpers add parsed forecast, report, and warning time columns; Tagesschau and Autobahn helpers add parsed date/time columns.
- Documented parameter discovery helpers and time columns in README/vignette and adapter helpfiles.
- Initial package scaffolding with registry and OpenAPI helpers.
