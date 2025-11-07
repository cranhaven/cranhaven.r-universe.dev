<!-- badges: start -->

[![R-CMD-check](https://github.com/vusaverse/vvshiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vusaverse/vvshiny/actions/workflows/R-CMD-check.yaml) [![CodeFactor](https://www.codefactor.io/repository/github/vusaverse/vvshiny/badge)](https://www.codefactor.io/repository/github/vusaverse/vvshiny) <!-- badges: end -->

# vvshiny

vvshiny is a helpful package that provides helper and wrapper functions for making shinydashboards more easily. With modular functions and support for choices on multiple levels, vvshiny simplifies the process of creating interactive and dynamic Shiny dashboards.

### Installation

You can install vvshiny directly from GitHub using the remotes package:

``` r
remotes::install_github("vusaverse/vvshiny")
```

### Features

#### gantt_app

One of the main functions provided by vvshiny is gantt_app. This function allows you to create Gantt charts within your Shiny dashboard. It takes the following parameters: - df: The data frame containing the Gantt chart data. - df_config_gantt: The data frame specifying the configuration options for the Gantt chart. - id: (Optional) The ID of the Gantt chart component (default: "gantt").

Here's an example of how to use the gantt_app function:

``` r
library(vvshiny)

# Create the Gantt chart app
gantt_app(df = my_data, df_config_gantt = my_config, id = "my_gantt_chart")
```

### Development

The package is still under active development. In addition to gantt more charts will be provided. In addition, the parameters of the exported functions will be made more flexible. Integrations with datamods and a LLM are also desired.

### Contribution

-   Feel free to open an issue or suggest improvements
-   Pull requests are in general also welcome
