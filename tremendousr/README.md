# tremendousr

#### Easily Send Rewards and Incentives from R

<!-- badges: start -->

[![R-CMD-check](https://github.com/jdtrat/tremendousr/workflows/R-CMD-check/badge.svg)](https://github.com/jdtrat/tremendousr/actions)

<!-- badges: end -->

------------------------------------------------------------------------

<img src="https://jdtrat.com/project/tremendousr/featured-hex.png" width="328" height="378" align="right"/>

[tremendous](https://www.tremendous.com/) is a platform that "empowers companies to buy, track and manage digital and physical payments." This package provides a slightly-opinionated R interface for the 'Tremendous' API with, dare I say, *tremendously* intuitive functions for sending rewards and incentives directly from R.

## Table of contents

-   [Installation](#installation)
-   [Getting Started](#getting-started)
-   [Further Reading](#further-reading)
-   [Feedback](#feedback)
-   [Code of Conduct](#code-of-conduct)

------------------------------------------------------------------------

## Installation

You can install and load the development version of tremendousr from GitHub as follows:

```r
# Install the development version from GitHub
if (!require("remotes")) install.packages("remotes")
remotes::install_github("jdtrat/tremendousr")

# Load package
library(tremendousr)
```

## Getting Started

Tremendous provides two environments for their platform: 
* 'Sandbox' environment, a "free and fully-featured environment for application development and testing."
* 'Production' environment, where real payments can be sent.

> Tremendous API users typically develop their applications against the sandbox environment, and then switch their credentials to production when they are ready to go live.

Unsurprisingly, in order to use this package, you must create a Tremendous account. For the Sandbox environment, you can sign up or log-in [here](https://app.testflight.tremendous.com/) and generate an API key by navigating to **Team Settings > Developers** and clicking on **Add API key** on the top right. You can follow the official documentation [here](https://developers.tremendous.com/reference/making-your-first-request#key--getting-an-api-key).

With an API key, you can create a Tremendous Client in R and send payments as shown below.

```r
test_client <- trem_client_new(api_key = "TEST_YOUR-KEY-HERE",
                               sandbox = TRUE)

trem_send_reward(client = test_client,
             name = "first last",
             email = "email@website.com",
             reward_amount = 10,
             currency_code = "USD",
             delivery_method = "EMAIL",
             payment_description_id = "payment-from-tremendousr-examples",
             funding_source_id = "your-funding-id-from-tremendous",
             reward_types = "Q24BD9EZ332JT", # ID for virtual visa gift card
             parse = TRUE # Return a parsed API response
             )
```

## Further Reading

For a more in-depth explanation of tremendousr, please see the ['Getting Started' Vignette](https://tremendousr.jdtrat.com/articles/tremendousr.html) and the official [Tremendous API Documentation](https://developers.tremendous.com/).

## Feedback

If you want to see a feature, or report a bug, please [file an issue](https://github.com/jdtrat/tremendousr/issues) or open a [pull-request](https://github.com/jdtrat/tremendousr/pulls)! As this package is just getting off the ground, we welcome all feedback and contributions. See our [contribution guidelines](https://github.com/jdtrat/tremendousr/blob/main/.github/CONTRIBUTING.md) for more details on getting involved!

## Code of Conduct

Please note that the tremendousr project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
