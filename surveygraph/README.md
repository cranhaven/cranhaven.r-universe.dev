# surveygraph

<!-- badges: start -->
[![R-CMD-check](https://github.com/surveygraph/surveygraphr/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/surveygraph/surveygraphr/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/surveygraph/surveygraphr/graph/badge.svg?token=SHFUE2Z36X)](https://app.codecov.io/gh/surveygraph/surveygraphr)
<!-- badges: end -->

## Overview

surveygraph is a lightweight library used to produce network representations of attitudes, as they emerge in tabular sociological data such as surveys.

We assume tabular data with rows representing agents, such as individuals, and columns representing items, such as survey questions. Table entries represent responses by agents to items. By computing the similarity between each pair of rows and each pair of columns we obtain, respectively,

1. the *agent* network, where nodes represent agents, with weighted edges drawn between similar agents
2. the *symbolic* network, where nodes represent items, with weighted edges drawn between similar items

By plotting these networks with tools such as igraph, we obtain accessible and visually striking representations of structure within the survey data.

## Installation

Still in the development stage, the surveygraph package  is not yet available on CRAN, but can be installed from [GitHub](https://github.com/surveygraph/surveygraphr/) via the devtools package as follows.

```
library("devtools")
devtools::install_github("surveygraph/surveygraphr")
```

The package can then be loaded in an R session as follows.

```
library("surveygraph")
```

## Usage

Generate a synthetic survey dataset

```
S <- make_synthetic_data(500, 9, polarisation=0.2)
```

Compute a network representation of the survey respondents, retaining edges if the corresponding pair of respondents are sufficiently similar

```
e <- make_projection(S, layer="agent")

```

## Gallery

<a><img src="man/figures/eg1.jpg" align="center" height="238" /></a>

## Getting help

Please contact samuel.l.unicomb@ul.ie for support, and report bugs with a minimal illustrative example on [GitHub](https://github.com/surveygraph/surveygraphr/issues/). Since the package is in its infancy, we welcome feature requests and general feedback.
