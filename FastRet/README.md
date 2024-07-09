<!-- badges: start -->
[![R CMD check](https://github.com/spang-lab/FastRet/workflows/r-cmd-check/badge.svg)](https://github.com/spang-lab/FastRet/actions)
<!-- badges: end -->

# FastRet

FastRet is an R package for predicting retention times in liquid chromatography. It can be used through the R console or through a graphical user interface (GUI). The package's key features include the ability to

1. Train new predictive models specific for your own chromatography column
2. Use pre-trained models to predict retention times of molecules
3. Adjust pre-trained models to accommodate modifications in chromatography columns

## Installation

You can install the development version of FastRet from [GitHub](https://github.com/) with:

```R
install.packages("devtools")
devtools::install_github("spang-lab/FastRet", build_vignettes = TRUE)
```

## Usage

The easiest way to use FastRet is through its GUI. To start the GUI, [install the package](#installation) and then run the following command in an interactive R terminal:

```R
FastRet::start_gui()
```

After running the above code, you should see an output like

```
Listening on http://localhost:8080
```

in your R console. This means that the GUI is now running and you can access it via the URL `http://localhost:8080` in your browser. If your terminal supports it, you can also just click on the displayed link.

<div style="display: inline-block;">
<img src="https://raw.githubusercontent.com/spang-lab/FastRet/main/vignettes/GUI-Usage/start-page.png" alt="start-page.png" width="45%">
<img src="https://raw.githubusercontent.com/spang-lab/FastRet/main/vignettes/GUI-Usage/mode-help.png" alt="mode-help.png" width="45%">
</div>

By default, the GUI opens in Mode *Train new Model*. To apply or adjust pretrained models, select mode *Predict Retention Time* or *Adjust existing Model* instead. For more information about the individual modes and the various input fields, click on the little question mark symbols next to the different input fields or have a look at the documentation page for [GUI Usage](https://spang-lab.github.io/FastRet/articles/GUI-Usage.html).

## Documentation

FastRet's documentation is available at [spang-lab.github.io/FastRet](https://spang-lab.github.io/FastRet/). It includes pages about

- [GUI Usage](https://spang-lab.github.io/FastRet/articles/GUI-Usage.html)
- [CLI Usage](https://spang-lab.github.io/FastRet/articles/CLI-Usage.html)
- [Package Internals](https://spang-lab.github.io/FastRet/articles/Package-Internals.html)
- [Contribution Guidelines](https://spang-lab.github.io/FastRet/articles/Contributing.html)
- [Function Reference](https://spang-lab.github.io/FastRet/reference/index.html)
