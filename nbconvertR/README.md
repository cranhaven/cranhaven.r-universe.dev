nbconvertR
==========

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/theislab/nbconvertR.svg?branch=master)](https://travis-ci.com/theislab/nbconvertR)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/theislab/nbconvertR?branch=master&svg=true)](https://ci.appveyor.com/project/theislab/nbconvertR)
<!-- badges: end -->

A small shim to use [`jupyter`][jupyter]’s [`nbconvert`][nbconvert] as [vignette engine][] and Jupyter notebooks as vignette sources.

To get started, create a notebook in the `vignettes/` directory of your package, and a `.ipynbmeta` file of the same name next to it.

E.g. next to the notebook `vignettes/floob.ipynb`, create `vignettes/floob.ipynbmeta`:

```latex
%\VignetteIndexEntry{About Floob}
%\VignetteEngine{nbconvertR::nbconvert}
```

Don’t forget `VignetteBuilder: nbconvertR` in your `DESCRIPTION` file!

Options
-------

There are some customization options available that you can put into your `.ipynbmeta` file.

1. You can use custom templates via `%\VignetteTemplate{<format>}{<filename>}`.

	E.g. `%\VignetteTemplate{latex}{floob.tplx}` will result in `nbconvert --template floob.tplx ...` being called when converting to LaTeX.

2. Another mighty customization option are preprocessors: `%\VignettePreprocessors{<format>}{<module>.<Preproc>[, ...]}`

	This will pass `--<Format>Exporter.preprocessors=["<module>.<Preproc>",...]` to `nbconvert`. It’s possible to specify multiple comma-separated preprocessors in one line.

Installation
------------

The system requirements include [`nbconvert`][nbconvert] and [`pandoc`][pandoc].

[`nbconvert`][nbconvert] can easily be installed with the usual python package managers: `pip install nbconvert` or `conda install nbconvert`. At least Arch Linux users can find it in the official repositories: `pacman -S jupyter-nbconvert`

Pandoc is in the repositories of most linux distributions (e.g. `apt-get install pandoc` or `pacman -S pandoc`) and [Homebrew][] for OS X (`brew install pandoc`), and has [windows and OS X installers][pandoc releases] for each release (Download links are below the release notes). Otherwise look [here][pandoc installing].

[jupyter]: https://jupyter.org
[nbconvert]: https://nbconvert.readthedocs.io
[vignette engine]: https://stat.ethz.ch/R-manual/R-devel/library/tools/html/buildVignettes.html
[pandoc]: https://pandoc.org
[Homebrew]: http://brew.sh
[pandoc releases]: https://github.com/jgm/pandoc/releases
[pandoc installing]: https://pandoc.org/installing.html
