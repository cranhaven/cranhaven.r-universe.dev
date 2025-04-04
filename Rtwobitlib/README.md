**Rtwobitlib** is an R package that contains a trimmed down copy of
the _kent-core source tree_ turned into a `C` library for manipulation
of `.2bit` files. See https://genome.ucsc.edu/FAQ/FAQformat.html#format7
for a quick overview of the _2bit_ format.

The _kent-core source tree_ can be found
[here](https://github.com/ucscGenomeBrowser/kent-core/).
Only the `.c` and `.h` files from the source tree that are related
to manipulation of `.2bit` files were kept.

Note that the package is primarily useful to developers of other R packages
who wish to use the _2bit_ `C` library in their own `C`/`C++` code.

