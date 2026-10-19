# uqsa 0.8.1

- Fixed a bug in `parse_concide()`.
- Reduced the amount of warnings about _possible_ model feature
  incompatibility discovered while parsing the model.

In the future, this feature analysis will issue warnings only when
it is a true conflict, not a possible conflict (e.g.: scheduled events
modifying species that are subject to conservation law experssions)

# uqsa 0.8.0

- Most output can be suppressed with a global option: `options(uqsa.verbose=FALSE)`.
- Objects of class `errors` seem to cause _slow downs_ on windows when doing arithmetic, we now more carefully drop errors when a function doesn't need them.
- Fixed some bugs related to extensions of shared libraries on windows (`dll` rather than `so`).

# uqsa 0.7.8

- A new function `abc_mcmc` was introduced, it is a rewrite of the
  older `ABCMCMC` function, with different arguments.
- New Kullback-Leibler divergence function: `KLD` which can serve as
  fallback if other packages are unavailable for KLD calculations.
- made some examples smaller, and tests smaller so that they run faster.
- fixed several small bugs.
- removed most printing of messages, unless they are part of a warning
- interactive calls to functions that do heavy work now print a progress bar.
- examples, vignettes, and articles all restore `par` and `option` settings on completion.

# uqsa 0.7.1

Windows compatibility: all checks pass.


# uqsa 0.6.3

The way that we generate C code and how shared libraries are made has
changed. In the past we were aiming for a workflow, where a user would
have their model in a repository. This repository would have the
generated C code for their model as well, so that there was no need to
keep regenerating it. The same repository could have a copy of the
model in various formats such as `mod`, `sbml`, `vf`, etc.

However, these derived formats make it quite difficult to keep
everything synchronized and up to date. Therefore we consider it the
more common case that we frequently change the model TSV files and
then update the C code.

The model, once converted to an ODE or CME (for chemical master
equation type models) now has a field for the C file storage location
and the `shlib` function uses this field to compile the model to a
shared library (same directory). The path to the `so` file is also
stored in the ODE or CME object.

Many other changes have been made to improve convenience, as well es
to fix bugs.
