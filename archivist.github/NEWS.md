# Ver 0.2.4-0.2.6

- Documentation updates to reflect CRAN canonical URL form and to adopt new blogdown linking protocol.

# Ver 0.2.3

- [Rely on S3 interface of git2r instead of S4](https://github.com/MarcinKosinski/archivist.github/pull/35)

# Ver 0.2.2

* Minor updates
	- `addHooksToPrintGitHub()` archives with names now.
* Maintaining
	- VignetteBuilder: knitr added to DESCRIPTION, due to missing vignettes on CRAN


# Ver 0.2.1

* Minor updates
  - Proper extension of `createGitHubRepo()` feature that produce `README.md`.

# Ver 0.2.0

* New functions
  - `authoriseGitHub()` - easier GitHub token creation
  - `addHooksToPrintGitHub()` - equivalent of `archivist::addHooksToPrint` but with `archivist.github::archive()` usage instead of `archivist::asave()/saveToLocalRepo()`

* Minor updates
  - `createGitHubRepo()` has extended `README.md` file.

* Other
  - Added vignette.

# Ver 0.1.1

- Fixed `artifactName` parameter in `archive` so that now it uses `deparse` instead of `digest` and one can now archive artifacts with their names.

# Ver 0.1

Extensions to [archivist](https://github.com/pbiecek/archivist)

* GitHub Archiving
- `archive()`

* GitHub and archivist Repository Creation/Deletion
- `createGitHubRepo()`, `cloneGitHubRepo()`, `deletGitHubRepo()`

* GitHub and archivist Repository Synchronization
- `pullGitHubRepo()`, `puchGitHubRepo()`