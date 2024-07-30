

<!--- next entry here -->

# MetaculR 0.4.0
2022-04-25

## Features

- **metaculr_plot:** Add rug_plot of current weighted distribution (2cb7b7b4339aebb1570a10b92bb2a2d6bc73bff6)
- **metaculr_plot:** Use ggrepel for labels (6d1e8517d87a56238408afa9037e11da8e297c29)
- **metaculr_probabilistic_consensus:** Add MetaculR_probabilistic_consensus for combining external forecasts (4a8c1dc369bcb243f9ac3afb8f0f34f6c1489bbe)
- **metaculr_plot:** Add tournament parameter for relative log score (4c1b015a192fdebd70b1b91cdeb1015150798a57)

## Fixes

- **news.md:** Update NEWS.md (b1cf0b62f8d9ca91e0659f8a25ff98f5daa328ee)
- **site:** Update Site (3525a24ef2c137678a1e141698e5628d24dcdda9)
- **metaculr_mydiff:** Force comparison against my most recent prediction (2499f800ad00b8784f3e1afc6d41b1d055aa7929)
- **metaculr_mydiff:** Add new condition to use questions with user prediction (c7565b30fb52312edbb95e85c461dfc91baf2ad0)
- **`metaculr_questions`:** Inform user of improper MetaculR_questions objects (d226334bc6cc674f55ebc166a28e47f653efa9dd)
- **metaculr_plot:** Gracefully handle duplicated questions from API (439cda0b4eb7a9da44d58ff5096ab4319d855628)
- **vignettes:** Update vignettes (02e58307d96a8161193a74e2fee7cb13668fc20d)

# MetaculR 0.3.0
2022-03-31

## Features

- **metaculr_excitement:** Add days parameter (78f9bb4238d7b088947aab1eaebaf30eab39739c)
- **metaculr_markdown_table:** Add function to quickly translate R tables to Metaculus Markdown (c9e1d9596ac0702930063fbe70b3515fcfaed316)
- **metaculr_brier:** Add community scores (edf5d367d723fb78ad88e8706dcc3be3e276a3d5)
- **metaculr_plot:** Add step-line to connect predictions (52c06b056b9d732cb96eaab440819a7550def8b5)
- **metaculr_aggregated_forecasts:** Aggregate forecasts in different ways (19cf3acc0c16ba0431842c9accc2aced910ebf0d)

## Fixes

- **site:** Update site after version bump (2bfe67247b02cd334633808aa41323de5fcb6afa)
- **cran:** Update CRAN_RELEASE (a99ba9c80213a30b52b5e854721e3ff6d156d724)
- **site:** Add cranlogs downloads badge (2d607bf0e10598963c6f6fcf2aefe6062ec0ad85)
- **metaculr.rmd:** Make small adjustments to include community scores (a685746f76d0f5cf68016ac6b7220754df829c75)
- **metaculr_plot:** Make more space for plot on both axes (f7b5913117c9c3074151db2ff68a9423e3cbe8fe)
- **metaculr.rmd:** Add community score to another plot (f9214c822d87f3ba5f3505d5bb01438f798a0061)
- **metaculr_brier:** Update for community median scores (e08b2d0ee6f5fb3037e2ee7fa723f3d009570f4b)
- **metaculr_plot:** Correct parameter name (db7faec27c2f8b053aafbb68f03bda66b156d05f)
- **metaculr_brier:** Further updates for community scores (bd8aba2b9ee12287ed469c7d918f1044786f05fa)
- **metaculr_aggregated_forecasts:** Add tests for mean_logodds and extremized_baseline (3d8e1567f5c8cceea3361f308feb3ff5710941fc)
- **.gitlab-ci.yml:** Only run job on protected branches (with variable) (904aebf5f89c7e2b6ec0e6c5d86635761a851b8d)
- **description:** Bump version number (45b5b548731e6940a6cd87c3d2b4830aab5c54dc)
- **.gitlab-ci.yml:** Only run job on protected branches (9c0a0219e9326ca686847d59c3716bcf0ed521fe)
- **metaculr_aggregated_forecasts:** No visible binding fix; https fix (7e06232e13e10febc181ded96f6c46df7317f90d)

# MetaculR 0.2.0
2022-03-21

## Breaking changes

### **cran:** Update Description field; add examples (e3e2af6b5fa677c73abaaaf773b2bf60a8cfdd64)

This commit changed functions with capital letters after the underscore to lowercase letters, e.g.,

- `MetaculR_Brier()` --> `MetaculR_brier()`
- `MetaculR_Questions()` --> `MetaculR_questions()`

# MetaculR 0.1.1
2022-03-21

## Fixes

- **.rbuildignore:** Add some unnecessary `httptest` files (6ea3a4e8ee95b7e6a4e03a971ab0de1cadc4f84b)
- **news.md:** Added NEWS.md and added to Site (4fff8e9fc037e52f2e1ebf94df892cce23bc60a8)
- **cran-comments.md:** Added more NOTES after `rhub::check_for_cran()` (fb246d2ef7b32f426de64f15b61d9aa2353e36c7)
- **cran:** Add CRAN-RELEASE to .Rbuildignore (0c23c006da8d1b7ddc0edc225a621f6eec7ca9a2)
- **site:** Add Site URL to README; update MetaculR.Rmd (382c6e99cf30f4bf2d2ad02ee4a6827ae8aedff5)
- **functions:** Add return values description (f71805f9c9ea05944c999386f70babd39d68d8f6)
- **metaculr_login:** Add user_agent; trim empty final page (0700722137837bc06731090daa365cebd7235e81)
- **site:** Build vignette and site (9f641bc77034c8b1b240ed4240a9b03bb3e59f95)
- **.gitlab-ci.yml:** Remove extra `devtools::check()` jobs (8690e9d249104c8087be643e495ff208f0ca5f45)
- **cran:** Remove extra test environments and fixed NOTE (1f3f549f8b415334a383c403c581961f2245c0df)
- **.gitlab-ci.yml:** Install pandoc in jobs (5dc7e5a0dd7c236a9cefec020b005942b3ee5a8b)
- **news.md:** Use NEWS.md with automated semantic versioning (cb3a7c488984892d5afc0b7cca7e3a9e7c57f122)
- **news.md:** Add tag for automated semantic versioning (65923936e87598bf779d25a8342f647e3bcfec60)

# MetaculR 0.1.0
2022-03-11

## Features

- Initialize project (da01a45c9e39e3bed9493658d78cd8236bcb65df)
- **site:** Add GitLab Pages to .gitlab-ci.yml (1b2f4c5fb056dc1f6d811b8b30c1e79dc40b133f)

## Fixes

- **site:** Remove `docs` from .gitignore (de31aa4ff16129d0a62653ad7d705a50988752b7)
- **.gitlab-ci.yml:** Installed devtools to run `devtools::check()` (bebaff5b67e12d6960cd85073922b886a7faa6bf)
- **.gitlab-ci.yml:** Remove `sudo` (ed26fca99eeb19b0d4a0b95e808169034b32eed1)
- **.gitlab-ci.yml:** Install dependencies (c5ca5e55aa713dc2b308e8334759295f2e22958c)
- **.gitlab-ci.yml:** Add rmarkdown to Check (b98b1594d4cdf5015acfaea90384474a34959aa0)
- **.rbuildignore:** Add go-semrel-gitlab files to be ignored (b6087d6c9eb6d47cdb55b565890e23e4397a6745)
- **news.md:** Use NEWS.md rather than CHANGELOG.md (025b2f66e51f9eb65a556ccac60c6a6259d25422)
- **site:** Update title, badges, pkgdown and bug links (751b45686a9141f7bed4f4e5a1f8a62062f07af1)
- **cran-comments:** Updated test images (d48d6e793cdf4a120425d4d74a9232acf9bcca82)
- **.gitlab-ci.yml:** Install dependencies differently (17d3d732643da3a9fa8ffb8325d8d5da83c6b3f8)
- **readme.md:** Update link to lifecyle definitions (77ed4502d940e8671600f9ca695d0ee7975d5fa5)
- **description:** Remove `LazyData: true` (e46f6e965a6339799936350b914830e58a8dbbda)
- **.gitlab-ci.yml:** Add `devtools::check_win_devel()` (c2fa1702db371c1a52033e0a44ac877289e3b177)
- **vignette:** Workaround to mock API calls (34b6f4318acd3a9de81d9705abc4d93dd733ac92)
- **site:** Build website with new vignette file (0efd3fd1871b88825d2b40db33e157859401207e)
- **man:** Updated roxygen notes for all functions (c2a604e204c54b2289a7cd8792dc8e9030c439aa)
- **cran-comments.md:** Added note about API authentication workaround (f7cf6035d07744c9bccf4c5288d092a9ade1ba6b)
- **.gitlab-ci.yml:** Add code coverage job (22dbd70cb89a69be01d70094f7f0901145935839)
- **.gitlab-ci.yml:** Update job dependency name (bc7c332a334a8b8789d9b125a94df47c73593822)
- **.gitlab-ci.yml:** Use `R` in script, not `Rscript` (e2b48bc9873a92908ee6fda4a51496679a7d3127)
- **.gitlab-ci.yml:** Add image to code coverage job (858c5d2da09a892f30890c2b4f425eec7fd9b700)
- **.gitlab-ci.yml:** Install 'covr' in job (11cd866348c86f7360a91e261d7f2e6b6be9a6fc)
- **.gitlab-ci.yml:** Add Debian packages needed for 'covr' (ccca15459245f7d1dc202df9c1b6be5e2a55189c)
- **.gitlab-ci.yml:** Add MetaculR dependencies to job (d041476db5dd9165b4ac2f4fd46c5d56f6e479b2)
- **.gitlab-ci.yml:** Add 'testthat' and 'devtools' to job (cf63056999a94abf0c1ae3fcb0ce6d522595579e)
- **.gitlab-ci.yml:** Use correct path, `/coverage` (2820435ae48c74190d16aac90b5882a28657759d)
- **site:** Updated with new function documentation (4b1a87911362328051de7389f52e85a93d11ea74)

<!--- downloads here -->
