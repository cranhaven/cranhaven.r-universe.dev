# 1. Full CRAN check
unlink("lapop.Rcheck", recursive = TRUE, force = TRUE)
unlink("lapop_2.1.6.tar.gz", force = TRUE)

devtools::document()
Sys.setenv(TMPDIR = tempdir())
devtools::check(args = "--as-cran") # REQUIRED

# 2. Unit tests
devtools::test() # ✅ Strongly Recommended

# 3. Platform checks
urlchecker::url_check()
urlchecker::url_update()

# 4. (Optional) Test coverage
#detach(package:lapop)
#covr::package_coverage() # 🔶 Optional but helpful

# 5. (Optional) Spell check
#spelling::spell_check_package() # 🔶 Optional but helpful
#spelling::update_wordlist()

# 6. Build vignettes and manual
devtools::build_vignettes()
#devtools::build(manual = T) # ✅ Manual PDF if submitting offline

# 7. Build tar.gz source package
devtools::build() # ✅ REQUIRED for submission

# 8. (Optional) Build pkgdown site
pkgdown::clean_site();
pkgdown::build_site() # ❌ Not needed for CRAN

# Final reminders
cat("Check DESCRIPTION, NEWS.md, cran-comments.md\n")
