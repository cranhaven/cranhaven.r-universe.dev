#####################################################
#####################################################
#####################################################
# R CMD check results
── R CMD check results ────────────────────────────────────────────────────────────────────────────────────────────────── rgnoisefilt 1.1.0 ────
Duration: 3m 15.7s

❯ checking top-level files ... NOTE
  Non-standard file/directory found at top level:
    ‘rgnoisefilt.Rproj’

0 errors ✔ | 0 warnings ✔ | 1 note ✖

#####################################################
#####################################################
#####################################################
# CRAN check results
## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... [10s] NOTE
  Maintainer: 'Juan Martin <juanmartin@usal.es>'
  
  Possibly misspelled words in DESCRIPTION:
    Arnaiz (43:428)
    al (43:377, 43:447)
    et (43:374, 43:444)

❯ On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking top-level files ... NOTE
  File
    inst/LICENSE
  will install at top-level and is not mentioned in the DESCRIPTION file.
  Non-standard file/directory found at top level:
    'rgnoisefilt.Rproj'

❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

❯ On ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... [7s/12s] NOTE
  Maintainer: ‘Juan Martin <juanmartin@usal.es>’
  
  Possibly misspelled words in DESCRIPTION:
    al (43:377, 43:447)
    Arnaiz (43:428)
    et (43:374, 43:444)

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [6s/17s] NOTE
  Maintainer: ‘Juan Martin <juanmartin@usal.es>’
  
  Possibly misspelled words in DESCRIPTION:
    Arnaiz (43:428)
    al (43:377, 43:447)
    et (43:374, 43:444)

0 errors ✔ | 0 warnings ✔ | 7 notes ✖

### The names are spelled correctly, they just refer to names in Spanish.

#####################################################
#####################################################
#####################################################
# Tests results
ℹ Testing rgnoisefilt
✔ | F W S  OK | Context
✔ |        17 | discCNN [1.1s]                                                                                                                
✔ |        19 | discENN [0.7s]                                                                                                                
✔ |        19 | discNCL [0.5s]                                                                                                                
✔ |        17 | discTL [0.5s]                                                                                                                 
✔ |        21 | regAENN [0.7s]                                                                                                                
✔ |        21 | regBBNR [1.4s]                                                                                                                
✔ |        19 | regCNN [1.1s]                                                                                                                 
✔ |        21 | regCVCF [1.4s]                                                                                                                
✔ |        25 | regDF [10.2s]                                                                                                                 
✔ |        21 | regEF [1.6s]                                                                                                                  
✔ |        21 | regENN [0.3s]                                                                                                                 
✔ |        19 | regFMF [11.4s]                                                                                                                
✔ |        21 | regGE [0.4s]                                                                                                                  
✔ |        19 | regHRRF [2.3s]                                                                                                                
✔ |        29 | regIPF [9.5s]                                                                                                                 
✔ |        19 | regIRF [0.3s]                                                                                                                 
✔ |        21 | regRND [3.6s]                                                                                                                 
✔ |        19 | regRNN [1.9s]                                                                                                                 
✔ |        23 | rfCDF [1.5s]                                                                                                                  
✔ |        19 | rfDROP2 [8.5s]                                                                                                                
✔ |        19 | rfDROP3 [5.9s]                                                                                                                
✔ |        21 | rfMIF [1.3s]                                                                                                                  

══ Results ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
Duration: 66.7 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 450 ]

#####################################################
#####################################################
#####################################################
# LICENSE comment
This package is a derivative work of the NoiseFiltersR package, licensed under GPL-3, of which it uses some conveniently modified and adapted functions. This code therefore is also licensed under the terms of the GNU Public License, version 3.
