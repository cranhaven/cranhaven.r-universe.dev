#!/bin/bash
#
#$ -cwd
#$ -j y
#$ -S /bin/bash
  source ~/.bash_profile


#   cd /data/guow4/project0/GGIR/postGGIR/postGGIR_compile/postGGIR3.0/inst/extdata/example/afterGGIR; module load R ;   R --no-save --no-restore --args  < Example_part0.maincall.R  0
   cd /data/guow4/project0/GGIR/postGGIR/postGGIR_compile/postGGIR3.0/inst/extdata/example/afterGGIR; module load R ;   R --no-save --no-restore --args  < Example_part0.maincall.R  1
   cd /data/guow4/project0/GGIR/postGGIR/postGGIR_compile/postGGIR3.0/inst/extdata/example/afterGGIR; module load R ;   R --no-save --no-restore --args  < Example_part0.maincall.R  2
   cd /data/guow4/project0/GGIR/postGGIR/postGGIR_compile/postGGIR3.0/inst/extdata/example/afterGGIR; module load R ;   R --no-save --no-restore --args  < Example_part0.maincall.R  3
   cd /data/guow4/project0/GGIR/postGGIR/postGGIR_compile/postGGIR3.0/inst/extdata/example/afterGGIR; module load R ;   R --no-save --no-restore --args  < Example_part0.maincall.R  4



   cd /data/guow4/project0/GGIR/postGGIR/postGGIR_compile/postGGIR3.0/inst/extdata/example/afterGGIR; module load R ; R -e "rmarkdown::render('part5_Example_postGGIR.report.Rmd'   )" 
   cd /data/guow4/project0/GGIR/postGGIR/postGGIR_compile/postGGIR3.0/inst/extdata/example/afterGGIR; module load R ; R -e "rmarkdown::render('part6_Example_postGGIR.nonwear.report.Rmd'   )" 
   cd /data/guow4/project0/GGIR/postGGIR/postGGIR_compile/postGGIR3.0/inst/extdata/example/afterGGIR; module load R ; R -e "rmarkdown::render('part7a_Example_postGGIR_JIVE_1_somefeatures.Rmd'   )" 
   cd /data/guow4/project0/GGIR/postGGIR/postGGIR_compile/postGGIR3.0/inst/extdata/example/afterGGIR; module load R ; R -e "rmarkdown::render('part7b_Example_postGGIR_JIVE_2_allfeatures.Rmd'   )" 
   cd /data/guow4/project0/GGIR/postGGIR/postGGIR_compile/postGGIR3.0/inst/extdata/example/afterGGIR; module load R ; R -e "rmarkdown::render('part7c_Example_postGGIR_JIVE_3_excelReport.Rmd'   )" 
   cd /data/guow4/project0/GGIR/postGGIR/postGGIR_compile/postGGIR3.0/inst/extdata/example/afterGGIR; module load R/3.6.3 ; R -e "rmarkdown::render('part7d_Example_postGGIR_JIVE_4_outputReport.Rmd'   )" 
   cd /data/guow4/project0/GGIR/postGGIR/postGGIR_compile/postGGIR3.0/inst/extdata/example/afterGGIR; module load R ; R -e "rmarkdown::render('part7e_Example_postGGIR_JIVE_5_somefeatures_weekday.Rmd'   )" 
