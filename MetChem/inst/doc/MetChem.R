## ----echo=FALSE, results='asis'-----------------------------------------------
Feature=c(
 "Mod1, median [IQR]", "Mod2, median [IQR]",  "Mod3, median [IQR]",  "Mod4, median [IQR]" , "Mod5, median [IQR]" , "Mod6, median [IQR]", 
 "Mod7, median [IQR]", "Mod8, median [IQR]", "Mod9, median [IQR]", "Mod10, median [IQR]", "Mod11, median [IQR]", "Mod12, median [IQR]",
 "Mod13, median [IQR]", "Mod14, median [IQR]", "Mod15, median [IQR]")

MYC=c( "-2.919 [-3.833 -1.536]", "-2.17 [-2.638 -1.706]","-1.798 [-2.033 -1.215]", "-2.45 [-3.037 -1.381]",  "-1.902 [-2.171 -1.425]", "-1.772 [-3.022 -1.119]",
  "0.886 [-1.181 2.231]"  , "-1.674 [-2.485 -0.656]", "1.609 [0.961 2.06]",     "-0.84 [-1.956 0.087]",   "-0.023 [-0.639 1.15]",   "-1.347 [-1.928 -0.681]",
 "0.757 [-0.717 1.532]",   "1.691 [0.59 1.952]" ,    "0.196 [-1.074 0.842]" )

WT=c("2.938 [2.195 3.563]",   "2.237 [1.98 2.478]"   ,  "1.409 [1.015 2.228]"   , "2.139 [1.776 2.827]" ,   "1.688 [1.563 1.941]"   , "1.701 [0.742 2.204]",   
  "-0.329 [-1.224 0.379]",  "1.248 [0.609 1.995]",    "-1.412 [-1.706 -1.203]", "0.8 [-0.148 1.161]"    , "-0.583 [-1.229 0.027]",  "1.305 [0.711 1.736]" ,  
 "-0.004 [-1.003 0.6]"  ,  "-1.389 [-1.662 -0.889]", "0.388 [-0.853 0.638]" )

pvalue=c( "3.66e-05" ,"3.66e-05", "3.66e-05" ,"3.66e-05", "3.66e-05", "2.46e-04", "4.03e-01", "3.84e-04", "4.69e-05", "4.64e-02", "1.75e-01" ,
          "3.66e-05", "5.07e-01", "3.66e-05" ,"7.95e-01")

FDR=c(  "7.84e-05","7.84e-05", "7.84e-05", "7.84e-05", "7.84e-05", "4.10e-04", "4.64e-01", "5.76e-04", "8.80e-05", "6.33e-02" ,"2.19e-01",
        "7.84e-05", "5.43e-01", "7.84e-05", "7.95e-01")

da=data.frame(Feature=Feature,MYC=MYC,WT=WT,'p-value'=pvalue,FDR=FDR,check.names = FALSE)
  
knitr::kable(da, align = "lcccc")

## ----echo=FALSE, results='asis'-----------------------------------------------
pval=c("1.44e-06","1.21e-05","4.97e-05","1.21e-03","1.31e-03","1.53e-03","1.77e-03",
       "2.33e-03", "8.62e-03", "1.18e-02", "1.47e-02", "1.91e-02", "2.04e-02", "2.05e-02", "2.32e-02",
       "2.32e-02", "3.95e-02", "4.13e-02")
nam=c( "Histidine or derivatives","Imidazolyl carboxylic acid derivative","Hybrid peptide",
       "N-acyl-alpha amino acid or derivatives","Imidazole","Azole",
       "Aromatic heteromonocyclic compound","N-acyl-alpha-amino acid","Beta amino acid or derivatives",
       "Organoheterocyclic compound","Heteroaromatic compound","Carboximidic acid",
       "Carboximidic acid derivative","Azacycle","Organic 1,3-dipolar compound",
       "Propargyl-type 1,3-dipolar organic compound","Alpha-amino acid or derivatives","Amino acid or derivatives"   )
da=data.frame(Substituents=nam,'p-value'=pval,check.names = FALSE)
knitr::kable(da, align = "lc")

## ----echo=FALSE, results='asis'-----------------------------------------------
pval=c("2.97e-04", "1.38e-03", "8.62e-03", "8.62e-03", "8.62e-03", "2.57e-02")
nam=c( "CNDP1","CARNS1",  "VIM" ,    "HSPA1A" , "SLC15A2", "MPO"  )
da=data.frame(Substituents=nam,'p-value'=pval,check.names = FALSE)
knitr::kable(da, align = "lc")

