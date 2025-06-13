Changes in Version 1.0.1

  o Fix for select_output function. When all the SNP's P value are great than the pv_thold, it provides a notice that            "No SNP's P value satisfies pv_thold requirement", instead of showing error. 

  o Coefficient name is fixed when model is selected as "binary" or "linear".
 
  o Bug fix for the MAF function when calculating the Minor allele frequency and round MAF to 4 decimal places.