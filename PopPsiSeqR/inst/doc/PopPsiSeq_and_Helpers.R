## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",

  fig.width = 8,
  fig.height = 5,
  fig.align = "center"
  )

## ----setup, include=FALSE-----------------------------------------------------
#library(PopPsiSeqR)
devtools::load_all()

library("ggplot2")
library("dplyr")
library("tidyr")
library("ggbio")
library("rtracklayer")
library("patchwork")


## ----include=TRUE, echo=TRUE, eval=TRUE---------------------------------------

merged_frequencies.filename <- system.file("extdata", 
  "merged_frequencies.example_data.tbl", package = "PopPsiSeqR")
merged_frequencies.bg <- import.freqtbl(merged_frequencies.filename)

## ----include=TRUE, echo=TRUE, eval=TRUE---------------------------------------

head(merged_frequencies.bg %>% as.data.frame() 
     %>% select(-c(ends_with("_count"), "name", "score")) 
     %>% GRanges(), n=5)


## ----include=TRUE, echo=FALSE-------------------------------------------------

autoplot(merged_frequencies.bg %>% head(n=36), aes(y = selected_parent_alt_af), geom='point', color = "red", alpha=0) +geom_hline(yintercept = 0, color ="black", linetype = "dotted")  +geom_hline(yintercept = 1, color ="black", linetype = "dotted") + geom_point(color = "red", alpha=1) + geom_point(aes(y=backcrossed_parent_alt_af), color = "mediumblue", alpha = 0.5) + geom_segment(data= . %>% as.data.frame(),  aes(x=start, xend=start, y =selected_parent_alt_af, yend=backcrossed_parent_alt_af ), color = "black", alpha=0.25) + geom_point(aes(y=offspring_alt_af), color = "black", shape = "x", size=4) + geom_point(aes(y=offspring_alt_af), color = "black", shape = "o", size=4) + geom_text( data = . %>% as.data.frame() %>% mutate(why = rep(c(-0.1,-0.05), n()/2)) ,aes(label=ref,y = why) ) + geom_text( data = . %>% as.data.frame() %>% mutate(why = rep(c(1.05,1.1), n()/2)) ,aes(label=alt,y = why) ) + facet_wrap(~seqnames, scales = "free_x")  + theme_clear() + theme(axis.text.x = element_text(angle = 30, hjust = 1)) + labs(y="Allele Frequency", title = "Allele Frequencies for Parental and Offspring Populations", subtitle = "small subset shown for clarity", caption ="blue dots - backcross parent (D. sechellia); red dots - selection parent (D. simulans); crosshactched circles - offspring",x= "coordinate (droSim1 reference genome)" ) 


## ----include=TRUE, echo=FALSE, eval=TRUE--------------------------------------

merged_frequencies.gath.full.df <- merged_frequencies.bg %>% as.data.frame() %>% select(c(selected_parent_alt_af, backcrossed_parent_alt_af, offspring_alt_af)) %>% gather(key="population", value = "allele_frequency") %>% mutate(population = gsub("_alt_af","",population)) %>% mutate(population = gsub("_"," ",population))


ggplot(merged_frequencies.gath.full.df) + geom_step( data = . %>% filter(population=="offspring") , aes(x=allele_frequency, group = population, color = population),  position = "identity", stat="bin", bins=31, alpha=0.666) + geom_step(  data = . %>% filter(population!="offspring") , aes(x=allele_frequency, group = population, color = population),  position = "identity", stat="bin", bins=31) +theme_bw() + scale_color_manual(values=c("red", "black", "blue"))  + labs(y="# variant sites", title = "Allele Frequency Spectra", subtitle = "(full example dataset, grouped by population)", caption ="", x= "frequency of alternate (non-reference) allele" ) 



## ----include = TRUE, echo=FALSE, eval=TRUE------------------------------------

merged_frequencies.diffhist.gg <- ggplot(merged_frequencies.bg %>% as.data.frame() %>% select(c(selected_parent_alt_af, backcrossed_parent_alt_af)) %>% mutate(alt_af_diff = backcrossed_parent_alt_af - selected_parent_alt_af)) + geom_histogram(aes(x=alt_af_diff), fill="darkgray", color ="darkgray", bins = 31) + geom_vline(xintercept = 0, color = "black", linetype = "dotted")  +theme_bw()+ labs(y="# variant sites", title = "Difference in frequency of alternate (nonreference) allele between parental populations", subtitle = " ", caption ="",x= "frequency difference (sechellia - simulans)" ) 
merged_frequencies.diffhist.gg

## ----eval=TRUE, echo = TRUE, include = TRUE-----------------------------------
frequency_shifts.bg <- freqShifter(merged_frequencies.bg)

head(frequency_shifts.bg %>% as.data.frame() 
     %>% select(-c(ends_with("_count"), ends_with("_deltaF"),  "name", "score")) 
     %>% GRanges(), n=5)


## ----include = TRUE, echo=FALSE, warning=FALSE--------------------------------
subsample_number <- 36
frequency_shifts.raw.gg <- autoplot(frequency_shifts.bg %>% head(n=subsample_number), aes(y = selected_parent_alt_af), geom='point', color = "red", alpha = 0) +geom_hline(yintercept = 0, color ="black", linetype = "dotted")  +geom_hline(yintercept = 1, color ="black", linetype = "dotted") + geom_point(aes(y=backcrossed_parent_alt_af), color = "mediumblue", alpha = 1, size=2,shape=21) + geom_point(aes(y=selected_parent_alt_af), color = "red", alpha = 1, size=2,shape=21)  + geom_segment(data= . %>% as.data.frame(), aes(x=start, xend=start, y =selected_parent_alt_af, yend=backcrossed_parent_alt_af ), color = "black", alpha=0.25)  + geom_point(aes(y=central), color = "black", alpha = 1, shape = "o", size=2) + geom_point(aes(y=offspring_alt_af), color = "black", size=3, shape = "x") + labs(y="Allele Frequency (raw)", title = "Taring the Frequencies to Hardy-Weinberg Expectation", subtitle = " ", caption ="",x= "" ) + theme_clear() + theme(axis.text.x = element_blank()) 



frequency_shifts.tared.gg <- autoplot(frequency_shifts.bg %>% head(n=subsample_number), aes(y = selected_parent_alt_af - central), geom='point', color = "red", alpha = 0) +geom_hline(yintercept = c(-0.5,0.5), color ="black", linetype = "dotted")  + geom_point(aes(y=backcrossed_parent_alt_af-central), color = "mediumblue", alpha = 1, size=2,shape=21) + geom_point(aes(y=selected_parent_alt_af - central), color = "red", alpha = 1, size=2,shape=21)  + geom_segment(data= . %>% as.data.frame(), aes(x=start, xend=start, y =selected_parent_alt_af-central, yend=backcrossed_parent_alt_af-central ), color = "black", alpha=0.25) + geom_point(y=0, color = "black", alpha = 1, shape = "o", size=2) + geom_point(aes(y=offspring_alt_af- central), color = "black", size=3, shape = "x") + labs(y="Allele Frequency (tared)", title = "", subtitle = "", caption ="blue circles - backcross parent (D. sechellia); red circles - selection parent (D. simulans);\nblack circles - neutral expectation; black x - offspring",x= "coordinate (droSim1 reference genome)" )  + theme_clear() + theme(axis.text.x = element_text(angle = 30, hjust = 1)) 



frequency_shifts.raw.gg@ggplot / frequency_shifts.tared.gg@ggplot


## ----include = TRUE, echo=FALSE, warning=FALSE--------------------------------


arrow_len <- unit(0.05, "npc")
frequency_shifts.oriented.gg <- autoplot(frequency_shifts.bg %>% head(n=subsample_number), aes(y = sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(selected_parent_alt_af - central)), geom='point', color = "red",  alpha = 0) + geom_point(aes(y=sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(backcrossed_parent_alt_af-central)), color = "mediumblue", alpha = 1, shape=21, size=2,) + geom_point(color = "red", alpha = 1,shape=21, size=2,) + geom_point(y=0, color = "black", alpha = 1, size=2,shape=21) + geom_point(aes(y=sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(offspring_alt_af- central)), color = "black", size=3, shape = "x") + geom_segment(data= . %>% as.data.frame(), aes(x=start, xend=start, yend =sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(selected_parent_alt_af-central)) , y=0, color = "black", alpha=0.75, arrow=arrow(ends = "last", length=arrow_len, type="open")) + geom_segment(data= . %>% as.data.frame(), aes(x=start, xend=start, yend = 0, y = sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(backcrossed_parent_alt_af-central)), color = "black", alpha=0.5, arrow=arrow(ends = "last", length=arrow_len, type="open"))  + labs(y="Allele Frequency (polarized)", title = "", subtitle = "", caption ="blue circles - backcross parent (D. sechellia); red circles - selection parent (D. simulans);\nblack circles - neutral expectation; black x - offspring",x= "coordinate (droSim1 reference genome)" )  + theme_clear() + theme(axis.text.x = element_text(angle = 30, hjust = 1))


  


frequency_shifts.tared.mod.gg <- frequency_shifts.tared.gg + labs( title = "Polarizing the Allele Frequencies to a Common Orientation", x="", caption = "") + theme(axis.text.x = element_blank())
frequency_shifts.tared.mod.gg@ggplot$layers[[2]]$aes_params$alpha <- 0

frequency_shifts.tared.mod.gg@ggplot / frequency_shifts.oriented.gg@ggplot



## ----include=TRUE, echo = FALSE, eval=TRUE------------------------------------

autoplot(frequency_shifts.bg %>% head(n=subsample_number), aes(y = sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(selected_parent_alt_af - central)), geom='point', color = "red", shape="o", alpha = 0 ) + geom_line(aes(y= sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(selected_parent_alt_af - central)), color = "red", alpha=0.5) + geom_point(aes(y=sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(backcrossed_parent_alt_af-central)), color = "mediumblue", shape="o", alpha = 0 ) + geom_line(aes(y=sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(backcrossed_parent_alt_af-central)), color = "mediumblue", alpha=0.5) + geom_point(aes(y=sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(offspring_alt_af- central)), color = "black", size=3, shape = "x", alpha=0)  + labs(y="Allele Frequency (transformed)", title = "Ancestral Populations, Relative to Expected Equilibria", subtitle = " ", caption ="blue - backcross parent (D. sechellia); red - selection parent (D. simulans); black x - offspring",x= "coordinate (droSim1 reference genome)" ) + theme_clear()

## ----include=TRUE, eval=TRUE, echo=FALSE--------------------------------------


autoplot(frequency_shifts.bg %>% head(n=subsample_number), aes(y = sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(selected_parent_alt_af - central)), geom='point', color = "red", shape="o", alpha = 0 ) + geom_line(aes(y= sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(selected_parent_alt_af - central)), color = "red", alpha=0.5) + geom_line(aes(y=sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(backcrossed_parent_alt_af-central)), color = "mediumblue", alpha=0.5)  + geom_line(aes(y= max_oriented_shift), color = "red", alpha=1, linetype="dotted") + geom_line(aes(y= min_oriented_shift), color = "mediumblue", alpha=1, linetype="dotted") + labs(y="Allele Frequency (transformed)", title = "Ancestral Populations, Relative to Expected Equilibria", subtitle = "with gene fixation envelope", caption ="blue - backcross parent (D. sechellia); red - selection parent (D. simulans)",x= "coordinate (droSim1 reference genome)" ) + theme_clear()


## ----include=TRUE, eval=TRUE, echo=FALSE--------------------------------------


autoplot(frequency_shifts.bg %>% head(n=subsample_number), aes(y = sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(selected_parent_alt_af - central)), geom='point', color = "red", shape="o", alpha = 0 ) + geom_line(aes(y= sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(selected_parent_alt_af - central)), color = "red", alpha=0.5) + geom_line(aes(y=sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(backcrossed_parent_alt_af-central)), color = "mediumblue", alpha=0.5)  + geom_line(aes(y= max_oriented_shift), color = "red", alpha=1, linetype="dotted") + geom_line(aes(y= min_oriented_shift), color = "mediumblue", alpha=1, linetype="dotted")  + geom_point(aes(y=mean_oriented_shift), color = "black", shape = "x", size=3)  + geom_line(aes(y=mean_oriented_shift), color = "black", alpha = 0.8)  + labs(y="Allele Frequency (transformed)", title = "Offspring Population, Relative to Expected Equilibria", subtitle = "with parental populations and fixation envelope", caption ="blue - backcross parent (D. sechellia); red - selection parent (D. simulans); black - offspring",x= "coordinate (droSim1 reference genome)" ) + theme_clear()




## ----include=TRUE, eval=TRUE, echo=FALSE--------------------------------------

autoplot(frequency_shifts.bg , aes(y = sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(selected_parent_alt_af - central)), geom='point', color = "red", shape="o", alpha = 0 ) + geom_point(aes(y= sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(selected_parent_alt_af - central)), color = "red", alpha=0.1) + geom_point(aes(y=sign(selected_parent_alt_af-backcrossed_parent_alt_af)*(backcrossed_parent_alt_af-central)), color = "mediumblue", alpha=0.1)  + geom_line(aes(y= max_oriented_shift), color = "red", alpha=0, linetype="dotted") + geom_line(aes(y= min_oriented_shift), color = "mediumblue", alpha=0, linetype="dotted")  + geom_point(aes(y=mean_oriented_shift), color = "black", shape = "x", size=3)  + geom_line(aes(y=mean_oriented_shift), color = "black", alpha = 0.0)  + labs(y="Allele Frequency (tared and polarized)", title = "Offspring Population, Relative to Expected Equilibria", subtitle = "with parental populations", caption ="blue - backcross parent (D. sechellia); red - selection parent (D. simulans); black - offspring",x= "coordinate (droSim1 reference genome)" ) + theme_clear()


## ----include=TRUE, echo=TRUE, eval=TRUE---------------------------------------
export.freqshft(frequency_shifts.bg , tempfile())

## ----warning=FALSE, include=TRUE, echo=TRUE-----------------------------------
windowed_shifts.filename <- system.file("extdata", "windowed_shifts.example_data.bed", 
  package = "PopPsiSeqR")

windowed_shifts.bg <- import.smvshift(windowed_shifts.filename, selected_parent = "sim",
  backcrossed_parent = "sech")

windowed_shifts.bg %>% head()

## ----warning=FALSE, include=TRUE, echo=TRUE-----------------------------------
windowedFrequencyShift.plotter(windowed_shifts.bg, 
  selected_parent = "sim", backcrossed_parent = "sech", main_title = 
  "PopPsiSeq Results: offspring allele frequency\nrelative to neutral expectation, parental populations, and fixation")


## ----warning=FALSE, include=TRUE, echo=TRUE-----------------------------------

lab_sechellia.filename <- system.file("extdata", 
  "wild_sechellia.example_data.bed", package = "PopPsiSeqR")
lab.bg <- import.smvshift(lab_sechellia.filename)
lab.bg$sechellia <- "lab"


wild_sechellia.filename <- system.file("extdata",
  "lab_sechellia.example_data.bed", package = "PopPsiSeqR")
wild.bg <- import.smvshift(wild_sechellia.filename)
wild.bg$sechellia <- "wild"


windowedFrequencyShift.plotter(c(lab.bg,wild.bg),
  selected_parent = "sim", backcrossed_parent = "sec",
  primary_aesthetic = aes(color=sechellia) ,
  main_title = "PopPsiSeq Results: offspring allele frequency\nrelative to neutral expectation, parental populations, and fixation"
  ) + facet_grid(sechellia~seqnames
  ) + labs(subtitle = "analyses based on lab-reared and wild-caught sechellia")


## ----warning=FALSE, include=TRUE, echo=TRUE-----------------------------------

sub.traction <- subTractor(lab.bg, wild.bg ,treament_name = "sechellia")

autoplot(sub.traction, aes(y=lab_minus_wild), geom="line"
  ) + labs(y="Difference in Allele Frequency Shift\n(lab - wild)", 
  title = "Difference between PopPsiSeq analyses based on lab-reared and wild-caught sechellia",
  subtitle = "", caption ="",x= "coordinate (droSim1 reference genome)" 
  ) + theme_clear()  



## ----include=TRUE, echo=TRUE, eval=TRUE---------------------------------------

lab_frequencies.filename <- system.file("extdata", 
  "lab_frequencies.example_data.tbl", package = "PopPsiSeqR")
lab_frequencies.bg <- import.freqtbl(lab_frequencies.filename)
lab_shifts.bg <- freqShifter(lab_frequencies.bg)
lab_shifts.bg$avg_simward_AFshift <- lab_shifts.bg$mean_oriented_shift
lab_shifts.bg$sechellia <- "lab"    

wild_frequencies.filename <- system.file("extdata", 
  "wild_frequencies.example_data.tbl", package = "PopPsiSeqR")
wild_frequencies.bg <- import.freqtbl(wild_frequencies.filename)
wild_shifts.bg <- freqShifter(wild_frequencies.bg)
wild_shifts.bg$avg_simward_AFshift <- wild_shifts.bg$mean_oriented_shift
wild_shifts.bg$sechellia <- "wild"


fine_subtraction.bg <- subTractor(lab_shifts.bg, wild_shifts.bg,
  treament_name = "sechellia")

fine_subtraction.df <- fine_subtraction.bg %>% as.data.frame() %>% 
  mutate(name = n(), score = 0) %>% select(
    c("seqnames","start","end","name","score","strand", "lab_minus_wild") ) 

write.table(fine_subtraction.df, file=tempfile(), quote=F, 
  sep="\t", row.names=F, col.names = F)


