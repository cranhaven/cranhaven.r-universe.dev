# MixviR 3.3.5

Initial release of MixviR

# MixviR 3.5.0

1.) Optional 'Sublineage' column can now be included in lineage.muts file in explore_mutations() and estimate_lineages()
2.) Sample mutations file optionally written by call_mutations() with 'write.mut.table' is now comma-separated instead of tab-separated.
3.) Consistent with 3, if data are read in to either estimate_lineages() or explore_mutations() with a 'read.muts.from' file, that file now needs to be comma-separated (previously tab-separated).
4.) lineages and sublineages should now be identified as present if >= the proportion threshold vs just > (use of >= vs > was inconsistent in previous version).
5.) output of estimate_lineages() has been reformatted slightly and bugs addressed in which some output columns weren't printed in previous version.
6.) minor bug fix for case of dates (but no lineages.muts) file in explore_mutations() (removed trailing comma at line 424 of app.R of v3.3.5)
7.) general code restructuring 

 



