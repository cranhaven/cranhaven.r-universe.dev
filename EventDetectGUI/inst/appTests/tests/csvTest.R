app <- ShinyDriver$new("../", seed = 1)
app$snapshotInit("csvTest")
app$setInputs(tabs = "dataSelection")
app$uploadFile(inputFile = system.file("/extdata/test_station_b.csv",package="EventDetectGUI"))
app$setInputs(csvSelectColumns = TRUE)
app$setInputs(csvColumnCheckBox = c("B_PH_VAL", "B_COND_VAL", "B_TEMP_VAL", "B_PRES_OP", "B_PLNT_PRES_OP", "B_PLNT_PH_VAL", "B_PLNT_TURB_VAL", "B_PLNT_FLOW_OP", "B_PLNT_CL2_VAL"))
app$snapshot(list(output = "outDataHead"))

