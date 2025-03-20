# We can call peaks independent of ArchR

    Code
      tiles
    Output
      A MultiAssayExperiment object of 2 listed
       experiments with user-defined names and respective classes.
       Containing an ExperimentList class object of length 2:
       [1] C2: RaggedExperiment with 71764 rows and 1 columns
       [2] C5: RaggedExperiment with 66915 rows and 1 columns
      Functionality:
       experiments() - obtain the ExperimentList instance
       colData() - the primary/phenotype DataFrame
       sampleMap() - the sample coordination DataFrame
       `$`, `[`, `[[` - extract colData columns, subset, or experiment
       *Format() - convert into a long or wide DataFrame
       assays() - convert ExperimentList to a SimpleList of matrices
       exportClass() - save data to flat files

