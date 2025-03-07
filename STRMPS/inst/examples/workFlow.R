readPath <- system.file('extdata', 'sampleSequences.fastq', package = 'STRMPS')
\donttest{
STRMPSWorkflow(
    readPath,
    control = workflow.control(
        restrictType = "Autosomal",
        numberOfThreads = 1
    )
)
}
