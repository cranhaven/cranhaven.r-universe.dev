
test_that("Test Case 1: json2csv works on sample data (JSON_sample), check df shape and column names",{
    csv_df <- json2csv(JSON_sample, protein_name="A")
    expect_equal(unique(csv_df$proteinName), "A")
    expect_equal(nrow(csv_df),183)
    expect_equal(ncol(csv_df),17)

    col_names<-colnames(csv_df)
    expect_equal(col_names[1],'proteinName')
    expect_equal(col_names[2],'position')
    expect_equal(col_names[3],'count')
    expect_equal(col_names[4],'lowSupport')
    expect_equal(col_names[5],'entropy')
    expect_equal(col_names[6],'indexSequence')
    expect_equal(col_names[7],'index.incidence')
    expect_equal(col_names[8],'major.incidence')
    expect_equal(col_names[9],'minor.incidence')
    expect_equal(col_names[10],'unique.incidence')
    expect_equal(col_names[11],'totalVariants.incidence')
    expect_equal(col_names[12],'distinctVariant.incidence')
    expect_equal(col_names[13],'multiIndex')
    expect_equal(col_names[14],'host')
    expect_equal(col_names[15],'highestEntropy.position')
    expect_equal(col_names[16],'highestEntropy')
    expect_equal(col_names[17],'averageEntropy')


})

test_that("Test Case 2: json2csv works on sample data (JSON_sample), check data integrity",{
    csv_df <- json2csv(JSON_sample, protein_name="A")

    expect_equal(unique(csv_df$highestEntropy.position),66)
    expect_equal(unique(csv_df$highestEntropy),5.1592951)
    expect_equal(unique(csv_df$averageEntropy),1.81526603)
    expect_equal(unique(csv_df$host), 'unknown host')

    fifthRow <- csv_df[5,]
    expect_equal(fifthRow$proteinName, 'A')
    expect_equal(fifthRow$position, 5)
    expect_equal(fifthRow$count, 4424)
    expect_equal(fifthRow$lowSupport, FALSE)

    expect_equal(fifthRow$entropy, 1.9135753)
    expect_equal(fifthRow$indexSequence, 'PKPQRKTKR')
    expect_equal(fifthRow$index.incidence, 76.243225)
    expect_equal(fifthRow$major.incidence, 7.617541)
    expect_equal(fifthRow$minor.incidence, 13.743219)
    expect_equal(fifthRow$unique.incidence, 2.3960219)

    expect_equal(fifthRow$totalVariants.incidence, 23.75678)
    expect_equal(fifthRow$distinctVariant.incidence, 16.745956)
    expect_equal(fifthRow$multiIndex, FALSE)
})
