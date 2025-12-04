test_that("Test Case 1: 9-mer HCS (single host), with CSV output",{
    HCS_1host<- concat_conserved_kmer(proteins_1host)$csv

    expect_equal(nrow(HCS_1host),7)
    expect_equal(ncol(HCS_1host),3)

    col_names<-colnames(HCS_1host)
    expect_equal(col_names[1],'HCS')
    expect_equal(col_names[2],'Position')
    expect_equal(col_names[3],'Sequence')

    #randomly pick 3 rows to check
    #row 1
    firstRow <- HCS_1host[1,]
    expect_equal(firstRow$HCS, 'HCS_A_1')
    expect_equal(firstRow$Position, '1-18')
    expect_equal(firstRow$Sequence, 'MSTNPKPQRKTKRNTNRR')

    #row 3
    thirdRow <- HCS_1host[3,]
    expect_equal(thirdRow$HCS, 'HCS_A_3')
    expect_equal(thirdRow$Position, '50-67')
    expect_equal(thirdRow$Sequence, 'RKTSERSQPRGRRQPIPK')

    #last row, row 7
    lastRow <- HCS_1host[7,]
    expect_equal(lastRow$HCS, 'HCS_B_2')
    expect_equal(lastRow$Position, '154-166')
    expect_equal(lastRow$Sequence, 'FRAAVCTRGVAKA')
})

test_that("Test Case 2: 9-mer CCS (single host), with CSV output",{
    CCS_1host<- concat_conserved_kmer(proteins_1host, conservation_level = 'CCS')$csv

    expect_equal(nrow(CCS_1host),1)
    expect_equal(ncol(CCS_1host),3)

    col_names<-colnames(CCS_1host)
    expect_equal(col_names[1],'CCS')
    expect_equal(col_names[2],'Position')
    expect_equal(col_names[3],'Sequence')

    #row 1
    firstRow <- CCS_1host[1,]
    expect_equal(firstRow$CCS, 'CCS_A_1')
    expect_equal(firstRow$Position, '1-18')
    expect_equal(firstRow$Sequence, 'MSTNPKPQRKTKRNTNRR')

})

test_that("Test Case 3: 9-mer HCS (single host), with FASTA output",{
    HCS_1host<- concat_conserved_kmer(proteins_1host)$fasta

    expect_equal(nrow(HCS_1host),14)
    expect_equal(ncol(HCS_1host),1)

    #randomly pick 3 FASTA to check
    expect_equal(HCS_1host[1,1], '>HCS_A_1')
    expect_equal(HCS_1host[2,1], 'MSTNPKPQRKTKRNTNRR')

    expect_equal(HCS_1host[5,1], '>HCS_A_3')
    expect_equal(HCS_1host[6,1], 'RKTSERSQPRGRRQPIPK')

    expect_equal(HCS_1host[11,1], '>HCS_B_1')
    expect_equal(HCS_1host[12,1], 'TSLTGRDKN')
})

test_that("Test Case 4: 9-mer CCS (single host), with FASTA output",{
    CCS_1host<- concat_conserved_kmer(proteins_1host, conservation_level = 'CCS')$fasta

    expect_equal(nrow(CCS_1host),2)
    expect_equal(ncol(CCS_1host),1)

    expect_equal(CCS_1host[1,1], '>CCS_A_1')
    expect_equal(CCS_1host[2,1], 'MSTNPKPQRKTKRNTNRR')

})
