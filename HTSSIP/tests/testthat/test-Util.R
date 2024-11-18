expect_fun = function(df1, df2){
  expect_is(df1, 'data.frame')
  expect_equal(class(df1), class(df2))
  expect_equal(nrow(df1), nrow(df2))
  expect_equal(ncol(df1), ncol(df2))
}

test_that('phyloseq sample_data can be converted to dataframe',{
  df1 = phyloseq2df(physeq_S2D2, phyloseq::sample_data)
  df2 = suppressWarnings(as.data.frame(as.matrix(sample_data(physeq_S2D2))))

  expect_fun(df1, df2)
})


test_that('phyloseq tax_table can be converted to dataframe',{
  df1 = phyloseq2df(physeq_S2D2, phyloseq::tax_table)
  df2 = suppressWarnings(as.data.frame(as.matrix(tax_table(physeq_S2D2))))

  expect_fun(df1, df2)
})

test_that('phyloseq otu_table can be converted to dataframe',{
  df1 = phyloseq2df(physeq_S2D2, phyloseq::otu_table)
  df2 = suppressWarnings(as.data.frame(as.matrix(otu_table(physeq_S2D2))))

  expect_fun(df1, df2)
})


#-- other dataset --#

test_that('phyloseq_re sample_data can be converted to dataframe',{
  df1 = phyloseq2df(physeq_rep3, phyloseq::sample_data)
  df2 = suppressWarnings(as.data.frame(as.matrix(sample_data(physeq_rep3))))

  expect_fun(df1, df2)
})

test_that('phyloseq tax_table can be converted to dataframe',{
  expect_error(
    phyloseq2df(physeq_rep3, phyloseq::tax_table)
  )
})

test_that('phyloseq otu_table can be converted to dataframe',{
  df1 = phyloseq2df(physeq_rep3, phyloseq::otu_table)
  df2 = suppressWarnings(as.data.frame(as.matrix(otu_table(physeq_rep3))))

  expect_fun(df1, df2)
})



