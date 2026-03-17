
rm(list = ls(all.names = TRUE))

bioinformatics_btaf430 = bibentry(
  bibtype = 'article',
  title = 'Quantile Index predictors using R package `hyper.gam`',
  author = c('Tingting Zhan', 'Misung Yi', 'Inna Chervoneva' ),
  journal = 'Bioinformatics',
  volume = {41}, number = {8}, pages = 'btaf430',
  year = '2025', month = '07',
  issn = '1367-4811',
  doi = '10.1093/bioinformatics/btaf430'
)

save.image(file = './data/bib.rda', compress = 'xz')
