test_that('getPlatforms', {
  d = getPlatforms('cdf')
  expect_s3_class(d, 'data.table')
  cols = c('platform', 'custom_cdf_prefix', 'ae_accession', 'ensembl', 'entrez')
  expect_names(colnames(d), permutation.of = cols, what = 'colnames')

  d = getPlatforms('mapping')
  expect_s3_class(d, 'data.table')
  cols = c('platform', 'mappingFunction', 'dbName', 'interName',
           'geneColname', 'splitColumn', 'organism')
  expect_names(colnames(d), permutation.of = cols, what = 'colnames')

  expect_error(getPlatforms('platt'))
})


test_that('getCdfname', {
  platform = 'GPL6246'
  expect_identical(getCdfname(platform, 'ensembl'), 'mogene10stmmensgcdf')
  expect_identical(getCdfname(platform, 'entrez'), 'mogene10stmmentrezgcdf')
  expect_length(getCdfname('GPL0', 'ensembl'), 0)
})


test_that('installCustomCdfPackages', {
  urls = installCustomCdfPackages('mogene10stmmensgcdf', dryRun = TRUE)
  expect_string(urls)
  expect_warning(installCustomCdfPackages('mogenesmoproblems', dryRun = TRUE))
})


test_that('getNaiveEsetGeo', {
  skip_on_cran()
  studies = c('GSE147674', 'GSE167458', 'GSE50143')

  for (study in studies) {
    outputDir = tempfile()
    withr::local_file(outputDir)
    dir.create(outputDir)

    resObs = getNaiveEsetGeo(study, outputDir, file.path(outputDir, 'raw'))
    # reduce disk size of expression set while preserving dimensions
    Biobase::exprs(resObs$eset)[-seq(1, nrow(resObs$eset), 1000), ] = 1
    rownames(resObs$eset@featureData@data) = NULL

    filesObs = dir(outputDir, recursive = TRUE)

    path = file.path(dataDir, paste0(study, '_eset.qs'))
    resExp = snapshot(resObs, path)
    filesExp = snapshot(filesObs, gsub('_eset', '_files', path))

    expect_names(names(resObs), permutation.of = c('eset', 'rmaOk'))
    expect_equal(resObs$rmaOk, resExp$rmaOk)
    expect_equal(resObs$eset@annotation, resExp$eset@annotation)
    expect_equal(resObs$eset@phenoData@data, resExp$eset@phenoData@data)
    expect_equal(resObs$eset@assayData$exprs, resExp$eset@assayData$exprs)
    expect_equal(filesObs, filesExp)}
})


# test_that('getNaiveEsetAe supported', {
#   skip_on_cran()
#   study = 'E-MTAB-8714'
#
#   outputDir = tempfile()
#   withr::local_file(outputDir)
#   dir.create(outputDir)
#
#   resObs = getNaiveEsetAe(study, outputDir, file.path(outputDir, 'raw'))
#   # reduce disk size of expression set while preserving dimensions
#   Biobase::exprs(resObs$eset)[-seq(1, nrow(resObs$eset), 1000), ] = 1
#   rownames(resObs$eset@featureData@data) = NULL
#
#   filesObs = dir(outputDir, recursive = TRUE)
#
#   path = file.path(dataDir, paste0(study, '_eset.qs'))
#   resExp = snapshot(resObs, path)
#   filesExp = snapshot(filesObs, gsub('_eset', '_files', path))
#
#   expect_names(names(resObs), permutation.of = c('eset', 'rmaOk'))
#   expect_equal(resObs$rmaOk, resExp$rmaOk)
#   expect_equal(resObs$eset@annotation, resExp$eset@annotation)
#   expect_equal(resObs$eset@phenoData@data, resExp$eset@phenoData@data)
#   expect_equal(resObs$eset@assayData$exprs, resExp$eset@assayData$exprs)
#   expect_equal(filesObs, filesExp)
# })


# test_that('getNaiveEsetAe unsupported', {
#   study = 'E-MEXP-3696'
#
#   outputDir = tempfile()
#   withr::local_file(outputDir)
#   dir.create(outputDir)
#
#   if (onCran) {
#     expect_error(
#       getNaiveEsetAe(study, outputDir, file.path(outputDir, 'raw')),
#       regexp = NA)
#
#   } else {
#     resObs = getNaiveEsetAe(study, outputDir, file.path(outputDir, 'raw'))
#     filesObs = dir(outputDir, recursive = TRUE)
#
#     path = file.path(dataDir, paste0(study, '_eset.qs'))
#     resExp = snapshot(resObs, path)
#     filesExp = snapshot(filesObs, gsub('_eset', '_files', path))
#
#     expect_equal(resObs, resExp)
#     expect_equal(filesObs, filesExp)}
# })


test_that('getNaiveEsetLocal', {
  result = getNaiveEsetLocal('LOCAL01', 'GPL1261')
  expect_names(names(result), permutation.of = c('eset', 'rmaOk'))
  expect_s4_class(result$eset, 'Eset')
  expect_true(result$rmaOk)

  result = getNaiveEsetLocal('LOCAL01', 'GPL0')
  expect_names(names(result), permutation.of = c('eset', 'rmaOk'))
  expect_null(result$eset)
  expect_character(result$rmaOk)
})


# test_that('getAeMetadata', {
#   study = 'E-MEXP-3780'
#   expers = getAeMetadata(study, type = 'experiments')
#   expect_s3_class(expers, 'data.frame')
#   expect_equal(nrow(expers), 1L)
#
#   files = getAeMetadata(study, type = 'files')
#   expect_s3_class(files, 'data.frame')
# })


test_that('stripFileExt', {
  x = c('S1.cel', 'S2.cel.gz', 'S3', 'S4.CEL')
  y = stripFileExt(x)
  expect_identical(y, paste0('S', seq_len(length(x))))
})


test_that('getNewEmatColnames', {
  newExp = paste0('GSM', 8:12)
  old = paste0(newExp, '_lush', '.cel.gz')
  expect_identical(getNewEmatColnames(old, 'geo'), newExp)
  expect_identical(getNewEmatColnames(old, 'ae'), paste0(newExp, '_lush'))
})


test_that('getProbeGeneMappingDirect', {
  skip_on_cran()
  featureDt = data.table(
    ID = paste0('A', 1:3), GENE = c('12', '', '17'), SEQ = c('at', 'ca', 'tg'))
  mappingExp = featureDt[, c('ID', 'GENE')]
  setnames(mappingExp, c('probe_set', 'gene_id'))
  mappingExp = mappingExp[gene_id != '']
  mappingObs = getProbeGeneMappingDirect(featureDt, 'GENE')
  expect_identical(mappingExp, mappingObs)
})


test_that('getEntrezEnsemblMapping', {
  skip_on_cran()
  m = getEntrezEnsemblMapping('Mm')
  expect_s3_class(m, 'data.table')
  expect_names(
    colnames(m), permutation.of = c('entrez', 'ensembl'), what = 'colnames')
})


test_that('getProbeGeneMapping', {
  skip_on_cran()
  mapPkgVerObs = packageVersion('org.Mm.eg.db')
  mapPkgVerExp = snapshot(mapPkgVerObs, file.path(dataDir, 'map_pkg_ver.qs'))
  if (mapPkgVerObs != mapPkgVerExp) {
    if (mapPkgVerObs < mapPkgVerExp) {
      w = c('New version of map package available,',
            'test snapshots should be regenerated.')
      warning(paste(w, collapse = ' '))}
    skip('Mapping package is different version, skipping.')}

  platforms = getPlatforms('mapping')
  annos = c('GPL6887', 'GPL7202')

  for (anno in annos) {
    featureMetadata = GEOquery::getGEO(anno)
    featureDt = setDT(featureMetadata@dataTable@table)
    platformDt = platforms[platforms$platform == anno]
    mappingObs = getProbeGeneMapping(featureDt, platformDt, 'ensembl')

    path = file.path(dataDir, paste0(anno, '.qs'))
    mappingExp = snapshot(mappingObs, path)
    expect_equal(mappingObs, mappingExp)}
})


test_that('getEmatGene', {
  ematProbe = matrix(
    as.numeric(1:8), nrow = 4L,
    dimnames = list(paste0('probe', 1:4), paste0('sample', 1:2)))
  mapping = data.table(
    probe_set = paste0('probe', 4:1),
    gene_id = paste0('gene', c(1, 1, 2, 3)))
  ematGeneObs = getEmatGene(ematProbe, mapping)
  ematGeneExp = matrix(
    c(3.5, 2, 1, 7.5, 6, 5), nrow = 3L,
    dimnames = list(paste0('gene', 1:3), paste0('sample', 1:2)))
  expect_equal(ematGeneObs, ematGeneExp)
})
