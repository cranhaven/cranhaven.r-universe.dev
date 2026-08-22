test_that("species index normalizes columns and derives fields", {
  file <- tempfile(fileext = ".csv")
  writeLines(c(
    "Sequence ID,Data source,Order,Family,Genus,Specific epithet,Specimen reference,Specimen barcode,Collection date,Country of origin,Material sampled,No. of genes recovered,No. of bp recovered,Fasta file url",
    "224,PAFTOL,Saxifragales,Saxifragaceae,Saxifraga,fortunei,JV_Sf (K),,NA,,Live plant,306,203583,https://example.org/INSDC.ERR5006173.Saxifraga_fortunei.a353.fasta",
    "230,PAFTOL,Malpighiales,Lophopyxidaceae,Lophopyxis,maingayi,Kerenga,K001275029,1978,,Herbarium,322,216576,https://example.org/INSDC.ERR4180090.Lophopyxis_maingayi.a353.fasta"
  ), file)

  index <- tol_species_index(file)

  expect_equal(nrow(index), 2)
  expect_true(all(c("sequence_id", "scientific_name", "fasta_file_name") %in% names(index)))
  expect_equal(index$scientific_name[1], "Saxifraga fortunei")
})

test_that("built-in species index is available as a tibble", {
  index <- tol_species_index()

  expect_s3_class(index, "tbl_df")
  expect_equal(nrow(index), 20485)
  expect_true(all(c("sequence_id", "scientific_name", "fasta_file_url") %in% names(index)))
})

test_that("species search filters by taxonomy and sequence id", {
  index <- data.frame(
    sequence_id = c(224L, 230L),
    order = c("Saxifragales", "Malpighiales"),
    family = c("Saxifragaceae", "Lophopyxidaceae"),
    genus = c("Saxifraga", "Lophopyxis"),
    specific_epithet = c("fortunei", "maingayi"),
    scientific_name = c("Saxifraga fortunei", "Lophopyxis maingayi"),
    specimen_reference = c("JV_Sf (K)", "Kerenga"),
    specimen_barcode = c(NA, "K001275029"),
    fasta_file_url = c("https://example.org/a.fasta", "https://example.org/b.fasta")
  )

  expect_equal(nrow(tol_search_species(index, genus = "saxifraga")), 1)
  expect_equal(tol_search_species(index, sequence_id = 230)$genus, "Lophopyxis")
  expect_equal(nrow(tol_search_species(index, query = "K001275029")), 1)
})

test_that("species matching reports exact matches and missing names", {
  index <- data.frame(
    sequence_id = c(1L, 2L),
    scientific_name = c("Cnestis ferruginea", "Agelaea pentagyna"),
    order = c("Oxalidales", "Oxalidales"),
    family = c("Connaraceae", "Connaraceae"),
    genus = c("Cnestis", "Agelaea"),
    specific_epithet = c("ferruginea", "pentagyna"),
    no_of_genes_recovered = c(300L, 200L),
    no_of_bp_recovered = c(200000L, 100000L),
    fasta_file_url = c("https://example.org/a.fasta", "https://example.org/b.fasta")
  )

  matches <- tol_match_species(
    c("Cnestis ferruginea", "Manotes expansa"),
    index = index
  )

  expect_equal(nrow(matches), 2)
  expect_equal(matches$match_type, c("exact", "none"))
  expect_equal(matches$has_data, c(TRUE, FALSE))
  expect_equal(matches$requested_name, c("Cnestis ferruginea", "Manotes expansa"))
})

test_that("species matching can keep best record for duplicates", {
  index <- data.frame(
    sequence_id = c(1L, 2L),
    scientific_name = c("Cnestis ferruginea", "Cnestis ferruginea"),
    order = "Oxalidales",
    family = "Connaraceae",
    genus = "Cnestis",
    specific_epithet = "ferruginea",
    no_of_genes_recovered = c(100L, 300L),
    no_of_bp_recovered = c(100000L, 200000L),
    fasta_file_url = c("https://example.org/a.fasta", "https://example.org/b.fasta")
  )

  matches <- tol_match_species("Cnestis ferruginea", index = index, multiple = "best")

  expect_equal(nrow(matches), 1)
  expect_equal(matches$sequence_id, 2L)
})

test_that("species matching supports conservative fuzzy matching", {
  index <- data.frame(
    sequence_id = 1L,
    scientific_name = "Agelaea pentagyna",
    order = "Oxalidales",
    family = "Connaraceae",
    genus = "Agelaea",
    specific_epithet = "pentagyna",
    no_of_genes_recovered = 200L,
    no_of_bp_recovered = 100000L,
    fasta_file_url = "https://example.org/a.fasta"
  )

  matches <- tol_match_species("Agelaea pentagina", index = index, fuzzy = TRUE, max_distance = 2)

  expect_equal(matches$match_type, "fuzzy")
  expect_equal(matches$matched_name, "Agelaea pentagyna")
  expect_true(matches$has_data)
})

test_that("FASTA resolution creates local paths without downloading", {
  records <- data.frame(
    sequence_id = 224L,
    order = "Saxifragales",
    family = "Saxifragaceae",
    genus = "Saxifraga",
    specific_epithet = "fortunei",
    scientific_name = "Saxifraga fortunei",
    fasta_file_url = "https://example.org/INSDC.ERR5006173.Saxifraga_fortunei.a353.fasta"
  )

  plan <- tol_resolve_fasta(records, dest_dir = tempdir())

  expect_equal(plan$fasta_file_name, "INSDC.ERR5006173.Saxifraga_fortunei.a353.fasta")
  expect_equal(plan$status, "missing")
})

test_that("FASTA resolution uses a temporary directory by default", {
  records <- data.frame(
    sequence_id = 224L,
    order = "Saxifragales",
    family = "Saxifragaceae",
    genus = "Saxifraga",
    specific_epithet = "fortunei",
    scientific_name = "Saxifraga fortunei",
    fasta_file_url = "https://example.org/INSDC.ERR5006173.Saxifraga_fortunei.a353.fasta"
  )

  plan <- tol_resolve_fasta(records)

  expect_true(startsWith(plan$local_path, normalizePath(tempdir(), winslash = "/", mustWork = FALSE)))
})

test_that("download FASTA validate_only does not write a manifest by default for temp files", {
  records <- data.frame(
    sequence_id = 224L,
    order = "Saxifragales",
    family = "Saxifragaceae",
    genus = "Saxifraga",
    specific_epithet = "fortunei",
    scientific_name = "Saxifraga fortunei",
    fasta_file_url = "https://example.org/INSDC.ERR5006173.Saxifraga_fortunei.a353.fasta"
  )

  plan <- tol_download_fasta(records, validate_only = TRUE)

  expect_equal(plan$status, "missing")
  expect_true(startsWith(plan$local_path, normalizePath(tempdir(), winslash = "/", mustWork = FALSE)))
})

test_that("save FASTA copies existing temporary files to a permanent directory", {
  source_dir <- tempfile("source")
  dest_dir <- tempfile("dest")
  dir.create(source_dir)
  source_file <- file.path(source_dir, "sample.fasta")
  writeLines(">seq\nACGT", source_file)

  plan <- data.frame(
    fasta_file_name = "sample.fasta",
    local_path = normalizePath(source_file, winslash = "/", mustWork = TRUE),
    status = "ok"
  )

  saved <- tol_save_fasta(plan, dest_dir = dest_dir)

  expect_equal(saved$status, "ok")
  expect_true(file.exists(file.path(dest_dir, "sample.fasta")))
})

test_that("export FASTA copies files and writes a manifest", {
  source_dir <- tempfile("source")
  dest_dir <- tempfile("dest")
  manifest <- tempfile(fileext = ".csv")
  dir.create(source_dir)
  source_file <- file.path(source_dir, "sample.fasta")
  writeLines(">seq\nACGT", source_file)

  plan <- data.frame(
    fasta_file_name = "sample.fasta",
    local_path = normalizePath(source_file, winslash = "/", mustWork = TRUE),
    status = "ok"
  )

  exported <- tol_export_fasta(plan, dest_dir = dest_dir, manifest_path = manifest)

  expect_equal(exported$export_status, "ok")
  expect_true(file.exists(file.path(dest_dir, "sample.fasta")))
  expect_true(file.exists(manifest))
})

test_that("read FASTA parses sequence records", {
  file <- tempfile(fileext = ".fasta")
  writeLines(c(">gene1 sample", "ACG", "T", ">gene2", "TTAA"), file)

  fasta <- tol_read_fasta(file)

  expect_equal(nrow(fasta), 2)
  expect_equal(fasta$header[1], "gene1 sample")
  expect_equal(fasta$sequence[1], "ACGT")
  expect_equal(fasta$width[2], 4)
})

test_that("attach FASTA creates a list-column for multiple species", {
  dir <- tempfile("fasta")
  dir.create(dir)
  file_a <- file.path(dir, "a.fasta")
  file_b <- file.path(dir, "b.fasta")
  writeLines(c(">a1", "ACGT"), file_a)
  writeLines(c(">b1", "TTAA", ">b2", "CCGG"), file_b)

  plan <- data.frame(
    sequence_id = c(1L, 2L),
    scientific_name = c("Species a", "Species b"),
    fasta_file_name = c("a.fasta", "b.fasta"),
    local_path = normalizePath(c(file_a, file_b), winslash = "/", mustWork = TRUE),
    status = "ok",
    stringsAsFactors = FALSE
  )

  attached <- tol_attach_fasta(plan)

  expect_true("fasta" %in% names(attached))
  expect_true(is.list(attached$fasta))
  expect_equal(nrow(attached$fasta[[1]]), 1)
  expect_equal(nrow(attached$fasta[[2]]), 2)
})
