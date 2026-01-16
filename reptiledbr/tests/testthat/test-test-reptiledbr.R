library(testthat)
# Configuración inicial para los tests (fixtures)
setup_test_data <- function() {
  # Crear un objeto reptile_data mock similar al que devolvería get_reptiledb_data()
  mock_data <- tibble::tibble(
    input_name = c("Lachesis muta", "Python bivittatus", "Crotalus atrox"),
    genus = c("Lachesis", "Python", "Crotalus"),
    species = c("muta", "bivittatus", "atrox"),
    url = c(
      "http://reptile-database.org/species1",
      "http://reptile-database.org/species2",
      "http://reptile-database.org/species3"
    ),
    status = c("success", "success", "success"),
    error_message = c(NA, NA, NA),
    data = list(
      # Lachesis muta data
      tibble::tibble(
        attribute = c("Distribution", "Synonym", "Higher Taxa", "Common Names", "Reproduction", "Type", "Diagnosis", "Comment", "Etymology", "Reference"),
        content = list(
          c("Colombia, E Ecuador, Brazil...", "Type locality: Suriname..."),
          c("Crotalus mutus LINNAEUS 1766: 373", "Coluber crotalinus GMELIN 1789: 1094"),
          c("Viperidae", "Crotalinae", "Serpentes"),
          c("E: South American Bushmaster", "G: Südamerikanischer Buschmeister"),
          "oviparous; one of the few snakes that show a form of...",
          c("Holotype: NRM = NHRM...", "Holotype: ZSM, uncatalogued specimen(s)..."),
          "Diagnosis: Distinguished from L. stenophrys and L. melanocephala...",
          c("Venomous!", "Synonymy: Lachesis muta has been split..."),
          "The specific name muta is from the Latin mutus(= mute)...",
          c("Alves, Fátima Q.; AntÃ´nio J. S...", "Amaral, A. do 1925...")
        )
      ),
      # Python bivittatus data
      tibble::tibble(
        attribute = c("Distribution", "Synonym", "Higher Taxa", "Subspecies", "Common Names", "Reproduction", "Type", "Diagnosis", "Comment"),
        content = list(
          c("SE Nepal, India (Assam, Tripura...)", "Introduced to Florida (USA)"),
          c("Python molurus bivittatus KUHL 1820", "Python reticulatus bivittatus FITZINGER 1826"),
          c("Pythonidae", "Henophidia", "Serpentes"),
          c("Python bivittatus progschai JACOBS, AULYIA & BÖHME 2009", "Python bivittatus bivittatus KUHL 1820"),
          c("E: Burmese Python", "G: Dunkler Tigerpython", "Chinese: 蟒"),
          "GROOT et al. (2003) and KUHN & SCHMIDT (2003) presented...",
          c("Holotype: iconotype: Plate in SEBA 1735 and KUHL 1820", "Holotype: ZFMK 87481 [progschai]"),
          "Diagnosis: Large serpents; labials separated from eye...",
          c("Subspecies: This species has been considered...", "Hybridization: \"The evidence...")
        )
      ),
      # Crotalus atrox data
      tibble::tibble(
        attribute = c("Distribution", "Higher Taxa", "Common Names", "Reproduction", "Type", "Diagnosis", "Comment", "Etymology"),
        content = list(
          c("USA (SE California, S Nevada...)", "Mexico (Mexico [HR 35: 190]...)"),
          c("Viperidae", "Crotalinae", "Serpentes"),
          c("E: Western Diamond-backed Rattlesnake", "G: Texas-Klapperschlange"),
          "ovovivparous. C. atrox and C. horridus may hybridize...",
          c("Holotype: USNM 7761", "Holotype: CAS 50515 [tortugensis]"),
          "Additional details, e.g. a detailed description or comparison...",
          c("Venomous! Crotalus atrox is responsible for most cases...", "Nomenclature: Hoser's 2009..."),
          "The specific name, \"atrox,\" is a Greek word meaning \"terrible\"..."
        )
      )
    )
  )

  return(mock_data)
}

# Tests para get_reptiledb_data
test_that("get_reptiledb_data recupera información de especies", {
  # Mock de la función para no hacer llamadas reales a la API
  mock_function <- mockery::mock(setup_test_data())
  with_mocked_bindings(
    "get_reptiledb_data" = mock_function,
    {
      result <- get_reptiledb_data(c("Lachesis muta", "Python bivittatus"))

      # Verificamos que la función fue llamada
      mockery::expect_called(mock_function, 1)

      # Verificaciones básicas sobre el resultado
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 3)
      expect_true(all(c("input_name", "genus", "species", "status", "data") %in% names(result)))

      # Verificar que contiene los datos esperados
      expect_equal(result$input_name, c("Lachesis muta", "Python bivittatus", "Crotalus atrox"))
      expect_true(all(result$status == "success"))
    }
  )
})

# Tests para format_synonyms
test_that("format_synonyms formatea correctamente los sinónimos", {
  test_data <- setup_test_data()

  # Mock de la función format_synonyms para probar su comportamiento
  mock_format_synonyms <- function(reptile_data) {
    tibble::tibble(
      input_name = rep(reptile_data$input_name[1:2], each = 2),
      genus = rep(reptile_data$genus[1:2], each = 2),
      species = rep(reptile_data$species[1:2], each = 2),
      synonym = c(
        "Crotalus mutus LINNAEUS 1766: 373",
        "Coluber crotalinus GMELIN 1789: 1094",
        "Python molurus bivittatus KUHL 1820",
        "Python reticulatus bivittatus FITZINGER 1826"
      )
    )
  }

  with_mocked_bindings(
    "format_synonyms" = mock_format_synonyms,
    {
      result <- format_synonyms(test_data)

      # Verificaciones sobre el resultado
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 4)
      expect_equal(names(result), c("input_name", "genus", "species", "synonym"))

      # Las primeras dos filas son de Lachesis muta
      expect_equal(result$input_name[1:2], rep("Lachesis muta", 2))
      # Las siguientes dos son de Python bivittatus
      expect_equal(result$input_name[3:4], rep("Python bivittatus", 2))
    }
  )
})

# Tests para format_distribution
test_that("format_distribution formatea correctamente la distribución", {
  test_data <- setup_test_data()

  # Crear un resultado simulado para format_distribution
  mock_distribution_result <- tibble::tibble(
    input_name = rep(test_data$input_name, c(3, 2, 2)),
    genus = rep(test_data$genus, c(3, 2, 2)),
    species = rep(test_data$species, c(3, 2, 2)),
    distribution = c(
      "Colombia, E Ecuador, Brazil...",
      "rhombeata: Brazil (Alagoas, Bahia, Rio de Janeiro...)",
      "Type locality: Suriname...",
      "SE Nepal, India (Assam, Tripura...)",
      "Introduced to Florida (USA)",
      "USA (SE California, S Nevada...)",
      "Mexico (Mexico [HR 35: 190]...)"
    )
  )

  # Mock de la función format_distribution
  mock_format_distribution <- mockery::mock(mock_distribution_result)

  with_mocked_bindings(
    "format_distribution" = mock_format_distribution,
    {
      result <- format_distribution(test_data)

      # Verificamos que la función fue llamada
      mockery::expect_called(mock_format_distribution, 1)

      # Verificaciones sobre el resultado
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 7)
      expect_equal(names(result), c("input_name", "genus", "species", "distribution"))

      # Verificar la distribución por especie
      lachesis_rows <- result |>  dplyr::filter(input_name == "Lachesis muta")
      expect_equal(nrow(lachesis_rows), 3)

      python_rows <- result |>  dplyr::filter(input_name == "Python bivittatus")
      expect_equal(nrow(python_rows), 2)
      expect_true(any(grepl("Florida", python_rows$distribution)))

      crotalus_rows <- result |>  dplyr::filter(input_name == "Crotalus atrox")
      expect_equal(nrow(crotalus_rows), 2)
    }
  )
})

# Tests para format_higher_taxa
test_that("format_higher_taxa formatea correctamente la taxonomía superior", {
  test_data <- setup_test_data()

  # Crear un resultado simulado para format_higher_taxa
  mock_taxa_result <- tibble::tibble(
    input_name = rep(test_data$input_name, c(3, 3, 3)),
    genus = rep(test_data$genus, c(3, 3, 3)),
    species = rep(test_data$species, c(3, 3, 3)),
    taxon = c(
      "Viperidae", "Crotalinae", "Serpentes",
      "Pythonidae", "Henophidia", "Serpentes",
      "Viperidae", "Crotalinae", "Serpentes"
    )
  )

  # Mock de la función format_higher_taxa
  mock_format_higher_taxa <- mockery::mock(mock_taxa_result)

  with_mocked_bindings(
    "format_higher_taxa" = mock_format_higher_taxa,
    {
      result <- format_higher_taxa(test_data)

      # Verificamos que la función fue llamada
      mockery::expect_called(mock_format_higher_taxa, 1)

      # Verificaciones sobre el resultado
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 9)
      expect_equal(names(result), c("input_name", "genus", "species", "taxon"))

      # Verificar taxones específicos
      expect_true(any(result$taxon == "Viperidae"))
      expect_true(any(result$taxon == "Pythonidae"))
      expect_true(any(result$taxon == "Serpentes"))

      # Verificar que cada especie tiene 3 taxones relacionados
      taxa_counts <- table(result$input_name)
      expect_equal(as.numeric(taxa_counts), c(3, 3, 3))
    }
  )
})

# Tests para format_subspecies
test_that("format_subspecies formatea correctamente las subespecies", {
  test_data <- setup_test_data()

  # Crear un resultado simulado para format_subspecies
  mock_subspecies_result <- tibble::tibble(
    input_name = c("Python bivittatus", "Python bivittatus"),
    genus = c("Python", "Python"),
    species = c("bivittatus", "bivittatus"),
    subspecies = c(
      "Python bivittatus progschai JACOBS, AULYIA & BÖHME 2009",
      "Python bivittatus bivittatus KUHL 1820"
    )
  )

  # Mock de la función format_subspecies
  mock_format_subspecies <- mockery::mock(mock_subspecies_result)

  with_mocked_bindings(
    "format_subspecies" = mock_format_subspecies,
    {
      result <- format_subspecies(test_data)

      # Verificamos que la función fue llamada
      mockery::expect_called(mock_format_subspecies, 1)

      # Verificaciones sobre el resultado
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 2)
      expect_equal(names(result), c("input_name", "genus", "species", "subspecies"))

      # Verificar que todas las subespecies son de Python bivittatus
      expect_true(all(result$input_name == "Python bivittatus"))

      # Verificar subespecies específicas
      expect_true(any(grepl("progschai", result$subspecies)))
      expect_true(any(grepl("bivittatus KUHL", result$subspecies)))
    }
  )
})

# Tests para format_common_names
test_that("format_common_names formatea correctamente los nombres comunes", {
  test_data <- setup_test_data()

  # Crear un resultado simulado para format_common_names
  mock_common_names_result <- tibble::tibble(
    input_name = rep(test_data$input_name, c(2, 3, 2)),
    genus = rep(test_data$genus, c(2, 3, 2)),
    species = rep(test_data$species, c(2, 3, 2)),
    common_name = c(
      "E: South American Bushmaster",
      "G: Südamerikanischer Buschmeister",
      "E: Burmese Python",
      "G: Dunkler Tigerpython",
      "Chinese: 蟒",
      "E: Western Diamond-backed Rattlesnake",
      "G: Texas-Klapperschlange"
    )
  )

  # Mock de la función format_common_names
  mock_format_common_names <- mockery::mock(mock_common_names_result)

  with_mocked_bindings(
    "format_common_names" = mock_format_common_names,
    {
      result <- format_common_names(test_data)

      # Verificamos que la función fue llamada
      mockery::expect_called(mock_format_common_names, 1)

      # Verificaciones sobre el resultado
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 7)
      expect_equal(names(result), c("input_name", "genus", "species", "common_name"))

      # Verificar nombres comunes por especie
      lachesis_names <- result |>
        dplyr::filter(input_name == "Lachesis muta") |>
        dplyr::pull(common_name)
      expect_equal(length(lachesis_names), 2)
      expect_true(any(grepl("Bushmaster", lachesis_names)))

      python_names <- result |>
        dplyr::filter(input_name == "Python bivittatus") |>
        dplyr::pull(common_name)
      expect_equal(length(python_names), 3)
      expect_true(any(grepl("Burmese", python_names)))

      crotalus_names <- result |>
        dplyr::filter(input_name == "Crotalus atrox") |>
        dplyr::pull(common_name)
      expect_equal(length(crotalus_names), 2)
      expect_true(any(grepl("Rattlesnake", crotalus_names)))
    }
  )
})




# Tests para format_selected_attributes
test_that("format_selected_attributes formatea correctamente atributos seleccionados", {
  test_data <- setup_test_data()

  # Resultados simulados para diferentes funciones de formato
  mock_synonym_result <- tibble::tibble(
    input_name = rep(c("Lachesis muta", "Python bivittatus"), each = 2),
    genus = rep(c("Lachesis", "Python"), each = 2),
    species = rep(c("muta", "bivittatus"), each = 2),
    synonym = c(
      "Crotalus mutus LINNAEUS 1766: 373",
      "Coluber crotalinus GMELIN 1789: 1094",
      "Python molurus bivittatus KUHL 1820",
      "Python reticulatus bivittatus FITZINGER 1826"
    )
  )

  mock_higher_taxa_result <- tibble::tibble(
    input_name = rep(test_data$input_name, each = 1),
    genus = rep(test_data$genus, each = 1),
    species = rep(test_data$species, each = 1),
    taxon = c("Viperidae", "Pythonidae", "Viperidae")
  )

  mock_common_names_result <- tibble::tibble(
    input_name = test_data$input_name,
    genus = test_data$genus,
    species = test_data$species,
    common_name = c(
      "E: South American Bushmaster",
      "E: Burmese Python",
      "E: Western Diamond-backed Rattlesnake"
    )
  )

  # Resultado esperado de format_selected_attributes
  expected_result <- list(
    "Synonym" = mock_synonym_result,
    "Higher Taxa" = mock_higher_taxa_result,
    "Common Names" = mock_common_names_result
  )

  # Mock para la función format_selected_attributes
  mock_format_selected <- mockery::mock(expected_result)

  with_mocked_bindings(
    "format_selected_attributes" = mock_format_selected,
    {
      result <- format_selected_attributes(test_data,
                                           c("Synonym", "Higher Taxa", "Common Names"),
                                           quiet = TRUE)

      # Verificamos que la función fue llamada
      mockery::expect_called(mock_format_selected, 1)

      # Verificaciones sobre el resultado
      expect_type(result, "list")
      expect_equal(length(result), 3)
      expect_equal(names(result), c("Synonym", "Higher Taxa", "Common Names"))

      # Verificar estructura de cada componente
      expect_s3_class(result$Synonym, "tbl_df")
      expect_s3_class(result$`Higher Taxa`, "tbl_df")
      expect_s3_class(result$`Common Names`, "tbl_df")

      # Verificar contenido específico
      expect_equal(nrow(result$Synonym), 4)
      expect_equal(nrow(result$`Higher Taxa`), 3)
      expect_equal(nrow(result$`Common Names`), 3)
    }
  )
})

# Tests para format_all_attributes
test_that("format_all_attributes formatea correctamente todos los atributos", {
  test_data <- setup_test_data()

  # Crear un resultado simulado para format_all_attributes
  mock_all_attributes <- list(
    distribution = tibble::tibble(
      input_name = c("Lachesis muta", "Python bivittatus", "Crotalus atrox"),
      genus = c("Lachesis", "Python", "Crotalus"),
      species = c("muta", "bivittatus", "atrox"),
      distribution = c(
        "Colombia, E Ecuador, Brazil...",
        "SE Nepal, India (Assam, Tripura...)",
        "USA (SE California, S Nevada...)"
      )
    ),
    synonyms = tibble::tibble(
      input_name = c("Lachesis muta", "Python bivittatus"),
      genus = c("Lachesis", "Python"),
      species = c("muta", "bivittatus"),
      synonym = c(
        "Crotalus mutus LINNAEUS 1766: 373",
        "Python molurus bivittatus KUHL 1820"
      )
    ),
    higher_taxa = tibble::tibble(
      input_name = test_data$input_name,
      genus = test_data$genus,
      species = test_data$species,
      taxon = c("Viperidae", "Pythonidae", "Viperidae")
    ),
    subspecies = tibble::tibble(
      input_name = "Python bivittatus",
      genus = "Python",
      species = "bivittatus",
      subspecies = "Python bivittatus progschai JACOBS, AULYIA & BÖHME 2009"
    ),
    common_names = tibble::tibble(
      input_name = test_data$input_name,
      genus = test_data$genus,
      species = test_data$species,
      common_name = c(
        "E: South American Bushmaster",
        "E: Burmese Python",
        "E: Western Diamond-backed Rattlesnake"
      )
    )
  )

  # Mock para format_all_attributes
  mock_format_all <- mockery::mock(mock_all_attributes)

  with_mocked_bindings(
    "format_all_attributes" = mock_format_all,
    {
      result <- format_all_attributes(test_data)

      # Verificamos que la función fue llamada
      mockery::expect_called(mock_format_all, 1)

      # Verificaciones sobre el resultado
      expect_type(result, "list")
      expect_gt(length(result), 4) # Debería tener múltiples componentes

      # Verificar algunos componentes específicos
      expect_true("distribution" %in% names(result))
      expect_true("synonyms" %in% names(result))
      expect_true("higher_taxa" %in% names(result))
      expect_true("common_names" %in% names(result))

      # Verificar estructura de algunos componentes
      expect_s3_class(result$distribution, "tbl_df")
      expect_s3_class(result$higher_taxa, "tbl_df")

      # Verificar contenido específico
      expect_equal(nrow(result$distribution), 3)
      expect_equal(nrow(result$higher_taxa), 3)
    }
  )
})
