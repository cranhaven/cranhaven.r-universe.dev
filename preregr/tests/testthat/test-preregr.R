
####----------------------------------------------------------------------------

testthat::test_that("exporting and reading a google sheet works", {

  testthat::expect_true({

    ### devtools::load_all("B:/Data/R/preregr");

    ### Qualitative prereg form
    gSheet_url <-
      "https://docs.google.com/spreadsheets/d/1l3N0bWnKlRvXuHO-JFg3XHdWzgJWLNli90bnUUa5op4";

    dat <-
      preregr::read_spreadsheet(
        gSheet_url,
        exportGoogleSheet = TRUE,
        silent=FALSE
      );

    TRUE;

  });

});

####----------------------------------------------------------------------------

testthat::test_that("writing a local backup for an exported google sheet works", {

  testthat::expect_true({

    ### devtools::load_all("B:/Data/R/preregr");

    ### Qualitative prereg form
    gSheet_url <-
      "https://docs.google.com/spreadsheets/d/1l3N0bWnKlRvXuHO-JFg3XHdWzgJWLNli90bnUUa5op4";

    ### Systematic review prereg form
    gSheet_url <-
      "https://docs.google.com/spreadsheets/d/1bHDzpCu4CwEa5_3_q_9vH2691XPhCS3e4Aj_HLhw_U8";

    localBackupFile <-
      tempfile(pattern="preregr-localBackup-", fileext = ".xlsx");

    # localBackupFile <-
    #   here::here(
    #     "inst", "extdata", "preregr-spec-incl-sysrev-reg-form.xlsx"
    #   );

    form_inclSysRev_v0_92 <-
      preregr::read_spreadsheet(
        gSheet_url,
        exportGoogleSheet = TRUE,
        localBackup = localBackupFile,
        silent=FALSE
      );

    ### saveRDS(form_inclSysRev_v0_92, here::here("inst", "extdata", "form_inclSysRev_v0_92.rds"));

    TRUE;

  });

});

####----------------------------------------------------------------------------

testthat::test_that("reading a local excel file works", {

  testthat::expect_true({

    ### devtools::load_all("B:/Data/R/preregr");

    xlsx_file <-
      system.file("extdata", "preregr-form_inclSysRev_v0_92.xlsx",
                  package = "preregr");

    dat <-
      preregr::read_spreadsheet(
        xlsx_file,
        silent=FALSE
      );

    ###---------------------------------------------------------------------------

    localBackupFile <-
      tempfile(pattern="preregr-localBackup-", fileext = ".xlsx");

    ### Local backup should be ignored

    form_inclSysRev_v0_92 <-
      preregr::read_spreadsheet(
        xlsx_file,
        localBackup = localBackupFile,
        silent=FALSE
      );

    ### saveRDS(form_inclSysRev_v0_92, here::here("inst", "extdata", "form_inclSysRev_v0_92.rds"));

    TRUE;

  })
});

####----------------------------------------------------------------------------


testthat::test_that("structuring for serialization works", {

  testthat::expect_true({

    ### devtools::load_all("B:/Data/R/preregr");

    preregr::prereg_initialize(
      'inclSysRev_v0_92'
    ) |>
      preregr::structure_for_serialization() ->
      testObject;

    TRUE;

  });

});

####----------------------------------------------------------------------------

testthat::test_that("converting a prereg spec to YAML works", {

  testthat::expect_true({

    ### devtools::load_all("B:/Data/R/preregr");

    preregr::prereg_initialize(
      "inclSysRev_v0_92"
    ) |>
      preregr::prereg_specify(
        title = "Example Study",
        authors = "Littlebottom, C., Dibbler, C., & Aching, T.",
        nonExistent_item = "This can't be stored anywhere"
      ) |>
      preregr::prereg_spec_to_yaml();

    TRUE;

  });

});

####----------------------------------------------------------------------------

testthat::test_that("writing a prereg spec to a YAML file works", {

  testthat::expect_true({

    ### devtools::load_all("B:/Data/R/preregr");

    tempFile <-
      tempfile(fileext = ".yml");
    # tempFile <-
    #   file.path(
    #     system.file("inst", "extdata", package = "preregr"),
    #     "preregr-spec-incl-sysrev-reg.yml"
    #   );

    preregExample <-
      preregr::prereg_initialize(
        "inclSysRev_v0_92"
      ) |>
      preregr::prereg_specify(
        title = "Example Study",
        authors = "Littlebottom, C., Dibbler, C., & Aching, T.",
        nonExistent_item = "This can't be stored anywhere"
      ) |>
      preregr::prereg_spec_to_yaml(
        file = tempFile
      );

    # writeClipboard(normalizePath(dirname(tempFile)));

    TRUE;

  });

});

####----------------------------------------------------------------------------

testthat::test_that("converting a prereg spec to JSON works", {

  testthat::expect_true({

    ### devtools::load_all("B:/Data/R/preregr");

    preregr::prereg_initialize(
      "inclSysRev_v0_92"
    ) |>
      preregr::prereg_specify(
        title = "Example Study",
        authors = "Littlebottom, C., Dibbler, C., & Aching, T.",
        nonExistent_item = "This can't be stored anywhere"
      ) |>
      preregr::prereg_spec_to_json();

    TRUE;

  });

});

####----------------------------------------------------------------------------

testthat::test_that("writing a prereg spec to a JSON file works", {

  testthat::expect_true({

    ### devtools::load_all("B:/Data/R/preregr");

    tempFile <-
      tempfile(fileext = ".json");

    preregExample <-
      preregr::prereg_initialize(
        "inclSysRev_v0_92"
      ) |>
      preregr::prereg_specify(
        title = "Example Study",
        authors = "Littlebottom, C., Dibbler, C., & Aching, T.",
        nonExistent_item = "This can't be stored anywhere"
      ) |>
      preregr::prereg_spec_to_json(
        file = tempFile
      );

    # writeClipboard(normalizePath(dirname(tempFile)));

    TRUE;

  });

});

####----------------------------------------------------------------------------

testthat::test_that("reading a prereg spec from a YAML file works", {


  testthat::expect_true({

    ### devtools::load_all("B:/Data/R/preregr");

    examplePreregFile <-
      system.file(
        "inst", "extdata",
        "preregr-spec-example1.yml",
        package = "preregr"
      );

    freshImport <-
      preregr::yaml_to_prereg_spec(
        examplePreregFile
      );

    # writeClipboard(normalizePath(dirname(tempFile)));

    TRUE;

  });

});

####----------------------------------------------------------------------------

testthat::test_that("writing a prereg form to an excel file and importing it again works", {

  testthat::expect_true({

    formExample <-
      preregr::form_create(
        title = "Minimal form with only a few fields",
        version = "0.1.0",
        author = "Stibbons, P."
      ) |>
      preregr::form_add_instruction(
        heading = "Instructions",
        description = paste0(
          "This form is simple, so it doesn't require much ",
          "instruction. Still, it's advisable to always RTFM, ",
          "and in the case of (pre)registration forms, the ",
          "instructions are the manual. So better read closely!"
        )
      ) |>
      preregr::form_add_section(
        id = "only_section",
        label = "Only Section",
        description = paste0(
          "This is the only section in this form. That's ",
          "because this is such a simple form. However, that ",
          "means this section is very important, because it ",
          "contains all the items. Therefore, you may want to ",
          "study this section description very carefully."
        )
      ) |>
      preregr::form_add_item(
        id = "study_title",
        label = "Study Title",
        section_id = "only_section",
        description = paste0(
          "Think of a catchy title, preferably with a colon in ",
          "the middle. Bonus points for pop culture references."
        )
      ) |>
      preregr::form_add_item(
        id = "registration_type",
        label = "Registration type",
        section_id = "only_section",
        description = paste0(
          "Describe briefly why you are (pre)registering this ",
          "study. For example, this might be a preregistration ",
          "to allow others to know you're doing this study; or to ",
          "make it clear you value transparency in science; or to ",
          "remember your original plans later on. Or this might be ",
          "a registration to update your plans after the data came ",
          "in; or to document pragmatic changes in plans."
        )
      );

    tempFile <- tempfile(fileext = ".xlsx");

    preregr::form_to_xlsx(
      formExample,
      file = tempFile
    );

    #writeClipboard(normalizePath(dirname(tempFile)));

    newForm <-
      preregr::prereg_initialize(
        tempFile
      );

    preregr::form_show(newForm);

    TRUE;

  });

});

####----------------------------------------------------------------------------

testthat::test_that("Creating an Rmd template from a new fresh form", {

  testthat::expect_true({

    tempFile <- tempfile(fileext = ".Rmd");

    preregr::form_create(
      title = "Example form",
      version = "0.1.0"
    ) |>
      preregr::form_to_rmd_template(
        file = tempFile
      );

    TRUE;

  });

});

####----------------------------------------------------------------------------

testthat::test_that("Creating an Rmd template from an included form", {

  testthat::expect_true({

    tempFile <- tempfile(fileext = ".Rmd");

    preregr::form_to_rmd_template(
      "inclSysRev_v0_92",
      file = tempFile
    );

    TRUE;

  });

});

####----------------------------------------------------------------------------

