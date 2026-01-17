test_that("test 'dola example'", {
  expect_snapshot_file(dola(ano_ini = 2019, ano_fim = 2024,
                              nome_instituicao = c("Universidade Federal de Santa Catarina"),
                              nome_ppg = c("Programa de Pós-graduação em Oceanografia","oceanografia"),
                              nome_area = c("Geociências"),
                              xlsx_qualis = system.file("extdata","Qualis.xlsx", package="DoLa"),
                              cv_docentes = system.file("extdata","cv_do", package="DoLa"),
                              cv_discentes = system.file("extdata","cv_di", package="DoLa")),
                       "DoLa.html")
})
