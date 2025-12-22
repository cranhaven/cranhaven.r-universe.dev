ui_about <- function(id){
  tagList(
    fluidRow(
      tags$section(
        id="logo_about",
        imageOutput("logoabout")
      ),
      tags$section(
        id = "text_about",
        tags$h3("About"),
        tags$p("The DataMetProcess application is a shiny tool designed for basic and fundamental meteorological data processing based on the DataMetProcess package (link). Although it was developed using INMET data, it was programmed to be easily applicable to other databases."),
        tags$h3("Authors"),
        tags$p("
               Wagner Martins dos Santos, Hoi Leong Lee, Edimir Xavier Leal Ferraz, Abelardo Antônio de Assunção Montenegro, Lady Daiane Costa de Sousa Martins, Alan Cézar Bezerra, Ênio Farias de França e Silva, Thieres George Freire da Silva, João L.M.P. de Lima, Xuguang Tang and Alexandre Maniçoba da Rosa Ferraz Jardim
               "),
        tags$h3("Citation"),
        tags$p("
               Santos, W. M. et al.(2025). DataMetProcess: An open-source package and Shiny application for the acquisition and processing of meteorological data from INMET. SoftwareX, 30. https://doi.org/10.1016/J.SOFTX.2025.102185
               "),
        tags$h3("Contacts"),
        tags$p("
               wagnnerms97@gmail.com
               "),
        tags$h3("Help"),
        tags$a(
          "Visit github for help on how to use the application",
          href = "https://github.com/wagnnerms97/DataMetProcess",
          target="_blank"
        ),
        tags$h3("Acknowledgements"),
        tags$section(
          class="logos_ack",
          tags$a(
            href = "https://www.instagram.com/grupo_gas/",
            imageOutput("gas",height = "70px"),
            target="_blank"
          ),
          tags$a(
            href = "https://www.ufrpe.br/",
            imageOutput("ufrpe",height = "70px"),
            target="_blank"
          ),
          tags$a(
            href = "http://www.uast.ufrpe.br/br",
            imageOutput("uast",height = "70px"),
            target="_blank"
          ),
          tags$a(
            href = "http://www.pgea.ufrpe.br/",
            imageOutput("pgea",height = "70px"),
            target="_blank"
          ),
          tags$a(
            href = "https://www.gov.br/capes/",
            imageOutput("capes",height = "70px"),
            target="_blank"
          ),
          tags$a(
            href = "https://www.gov.br/cnpq/pt-br",
            imageOutput("cnpq",height = "70px"),
            target="_blank"
          ),
          tags$a(
            href = "https://www.facepe.br/",
            imageOutput("facepe",height = "70px"),
            target="_blank"
          )
        )
      )
    )#fluidRow
  )
}
