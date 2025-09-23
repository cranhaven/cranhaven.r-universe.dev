## ---- echo=FALSE, out.width="60%", fig.align="center"-------------------------
knitr::include_graphics("example_form.png")

## -----------------------------------------------------------------------------
myForm <- shinyreforms::ShinyForm$new(
    "myForm",
    submit = "Submit",
    onSuccess = function(self, input, output) {
        yourName <- self$getValue(input, "name_input")

        output$result <- shiny::renderText({
            paste0("Your name is ", yourName, "!")
        })
    },
    onError = function(self, input, output) {
        output$result <- shiny::renderText({
            "Form is invalid!"
        })
    },
    shinyreforms::validatedInput(
        shiny::textInput("name_input", label = "Username"),
        helpText="Username length is between 4 and 12 characters.",
        validators = c(
            shinyreforms::ValidatorMinLength(4),
            shinyreforms::ValidatorMaxLength(12),
            shinyreforms::Validator$new(
                test = function(value) value != "test",
                failMessage = "Username can't be 'test'!"
            )
        )
    ),
    shinyreforms::validatedInput(
        shiny::checkboxInput("checkbox", label = "I accept!"),
        validators = c(
            shinyreforms::ValidatorRequired()
        )
    )
)

## ---- eval=FALSE--------------------------------------------------------------
#    # snip
#      shinyreforms::validatedInput(
#          shiny::textInput("name_input", label = "Username"),
#          helpText="Username length is between 4 and 12 characters.",
#          validators = c(
#              shinyreforms::ValidatorMinLength(4),
#              shinyreforms::ValidatorMaxLength(12),
#              shinyreforms::Validator$new(
#                  test = function(value) value != "test",
#                  failMessage = "Username can't be 'test'!"
#              )
#          )
#      ),
#    # snip

## -----------------------------------------------------------------------------
ui <- shiny::bootstrapPage(
    shinyreforms::shinyReformsPage(  # This adds a dependency on shinyreforms .css
        shiny::fluidPage(
            shiny::tags$h1("Example ShinyForm!"),
            myForm$ui(),  # <- ShinyForm will be included here!
            shiny::tags$h4("Result:"),
            shiny::textOutput("result")
        )
    )
)

## -----------------------------------------------------------------------------
server <- function(input, output, session) {
    myForm$server(input, output)

    # More server logic
}

## ---- eval=FALSE--------------------------------------------------------------
#  shiny::shinyApp(ui = ui, server = server)

