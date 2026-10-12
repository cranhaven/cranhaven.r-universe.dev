# Package "markdownInput"
This directory contains the "markdownInput" package for the R programming language. This package is a [R-Shiny module](https://shiny.rstudio.com/articles/modules.html) providing a Shiny input to write some markdown code and to preview the result. This input has been inspired by the "comment" window of https://github.com/.

## Installation
To install this package, the easiest is to directly install the package from GitHub:

<sub>In your `R` console:</sub>
```R
install.package("devtools")
devtools::install_github("juliendiot42/markdownInput")
```

Once the package is installed, start using it:

<sub>In your `R` console:</sub>
```R
library(markdownInput)
help(package=markdownInput)
```

## Example

Once you have downloaded the package, you can run an example app:

<sub>In your `R` console:</sub>
```R
markdownInput::runExample()
```

## Usage


### UI

In the UI part of your app you should call the `markdownInput` function.

For example:
```R
ui <- fluidPage(titlePanel("Markdown input"),
                 sidebarLayout(
                   # inputs:
                   sidebarPanel(
                     markdownInput(
                       "mdInputID",
                       label = "Write your text",
                       value = "Write some _markdown_ **here:**"
                       )
                   ),

                   # outputs:
                   mainPanel(
                   h3("Raw value of the input:"),
                   verbatimTextOutput("rawResult"))
                 ))
```

### Server

You can access to the input's value in the server side by calling the module:

```R
server <- function(input, output, session) {
    # myText is a reactive variable containing the raw markdown text
    myText <- callModule(moduleMarkdownInput, "mdInputID")

    # show "myText"
    output$rawResult <- renderPrint({
        print(myText())
    })
}
```

## Issues
When encountering a problem with the package, you can report issues on GitHub directly [here](https://github.com/juliendiot42/markdownInput/issues).



## Contributing
You can contribute in various ways:

* report an issue (online, see the above section);

* suggest improvements (in the same way as issues);

* propose a [pull request](https://help.github.com/articles/about-pull-requests/) (after creating a new branch).


## Citation
I invest some time and effort to create this package. Please cite it when using it:

<sub>In your `R` console:</sub>
```R
citation("markdownInput")
```
See also `citation()` for citing R itself.

## References

* [shinyAce](https://CRAN.R-project.org/package=shinyAce): Vincent Nijs, Forest Fang, Trestle Technology, LLC and Jeff Allen (2019). shinyAce: Ace Editor Bindings for Shiny.

* [shiny](https://CRAN.R-project.org/package=shiny): Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2018). shiny: Web Application Framework for R.

* [markdown](https://CRAN.R-project.org/package=markdown): JJ Allaire, Jeffrey Horner, Yihui Xie, Vicent Marti and Natacha Porte (2018). markdown: 'Markdown' Rendering for R.
