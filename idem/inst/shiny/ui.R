##    -----------------------------------------------------------------------
##    Copyright (C) 2015  Daniel O. Scharfstein and Chenguang Wang
##    This program is free software: you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation, either version 3 of the License, or
##    (at your option) any later version.

##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.

##    You should have received a copy of the GNU General Public License
##    along with this program.  If not, see <http://www.gnu.org/licenses/>.
##    -----------------------------------------------------------------------

##
##  date    : 09/15/2014
##  contact : CWANG68@JHMI.EDU
##

library(shinythemes);

shinyUI(
    fluidPage(theme = shinytheme("cosmo"),
              includeScript('www/tools.js'),
              ##css
              tags$head(tags$title("Composite"),
                        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                        tags$link(rel = "stylesheet", type = "text/css", href = "//fonts.googleapis.com/css?family=Oswald"),
                        tags$link(rel = "stylesheet", type = "text/css", href = "//fonts.googleapis.com/css?family=Lora")
                        ),

              ##title box
              withTags({
                  div(class="cheader",
                      "Composite Endpoint Death Truncated Data Analysis",
                      tags$button(
                               id = 'close',
                               type = "button",
                               class = "btn action-button",
                               onclick = "setTimeout(function(){window.close();},500);",  # close browser
                               "Exit",
                               style="float: right;
                                      background-image: url(texturebg2.jpg);"
                           )
                      )
              }),

              ##main page
              uiOutput("mainpage"),

              ##foot
              withTags({
                  div(class="cfooter",
                      "A",
                      a("Missing Data Matters", href="http://www.missingdatamatters.org/"),
                      "Project")
              })
              )
)
