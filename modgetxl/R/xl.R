# xl variable is module internal reactive that changes with input file
xl<- reactiveValues(sheets=NULL, sheetdata=NULL)
#' @title getxl
#' @description Convert excel sheets to dataframes, display in DT, and return the dataframes
#' @details This is a shiny module that presents a file picker UI to get an Excel file name, and reads the Excel sheets using readxl package and returns the resulting sheet(s) as a vector and data in dataframe(s). 
#' @seealso See xlex for examples
#' 
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @rawNamespace import(shiny, except = c(renderDataTable, dataTableOutput))
#' @import readxl
#' @import DT
#' @importFrom shinydashboard box
# Server function getxl
#' @export
	
getxl<- function(input, output, session) {
	ns<- session$ns

	output$xldtls<- renderUI({
		req(input$filename)
		xl<- callModule(xldisp, 'getxl', input$filename$datapath, input$skiprows)
		xldispUI(ns('getxl'), input$filename$datapath)
		})
	return(xl)
	}

#' @title getxlUI
#' @description UI to get a excel file name
#' @param id is the caller's id
#' @export
getxlUI<- function(id) {
	ns<- NS(id)

	fluidPage(
	fluidRow(
		box(title=NULL, width=12,
			column(9, fileInput(placeholder=".xls or .xlsx file", ns('filename'), width='100%', label="Add New Excel", accept=c('.xls', '.xlsx'))),
			column(3, numericInput(ns('skiprows'), label='Rows to skip', value=0))
			)
		),
	br(),
	fluidRow(
		uiOutput(ns('xldtls'))
		)
	)
	}

#' @title xldisp
#' @description Server function to display excel data as DT
#' @param xlfile is the name of the xl file being got
#' @param skiprows is number of rows to skip in the excel file
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
xldisp<- function(input, output, session, xlfile, skiprows) {
	sheets<- excel_sheets(xlfile)
	sheetdata<- list()
	for(i in 1:length(sheets)) {
		df<- as.data.frame(read_excel(xlfile, sheet=i, skip=skiprows))
		colnames(df)<- gsub(" ", "_", colnames(df))
		sheetdata[[i]]<- df
		local({
			my_i<- i
			sheetid<- paste0('sheet', my_i)
			output[[sheetid]]<- renderDT({
				datatable(sheetdata[[my_i]], class='compact', options=list(dom='tp', autowidth=F, pageLength=10, scrollX=T))
				})
			})
		}
	xl$sheets<- sheets
	xl$sheetdata<- sheetdata
	return(xl)
	}
#' @title xldispUI
#' @description UI to display excel sheets read as individual data tables
#' @param id is caller's id
#' @param xlfile is the name of the xl file being got
xldispUI<- function(id, xlfile) {
	ns<- NS(id)

	sheets<- excel_sheets(xlfile)
	xltabs<- list()
	for(i in 1:length(sheets)) {
		xltabs[[i]]<- tabPanel(title=sheets[i], box(title=NULL, width=12, DTOutput(ns(paste0('sheet', i)))))
		}
	do.call(tabsetPanel,xltabs)
	}
