pers_reactable <- function(table=NULL){
  reactable(
    table,
    resizable = T,
    filterable = TRUE,
    showPageSizeOptions = T,
    defaultColDef = colDef(
      style = list(border = "none"),
      align = "center",
      headerStyle = list(border = "none")
    ),
    wrap = F,
    theme = reactableTheme(
      style = list(fontFamily = "Helvetica, Segoe UI, Arial, sans-serif")
    ),
    paginationType = "jump",
    rowStyle = JS("function(rowInfo) {
    if (rowInfo && rowInfo.selected) {
      return { backgroundColor: '#eee',
    borderBottom: '1px solid #ffa62d',
    borderRadius: '0px'
    }}
  }")
  )
}
