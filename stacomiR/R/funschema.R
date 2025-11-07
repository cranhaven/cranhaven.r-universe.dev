#' Creates a list of available schemas in the db
#' 
#' @return A table with of data providers with org_code, the user of each schema, and  org_description the description of the schema
#' @export
fun_schema <- function(){
	req = new("RequeteDB")
	# this query will get characteristics from lot_pere when null
	req@sql = "SELECT * FROM ref.ts_organisme_org too WHERE NOT  org_code IN ('nat','invite') ORDER BY org_code"
	schema_table <- query(req)@query	
	return(schema_table)
}
	


