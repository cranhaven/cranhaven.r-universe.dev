# Global variables for test

# this file is called before testhat so funcion will be available in all test
# https://testthat.r-lib.org/articles/test-fixtures.html#withr-defer-
# could have used with_envvar and local_envvar but had to set them each time
options(					
    stacomiR.dbname = "bd_contmig_nat_test",
    stacomiR.host ="localhost",
    stacomiR.port = "5432",
    stacomiR.user = "test",
    stacomiR.password = "test"				
)
password <- getOption("stacomiR.password") #if not set will be ""
dbname <- getOption("stacomiR.dbname") #if not set will be ""
user <- getOption("stacomiR.user")
host <- getOption("stacomiR.host")
port <- getOption("stacomiR.port")
if (user == ""){
	user <- "postgres"
}
if (password == ""){
	password <- "postgres"
}
#options(					
#		stacomiR.dbname = dbname,
#		stacomiR.host = host,
#		stacomiR.port = port,
#		stacomiR.user = user,
#		stacomiR.password = password					
#)
env_set_test_stacomi <- function(env = parent.frame()) {


  
	password <- getOption("stacomiR.password") #if not set will be ""
	dbname <- getOption("stacomiR.dbname") #if not set will be ""
	user <- getOption("stacomiR.user")
	host <- getOption("stacomiR.host")
	port <- getOption("stacomiR.port")


	if (user == ""){
		user <- "postgres"
	}
	if (password == ""){
		password <- "postgres"
	}
	# defer untill the parent env has closed to reset values after test	
#	withr::defer(options(					
#			stacomiR.dbname = dbname,
#			stacomiR.host = host,
#			stacomiR.port = port,
#			stacomiR.user = user,
#			stacomiR.password = password					
#	),	env)

assign("user",user,envir=env)
assign("password",password,envir=env)
assign("host", host, envir=env)
assign("schema", "test", envir=env)
# test for foreign keys in the database ? set TRUE to test FALSE to avoid tests
# if set to TRUE be sure that user and password correspond to superuser
assign("test_foreign_keys", TRUE, envir=env)

}
