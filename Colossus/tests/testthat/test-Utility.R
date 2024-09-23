## ------------------------------------- ##
## Verify the system check code
## ------------------------------------- ##
test_that( "System version", {
    expect_no_error(System_Version())
})

## ------------------------------------- ##
## Default control
## ------------------------------------- ##

test_that( "Default control no error", {
    control_def<- list()
    expect_no_error(Def_Control(control_def))
})
test_that( "Default control error", {
    control_def<- list( "ncores"=detectCores()+100, "verbose"=T)
    expect_error(Def_Control(control_def))
})
test_that( "Default control no error", {
    control_def <- list( "temp"=FALSE)
    a_n <- c(1,2,3)
    expect_no_error(Def_Control_Guess(control_def,a_n))
})

## ------------------------------------- ##
## Truncation
## ------------------------------------- ##

test_that( "No truncation columns", {
    df <- data.table( "time0"=c(0,1,2,3,4,5,6), "time1"=c(1,2,3,4,5,6,7), "dummy"=c(0,0,1,1,0,1,0))
    expect_equal(Check_Trunc(df,c( "time0", "time1" ))$ce, c( "time0", "time1" ))
})
test_that( "Right truncation columns", {
    df <- data.table( "time0"=c(0,1,2,3,4,5,6), "time1"=c(1,2,3,4,5,6,7), "dummy"=c(0,0,1,1,0,1,0))
    expect_equal(Check_Trunc(df,c( "%trunc%", "time1" ))$ce, c( "right_trunc", "time1" ))
})
test_that( "Left truncation columns", {
    df <- data.table( "time0"=c(0,1,2,3,4,5,6), "time1"=c(1,2,3,4,5,6,7), "dummy"=c(0,0,1,1,0,1,0))
    expect_equal(Check_Trunc(df,c( "time0", "%trunc%" ))$ce, c( "time0", "left_trunc" ))
})
test_that( "Truncation no column error", {
    df <- data.table( "time0"=c(0,1,2,3,4,5,6), "time1"=c(1,2,3,4,5,6,7), "dummy"=c(0,0,1,1,0,1,0))
    expect_error(Check_Trunc(df,c()))
})
test_that( "Truncation left column not in df error", {
    df <- data.table( "time0"=c(0,1,2,3,4,5,6), "time1"=c(1,2,3,4,5,6,7), "dummy"=c(0,0,1,1,0,1,0))
    expect_error(Check_Trunc(df,c( "timebad", "%trunc%" )))
})
test_that( "Truncation right column not in df error", {
    df <- data.table( "time0"=c(0,1,2,3,4,5,6), "time1"=c(1,2,3,4,5,6,7), "dummy"=c(0,0,1,1,0,1,0))
    expect_error(Check_Trunc(df,c( "%trunc%", "timebad" )))
})
test_that( "Truncation both sides", {
    df <- data.table( "time0"=c(0,1,2,3,4,5,6), "time1"=c(1,2,3,4,5,6,7), "dummy"=c(0,0,1,1,0,1,0))
    expect_error(Check_Trunc(df,c( "%trunc%", "%trunc%" )))
})

## ------------------------------------- ##
## Duplicate Columns
## ------------------------------------- ##

test_that( "No dupe columns", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    expect_equal(Check_Dupe_Columns(df,c( "a", "b", "c", "d" ),c(0,0,0,0),TRUE), c( "a", "b", "c", "d" ))
})
test_that( "No columns", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    expect_equal(Check_Dupe_Columns(df,c(),c(),TRUE), c())
})
test_that( "One column with varying", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    expect_equal(Check_Dupe_Columns(df,c( "a" ),c(0),TRUE), c( "a" ))
})
test_that( "One column with constant", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    expect_equal(Check_Dupe_Columns(df,c( "c" ),c(0),TRUE), c( "c" ))
})
test_that( "One duplicate column", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d, "e"=a)
    expect_equal(Check_Dupe_Columns(df,c( "a", "b", "c", "d", "e" ),c(0,0,0,0,0),TRUE), c( "a", "b", "c", "d" ))
})
test_that( "One duplicate column, different term", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d, "e"=a)
    expect_equal(Check_Dupe_Columns(df,c( "a", "b", "c", "d", "e" ),c(0,0,0,1,1),TRUE), c( "a", "b", "c", "d", "e" ))
})
test_that( "Multiple duplicate columns", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d, "e"=a, "f"=b)
    expect_equal(Check_Dupe_Columns(df,c( "a", "b", "c", "e", "f" ),c(0,0,0,0,0),TRUE), c( "a", "b", "c" ))
})
test_that( "All duplicate columns, different terms", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=a, "c"=a, "d"=a, "e"=a, "f"=a)
    expect_equal(Check_Dupe_Columns(df,c( "a", "b", "c", "e", "f" ),c(0,1,2,3,4),TRUE), c( "a", "b", "c", "e", "f" ))
})
test_that( "Repeated duplicate columns", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=a, "e"=a, "f"=a)
    expect_equal(Check_Dupe_Columns(df,c( "a", "b", "c", "d", "f" ),c(0,0,0,0,0),TRUE), c( "a", "b", "c" ))
})
test_that( "All but one duplicate column with varying", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=a, "c"=a)
    expect_equal(Check_Dupe_Columns(df,c( "a", "b", "c" ),c(0,0,0),TRUE), c( "a" ))
})
test_that( "All but one duplicate column with constant", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=c, "b"=c, "c"=c)
    expect_equal(Check_Dupe_Columns(df,c( "a", "b", "c" ),c(0,0,0),TRUE), c())
})
test_that( "Duplicate with column not in df error", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=c, "b"=c, "c"=c)
    expect_error(Check_Dupe_Columns(df,c( "a", "b", "c", "e" ),c(0,0,0,0),TRUE))
    expect_error(Check_Dupe_Columns(df,c( "a", "e", "c", "c" ),c(0,0,0,0),TRUE))
})

## ------------------------------------- ##
## LRT
## ------------------------------------- ##

test_that( "Improve Ratio test", {
    a <- list( "LogLik"=-400)
    b <- list( "LogLik"=-350)
    expect_equal(Likelihood_Ratio_Test(b,a), 100)
})
test_that( "Worse Ratio test", {
    a <- list( "LogLik"=-300)
    b <- list( "LogLik"=-350)
    expect_equal(Likelihood_Ratio_Test(b,a), -100)
})
test_that( "Same Ratio test", {
    a <- list( "LogLik"=-300)
    b <- list( "LogLik"=-300)
    expect_equal(Likelihood_Ratio_Test(a,b), 0)
})
test_that( "No Data Ratio test", {
    a <- list( "baditem"=-300)
    b <- list( "LogLik"=-300)
    expect_error(Likelihood_Ratio_Test(a,b))
})

## ------------------------------------- ##
## Interaction Terms
## ------------------------------------- ##

test_that( "Iteract no dupes", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    df <- data.table( "a"=c, "b"=c, "c"=c)
    interactions <- c( "a?+?b", "a?*?b" )
    new_names <- c( "", "" )
    expect_equal(interact_them(df,interactions,new_names,FALSE)$cols, c( "a+b", "a*b" ))
})
test_that( "Iteract no dupes with rename", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    df <- data.table( "a"=c, "b"=c, "c"=c)
    interactions <- c( "a?+?b", "a?*?b" )
    new_names <- c( "", "formtemp" )
    expect_equal(interact_them(df,interactions,new_names,FALSE)$cols, c( "a+b", "formtemp" ))
})
test_that( "Iteract with direct dupes", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    df <- data.table( "a"=c, "b"=c, "c"=c)
    interactions <- c( "a?+?b", "a?*?b", "a?+?b", "a?+?a" )
    new_names <- c( "", "", "", "" )
    expect_equal(interact_them(df,interactions,new_names,TRUE)$cols, c( "a*b", "a+b", "a+a" ))
})
test_that( "Iteract with reverse dupes", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    df <- data.table( "a"=c, "b"=c, "c"=c)
    interactions <- c( "a?+?b", "a?*?b", "b?+?a", "a?+?a" )
    new_names <- c( "", "", "", "" )
    expect_equal(interact_them(df,interactions,new_names,TRUE)$cols, c( "a*b", "b+a", "a+a" ))
})
test_that( "Iteract formula long error", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    df <- data.table( "a"=c, "b"=c, "c"=c)
    interactions <- c( "a?+?b?+c", "a?*?b" )
    new_names <- c( "", "" )
    expect_error(interact_them(df,interactions,new_names,TRUE))
})
test_that( "Iteract formula operation error", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    df <- data.table( "a"=c, "b"=c, "c"=c)
    interactions <- c( "a?++?b", "a?*?b" )
    new_names <- c( "", "" )
    expect_error(interact_them(df,interactions,new_names,TRUE))
})

#######################################
## Modelform Fixes
#######################################

# test_that( "Check no error", {
#     control <- list( 'verbose'=4, "ncores"=2, 'lr' = 0.75, 'maxiter' = 5, 'ties'='breslow', 'double_step'=1)
#     control <- Def_Control(control)
#     model_control <- list( "single"=TRUE)
#     model_control <- Def_model_control(model_control)
#     term_n <- c(0,1,1)
#     modelform <- 'a'
#     expect_no_error(Def_modelform_fix(control,model_control,modelform,term_n))
# })

test_that( "Modelform Fixes Additives", {
    control <- list( 'verbose'=4, "ncores"=2, 'lr' = 0.75, 'maxiter' = 5, 'ties'='breslow', 'double_step'=1)
    control <- Def_Control(control)
    model_control <- list( "single"=TRUE)
    model_control <- Def_model_control(model_control)
    term_n <- c(0,1,1)
    modelform <- 'a'
    expect_equal(Def_modelform_fix(control,model_control,modelform,term_n)$modelform, 'A' )
    modelform <- 'pa'
    expect_equal(Def_modelform_fix(control,model_control,modelform,term_n)$modelform, 'PA' )
    modelform <- 'pae'
    expect_equal(Def_modelform_fix(control,model_control,modelform,term_n)$modelform, 'PAE' )
})
test_that( "Modelform Fixes Additives", {
    control <- list( 'verbose'=4, "ncores"=2, 'lr' = 0.75, 'maxiter' = 5, 'ties'='breslow', 'double_step'=1)
    control <- Def_Control(control)
    model_control <- list( "single"=TRUE)
    model_control <- Def_model_control(model_control)
    term_n <- c(0,1,1)
    modelform <- 'm'
    expect_equal(Def_modelform_fix(control,model_control,modelform,term_n)$modelform, 'M' )
    modelform <- 'me'
    expect_equal(Def_modelform_fix(control,model_control,modelform,term_n)$modelform, 'M' )
})
test_that( "Modelform Fixes gmix", {
    control <- list( 'verbose'=4, "ncores"=2, 'lr' = 0.75, 'maxiter' = 5, 'ties'='breslow', 'double_step'=1, 'verbose'=TRUE)
    control <- Def_Control(control)
    model_control <- list( "single"=TRUE)
    model_control <- Def_model_control(model_control)
    term_n <- c(0,1,1)
    modelform <- 'gmix-r'
    expect_equal(Def_modelform_fix(control,model_control,modelform,term_n)$modelform, 'GMIX' )
    modelform <- 'gmix-e'
    expect_equal(Def_modelform_fix(control,model_control,modelform,term_n)$modelform, 'GMIX' )
    model_control$gmix_term <- c(1,1)
    modelform <- 'gmix'
    expect_equal(Def_modelform_fix(control,model_control,modelform,term_n)$modelform, 'GMIX' )
})

test_that( "gmix error", {
    control <- list( 'verbose'=4, "ncores"=2, 'lr' = 0.75, 'maxiter' = 5, 'ties'='breslow', 'double_step'=1)
    control <- Def_Control(control)
    model_control <- list( "single"=TRUE)
    model_control <- Def_model_control(model_control)
    term_n <- c(0,1,1)
    modelform <- 'gmix'
    expect_error(Def_modelform_fix(control,model_control,modelform,term_n))
})

test_that( "unused model formula error", {
    control <- list( 'verbose'=4, "ncores"=2, 'lr' = 0.75, 'maxiter' = 5, 'ties'='breslow', 'double_step'=1)
    control <- Def_Control(control)
    model_control <- list( "single"=TRUE)
    model_control <- Def_model_control(model_control)
    term_n <- c(0,1,1)
    modelform <- 'failing_choice'
    expect_error(Def_modelform_fix(control,model_control,modelform,term_n))
    modelform <- 'ma'
    expect_error(Def_modelform_fix(control,model_control,modelform,term_n))
    modelform <- 'ea'
    expect_error(Def_modelform_fix(control,model_control,modelform,term_n))
})

######################################
# FACTORING
######################################

test_that( "Factorize factor", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    df <- data.table( "a"=a, "b"=b, "c"=c)
    col_list <- c( "c" )
    expect_equal(factorize(df,col_list,TRUE)$cols, c( "c_1" ))
})
test_that( "Factorize discrete", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    df <- data.table( "a"=a, "b"=b, "c"=c)
    col_list <- c( "a" )
    expect_equal(factorize(df,col_list,TRUE)$cols, c( "a_0", "a_1", "a_2", "a_3", "a_4", "a_5", "a_6" ))
})
test_that( "Factorize missing", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    df <- data.table( "a"=a, "b"=b, "c"=c)
    col_list <- c( "d" )
    expect_error(factorize(df,col_list,TRUE))
})


test_that( "Factorize parallel factor", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    df <- data.table( "a"=a, "b"=b, "c"=c)
    col_list <- c( "c" )
    expect_equal(factorize_par(df,col_list,TRUE,2)$cols, c( "c_1" ))
})
test_that( "Factorize parallel discrete", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    df <- data.table( "a"=a, "b"=b, "c"=c)
    col_list <- c( "a" )
    expect_equal(factorize_par(df,col_list,TRUE,2)$cols, c( "a_0", "a_1", "a_2", "a_3", "a_4", "a_5", "a_6" ))
})
test_that( "Factorize parallel missing", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,0,0,0,0,0,0)
    df <- data.table( "a"=a, "b"=b, "c"=c)
    col_list <- c( "d" )
    expect_error(factorize_par(df,col_list,TRUE,2))
})



######################################
# Time Dependent Cov gens
######################################

test_that( "Gen_time_dep time error", {
    a <- c(20,20,5,10,15)
    b <- c(1,2,1,1,2)
    c <- c(0,0,1,1,1)
    df <- data.table( "a"=a, "b"=b, "c"=c)
    
    time1 <- "%trunc%"
    time2 <- "a_bad"
    event <- "c"
    control <- list( 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    grt_f <- function(df,time_col){
        return ((df[, "b"] * df[,get(time_col)])[[1]])
    }
    func_form <- c( "lin" )
    
    
    expect_error(gen_time_dep(df,time1,time2,event,TRUE,0.01,c( "grt" ),c(),c(grt_f),paste( "test", "_new.csv",sep="" ), func_form,2))
})
test_that( "Gen_time_dep event error", {
    a <- c(20,20,5,10,15)
    b <- c(1,2,1,1,2)
    c <- c(0,0,1,1,1)
    df <- data.table( "a"=a, "b"=b, "c"=c)
    
    time1 <- "%trunc%"
    time2 <- "a"
    event <- "c_bad"
    control <- list( 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    grt_f <- function(df,time_col){
        return ((df[, "b"] * df[,get(time_col)])[[1]])
    }
    func_form <- c( "lin" )
    
    
    expect_error(gen_time_dep(df,time1,time2,event,TRUE,0.01,c( "grt" ),c(),c(grt_f),paste( "test", "_new.csv",sep="" ), func_form,2))
})
test_that( "Gen_time_dep function error", {
    a <- c(20,20,5,10,15)
    b <- c(1,2,1,1,2)
    c <- c(0,0,1,1,1)
    df <- data.table( "a"=a, "b"=b, "c"=c)
    
    time1 <- "%trunc%"
    time2 <- "a"
    event <- "c_bad"
    control <- list( 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    grt_f <- function(df,time_col){
        stop()
        return ((df[, "b"] * df[,get(time_col)])[[1]])
    }
    func_form <- c( "lin" )
    
    
    expect_error(gen_time_dep(df,time1,time2,event,TRUE,0.01,c( "grt" ),c(),c(grt_f),paste( "test", "_new.csv",sep="" ), func_form,2))
})
test_that( "Gen_time_dep functional form error", {
    a <- c(20,20,5,10,15)
    b <- c(1,2,1,1,2)
    c <- c(0,0,1,1,1)
    df <- data.table( "a"=a, "b"=b, "c"=c)
    
    time1 <- "%trunc%"
    time2 <- "a"
    event <- "c"
    control <- list( 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    grt_f <- function(df,time_col){
        return ((df[, "b"] * df[,get(time_col)])[[1]])
    }
    func_form <- c( "badbad" )
    
    
    expect_error(gen_time_dep(df,time1,time2,event,TRUE,0.01,c( "grt" ),c(),c(grt_f),paste(tempfile(), "test", "_new.csv",sep="" ), func_form,2))
})

test_that( "Gen_time_dep no error lin cox", {
    a <- c(20,20,5,10,15)
    b <- c(1,2,1,1,2)
    c <- c(0,0,1,1,1)
    df <- data.table( "a"=a, "b"=b, "c"=c)
    
    time1 <- "%trunc%"
    time2 <- "a"
    event <- "c"
    control <- list( 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    grt_f <- function(df,time_col){
        return ((df[, "b"] * df[,get(time_col)])[[1]])
    }
    func_form <- c( "lin" )
    
    
    expect_no_error(gen_time_dep(df,time1,time2,event,TRUE,0.01,c( "grt" ),c(),c(grt_f),paste(tempfile(), "test", "_new.csv",sep="" ), func_form,2))
})
test_that( "Gen_time_dep no error step cox", {
    a <- c(20,20,5,10,15)
    b <- c(1,2,1,1,2)
    c <- c(0,0,1,1,1)
    df <- data.table( "a"=a, "b"=b, "c"=c)
    
    time1 <- "%trunc%"
    time2 <- "a"
    event <- "c"
    control <- list( 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    grt_f <- function(df,time_col){
        return ((df[, "b"] * df[,get(time_col)])[[1]])
    }
    func_form <- c( "step?0g?7l?12a?18b?" )
    
    
    expect_no_error(gen_time_dep(df,time1,time2,event,TRUE,0.01,c( "grt" ),c(),c(grt_f),paste(tempfile(), "test", "_new.csv",sep="" ), func_form,2))
})

test_that( "Gen_time_dep no error lin not cox", {
    a <- c(20,20,5,10,15)
    b <- c(1,2,1,1,2)
    c <- c(0,0,1,1,1)
    df <- data.table( "a"=a, "b"=b, "c"=c)
    
    time1 <- "%trunc%"
    time2 <- "a"
    event <- "c"
    control <- list( 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    grt_f <- function(df,time_col){
        return ((df[, "b"] * df[,get(time_col)])[[1]])
    }
    func_form <- c( "lin" )
    
    
    expect_no_error(gen_time_dep(df,time1,time2,event,FALSE,0.01,c( "grt" ),c(),c(grt_f),paste(tempfile(), "test", "_new.csv",sep="" ), func_form,2))
})
test_that( "Gen_time_dep no error step not cox", {
    a <- c(20,20,5,10,15)
    b <- c(1,2,1,1,2)
    c <- c(0,0,1,1,1)
    df <- data.table( "a"=a, "b"=b, "c"=c)
    
    time1 <- "%trunc%"
    time2 <- "a"
    event <- "c"
    control <- list( 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    grt_f <- function(df,time_col){
        return ((df[, "b"] * df[,get(time_col)])[[1]])
    }
    func_form <- c( "step?0g?7l?10u?12a?18b?" )
    
    
    expect_no_error(gen_time_dep(df,time1,time2,event,FALSE,0.01,c( "grt" ),c(),c(grt_f),paste(tempfile(), "test", "_new.csv",sep="" ), func_form,2))
})

test_that( "linked quad negative slope error", {
    tforms <- list( "first"="quad" )
    paras  <- list( "first"=c(-0.1,10))
    expect_error(Linked_Dose_Formula(tforms,paras,TRUE))
})
test_that( "linked quad string slope error", {
    tforms <- list( "first"="quad" )
    paras  <- list( "first"=c( "a",10))
    expect_error(Linked_Dose_Formula(tforms,paras,TRUE))
})
test_that( "linked quad string threshold error", {
    tforms <- list( "first"="quad" )
    paras  <- list( "first"=c(0.1, "a" ))
    expect_error(Linked_Dose_Formula(tforms,paras,TRUE))
})
test_that( "linked quad no error", {
    tforms <- list( "first"="quad" )
    paras  <- list( "first"=c(0.1,10))
    expect_no_error(Linked_Dose_Formula(tforms,paras,TRUE))
})
test_that( "linked exp negative slope error", {
    tforms <- list( "first"="exp" )
    paras  <- list( "first"=c(-0.1,10,5))
    expect_error(Linked_Dose_Formula(tforms,paras,TRUE))
})
test_that( "linked exp string slope error", {
    tforms <- list( "first"="exp" )
    paras  <- list( "first"=c( "a",10,5))
    expect_error(Linked_Dose_Formula(tforms,paras,TRUE))
})
test_that( "linked exp string threshold error", {
    tforms <- list( "first"="exp" )
    paras  <- list( "first"=c(0.1, "a",5))
    expect_error(Linked_Dose_Formula(tforms,paras,TRUE))
})
test_that( "linked exp string exp slope error", {
    tforms <- list( "first"="exp" )
    paras  <- list( "first"=c(0.1,10, "a" ))
    expect_error(Linked_Dose_Formula(tforms,paras,TRUE))
})
test_that( "linked exp no error", {
    tforms <- list( "first"="exp" )
    paras  <- list( "first"=c(0.1,10,5))
    expect_no_error(Linked_Dose_Formula(tforms,paras,TRUE))
})

test_that( "linked exp parameter low goal error", {
    y <- 10
    a0 <- 1
    a_goal <- 5
    expect_error(Linked_Lin_Exp_Para(y,a0,a_goal,TRUE))
})
test_that( "linked exp parameter negative slope error", {
    y <- 10
    a0 <- -0.1
    a_goal <- 5
    expect_error(Linked_Lin_Exp_Para(y,a0,a_goal,TRUE))
})
test_that( "linked exp parameter no error", {
    y <- 10
    a0 <- 0.1
    a_goal <- 5
    expect_no_error(Linked_Lin_Exp_Para(y,a0,a_goal,TRUE))
})

test_that( "Missing Value missing column error", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    expect_error(Replace_Missing(df,c( "a", "e" ),0.0,T))
})
test_that( "Missing Value NA replacement error", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    expect_error(Replace_Missing(df,c( "a", "b", "c", "d" ),NA,T))
})
test_that( "Missing Value no error", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    expect_no_error(Replace_Missing(df,c( "a", "b", "c", "d" ),0.0,T))
})
test_that( "Missing Value checked replaced 0", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(NA,0,0,1,0,0,1)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    
    df0 <- Replace_Missing(df,c( "a", "b" ),0.0,T)
    expect_equal(c(sum(df0$a),sum(df0$b)),c(sum(df$a),2))
})
test_that( "Missing Value checked replaced 1", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(NA,0,0,1,0,0,1)
    c <- c(1,1,1,1,1,1,1)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    
    df0 <- Replace_Missing(df,c( "a", "b" ),1.0,T)
    expect_equal(c(sum(df0$a),sum(df0$b)),c(sum(df$a),3))
})

# test_that( "Check Date Shift", {
#     m0 <- c(1,1,2,2)
#     m1 <- c(2,2,3,3)
#     d0 <- c(1,2,3,4)
#     d1 <- c(6,7,8,9)
#     y0 <- c(1990,1991,1997,1998)
#     y1 <- c(2001,2003,2005,2006)
#     df <- data.table( "m0"=m0, "m1"=m1, "d0"=d0, "d1"=d1, "y0"=y0, "y1"=y1)
#     expect_no_error(Date_Shift(df,c( "m0", "d0", "y0" ),c( "m1", "d1", "y1" ), "date_since" ))
# })
test_that( "Check Date Shift, exact value", {
    m0 <- c(1,1,2,2)
    m1 <- c(2,2,3,3)
    d0 <- c(1,2,3,4)
    d1 <- c(6,7,8,9)
    y0 <- c(1990,1991,1997,1998)
    y1 <- c(2001,2003,2005,2006)
    df <- data.table( "m0"=m0, "m1"=m1, "d0"=d0, "d1"=d1, "y0"=y0, "y1"=y1)
    e <- Date_Shift(df,c( "m0", "d0", "y0" ),c( "m1", "d1", "y1" ), "date_since" )
    expect_equal(as.numeric(e$date_since), c(4054,4419,2955,2955))
})

# test_that( "Check Date Since", {
#     m0 <- c(1,1,2,2)
#     m1 <- c(2,2,3,3)
#     d0 <- c(1,2,3,4)
#     d1 <- c(6,7,8,9)
#     y0 <- c(1990,1991,1997,1998)
#     y1 <- c(2001,2003,2005,2006)
#     df <- data.table( "m0"=m0, "m1"=m1, "d0"=d0, "d1"=d1, "y0"=y0, "y1"=y1)
#     tref <- strptime( "3-22-1997", format = "%m-%d-%Y",tz = 'UTC' )
#     expect_no_error(Time_Since(df,c( "m1", "d1", "y1" ),tref, "date_since" ))
# })
test_that( "Check Date Since", {
    m0 <- c(1,1,2,2)
    m1 <- c(2,2,3,3)
    d0 <- c(1,2,3,4)
    d1 <- c(6,7,8,9)
    y0 <- c(1990,1991,1997,1998)
    y1 <- c(2001,2003,2005,2006)
    df <- data.table( "m0"=m0, "m1"=m1, "d0"=d0, "d1"=d1, "y0"=y0, "y1"=y1)
    tref <- "3-22-1997"
    expect_error(Time_Since(df,c( "m1", "d1", "y1" ),tref, "date_since" ))
})
test_that( "Check Date Since, exact value", {
    m0 <- c(1,1,2,2)
    m1 <- c(2,2,3,3)
    d0 <- c(1,2,3,4)
    d1 <- c(6,7,8,9)
    y0 <- c(1990,1991,1997,1998)
    y1 <- c(2001,2003,2005,2006)
    df <- data.table( "m0"=m0, "m1"=m1, "d0"=d0, "d1"=d1, "y0"=y0, "y1"=y1)
    tref <- strptime( "3-22-1997", format = "%m-%d-%Y",tz = 'UTC' )
    e <- Time_Since(df,c( "m1", "d1", "y1" ),tref, "date_since" )
    expect_equal(as.numeric(e$date_since), c(1417,2148,2908,3274))
})


#####################################
# Formula order
#####################################
test_that( "tform order, tform order", {
    term_n <- c(0,0,0,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,0,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    expect_equal(a_n, c(1,3,5,4,2))
})
test_that( "tform order, tform and term_n order", {
    term_n <- c(0,1,2,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,0,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    expect_equal(a_n, c(1,5,4,2,3))
})
test_that( "tform order, combined", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    expect_equal(a_n, c(1,5,4,3,2))
})
test_that( "tform order, tform order, list single", {
    term_n <- c(0,0,0,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,0,0)
    a_n <- list(c(1,2,3,4,5))
    names <- c( "a", "a", "a", "a", "a" )
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    expect_equal(a_n, c(1,3,5,4,2))
})
test_that( "tform order, tform and term_n order, list single", {
    term_n <- c(0,1,2,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,0,0)
    a_n <- list(c(1,2,3,4,5))
    names <- c( "a", "a", "a", "a", "a" )
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    expect_equal(a_n, c(1,5,4,2,3))
})
test_that( "tform order, combined, list single", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- list(c(1,2,3,4,5))
    names <- c( "a", "a", "a", "a", "a" )
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    expect_equal(a_n, c(1,5,4,3,2))
})
test_that( "formula order, too few parameters", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4)
    names <- c( "a", "a", "a", "a", "a" )
    expect_no_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, no free", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(1,1,1,1,1)
    a_n <- c(1,2,3,4,5,6)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, too many parameters", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5,6)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, too few term numbers", {
    term_n <- c(0,1,1,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, too many term numbers", {
    term_n <- c(0,1,1,0,0,1)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, too few term types", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, too many term types", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope", "lin" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})

test_that( "formula order, missing lin_int", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_slope", "lin" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, missing step_int", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "step_slope", "lin" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, missing loglin_top", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "loglin_slope", "lin" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, missing lin_quad_slope", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_quad_slope", "lin" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, missing lin_exp_int", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_exp_slope", "lin_exp_exp_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, missing lin_exp_exp_slope", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_exp_slope", "lin_exp_int" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})

test_that( "formula order, missing step_slope", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "step_int", "lin" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, missing lin_slope", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, missing lin_quad_slope", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_quad_int", "lin" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, missing lin_exp_slope", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_exp_int", "lin_exp_exp_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, missing lin_exp_exp_int", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_exp_slope", "lin_exp_exp_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- c(1,2,3,4,5)
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})

#####################################
# Formula order, List a_n
#####################################
test_that( "tform order, tform order, list double", {
    term_n <- c(0,0,0,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,0,0)
    a_n <- list(c(1,2,3,4,5),c(2,3,4,5,6))
    names <- c( "a", "a", "a", "a", "a" )
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    expect_equal(a_n, list(c(1,3,5,4,2),c(2,4,6,5,3)))
})
test_that( "tform order, tform and term_n order, list double", {
    term_n <- c(0,1,2,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,0,0)
    a_n <- list(c(1,2,3,4,5),c(2,3,4,5,6))
    names <- c( "a", "a", "a", "a", "a" )
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    expect_equal(a_n, list(c(1,5,4,2,3),c(2,6,5,3,4)))
})
test_that( "tform order, combined, list double", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- list(c(1,2,3,4,5),c(2,3,4,5,6))
    names <- c( "a", "a", "a", "a", "a" )
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    expect_equal(a_n, list(c(1,5,4,3,2),c(2,6,5,4,3)))
})
test_that( "formula order, different parameter lengths, list double", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- list(c(1,2,3,4,5),c(2,3,4,5))
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, too few parameters, list double", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- list(c(1,2,3,4),c(2,3,4,5))
    names <- c( "a", "a", "a", "a", "a" )
    expect_no_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, too many parameters, list double", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- list(c(1,2,3,4,5,6),c(2,3,4,5,6,7))
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, too few term numbers, list double", {
    term_n <- c(0,1,1,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- list(c(1,2,3,4,5),c(2,3,4,5,6))
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, too many term numbers, list double", {
    term_n <- c(0,1,1,0,0,1)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- list(c(1,2,3,4,5),c(2,3,4,5,6))
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, too few term types, list double", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- list(c(1,2,3,4,5),c(2,3,4,5,6))
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "formula order, too many term types, list double", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope", "lin" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- list(c(1,2,3,4,5),c(2,3,4,5,6))
    names <- c( "a", "a", "a", "a", "a" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
#####################################
# Formula order, Constraints and verbose check
#####################################
test_that( "Checking constraint matrix", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- list(c(1,2,3,4,5))
    names <- c( "a", "a", "a", "a", "a" )
    cons_mat <- matrix(c(1:12),nrow=3,byrow=T)
    cons_vec <- c(1,0,-1)

    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,0, cons_mat, cons_vec)
    cons_mat <- val$cons_mat

    expect_equal(cons_mat[1,],c(1, 4, 3,2))
    expect_equal(cons_mat[,3],c(3,7,11))
})
test_that( "Checking verbose", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- list(c(1,2,3,4,5))
    names <- c( "a", "a", "a", "a", "a" )
    cons_mat <- matrix(c(1:12),nrow=3,byrow=T)
    cons_vec <- c(1,0,-1)

    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,0, cons_mat, cons_vec,verbose=-1))
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,0, cons_mat, cons_vec,verbose=5))
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,0, cons_mat, cons_vec,verbose="bad" ))
    
    expect_no_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,0, cons_mat, cons_vec,verbose=T))
    expect_no_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,0, cons_mat, cons_vec,verbose=F))
    expect_no_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,0, cons_mat, cons_vec,verbose=4))
    expect_no_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,0, cons_mat, cons_vec,verbose=3))
    expect_no_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,0, cons_mat, cons_vec,verbose=2))
    expect_no_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,0, cons_mat, cons_vec,verbose=1))
    expect_no_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,0, cons_mat, cons_vec,verbose=0))
})
test_that( "Checking keep_constant limits", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'quad_slope', 'lin', "lin_int", "lin_slope" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- list(c(1,2,3,4,5),c(2,3,4,5,6))
    names <- c( "a", "a", "a", "a", "a" )
    keep_constant <- c(0,0,0,-1,0)
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
    keep_constant <- c(0,0,0,10,0)
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
    keep_constant <- c(0,0,0,1,0.5)
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "Checking term_n limits", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'loglin', 'loglin', "loglin", "loglin" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- list(c(1,2,3,4,5),c(2,3,4,5,6))
    names <- c( "a", "a", "a", "a", "a" )
    term_n <- c(0,0,0,-1,0)
    expect_no_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
    term_n <- c(0,0,0,1,0.5)
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
    term_n <- c(0,1,1,1,3)
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})
test_that( "Checking tform values", {
    term_n <- c(0,1,1,0,0)
    tform <- c( "loglin", 'loglin', 'loglin', "loglin", "loglin" )
    keep_constant <- c(0,0,0,1,0)
    a_n <- list(c(1,2,3,4,5),c(2,3,4,5,6))
    names <- c( "a", "a", "a", "a", "a" )
    term_n <- c(0,0,0,-1,0)
    tform <- c( "loglin", 'fake', 'loglin', "loglin", "loglin" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
    tform <- c( "loglin", 'fake', 'bad', "loglin", "loglin" )
    expect_error(Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T))
})

# ------------------------------------- ##
# gather guesses
# ------------------------------------- ##
test_that( "Gather Guesses no error", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,1,0,0,0,1,0)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    time1 <- "a"
    time2 <- "b"
    event <- "c"
    names <- c( "d", "d", "d", "d" )
    term_n <- c(0,0,0,0)
    tform <- c( "loglin", 'lin_exp_int', 'lin_exp_slope', 'lin_exp_exp_slope' )
    keep_constant <- c(0,0,0,0)
    a_n <- c(-0.1,6,-0.1,0.1)
    a_n_default <- a_n
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    guesses_control <- list()
    model_control <- list()
    
    all_names <- unique(names(df))
    
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    
    control <- Def_Control(control)
    guesses_control <- Def_Control_Guess(guesses_control, a_n)
    model_control <- Def_model_control(model_control)
    
    expect_no_error(Gather_Guesses_CPP(df, dfc, names, term_n, tform, keep_constant, a_n, x_all, a_n_default, modelform, fir, control, guesses_control))
    guesses_control$rmin <- c(-0.1,-1,-0.1,0)
    guesses_control$rmax <- c(0.1, 1, 0.1, 0.1)
    expect_no_error(Gather_Guesses_CPP(df, dfc, names, term_n, tform, keep_constant, a_n, x_all, a_n_default, modelform, fir, control, guesses_control))
})
test_that( "Gather Guesses error, many a_n", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,1,0,0,0,1,0)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    time1 <- "a"
    time2 <- "b"
    event <- "c"
    names <- c( "d", "d", "d", "d" )
    term_n <- c(0,0,0,0)
    tform <- c( "loglin", 'lin_exp_int', 'lin_exp_slope', 'lin_exp_exp_slope' )
    keep_constant <- c(0,0,0,0)
    a_n <- c(-0.1,6,-0.1,0.1)
    a_n_default <- a_n
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    guesses_control <- list()
    model_control <- list()
    a_n <- c(-0.1,6,-0.1,0.1,1,1,1)
    a_n_default <- a_n
    
    all_names <- unique(names(df))
    
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    
    control <- Def_Control(control)
    guesses_control <- Def_Control_Guess(guesses_control, a_n)
    model_control <- Def_model_control(model_control)
    
    expect_error(Gather_Guesses_CPP(df, dfc, names, term_n, tform, keep_constant, a_n, x_all, a_n_default, modelform, fir, control, guesses_control))
})
test_that( "Gather Guesses error, few term numbers", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,1,0,0,0,1,0)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    time1 <- "a"
    time2 <- "b"
    event <- "c"
    names <- c( "d", "d", "d", "d" )
    term_n <- c(0,0,0,0)
    tform <- c( "loglin", 'lin_exp_int', 'lin_exp_slope', 'lin_exp_exp_slope' )
    keep_constant <- c(0,0,0,0)
    a_n <- c(-0.1,6,-0.1,0.1)
    a_n_default <- a_n
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    guesses_control <- list()
    model_control <- list()
    
    all_names <- unique(names(df))
    
    dfc <- match(names,all_names)
    term_n <- c(0)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    
    control <- Def_Control(control)
    guesses_control <- Def_Control_Guess(guesses_control, a_n)
    model_control <- Def_model_control(model_control)
    
    expect_error(Gather_Guesses_CPP(df, dfc, names, term_n, tform, keep_constant, a_n, x_all, a_n_default, modelform, fir, control, guesses_control))
})
test_that( "Gather Guesses error, many term numbers", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,1,0,0,0,1,0)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    time1 <- "a"
    time2 <- "b"
    event <- "c"
    names <- c( "d", "d", "d", "d" )
    term_n <- c(0,0,0,0)
    tform <- c( "loglin", 'lin_exp_int', 'lin_exp_slope', 'lin_exp_exp_slope' )
    keep_constant <- c(0,0,0,0)
    a_n <- c(-0.1,6,-0.1,0.1)
    a_n_default <- a_n
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    guesses_control <- list()
    model_control <- list()
    term_n <- c(0,0,0,0,0,0,0,0)
    
    all_names <- unique(names(df))
    
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    
    control <- Def_Control(control)
    guesses_control <- Def_Control_Guess(guesses_control, a_n)
    model_control <- Def_model_control(model_control)
    
    expect_error(Gather_Guesses_CPP(df, dfc, names, term_n, tform, keep_constant, a_n, x_all, a_n_default, modelform, fir, control, guesses_control))
})
test_that( "Gather Guesses error, few term formula", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,1,0,0,0,1,0)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    time1 <- "a"
    time2 <- "b"
    event <- "c"
    names <- c( "d", "d", "d", "d" )
    term_n <- c(0,0,0,0)
    tform <- c( "loglin", 'lin_exp_int', 'lin_exp_slope', 'lin_exp_exp_slope' )
    keep_constant <- c(0,0,0,0)
    a_n <- c(-0.1,6,-0.1,0.1)
    a_n_default <- a_n
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    guesses_control <- list()
    model_control <- list()
    
    all_names <- unique(names(df))
    tform <- c( "loglin" )
    
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    
    control <- Def_Control(control)
    guesses_control <- Def_Control_Guess(guesses_control, a_n)
    model_control <- Def_model_control(model_control)
    
    expect_error(Gather_Guesses_CPP(df, dfc, names, term_n, tform, keep_constant, a_n, x_all, a_n_default, modelform, fir, control, guesses_control))
})
test_that( "Gather Guesses error, many term formula", {
    a <- c(0,1,2,3,4,5,6)
    b <- c(1,2,3,4,5,6,7)
    c <- c(0,1,0,0,0,1,0)
    d <- c(3,4,5,6,7,8,9)
    df <- data.table( "a"=a, "b"=b, "c"=c, "d"=d)
    time1 <- "a"
    time2 <- "b"
    event <- "c"
    names <- c( "d", "d", "d", "d" )
    term_n <- c(0,0,0,0)
    tform <- c( "loglin", 'lin_exp_int', 'lin_exp_slope', 'lin_exp_exp_slope' )
    keep_constant <- c(0,0,0,0)
    a_n <- c(-0.1,6,-0.1,0.1)
    a_n_default <- a_n
    modelform <- "M"
    fir <- 0
    der_iden <- 0
    
    val <- Correct_Formula_Order(term_n, tform, keep_constant, a_n, names,T)
    term_n <- val$term_n
    tform <- val$tform
    keep_constant <- val$keep_constant
    a_n <- val$a_n
    der_iden <- val$der_iden
    names <- val$names
    
    control <- list( "ncores"=2, 'lr' = 0.75, 'maxiter' = -1, 'halfmax' = 5, 'epsilon' = 1e-9,  'deriv_epsilon' = 1e-9, 'abs_max'=1.0, 'change_all'=TRUE, 'dose_abs_max'=100.0, 'verbose'=0, 'ties'='breslow', 'double_step'=1)
    guesses_control <- list()
    model_control <- list()
    tform <- c( "loglin", 'lin_exp_int', 'lin_exp_slope', 'lin_exp_exp_slope', 'lin' )
    
    all_names <- unique(names(df))
    
    dfc <- match(names,all_names)

    term_tot <- max(term_n)+1
    x_all <- as.matrix(df[,all_names, with = FALSE])
    
    control <- Def_Control(control)
    guesses_control <- Def_Control_Guess(guesses_control, a_n)
    model_control <- Def_model_control(model_control)
    
    expect_error(Gather_Guesses_CPP(df, dfc, names, term_n, tform, keep_constant, a_n, x_all, a_n_default, modelform, fir, control, guesses_control))
})




