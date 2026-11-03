#' Produce the final summary table for the output of the felogit function
#'
#' @param output the output of the felogit function
#' @param format  can take value "latex" to print the latex table
#'
#' @return a kableExtra or xtable table plotted respectively in the R viewer or terminal
#' @export
#'
#' @examples
#'
#' ### Simulating according to this DGP
#' n=200
#' Xnc_x = rnorm(n,0,1.5)
#' Xnc_y = rnorm(n,0,1.5)
#' epsilon = rnorm(n,0,1)
#'
#' ## true value
#' beta0 =1
#' Y = Xnc_y*beta0 + epsilon
#' out_var = "Y"
#' nc_var = "Xnc"
#'
#' # create the datasets
#' Ldata<- as.data.frame(Y)
#' colnames(Ldata) <- c(out_var)
#' Rdata <- as.data.frame(Xnc_x)
#' colnames(Rdata) <- c(nc_var)
#'
#'
#' ############# Estimation #############
#' output <- regCombin(Ldata,Rdata,out_var,nc_var)
#' mat = summary_regCombin(output)


summary_regCombin <- function(output ,format = NULL){

    nc_var=output$nc_var
    c_var=output$c_var

    formula_y = paste0("Outcome variable (Y): ", as.character(output$out_var), ". \n ")

    dimXnc = length(output$nc_var)
    if(dimXnc==1){
      formula_nc = paste0("Non commun covariates (Xnc): ", as.character(output$nc_var), ". \n ")
    }else{
      formula_nc = paste0("Non commun covariates (Xnc): ", paste0(as.character(output$nc_var),sep="",collapse = ", " ) , ". \n ")
    }
    if(!is.null(output$c_var)){
     formula_c = paste0("Commun covariates (Xc): ", as.character(output$c_var), ". \n ")
    }else{
     formula_c = paste0("No commun covariates (Xc). \n ")
    }


    if(!is.null(output[["dimXc_old"]])){
      dimXc_old = output[["dimXc_old"]]
    }else{
      dimXc_old = 0
    }


    if(dimXc_old ==0){
      formula_0 = " Formula: Y ~ Xnc. \n "
    }else{
      formula_0 =  " Formula: Y ~ Xnc + Xc. \n "
    }



  obsy =  paste0("Number of observations (Y): ", output$n_y , ". \n ")
  obsx =  paste0("Number of observations (Xnc): ", output$n_x , ". \n ")

  general_note = paste0(formula_0,formula_y,  formula_nc,  formula_c,  obsy,  obsx)

  if(!is.null(output$unselect_values)){
    if(length(output$unselect_values)==1){
      Footnote_1 = paste0("The category of Xc (", paste0(as.numeric(output$values_old[output$unselect_values[1],]),sep=",",collapse = "") , ") has been discarded because it has too few observations. \n")
    }else{
      list = NULL
      for(j in 1:length(output$unselect_values)){
        list= c(list,paste0(" (", paste0(as.numeric(output$values_old[output$unselect_values[j],]),sep="",collapse = ",") ,") ") )
      }
      Footnote_1 = paste0("The categories of Xc", paste0(list,sep="",collapse =","), "have not been discarded because they have too few observations. \n")
    }
    general_note = paste0(general_note,Footnote_1 )
  }




  if(!is.null(output$nc_sign)){
    for(k in 1:dimXnc){
      if(output$nc_sign[k]>0){
        Footnote_2 = paste0(" The sign contraint imposes that the coefficient of " , nc_var[k] ," (Xnc) is positive. \n")
        general_note = paste0(general_note,Footnote_2 )
      }else if (output$nc_sign[k]<0){
        Footnote_2 = paste0(" The sign contraint imposes that the coefficient of " , nc_var[k] ," (Xnc)  is negative. \n")
        general_note = paste0(general_note,Footnote_2 )
      }
    }
  }

  if(dimXc_old==0){
    dimXc = length(c_var)
    dimXc_old=dimXc
  }else{
    dimXc = dimXc_old
  }

  if(!is.null(output$c_sign)){
        for(k in 1:dimXc){
          if(output$c_sign[k]>0){
            Footnote_4 = paste0("The sign contraint imposes that the coefficient of " , c_var[k] ," (Xc) is positive. \n")
            general_note = paste0(general_note,Footnote_4 )
          }else if (output$c_sign[k]<0){
            Footnote_4 = paste0("The sign contraint imposes that the coefficient of " , c_var[k] ," (Xc)  is negative. \n")
            general_note = paste0(general_note,Footnote_4 )
          }else{

          }

        }


  }



  if(!is.null(output$c_sign) & dimXnc==2){
      Footnote_3 = "The specified sign contraint on the coefficient of Xc is not yet implemented in the package."
      general_note = paste0(general_note,Footnote_3)

  }

  # if(output$Opt=="boot"){
  #   inf_meth =paste0("CI obtained using the numerical boostrap method (", output$Bsamp   ," replications). \n")
  # }else{
    inf_meth ="CI obtained using the subsampling method. \n \n"
  # }
  general_note = paste0(general_note,inf_meth)

  #info_eps = "The data-driven choices of epsilon can be accessed using the attributes  output$DGMkp"
  #general_note = paste0(general_note,inf_meth)
  # output$DGMkp
  ### create the matrix for results
  # attributes(output)
  # output$

  nb_method = length(output$method)
  i=1
  for(i in 1:nb_method){
    method = output$method[i]
    dimXnc = dim(output[[paste0( method ,"pt")]])[1]
    if(!is.na(output[[paste0( method ,"beta1")]][1])){
      len_values = dim(output[[paste0( method ,"beta1")]])[1]
    }else{
      len_values = 0
    }



    #### According to sign constraints
    brakets <- function(x){return(paste0("[",dig(x[1]),",",dig(x[2]),"]"))}
    dig <- function(x){as.numeric(format(round(as.numeric(x), 3), nsmall = 3))}

    if(is.null(output$nc_sign) & is.null(output$c_sign) ){
      ## no sign constraints
      mat_results = matrix("", dimXnc+ len_values+1,2)

      for( j in 1:dimXnc){
        mat_results[j,1] <- brakets(output[[paste0( method ,"pt")]][j,])
        mat_results[j,2] <-  brakets(output[[paste0( method ,"CI")]][j,])
      }
      if(dimXc_old>0){
        for( j in 1:len_values){
          mat_results[dimXnc+j+1,1] <- brakets(output[[paste0( method ,"beta1_pt")]][j,])
          mat_results[dimXnc+j+1,2] <-  brakets(output[[paste0( method ,"beta1")]][j,])
        }
      }
      # dimXc_old
      # output$dimXc_old
      mat_label = matrix("", dimXnc+ len_values+1,dimXc_old+1)
      mat_label[1:dimXnc,1] <- nc_var
      if(dimXc_old>0){
        mat_label[dimXnc+1,(2:(dimXc_old+1))] <- c_var
        for( j in 1:len_values){
          mat_label[(dimXnc+j+1),(2:(dimXc_old+1))] <- as.numeric(output$values_old[output$select_values[j+1],])
        }
      }


      mat <- cbind( mat_label,mat_results)
      colnames(mat) <- c("Xnc",rep("Xc",dimXc_old),"Pt estimate",paste0(100 - output$alpha*100,"% CI"))

      #library(kableExtra)
      cat(paste0("Estimates of the ", method , " bounds in linear regression under data combination"))
      print(mat %>% kable(caption = ,format='rst'))
      cat(general_note)
    }else{
      ## with sign constraints
      ## no sign constraints
      mat_results = matrix("", dimXnc+ len_values+1,4);

      for( j in 1:dimXnc){
        mat_results[j,1] <- brakets(output[[paste0( method ,"pt")]])
        mat_results[j,2] <-  brakets(output[[paste0( method ,"CI")]])
        mat_results[j,3] <- brakets(output[[paste0( method ,"pt_sign")]])
        mat_results[j,4] <-  brakets(output[[paste0( method ,"CI_sign")]])
      }
      attributes(output)

      for( j in 1:len_values){
        mat_results[dimXnc+j+1,1] <- brakets(output[[paste0( method ,"beta1_pt")]][j,])
        mat_results[dimXnc+j+1,2] <-  brakets(output[[paste0( method ,"beta1")]][j,])
        mat_results[dimXnc+j+1,3] <- brakets(output[[paste0( method ,"beta1_sign_pt")]][j,])
        mat_results[dimXnc+j+1,4] <-  brakets(output[[paste0( method ,"beta1_sign")]][j,])
      }
      # dimXc_old
      # output$dimXc_old
      mat_label = matrix("", dimXnc+ len_values+1,dimXc_old+1)
      mat_label[1:dimXnc,1] <- nc_var
      mat_label[dimXnc+1,(2:(dimXc_old+1))] <- c_var
      for( j in 1:len_values){
        mat_label[(dimXnc+j+1),(2:(dimXc_old+1))] <- as.numeric(output$values_old[output$select_values[j+1],])
      }

      mat <- cbind( mat_label,mat_results)
      colnames(mat) <- c("Xnc",rep("Xc",dimXc_old),"Set estimate",paste0(100 - output$alpha*100,"% CI"),"Set estimate, with constraint",paste0(100 - output$alpha*100,"% CI,  with  constraint"))

      #library(kableExtra)
      cat(paste0("Estimates of the ", method , " bounds in linear regression under data combination"))
      print(mat %>% kable(caption = ,format='rst'))
      cat(general_note)
    }


  }

return(mat)




}

