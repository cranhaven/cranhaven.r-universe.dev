#' @title Get the example data
#' @description Get the example data of test package for litte trials.
#' @param exampleData A character, should be one of "exp_data", "meth_data", "cnv_data", "amp_gene", "del_gene" ,"label1","label2","zz","exp.p","meth.p","cnv.p"and "pathdata".
#' @details The function getExampleData(ExampleData = "exp.p)") obtains a vector of lncRNAs confirmed to be related with breast cancer. The function getExampleData(ExampleData = "Profile") obtains the expression pr
#' @references Subramanian, A., Tamayo, P., Mootha, V.K., Mukherjee, S., Ebert, B.L., Gillette, M.A., Paulovich, A., Pomeroy, S.L., Golub, T.R., Lander, E.S. et al. (2005) Gene set enrichment analysis: a knowledgebased approach for interpreting genome-wide expression profiles. Proc Natl Acad Sci U S A, 102, 15545-15550.
#' @export
GetExampleData<-function(exampleData){

  if(!exists("envData")) {
  envData<-initializetest()
  }

  if (exampleData=="exp.p")
  {
  dataset<- get("exp.p",envir=envData)
  return(dataset)
  }

  if (exampleData=="meth.p")
  {
    dataset<- get("meth.p",envir=envData)
    return(dataset)
  }

  if (exampleData=="cnv.p")
  {
    dataset<- get("cnv.p",envir=envData)
    return(dataset)
  }


  if (exampleData=="exp_data")
  {
    dataset<- get("exp_data",envir=envData)
    return(dataset)
  }

  if (exampleData=="label1")
  {
    dataset<- get("label1",envir=envData)
    return(dataset)
  }
  if (exampleData=="label2")
  {
    dataset<- get("label2",envir=envData)
    return(dataset)
  }
  if (exampleData=="meth_data")
  {
    dataset<- get("meth_data",envir=envData)
    return(dataset)
  }

  if (exampleData=="amp_gene")
  {
    dataset<- get("amp_gene",envir=envData)
    return(dataset)
  }
  if (exampleData=="del_gene")
  {
    dataset<- get("del_gene",envir=envData)
    return(dataset)
  }

  if (exampleData=="cnv_data")
  {
    dataset<- get("cnv_data",envir=envData)
    return(dataset)
  }
  if (exampleData=="zzz")
  {
    dataset<- get("zzz",envir=envData)
    return(dataset)
  }

  if (exampleData=="subpathdata")
  {
    dataset<- get("subpathdata",envir=envData)
    return(dataset)
  }
  if (exampleData=="opt_subpathways")
  {
    dataset<- get("opt_subpathways",envir=envData)
    return(dataset)
  }
  
  if (exampleData=="keysubpathways")
  {
    dataset<- get("keysubpathways",envir=envData)
    return(dataset)
  }
}
