

#' Information of distribution
#' 
#' Obtain all details regarding the null distribution such as c1(x).  
#'@param x a real number
#'@param strDistr a null distribution: "Normal", "Logistic", "Cauchy"
#'
#'
#'@return a list of the following values:
#'\describe{
#'\item{fl}{f(x)}
#'
#'\item{Fl}{F(x)}
#'\item{re}{r(x). Logistic only}
#'\item{Re}{R(x). Logistic only}
#'\item{phix}{\eqn{\phi(x)}}
#'\item{v0}{\eqn{v_{0}}}
#'\item{v1}{\eqn{v_{1}(x)}}
#'\item{v2}{\eqn{v_{2}(x)}}
#'\item{c0}{\eqn{c_{0}(x)}}
#'\item{c1}{\eqn{c_{1}(x)}}
#'\item{c2}{\eqn{c_{2}(x)}}
#'\item{Gamma}{\eqn{\Gamma(x)}}
#'\item{s1}{\eqn{s_{1}(x)}}
#'\item{s2}{\eqn{s_{2}(x)}}
#'\item{s3}{\eqn{s_{3}(x)}}
#'\item{S1}{\eqn{S_{1}(x)}}
#'\item{S2}{\eqn{S_{2}(x)}}
#'\item{S3}{\eqn{S_{3}(x)}}
#'
#'}
#'@examples
#' x = 1.2
#' lst= Distr_Information(x, "Normal")
#' phix = lst$phix 
#' phix
#' c0 = lst$c0    
#' c0
#'@export


Distr_Information = function(x, strDistr){
  
  #path = paste(strDistr, "_table.rda", sep="")
  
  
  #load(Normal_table)
  
  #tmpstr = paste0("load(", strDistr, "_table)")
  #eval(parse(text=tmpstr))
  
  # #SMat = Normal_table$SMat
  # tmpstr = paste0("SMat = ", strDistr, "_table$SMat")
  # eval(parse(text=tmpstr))
  # 
  # #VMat = Normal_table$VMat
  # tmpstr = paste0("VMat = ", strDistr, "_table$VMat")
  # eval(parse(text=tmpstr))
  # 
  # #ReVec = Normal_table$ReVec
  # tmpstr = paste0("ReVec = ", strDistr, "_table$ReVec")
  # eval(parse(text=tmpstr))
  # 
  # #xVec = Normal_table$xVec
  # tmpstr = paste0("xVec = ", strDistr, "_table$xVec")
  # eval(parse(text=tmpstr))
  # 
  
  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  
  fl = lst[[1]]
  Fl = lst[[2]]
  re = lst[[3]]
  Re = lst[[4]]
  phix = lst[[5]]
  v0 = lst[[6]]
  v1 = lst[[7]]
  v2 = lst[[8]]
  c0 = lst[[9]]
  c1 = lst[[10]]
  c2 = lst[[11]]
  
  Gamma = lst[[12]]
  
  s1 = lst[[13]]
  s2 = lst[[14]]
  s3 = lst[[15]]
  
  S1 = lst[[16]]
  S2 = lst[[17]]
  S3 = lst[[18]]
  
  lst2 = list(f=fl, Fl=Fl, r=re, R=Re, phix=phix,
              v0=v0, v1=v1, v2=v2,
              c0=c0, c1=c1, c2=c2,
              Gamma=Gamma,
              s1 = s1, s2 = s2, s3= s3,
              S1 = S1, S2 = S2, S3= S3)
  
  return(lst2)
  
  
  
}
