

#' Title
#'
#' @return
#' @export
#'
#' @examples
#'
#' @include project-portfolio.R
#' @include option-portfolio.R
#' @include factors-under-consideration.R
#'
FunctionalTest <- function(){

  fat1 <- Factor("fat1")
  fat2 <- Factor("fat2")
  fat3 <- Factor("fat3")
  fat4 <- Factor("fat4")
  fat5 <- Factor("fat5")

  ###################### Projeto 1
  p1.con1 <- Project.criterion(fat1, "Cr", FALSE)
  p1.con2 <- Project.criterion(fat2, "C", TRUE)
  p1.con3 <- Project.criterion(fat3, "I", FALSE)
  p1.con4 <- Project.criterion(fat4, "Cr", FALSE)
  p1.con5 <- Project.criterion(fat5, "LC", FALSE)

  #p1.cra <- Project.criteria(list(p1.con1, p1.con2, p1.con3, p1.con4, p1.con5))
  p1.cra <- Project.criteria(list(p1.con1,  p1.con3, p1.con4, p1.con5))

  p1 <- Project("p1", p1.cra)

  ###################### Projeto 2
  p2.con1 <- Project.criterion(fat1, "C", FALSE)
  p2.con2 <- Project.criterion(fat2, "LC", FALSE)
  p2.con3 <- Project.criterion(fat3, "C", FALSE)
  p2.con4 <- Project.criterion(fat4, "Cr", FALSE)
  p2.con5 <- Project.criterion(fat5, "C", FALSE)

  p2.cra <- Project.criteria(list(p2.con1, p2.con2, p2.con3, p2.con4, p2.con5))

  p2 <- Project("p2", p2.cra)


  ###################### Projeto 3
  p3.con1 <- Project.criterion(fat1, "Cr", FALSE)
  p3.con2 <- Project.criterion(fat2, "C", TRUE)
  p3.con3 <- Project.criterion(fat3, "I", FALSE)
  p3.con4 <- Project.criterion(fat4, "Cr", FALSE)
  p3.con5 <- Project.criterion(fat5, "LC", TRUE)

  p3.cra <- Project.criteria(list(p3.con1, p3.con2, p3.con3, p3.con4, p3.con5))

  p3 <- Project("p3", p3.cra)


  ###################### Projeto 4
  p4.con1 <- Project.criterion(fat1, "Cr", FALSE)
  p4.con2 <- Project.criterion(fat2, "C", FALSE)
  p4.con3 <- Project.criterion(fat3, "I", FALSE)
  p4.con4 <- Project.criterion(fat4, "Cr", TRUE)
  p4.con5 <- Project.criterion(fat5, "LC", FALSE)

  p4.cra <- Project.criteria(list(p4.con1, p4.con2, p4.con3, p4.con4, p4.con5))

  p4 <- Project("p4", p4.cra)



  ###################### Option 1
  o1.ofa1 <- Option.factor.availability(fat1, "W")
  o1.ofa2 <- Option.factor.availability(fat2, "Ex")
  o1.ofa3 <- Option.factor.availability(fat3, "G")
  o1.ofa4 <- Option.factor.availability(fat4, "R")
  o1.ofa5 <- Option.factor.availability(fat5, "G")

  o1.or <- Option.resources(list(o1.ofa1, o1.ofa2, o1.ofa3, o1.ofa4, o1.ofa5))

  o1 <- Option("o1", o1.or)



  ###################### Option 2
  o2.ofa1 <- Option.factor.availability(fat1, "G")
  o2.ofa2 <- Option.factor.availability(fat2, "Ex")
  o2.ofa3 <- Option.factor.availability(fat3, "Ex")
  o2.ofa4 <- Option.factor.availability(fat4, "R")
  o2.ofa5 <- Option.factor.availability(fat5, "R")

  o2.or <- Option.resources(list(o2.ofa1, o2.ofa2, o2.ofa3, o2.ofa4, o2.ofa5))

  o2 <- Option("o2", o2.or)



  ###################### Option 3
  o3.ofa1 <- Option.factor.availability(fat1, "G")
  o3.ofa2 <- Option.factor.availability(fat2, "G")
  o3.ofa3 <- Option.factor.availability(fat3, "Em")
  o3.ofa4 <- Option.factor.availability(fat4, "R")
  o3.ofa5 <- Option.factor.availability(fat5, "Z")

  o3.or <- Option.resources(list(o3.ofa1, o3.ofa2, o3.ofa3, o3.ofa4, o3.ofa5))

  o3 <- Option("o3", o3.or)



  ###################### Option 4
  o4.ofa1 <- Option.factor.availability(fat1, "Em")
  o4.ofa2 <- Option.factor.availability(fat2, "Z")
  o4.ofa3 <- Option.factor.availability(fat3, "Z")
  o4.ofa4 <- Option.factor.availability(fat4, "Em")
  o4.ofa5 <- Option.factor.availability(fat5, "G")

  #o4.or <- Option.resources(list(o4.ofa1, o4.ofa2, o4.ofa3, o4.ofa4, o4.ofa5))
  o4.or <- Option.resources(list(o4.ofa1, o4.ofa2, o4.ofa3,  o4.ofa5))

  o4 <- Option("o4", o4.or)

#############################################################

  #foi <- Factors.of.interest(list(fat1,fat2,fat3,fat4,fat5))
  foi <- Factors.of.interest(list(fat2,fat3,fat4,fat5))
  pp <- Project.portfolio(list(p1, p4, p3, p2))
  op <- Option.portfolio(list(o1, o4, o3, o2))



message("\n\n Testing: Coppe.cosenza(project.portfolio, option.portfolio, factors.of.interest)")
Coppe.cosenza(pp, op, foi, "default", FALSE)



message("\n\n Testing: Coppe.cosenza(p1, option.portfolio, factors.of.interest)")
Coppe.cosenza(p1, op, foi, "default", FALSE)


message("\n\n Testing: Coppe.cosenza(project.portfolio, o1, factors.of.interest)")
Coppe.cosenza(pp, o1, foi, "default", FALSE)

}
