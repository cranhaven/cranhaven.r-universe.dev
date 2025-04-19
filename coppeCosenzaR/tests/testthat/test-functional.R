require(testthat)



context("\n\ Functional Test \n")


    ###################### Projeto 1
   p1 <-  Project(
      "p1",
      Project.criteria(
        list(
          Project.criterion(
            Factor("fat1"),
            "Cr",
            TRUE
          ),
          Project.criterion(
            Factor("fat2"),
            "C",
            FALSE
          ),
          Project.criterion(
            Factor("fat3"),
            "Cr",
            FALSE
          )
        )
      )
    )
    ###################### Projeto 2
   p2 <-  Project(
      "p2",
      Project.criteria(
        list(
          Project.criterion(
            Factor("fat1"),
            "Cr",
            TRUE
          ),
          Project.criterion(
            Factor("fat2"),
            "Cr",
            TRUE
          ),
          Project.criterion(
            Factor("fat3"),
            "Cr",
            FALSE
          )
        )
      )
    )
    ###################### Projeto 3
    p3 <- Project(
      "p3",
      Project.criteria(
        list(
          Project.criterion(
            Factor("fat1"),
            "C",
            TRUE
          ),
          Project.criterion(
            Factor("fat2"),
            "Cr",
            FALSE
          ),
          Project.criterion(
            Factor("fat3"),
            "Cr",
            FALSE
          )
        )
      )
    )
    ###################### Projeto 4
   p4 <-  Project(
      "p4",
      Project.criteria(
        list(
          Project.criterion(
            Factor("fat1"),
            "LC",
            FALSE
          ),
          Project.criterion(
            Factor("fat2"),
            "I",
            FALSE
          ),
          Project.criterion(
            Factor("fat3"),
            "LC",
            FALSE
          )
        )
      )
    )
   project.portfolio <- Project.portfolio( list(p1, p2, p3, p4) )


    ##############################  Option 1
    o1 <- Option(
      name = "O1",
      Option.resources(
        list(
          Option.factor.availability(
            Factor("fat1"),
            "W"
          ),
          Option.factor.availability(
            Factor("fat2"),
            "Ex"
          ),
          Option.factor.availability(
            Factor("fat3"),
            "R"
          )
        )
      )
    )
    ##############################  Option 2
    o2 <- Option(
      name = "O2",
      Option.resources(
        list(
          Option.factor.availability(
            Factor("fat1"),
            "R"
          ),
          Option.factor.availability(
            Factor("fat2"),
            "W"
          ),
          Option.factor.availability(
            Factor("fat3"),
            "R"
          )
        )
      )
    )
    ##############################  Option 3
   o3 <-  Option(
      name = "O3",
      Option.resources(
        list(
          Option.factor.availability(
            Factor("fat1"),
            "G"
          ),
          Option.factor.availability(
            Factor("fat2"),
            "R"
          ),
          Option.factor.availability(
            Factor("fat3"),
            "In"
          )
        )
      )
    )

    option.portfolio <- Option.portfolio( list(o1, o2, o3))



factors.of.interest <- Factors.of.interest(
  list(
    Factor("fat1"),
    Factor("fat2"),
    Factor("fat3")
  )
  )

message("\n\n Testing: Coppe.cosenza(project.portfolio, option.portfolio, factors.of.interest)")
cc1 <- Coppe.cosenza(project.portfolio, option.portfolio, factors.of.interest)
message(cc1@result)

message("\n\n Testing: Coppe.cosenza(p1, option.portfolio, factors.of.interest)")
cc2 <- Coppe.cosenza(p2, option.portfolio, factors.of.interest,"default", FALSE)
message(cc2@result)

message("\n\n Testing: Coppe.cosenza(project.portfolio, o1, factors.of.interest)")
cc3 <- Coppe.cosenza(project.portfolio, o1, factors.of.interest,"default", FALSE)
message(cc3@result)


test_that("Functional Test \n",{
          expect_is(Coppe.cosenza(project.portfolio, option.portfolio, factors.of.interest), "Coppe.cosenza")
          expect_is(Coppe.cosenza(p1, option.portfolio, factors.of.interest, "default", FALSE), "Coppe.cosenza")
          expect_is(Coppe.cosenza(project.portfolio, o1, factors.of.interest,"default", FALSE), "Coppe.cosenza")
}
)

