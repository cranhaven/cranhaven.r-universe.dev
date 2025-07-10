# helper.R

create_data <- function() {
  data <- data.frame("from" = c("Z", "Z", "A", "A"),
                     "to" = c("A", "B", "C", "D"),
                     "Estimate" = c(4, 34, 9, 1),
                     "Total" = c(11, 70, 10, 10),
                     "Count" = c(NA, 500, NA, 50),
                     "Population" = c(FALSE, FALSE, FALSE, FALSE),
                     "Description" = c("First child of the root", "Second child of the root",
                                       "First grandchild", "Second grandchild"))
  return(data)
}
