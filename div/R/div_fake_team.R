# (C) Philippe J.S. De Brouwer -- 2021
# licensed under GNU Affero General Public License v3.0

#' Generate randomly team-data
#'
#' This function generates a data frame with data for a team (with salaries, gender, FTE, etc). This is a good start to test the package and to experiment what level of bias will be visible in the paygap for example.
#' @param seed numeric, the seed to be used in set.seed()
#' @param N numeric, the size of the team to be used (default = 200)
#' @param genders character, a vector of the genders to be used
#' @param gender_prob numeric, relative probabilities of the different genders to occur (must have the same length as 'genders')
#' @param gender_salaryBias numeric, vector with the relative salaries of the different genders (must have the same length as 'genders')
#' @param jobIDs character, a vector with the labels of the job categories in the team (they will appear in each grade)
#' @param jobID_prob numeric, a vector with the relative sizes of the different jobs in the team (must have the same length as 'jobIDs')
#' @param citizenships character, a vector of the citizenships to be generated
#' @param citizenship_prob numeric, relative probabilities of the different citizenships to occur (must have the same length as 'citizenships')
#' @returns dataframe (employees of the random team)
#' @export
#' @importFrom stats dnorm median pbinom qbinom rlnorm rnorm runif sd
#' @import tidyverse
#' @importFrom tibble as_tibble
#' @examples
#' library(div)
#' d <- div_fake_team()
#' head(d)
#' diversity(table(d$gender))
div_fake_team <- function(seed              = 100,
                          N                 = 200,
                          genders           = c("F", "M", "O"),
                          gender_prob       = c(0.4, 0.58, 0.02),
                          gender_salaryBias = c(1, 1.1, 1),
                          jobIDs            = c("sales", "analytics"),
                          jobID_prob        = c(0.6, 0.4),
                          citizenships      = c("Polish", "German", "Italian", "Indian", "Other"),
                          citizenship_prob  = c(0.6, 0.2, 0.1, 0.05, 0.05)
                          ) {


  # bindings to avoid the message 'no global bindings for ..'
  age <- grade <- tenure_firm <- tenure_grade <- tenure <- job <- tenure_team <- tenure_job <- NULL

set.seed(seed)

# Create the initial data frame, that will be improved later:
d0 <- data.frame("ID"      = 1:N,

                 # user defined bias in gender ofthe population:
                 "gender"      = factor(x = cut(runif(N), c(0, cumsum(gender_prob)), labels = genders)),

                 # FTE = full time equivalent (e.g. working 4 days per week -> fte = 4/5):
                 "fte"      = 1,

                 # Functional grade will be filled in later:
                 "grade"       = 0,

                 # job (as a sub-group of grade / the combination grad+jobID determines salary):
                 "jobID"       = factor(x = cut(runif(N), c(0, cumsum(jobID_prob)), labels = jobIDs)),

                 # Log-normal age distribution
                 "age"         = round(rlnorm(N, log(30), log(1.25))),

                 # A significant bias towards the old continent:
                 "citizenship"   = factor(x = cut(runif(N),
                                                  c(0, cumsum(citizenship_prob)),
                                                  labels = citizenships)),

                 # diploma level (Bachelor, Master of SCience, PhD):
                 "degree"      = 'M.Sc.',

                 # tenure in the firm/organisation (in years):
                 "tenure_firm"  = round(rlnorm(N, log(3.0), log(2.5)), 2),
                 # tenure in the firm/organisation (in years):
                 "tenure_grade" = round(rlnorm(N, log(3.0), log(2.25)), 2),
                 # tenure in the firm/organisation (in years):
                 "tenure_job" = round(rlnorm(N, log(3), log(1.25)), 2),

                 # Salary will be filled in later:
                 "salary"      = 0,

                 # Three teams of different sizes:
                 "team"        = ifelse(runif(N) < 0.6, "bigTeam",
                                        ifelse(runif(N) < 0.6,
                                               "mediumTeam",
                                               ifelse(runif(N) < 0.8, "smallTeam",
                                                      "XsmallTeam"))),

                 # Three teams of different sizes:
                 "subteam"        = ifelse(runif(N) < 0.6, "bigTeam",
                                        ifelse(runif(N) < 0.6,
                                               "mediumTeam",
                                               ifelse(runif(N) < 0.8, "smallTeam",
                                                      "XsmallTeam"))),

                                  # Most people have little people depending on them:
                 "dependents"  = round(rlnorm(N,log(0.75),log(2.5))),

                 # Random performance (no bias linked, but different group sizes):
                 "performance" = ifelse(runif(N) < 0.1, "L",
                                        ifelse(runif(N) < 0.6, "M",
                                               ifelse(runif(N) < 0.7, "H", "XH"))),

                 # a timestamp will be useful to show evolution (in a later phase):
                 "timestamp"   = as.Date("2020-01-01")
  )

# Now we clean up age and fill in grade, salary and lastPromoted without
# any bias for gender, origin -- but with a bias for age.
d1 <- d0                                                  %>%
  mutate(age    = ifelse((age < 18), age + 10, age))      %>%
  mutate(grade  = ifelse(runif(N) * age < 20, 0,
                         ifelse(runif(N) * age < 25, 1,
                                ifelse(runif(N) * age < 30, 2, 3)))) %>%
  mutate(salary = round(exp(0.75*grade)*4000 + rnorm(N,0,500)))      %>%
  mutate(lastPromoted = round(exp(0.05*(3-grade))*1 +
                                abs(rnorm(N,0,5))) -1)    %>%
  mutate(tenure_firm = pmax(tenure_firm, tenure_grade))   %>%
  mutate(tenure_grade = pmax(tenure_grade, tenure_job))

# Add the gender bias in salary:
for(i in 1:nrow(d1)) {
  d1$salary[i] <- d1$salary[i] * gender_salaryBias[which(d1$gender[i] == genders)]
}


d1 <- as_tibble(d1)
#class(d1) <- 'div'

# return
d1
}
