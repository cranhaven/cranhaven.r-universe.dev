library(amberr)

# Start Amber session
a <- amber.login(username = "admin@obiba.org", password = "password", url = "http://localhost:3030")
# 2FA is requested if enabled for the user
# a <- amber.login(username = "jim@muse.com", password = "password123", url = "http://localhost:3030")

#
# Studies
#
# Find all studies
amber.studies(a)
# Find studies with a specific name
amber.studies(a, query = list(name = "Trauma Registry"))
# Find studies with a specific ID (obviously there is only one)
amber.studies(a, query = list(`_id` = "617e50749f542d5771d448ad"))
# Find first study with a specific name
amber.study(a, id = "Trauma Registry")
# Find study with a specific ID
amber.study(a, id = "617e50749f542d5771d448ad")
# Find studies created by a user
amber.studies(a, query = list(createdBy = amber.user(a, id = "admin@obiba.org")$`_id`))

#
# Forms
# belong to a study
#
# Find all forms
amber.forms(a)
# Find forms from a study
amber.forms(a, study = "Trauma Registry")
# Find first form with given name
amber.form(a, id = "Adult trauma")
# Find form with specified ID
amber.form(a, id = "61e69a22fea2df2f3108b508")

#
# Form revisions
# are revisions of a study's form
#
# Find all form revisions
amber.form_revisions(a)
# Find all revisions of a form, by its name
amber.form_revisions(a, form = "Adult trauma")
# Find revisions of each study's forms, by the study ID
amber.form_revisions(a, study = "Trauma Registry", limit = 10)
# Find revisions of each study's forms, by the study name
amber.form_revisions(a, study = "Trauma Registry", limit = 100)
# Find all form revisions with a specific revision number
amber.form_revisions(a, query = list(revision = 1))

# End Amber session
amber.logout(a)
