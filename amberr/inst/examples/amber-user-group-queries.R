library(amberr)

# Start Amber session
a <- amber.login(username = "admin@obiba.org", password = "password", url = "http://localhost:3030")
# 2FA is requested if enabled for the user
# a <- amber.login(username = "jim@muse.com", password = "password123", url = "http://localhost:3030")

# Connection handle contains user ID and access token
a

#
# Users
#
# Find all users
amber.users(a)
# Find users with a specific email (only one expected)
amber.users(a, query = list(email = "jim@muse.com"))
# Find users with a specific role
amber.users(a, query = list(role = "guest"))
# Find users not having verified their email
amber.users(a, query = list(isVerified = "false"))
# Find a specific user by email or ID, get NULL if none are found
amber.user(a, id = "jim@muse.com")
amber.user(a, id = "615593fdda14d53617df7cc6")
amber.user(a, id = "jim@x.com")

#
# Groups
#
# Find all groups
amber.groups(a)
# Find group with a specific name or ID
amber.group(a, id = "obiba.org")
amber.group(a, id = "615abb061799dd6a2cdbbb10")

#
# Subjects
#
# Find all users and groups as subjects
amber.subjects(a)

# End Amber session
amber.logout(a)
