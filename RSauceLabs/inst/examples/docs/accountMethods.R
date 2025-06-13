\dontrun{
myAcc <- account()
appData <- getUser(myAcc)

createUser(myAcc, newUsername = "superstartester", password = "johndoe"
           , name = "John", email = "superstartester@example.com")
# $errors
# [1] "Subaccount capacity exhausted."
uC <- getUserConcurrency(myAcc)
#> rbindlist(uC$concurrency$self[c("allowed", "current")], fill = TRUE)
#manual mac overall real_device
#1:      5   5       5           0
#2:      0   0       0          NA
users <- getListOfSubAccounts(myAcc)
#> users$users_total
#[1] 1
siblings <- getListOfSiblingAccounts(myAcc)
#> getListOfSiblingAccounts(myAcc)
#list()
subAcc <- getSubAccountInformation(myAcc)

# change accesskey for a user
# changeAccessKey(myAcc, "rsaucelabs")
}
