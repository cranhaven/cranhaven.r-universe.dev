\dontrun{
myAcc <- account()
# create a temporary file
myTempFile <- file.path(tempdir(), "notsecret.html")
write("SUPER SECRET STUFF", myTempFile)
# check stored files
myStoredFiles <- getStoredFiles(myAcc)

# upload new file
res <- uploadFile(myAcc, file = myTempFile)
#> res
#$username
#[1] "seleniumPipes"
#
#$size
#[1] 19
#
#$md5
#[1] "e459fe3803b78d64cc5c2998804909a9"
#
#$filename
#[1] "notsecret.html"

#> digest::digest(file = myTempFile, algo = "md5")
#[1] "e459fe3803b78d64cc5c2998804909a9"

myStoredFiles <- getStoredFiles(myAcc)

#> rbindlist(myStoredFiles$files)
#size      mtime           name                              md5
#1:   19 1472401537 notsecret.html e459fe3803b78d64cc5c2998804909a9
#2:   14 1472350499      testDoc.R adfc8afc373f0b3fd6f93c3891bdd11b

}
