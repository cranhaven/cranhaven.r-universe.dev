# List of required packages
requiredPackages <- c("factset.protobuf.stach.v2")

# Install missing packages
missingPackages <- requiredPackages %in% installed.packages()[, "Package"]
if (any(missingPackages == FALSE)) {
  install.packages(requiredPackages, repos = "http://cran.us.r-project.org")
}

# Load packages
# In the below statement 'invisible' is used to make the printing statements invisible
# which means the printing statements will not appear
invisible(lapply(requiredPackages, library, character.only = TRUE))

# Column Organized with Stach Data
binaryFilePath <-
  system.file('testdata/V2ColumnStachData', package = 'factset.protobuf.stachextensions')
package <-
  read(factset.protobuf.stach.v2.Package, input = binaryFilePath)

binaryFilePath <-
  system.file('testdata/V2ColumnStachDataWithoutHeaderTable', package = 'factset.protobuf.stachextensions')

packagewithoutheader <-
  read(factset.protobuf.stach.v2.Package, input = binaryFilePath)
stachExtensioncol <-
  factset.protobuf.stachextensions::V2ColumnOrganizedStachExtension$new()
firstRow <-
  list("total0",
       "group1",
       "group2",
       "Port.+Weight",
       "Bench.+Weight",
       "Difference")
secondRow = list("Total", "", "", "100", "", "100")

# Column Organized (struct) with Stach Data
binaryFilePath <-
  system.file('testdata/V2ColumnStachWithStruct', package = 'factset.protobuf.stachextensions')

structpackage <-
  read(factset.protobuf.stach.v2.Package, input = binaryFilePath)
structfirstrow <-
  list("InputSecurity",
       "Scenario",
       "Horizon",
       "Run Status",
       "CF Coupon")
structsecondrow <-
  list(
    "3140JQHD",
    "Base",
    "Base",
    "",
    "{\"20210625\":\"3.5\",\"20210725\":\"3.5\",\"20210825\":\"3.5\",\"20210925\":\"3.5\"}"
  )

# Row Organized with Stach Data
binaryFilePath <-
  system.file('testdata/V2RowStachData', package = 'factset.protobuf.stachextensions')

rowpackage <-
  read(factset.protobuf.stach.v2.RowOrganizedPackage, input = binaryFilePath)
stachExtensionrow <-
  factset.protobuf.stachextensions::V2RowOrganizedStachExtension$new()
roworgfirstrow <- list("total0", "group1", "group2", "Difference")
roworgsecondrow <- list("Total", "", "", "")

# Row Organized (row and col span) with Stach Data
binaryFilePath <-
  system.file('testdata/V2RowStachWithRowAndColSpan', package = 'factset.protobuf.stachextensions')

rowcolspanpackage <-
  read(factset.protobuf.stach.v2.RowOrganizedPackage, input = binaryFilePath)
roworgthirdrow <-
  list(
    "Function",
    "Region",
    "Continent 1",
    "Continent 2",
    "Fund",
    "Bench",
    "Abbr",
    "Fund",
    "Bench",
    "Fund",
    "Bench"
  )
roworgfourthrow <-
  list("Max",
       "",
       "",
       "",
       "88.3",
       "89.62",
       "",
       "17.17",
       "15.67",
       "86.07",
       "89.18")

# Simplified Row Organized with Stach Data
binaryFilePath <-
  system.file('testdata/V2SimplifiedRowStachData', package = 'factset.protobuf.stachextensions')

simplifiedrowpackage <-
  read(factset.protobuf.stach.v2.RowOrganizedPackage, input = binaryFilePath)

# Simplified Row Organized with Struct Data
binaryFilePath <-
  system.file('testdata/V2SimplifiedRowWithStruct', package = 'factset.protobuf.stachextensions')

simplifiedrowstructpackage <-
  read(factset.protobuf.stach.v2.RowOrganizedPackage, input = binaryFilePath)

compressedFilePath <-
  system.file('testdata/V2ColumnOrganizedCompressed.json', package = 'factset.protobuf.stachextensions')
compressedData <-
  jsonlite::read_json(path = compressedFilePath, auto_unbox = TRUE)
