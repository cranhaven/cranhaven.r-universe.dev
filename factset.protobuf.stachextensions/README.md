
<img alt="FactSet" src="https://www.factset.com/hubfs/Assets/images/factset-logo.svg" height="56" width="290">

# STACH EXTENSION R SDK

Helps clients to convert stach to tabular format.

## Overview
This package was used to convert stach to tabular format and to get meta data from stach.

## Installation

```R
install.packages('factset.protobuf.stach.v2')
install.packages('factset.protobuf.stachextensions')

```

## Usage

```R
library(factset.protobuf.stach.v2)

# Column Organized Stach 
package <- read(factset.protobuf.stach.v2.Package,input='local path of your stach extension file')
stachExtensioncol <- factset.protobuf.stachextensions::V2ColumnOrganizedStachExtension$new()

# To get the Column Organized stach data in tabular format with merging the headers 
columnOrganizedData <- stachExtensioncol$ConvertToDataFrame(package)

# To get the Column Organized stach data in tabular format without merging the headers
columnOrganizedData <- stachExtensioncol$ConvertToDataFrame(package,FALSE)

# To get the Column Organized meta data 
columnOrganizedMetadata <- stachExtensioncol$GetMetadata(package)

# Row Organized Stach
package <- read(factset.protobuf.stach.v2.RowOrganizedPackage,input='local path of your stach extension file')
stachExtensionrow <- factset.protobuf.stachextensions::V2RowOrganizedStachExtension$new()

# To get the Row Organized stach data in tabular format with merging the headers
rowOrganizedData <- stachExtensionrow$ConvertToDataFrame(package)

# To get the Row Organized stach data in tabular format without merging the headers
rowOrganizedData <- stachExtensionrow$ConvertToDataFrame(package,FALSE)

# To get the Row Organized meta data 
rowOrganizedMetadata <- stachExtensionrow$GetMetadata(package)

```
## Author

analytics.api.support@factset.com
