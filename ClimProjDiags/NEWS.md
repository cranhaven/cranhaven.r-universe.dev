# 0.3.3 (Release date: 2024-01-25)
- Bugfix in Subset() for drop = T, didn't have correct given output dimension therefore returned error. It happened when the output dimension is 1 and the length is > 1.

# 0.3.2 (Release date: 2023-06-01)
- Remove climdex.pcic dependency

# 0.3.1 (Release date: 2023-03-23)
- Subset(): Prioritize the dimension names from names(dim(x)) rather than attribute 'dimensions'; If the input data doesn't have dimension names, the output doesn't have either.

# 0.3.0 (Release date: 2023-02-28)
- SelBox() and ShiftLon() to accept non-numerical data input  
- SelBox() uses the latitude and longitude dimension name instead of index  
- WeightedMean() uses multiApply::Apply inside  

# 0.2.1 (Release date: 2022-12-01)
- Fix the mistake that function "WeightedCells" was not included in the last submission.  

# 0.2.0 (Release date: 2022-11-04)
- New functions: ShiftLon, WeightedCells  
- Bugfix of Subset() when only one dimension left after subsetting and when 
parameter "indices" is not a list.
- Bugfix of WeightedMean() at the grid points that are across positive and
negative longitudes.


