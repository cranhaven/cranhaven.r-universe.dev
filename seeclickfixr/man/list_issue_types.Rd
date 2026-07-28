\name{list_issue_types} \alias{list_issue_types} \title{ Query issue types in
a given location } \description{ Returns all issue types in use in the
specific location, specified either by coordinates or by name/address. } 
\usage{ list_issue_types(city = NULL, lat = NULL, long = NULL, limit = 100) } 
\arguments{ \item{city}{ A written description of the location for which issue
types should be returned. If city is specified, lat/long should not be. } 
\item{lat}{ Latitude of coordinates, specified instead of city. } \item{long}{
Longitude of coordinates, specified instead of city. } \item{limit}{ Number of
items to return. Defaults to 100. } }



\value{ \item{title}{Category of issue} \item{organization}{Organization
handling this type of requests in this location} \item{url}{URL to this
category of issues} \item{potential_duplicate_issues_url}{URL to possible
duplicates in this category of issues} }

\seealso{ \code{\link{list_places}} } 

