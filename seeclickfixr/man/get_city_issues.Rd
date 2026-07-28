\name{get_city_issues} \alias{get_city_issues} \title{ Query all issues in a
given location } \description{ Returns all issues in the specific location,
specified either by coordinates or by name/address. } \usage{
get_city_issues(city=NULL, lat=NULL,long=NULL, status = 
"open,acknowledged,closed,archived", limit = 100) } \arguments{ \item{city}{ A
written description of the location for which issue types should be returned.
If city is specified, lat/long should not be. } \item{lat}{ Latitude of
coordinates, specified instead of city. } \item{long}{ Longitude of
coordinates, specified instead of city. } \item{status}{ Which status types of
issues should be returned. Separate statuses should be separated by commas
without spaces. Available options are open, acknowledged, closed, and
archived. Default is all. } \item{limit}{ Number of items to return. Defaults
to 100. } }

\value{ \item{issue_id}{Unique ID number for the given issue} 
\item{status}{Status of the issue (open/closed)} \item{summary}{Summary of the
status for the issue} \item{description}{Text description of the issue as 
reported} \item{rating}{Importance rating of the issue} \item{lat}{Latitude of
the issue as reported} \item{lng}{Longitude of the issue as reported} 
\item{issue_address}{Address of the issue as reported} \item{created_at}{Date 
and time when issue report was created} \item{acknowledged_at}{Date and time 
when issue report was acknowledged by city} \item{closed_at}{Date and time 
when issue report was closed by city} \item{reopened_at}{Date and time when 
issue report was reopened, if it was} \item{updated_at}{Date and time when 
issue report was last updated} \item{shortened_url}{Shortened URL of the issue
report} \item{video_url}{URL for the video of the issue, if provided} 
\item{image_full}{Image of the issue as reported} 
\item{image_square_100x100}{Square version of the image of the issue} 
\item{representative_image_url}{A representative image of the issue, if no 
actual image was submitted} \item{issue_type}{Type of issue} \item{url}{URL to
the report of the issue} \item{html_url}{URL to the report of the issue in 
HTML format} \item{comment_url}{URL to the comments on the issue} 
\item{flag_url}{URL to the flag for the issue} \item{close_url}{URL to the 
closing report of the issue} \item{open_url}{URL to the opening report of the 
issue} \item{reporter_id}{Issue reporter's unique ID number} 
\item{reporter_name}{Name of the issue reporter} 
\item{reporter_wittytitle}{Username/witty name of the issue reporter} 
\item{reporter_role}{Issue reporter's role in the city, if any} 
\item{reporter_civicpoints}{Number of civic points the issue reporter has, if 
any} \item{reporter_avatar_full}{Chosen avatar of the issue reporter} 
\item{reporter_avatar_square}{Square version of the avatar} }

\seealso{ \code{\link{get_specific_issue}} }