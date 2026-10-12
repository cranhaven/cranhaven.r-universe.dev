\name{USFlag}
\alias{USFlag}
\docType{data}
\title{United States Flag Privately-Owned Merchant Fleet Data
%%   ~~ data name/kind ... ~~
}
\description{
This dataset includes 10 categorical variables that describe U.S. flag privately owned merchant
 fleet vessels, based on data provided by the United Stated Department of Transportation
 Maritime Administration (MARAD).
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("USFlag")}
\format{
The format of the data is a list with components $lab and $obs. The component $lab contains
a categorical indicator of Ship Type (see below). The component $obs includes
a matrix of dimension 170x10 that contains categorical data on 170 United States flag privately
owned merchant fleet vessels. The columns are as follows:

Ship.Type
	a categorical variable with 5 levels, Containership [1], Dry Bulk [2], General Cargo [3],
	Ro-Ro [4], and Tanker [5], indicating the ship type. This is identical to $lab.

Gross.Tonnage
	a categorical variable with 6 levels, <20000GT [1], 20000-40000GT [2], 40000-60000GT [3],
	60000-80000GT [4],80000-100000GT [5], >100000GT [6], indicating the ship gross tonnage

Deadweight
	a categorical variable with 6 levels, <20000DWT [1], 20000-40000DWT [2], 40000-60000DWT [3],
	60000-100000DWT [4], 100000-140000DWT [5], >140000DWT [6], indicating the ship deadweight

Year.Built
	a categorical variable with 6 levels, <1960 [1], 1961-1980 [2], 1981-1990 [3], 1991-2000 [4],
	2001-2010 [5], and >2010 [6], indicating the year of completion of ship construction

Operator
	a categorical variable with 49 levels indicating the operator of the ship

MSP
	a binary variable indicating whether the ship is [1] or is not [0] part of the maritime
	security program

VISA
	a binary variable indicating whether the ship is [1] or is not [0] part of the
	Voluntary Intermodal Sealift Agreement

VTA
	a binary variable indicating whether the ship is [1] or is not [0] part of the
	Voluntary Tanker Agreement

Jones.Act.Eligible
	a binary variable indicating whether the ship is [1] or is not [0] Jones Act Eligible.
	These vessels are eligible to participate in domestic trade. Jones Act eligible vessels
	are built in the United States, owned by United States citizens and crewed by U.S. Mariners

Militarily.Useful
	a binary variable indicating whether the ship is [1] or is not [0] considered a militarily
	useful sealift vessel

For more information on these definitions please see IHS Maritime, Sea-Web. www.sea-web.com

}
\details{
 This data includes categorical variables that describe U.S. flag privately owned merchant
 fleet vessels. Information is provided only for oceangoing, self-propelled,
cargo-carrying vessels of 1,000 gross tons and above. These data are based on information
from the U.S. Department of Transportation Maritime Administration (MARAD) as of 3/3/2015,
obtained from the MARAD Open Data Portal
 (http://www.marad.dot.gov/resources/data-statistics/).
}
\source{
United States Maritime Administration (MARAD) Open Data Portal. http://www.marad.dot.gov/resources/data-statistics/
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}

\examples{
### load USFlag maritime data
data("USFlag")
### the following codes define the labels of the data by genera and host.
dim(USFlag$obs)
#[1] 170  10
length(USFlag$lab)
#[1] 170

}
\keyword{datasets}
