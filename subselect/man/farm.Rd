\name{farm}
\docType{data}
\alias{farm}
\title{Sixty-two economic indicators from 99 Portuguese farms.}
\description{
  This data set is a very small subset of economic data regarding
  Portuguese farms in the mid-1990s, from Portugal's Ministry of Agriculture
     }
\usage{farm}
\format{A 99x62 matrix. The 62 columns are numeric economic indicators,
  referenced by their database code. Monetary units are in thousands of
  Escudos (Portugal's pre-Euro currency).
 \tabular{rlll}{
         Column Number \tab Column Name   \tab Units \tab Description \cr
         [,1] \tab R15 \tab 1000 Escudos \tab Total Standard Gross Margins (SGM)\cr
         [,2] \tab R24 \tab Hectares \tab Total land surface \cr
         [,3] \tab R35 \tab Hectares \tab Total cultivated surface \cr
         [,4] \tab R36 \tab Man Work Units \tab Total Man Work Units\cr
         [,5] \tab R46 \tab 1000 Escudos \tab Land Capital\cr
         [,6] \tab R59  \tab 1000 Escudos \tab Total Capital (without forests) \cr
         [,7] \tab R65  \tab 1000 Escudos \tab Total Loans and Debts \cr
         [,8] \tab R72  \tab 1000 Escudos \tab Total Investment \cr
         [,9] \tab R79  \tab 1000 Escudos \tab Subsidies for Investment \cr
         [,10] \tab R86  \tab 1000 Escudos \tab Gross Plant Product Formation\cr
         [,11] \tab R91  \tab 1000 Escudos \tab Gross Animal Product Formation\cr
         [,12] \tab R104  \tab 1000 Escudos \tab Current Subsidies\cr
         [,13] \tab R110  \tab 1000 Escudos \tab Wheat Production\cr
         [,14] \tab R111  \tab 1000 Escudos \tab Maize Production\cr
         [,15] \tab R113  \tab 1000 Escudos \tab Other Cereals (except rice) Production\cr
         [,16] \tab R114  \tab 1000 Escudos \tab Dried Legumes Production\cr
         [,17] \tab R115  \tab 1000 Escudos \tab Potato Production\cr
         [,18] \tab R116  \tab 1000 Escudos \tab Industrial horticulture and Melon Production\cr
         [,19] \tab R117  \tab 1000 Escudos \tab Open-air horticultural Production\cr
         [,20] \tab R118  \tab 1000 Escudos \tab Horticultural forcing Production\cr
         [,21] \tab R119  \tab 1000 Escudos \tab Flower Production\cr
         [,22] \tab R121  \tab 1000 Escudos \tab Sub-products Production\cr
         [,23] \tab R122  \tab 1000 Escudos \tab Fruit Production\cr
         [,24] \tab R123  \tab 1000 Escudos \tab Olive Production\cr
         [,25] \tab R124  \tab 1000 Escudos \tab Wine Production\cr
         [,26] \tab R125  \tab 1000 Escudos \tab Horses\cr
         [,27] \tab R126  \tab 1000 Escudos \tab Bovines (excluding milk)\cr
         [,28] \tab R127  \tab 1000 Escudos \tab Milk and dairy products\cr
         [,29] \tab R129  \tab 1000 Escudos \tab Sheep\cr
         [,30] \tab R132  \tab 1000 Escudos \tab Goats\cr
         [,31] \tab R135  \tab 1000 Escudos \tab Pigs\cr
         [,32] \tab R137  \tab 1000 Escudos \tab Birds\cr
         [,33] \tab R140  \tab 1000 Escudos \tab Bees\cr
         [,34] \tab R142  \tab 1000 Escudos \tab Other animals (except rabbits)\cr
         [,35] \tab R144  \tab 1000 Escudos \tab Wood production\cr
         [,36] \tab R145  \tab 1000 Escudos \tab Other forest products (except cork)\cr
         [,37] \tab R146  \tab Hectares \tab Land surface affected to cereals\cr
         [,38] \tab R151  \tab Hectares \tab Land surface affected to dry legumes\cr
         [,39] \tab R152  \tab Hectares \tab Land surface affected to potatos\cr
         [,40] \tab R158  \tab Hectares \tab Land surface affected to fruits\cr
         [,41] \tab R159  \tab Hectares \tab Land surface affected to olive trees\cr
         [,42] \tab R160  \tab Hectares \tab Land surface affected to vineyards\cr
         [,43] \tab R164  \tab Hectares \tab Fallow land surface area \cr
         [,44] \tab R166  \tab Hectares \tab Forest surface area \cr
         [,45] \tab R168  \tab Head \tab Bovines \cr
         [,46] \tab R174  \tab Head \tab Adult sheep \cr
         [,47] \tab R176  \tab Head \tab Adult goats \cr
         [,48] \tab R178  \tab Head \tab Adult pigs \cr
         [,49] \tab R209  \tab Kg/hectare \tab Maize yield \cr
         [,50] \tab R211  \tab Kg/hectare \tab Barley yield \cr
         [,51] \tab R214  \tab Kg/hectare \tab Potato yield \cr
         [,52] \tab R215  \tab L/cow/year \tab Cow milk productivity \cr
         [,53] \tab R233  \tab 1000 Escudos \tab Wages and social expenditure \cr
         [,54] \tab R237  \tab 1000 Escudos \tab Taxes and tariffs \cr
         [,55] \tab R245  \tab 1000 Escudos \tab Interest and financial costs \cr
         [,56] \tab R250  \tab 1000 Escudos \tab Total real costs \cr
         [,57] \tab R252  \tab 1000 Escudos \tab Gross Product \cr
         [,58] \tab R256  \tab 1000 Escudos \tab Gross Agricultural Product \cr
         [,59] \tab R258  \tab 1000 Escudos \tab Gross Value Added (GVA) \cr
         [,60] \tab R263  \tab 1000 Escudos \tab Final Results \cr
         [,61] \tab R270  \tab 1000 Escudos \tab Family labour income \cr
         [,62] \tab R271  \tab 1000 Escudos \tab Capital Income \cr
       }
}

\source{Obtained directly from the source.}
%\references{  }
\keyword{datasets}