\name{spain.1900}
\alias{spain.1900}
\docType{data}
\encoding{UTF-8}
\title{
Spain_1900_(JMTL-Sept2011).xlsx (MS Excel file)
}
\description{
This file describes the labour relations in Spain in 1900. The file was kindly
provided by Prof. Lana-Berasain. See details for restricted use of this data.}
\usage{data(spain.1900)}
\format{
  A data frame with 823 observations on the following 52 variables.
  \describe{
    \item{\code{labour.rel.3}}{a numeric vector}
    \item{\code{labour.rel.2}}{a numeric vector}
    \item{\code{labour.rel.1}}{a numeric vector}
    \item{\code{id.labrel}}{a numeric vector}
    \item{\code{id.pop}}{a numeric vector}
    \item{\code{day}}{a numeric vector}
    \item{\code{month}}{a numeric vector}
    \item{\code{year}}{a numeric vector}
    \item{\code{year.start}}{a numeric vector}
    \item{\code{year.end}}{a numeric vector}
    \item{\code{locality}}{a factor with levels \code{[Text/NA]} \code{NA}}
    \item{\code{urbanized}}{a factor with levels \code{[Y/N/NA]} \code{NA} \code{Y}}
    \item{\code{region}}{a factor with levels \code{[Text/NA]} \code{NA}}
    \item{\code{country}}{a factor with levels \code{[mandatory]} \code{[Text]} \code{Spain}}
    \item{\code{total}}{a numeric vector}
    \item{\code{minimum}}{a numeric vector}
    \item{\code{maximum}}{a numeric vector}
    \item{\code{quality.total}}{a factor with levels \code{[A/C/E]} \code{[mandatory]} \code{A} \code{C}}
    \item{\code{gender}}{a factor with levels \code{[mandatory]} \code{[T/M/F/U]} \code{F} \code{M} \code{T}}
    \item{\code{mar.stat}}{a factor with levels \code{[mandatory]} \code{[T/M/S/W/D/U]} \code{M} \code{S} \code{T} \code{U} \code{W}}
    \item{\code{age.start}}{a numeric vector}
    \item{\code{age.end}}{a numeric vector}
    \item{\code{type.activity}}{a factor with levels \code{[mandatory]} \code{[P/L (+a/c/d/j)]} \code{L} \code{P}}
    \item{\code{branch}}{a factor with levels \code{Affluents} \code{Agricultural and animal husbandry workers} \code{Architects, engineers, etc} \code{Authors, journalists and related writers} \code{beggars,tramps and prostitutes} \code{Bookkeepers, cashiers and related workers} \code{Bookkeepers, cashiers, etc} \code{Bricklayers, carpenters, etc} \code{Buyers} \code{Cabinetmakers and related woodworkers} \code{Chemical processors} \code{Children} \code{Clerical and related workers} \code{Composers and perfoming artists} \code{Composers and performing artists} \code{Convicts and prisoners} \code{Cook, waiters, bartenders, etc} \code{Cooks, waiters, bartenders, etc} \code{Electrical fitters and related electrical and electronical workers} \code{Food and beverages processors} \code{Glass formers, potters,etc} \code{Government executive officials} \code{Housekeeping services workers} \code{Jurists} \code{Machinery fitters, machine assemblers, etc} \code{Machinery fitters, machine assemblers,etc} \code{Maids and Housekeeping services workers} \code{Mail distribution clerks} \code{Managers} \code{Material handling and related equipment operators, etc} \code{Medical, dental, veterinary, etc} \code{Metal processors} \code{Miners, quarrymen, well-drillers,etc} \code{NA} \code{Not working} \code{Other production workers} \code{Patients, people in hospices, mads and alienated} \code{Primary school} \code{Printers and related workers} \code{Protective services workers} \code{Retired and State pensioners} \code{Sales workers} \code{Salesmen, shop assistants, etc} \code{Sculptors, painters, photographers,etc} \code{Service workers not classified} \code{Spinners, weavers, knitters, dyers,etc} \code{Stenographers, typists,etc} \code{Students} \code{Tailors, dressmakers, sewers, upholsterers, etc} \code{Tanners, fellmongers and pelt dressers} \code{Teachers} \code{Text/NA} \code{Transport equipment operators} \code{Unemployed} \code{Wood preparation workers} \code{Workers} \code{Workers in Religion} \code{Workers not elsewhere classified} \code{Working proprietors (wholesale and retail)}}
    \item{\code{branch.hisco}}{a factor with levels \code{[0-9/NA]} \code{[note instructions]} \code{0} \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{NA}}
    \item{\code{occup}}{a factor with levels \code{"corretaje, comisión, exportación"} \code{"negociantes, comerciantes, tratantes"} \code{[Text/NA]} \code{Actors and performing artists} \code{Agricultural workers} \code{Architects and engineers} \code{Army} \code{Assistants to the cult} \code{Authors} \code{Bookkeepers and cashiers} \code{Building sales workers} \code{Cabinetmakers and woodworkers} \code{Chemical processors} \code{Chemical product,hardware and paint sales workers} \code{Circus performers} \code{Civil service workers} \code{Clerical and related workers (empleados)} \code{Clothing industry} \code{Combustible sales workers} \code{Composers, musicians and singers} \code{Construction workers} \code{Creative artists} \code{Domestic servant} \code{Domestic service} \code{Double counted professions} \code{Dress and hat sales workers} \code{Energy workers} \code{Finance clerk and Insurance} \code{Fishermen and hunters} \code{Food and beverages processors} \code{Food and beverages sales workers} \code{Furniture sales workers} \code{General managers (industriales, fabricantes)} \code{Hotels, coffe bars, guest houses and bars} \code{Individuals without profession} \code{Jurists ('profesiones judiciales')} \code{Leather and hide workers} \code{Leather sales workers} \code{Livestock workers} \code{Luxury products, Sciences and Arts products sales} \code{Machinery mechanic} \code{Mail, telephone and telegraph} \code{Medical workers} \code{Members of Religious Orders (clero regular)} \code{Metal workers} \code{Metals sales workers} \code{Military} \code{Miners and quarrymen} \code{Ministers of non catolic cults} \code{Ministers of religion (clero católico secular)} \code{Momentarily unemployed} \code{NA} \code{Policemen (guardia civil, carabineros y policía)} \code{Pottery sales workers} \code{Pottery workers} \code{Printers and related workers} \code{Prostitute} \code{Sales workers not clasified} \code{Salesmen, shop assistants and demonstrators} \code{Stenographers, typists, translators} \code{Teachers} \code{Textile sales workers} \code{Textile workers} \code{Transport by railway} \code{Transport by rivers and canals} \code{Transport by sea} \code{Transport by streets, roads and bridges} \code{Transport machine makers} \code{Transport sales workers} \code{Undertakers and embalmers} \code{Unknown profession} \code{Waiters (mozo de almacén,mozo de comedor,camarero)} \code{Warehouse porters} \code{Waste materials processors and other industries} \code{Waste materials sales workers} \code{Wood sales workers} \code{Wood treaters} \code{Workers (jornaleros,braceros,peones,destajistas)}}
    \item{\code{occup.hisco}}{a factor with levels \code{[010-999/NA]} \code{020} \code{061} \code{120} \code{130} \code{141} \code{149} \code{151} \code{161} \code{171} \code{173} \code{175} \code{211} \code{310} \code{321} \code{331} \code{339} \code{370} \code{393} \code{410} \code{422} \code{451} \code{490} \code{532} \code{540} \code{582} \code{583} \code{592} \code{599} \code{621} \code{624} \code{649} \code{711} \code{720} \code{731} \code{749} \code{750} \code{761} \code{779} \code{791} \code{810} \code{841} \code{849} \code{859} \code{892} \code{920} \code{949} \code{950} \code{971} \code{981} \code{984} \code{989} \code{999} \code{NA}}
    \item{\code{position}}{a factor with levels \code{[Text/NA]} \code{Affluents (personas que viven de sus rentas)} \code{Family members} \code{NA} \code{Retired and State pensioners}}
    \item{\code{nationality}}{a factor with levels \code{[Text/NA]} \code{NA}}
    \item{\code{ethnicity}}{a factor with levels \code{[Text/NA]} \code{NA}}
    \item{\code{race}}{a factor with levels \code{[Text/NA]} \code{NA}}
    \item{\code{religion}}{a factor with levels \code{[Text/NA]} \code{NA}}
    \item{\code{social.group}}{a factor with levels \code{[Text/NA]} \code{NA}}
    \item{\code{source}}{a numeric vector}
    \item{\code{volume}}{a numeric vector}
    \item{\code{page}}{a factor with levels \code{[#/Text/NA]} \code{210-219} \code{27-29} \code{298-323} \code{323} \code{479}}
    \item{\code{labour.rel.1.pct}}{a numeric vector}
    \item{\code{labour.rel.2.pct}}{a numeric vector}
    \item{\code{labour.rel.3.pct}}{a numeric vector}
    \item{\code{remark}}{a factor with levels \code{[Text/NA]} \code{349 settlements with more than 5000 inhabitants} \code{Also classificable as HISCO 441} \code{Children included in the group "retirados, pensionistas del Estado y de otras administraciones públicas y privadas"} \code{It has been supposed that children and old persons were kin producers} \code{Marital status unknown} \code{NA} \code{The shares of self-employed and wage earners has been estimated according to the average of salaried workers in 1860 and 1955 (sources 14 and 15, pp.162-163)} \code{The shares of self-employed and wage earners has been estimated according to the share of patrons in the census of 1920 (source 11, V, pp.421-424, 487)} \code{The source gives that number as age unclasified} \code{The source warns that the excess of 135,120 were persons registered with more than one profession} \code{Youngmen included in the group "retirados, pensionistas del Estado y de otras administraciones públicas y privadas"}}
    \item{\code{txt1.1}}{a factor with levels \code{Commodified} \code{EitherOr} \code{Non working} \code{Reciprocal} \code{Tributary} \code{Unknown}}
    \item{\code{txt1.2}}{a factor with levels \code{Community} \code{EitherOr} \code{Household} \code{Market} \code{Non-market} \code{Unknown}}
    \item{\code{txt1.3.ext}}{a factor with levels \code{Affluent (2)} \code{Cannot work (1)} \code{EitherOr} \code{Employers (13)} \code{Household} \code{Indent.lbr. (15)} \code{Indent.lbr. (9)} \code{Kin non-prod. (5b)} \code{Kin prod. (5)} \code{Kin prod. (5a)} \code{Leading prod. (4)} \code{Multiple} \code{Obligatory lbr. (8)} \code{Redist.lbr. (7)} \code{Self-Employed (12)} \code{Serfs (10)} \code{Serfs (16)} \code{Servants (6)} \code{Slaves (11)} \code{Slaves (17)} \code{Unemployed (3)} \code{Unknown} \code{Wage-Earners (14)} \code{Wage-Earners (18)}}
    \item{\code{txt2.1}}{a character vector}
    \item{\code{txt2.2}}{a factor with levels \code{Community} \code{EitherOr} \code{Household} \code{Market} \code{Non-market} \code{Unknown}}
    \item{\code{txt2.3.ext}}{a factor with levels \code{Affluent (2)} \code{Cannot work (1)} \code{EitherOr} \code{Employers (13)} \code{Household} \code{Indent.lbr. (15)} \code{Indent.lbr. (9)} \code{Kin non-prod. (5b)} \code{Kin prod. (5)} \code{Kin prod. (5a)} \code{Leading prod. (4)} \code{Multiple} \code{Obligatory lbr. (8)} \code{Redist.lbr. (7)} \code{Self-Employed (12)} \code{Serfs (10)} \code{Serfs (16)} \code{Servants (6)} \code{Slaves (11)} \code{Slaves (17)} \code{Unemployed (3)} \code{Unknown} \code{Wage-Earners (14)} \code{Wage-Earners (18)}}
    \item{\code{txt3.1}}{a numeric vector}
    \item{\code{txt3.2}}{a factor with levels \code{Community} \code{EitherOr} \code{Household} \code{Market} \code{Non-market} \code{Unknown}}
    \item{\code{txt3.3.ext}}{a factor with levels \code{Affluent (2)} \code{Cannot work (1)} \code{EitherOr} \code{Employers (13)} \code{Household} \code{Indent.lbr. (15)} \code{Indent.lbr. (9)} \code{Kin non-prod. (5b)} \code{Kin prod. (5)} \code{Kin prod. (5a)} \code{Leading prod. (4)} \code{Multiple} \code{Obligatory lbr. (8)} \code{Redist.lbr. (7)} \code{Self-Employed (12)} \code{Serfs (10)} \code{Serfs (16)} \code{Servants (6)} \code{Slaves (11)} \code{Slaves (17)} \code{Unemployed (3)} \code{Unknown} \code{Wage-Earners (14)} \code{Wage-Earners (18)}}
    \item{\code{sortID2}}{a numeric vector}
    \item{\code{ctry.time}}{a character vector}
    \item{\code{bmyear}}{a numeric vector}
  }
}
\details{
This dataset originates from the Labour Relations Collaboratory and specifically describes labour relations in Spain in 1900. The dataset and others in the project including full documentation and licenses are available from:
http://www.historyoflabourrelations.org, hosted by the International Institute of Social History (http://socialhistory.org). The dataset is constructed by Professor José-Miguel Lana-Berasain who has kindly agreed for this dataset to be used in the lar-package. Use of the dataset is limited to the conditions specified at the Collaboratory website: http://www.historyoflabourrelations.org .
}
\source{
https://collab.iisg.nl/c/document_library/get_file?p_l_id=273223&folderId=283117&name=DLFE-91302.pdf
}
\references{
Lana-Berasain, J-M. ?. Labour Relations in Spain, 1800, 1900 and 2001: A methodological approach. URL: https://collab.iisg.nl/c/document_library/get_file?p_l_id=273223&folderId=283117&name=DLFE-91302.pdf. Last accessed: April 28, 2014.
}
\examples{
data(spain.1900)
}
\keyword{datasets}
