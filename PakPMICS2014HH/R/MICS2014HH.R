#' @title Multiple Indicator Cluster Survey (MICS) 2014 Household Questionnaire Data for Punjab, Pakistan
#' @name   MICS2014HH
#' @docType data
#' @keywords datasets
#' @usage data(MICS2014HH)
#' @description \code{MICS2014HH} data of Household questionnaire, MICS Punjab, 2014.
#' @format A data frame with 41413 rows and 188 variables:
#' \describe{
#'   \item{\code{HH1}}{Cluster number}
#'   \item{\code{HH2}}{Household number}
#'   \item{\code{HH3}}{Interviewer number}
#'   \item{\code{HH4}}{Supervisor number}
#'   \item{\code{HH5D}}{Day of interview}
#'   \item{\code{HH5M}}{Month of interview }
#'   \item{\code{HH5Y}}{Year of interview }
#'   \item{\code{HH6}}{Area (Original)}
#'   \item{\code{HH7}}{District (Original)}
#'   \item{\code{HH8A}}{Household selected for salt test sample}
#'   \item{\code{HH9}}{Result of HH interview}
#'   \item{\code{HH10}}{Respondent to HH questionnaire}
#'   \item{\code{HH11}}{Number of HH members}
#'   \item{\code{HH12}}{Number of women 15 - 49 years}
#'   \item{\code{HH13}}{Number of woman' questionnaires completed}
#'   \item{\code{HH14}}{Number of children under age 5}
#'   \item{\code{HH15}}{Number of under - 5 questionnaires completed}
#'   \item{\code{HH16}}{Field editor}
#'   \item{\code{HH17}}{Data entry clerk}
#'   \item{\code{HH18H}}{Start of interview - Hour}
#'   \item{\code{HH18M}}{Start of interview - Minutes}
#'   \item{\code{HH19H}}{End of interview - Hour}
#'   \item{\code{HH19M}}{End of interview - Minutes}
#'   \item{\code{SL1}}{Total children aged 1-17 years}
#'   \item{\code{SL9A}}{Rank number of the selected child}
#'   \item{\code{SL9B}}{Child line number}
#'   \item{\code{SL9C}}{Child's age}
#'   \item{\code{CD.FLAG}}{Flag for correct child line number}
#'   \item{\code{CL2A}}{Worked or helped on garden}
#'   \item{\code{CL2B}}{Helped in family business}
#'   \item{\code{CL2C}}{Produced or sold articles}
#'   \item{\code{CL2D}}{Engaged in any other activity for income}
#'   \item{\code{CL4}}{Hours worked in past week}
#'   \item{\code{CL5}}{Activities required carrying heavy loads}
#'   \item{\code{CL6}}{Activities required working with dangerous tools or heavy machinery}
#'   \item{\code{CL7A}}{Description of work:  exposed to dust, fumes, or gas}
#'   \item{\code{CL7B}}{Description of work:  exposed to extreme temperatures or humidity}
#'   \item{\code{CL7C}}{Description of work:  exposed to loud noise or vibration}
#'   \item{\code{CL7D}}{Description of work:  required to work at heights}
#'   \item{\code{CL7E}}{Description of work:  required to work with chemicals}
#'   \item{\code{CL7F}}{Description of work:  exposed to other}
#'   \item{\code{CL8}}{Fetched water or collected firewood}
#'   \item{\code{CL9}}{Hours spent fetching water or collecting firewood}
#'   \item{\code{CL10A}}{Household chores:  shopping}
#'   \item{\code{CL10B}}{Household chores:  repairing equipment}
#'   \item{\code{CL10C}}{Household chores:  cooking or cleaning}
#'   \item{\code{CL10D}}{Household chores:  washing clothes}
#'   \item{\code{CL10E}}{Household chores:  caring for children}
#'   \item{\code{CL10F}}{Household chores:  caring for old or sick}
#'   \item{\code{CL10G}}{Household chores:  other}
#'   \item{\code{CL12}}{Number of hours}
#'   \item{\code{CD2}}{Child line number}
#'   \item{\code{CD3A}}{Took away privileges}
#'   \item{\code{CD3B}}{Explained why behaviour was wrong}
#'   \item{\code{CD3C}}{Shook child}
#'   \item{\code{CD3D}}{Shouted, yelled or screamed at child}
#'   \item{\code{CD3E}}{Gave child something else to do}
#'   \item{\code{CD3F}}{Spanked, hit or slapped child on bottom with bare hand}
#'   \item{\code{CD3G}}{Hit child on the bottom or elsewhere with belt, brush, stick, etc.}
#'   \item{\code{CD3H}}{Called child dumb, lazy or another name}
#'   \item{\code{CD3I}}{Hit or slapped child on the face, head or ears}
#'   \item{\code{CD3J}}{Hit or slapped child on the hand, arm or leg}
#'   \item{\code{CD3K}}{Beat child up as hard as one could}
#'   \item{\code{CD4}}{Child needs to be physically punished to be brought up properly}
#'   \item{\code{HC1B}}{Mother tongue of household head}
#'   \item{\code{HC2}}{Number of rooms used for sleeping}
#'   \item{\code{HC3}}{Main material of floor}
#'   \item{\code{HC4}}{Main material of roof}
#'   \item{\code{HC5}}{Main material of exterior wall}
#'   \item{\code{HC6}}{Type of fuel using for cooking}
#'   \item{\code{HC7}}{Cooking location}
#'   \item{\code{HC8A}}{Electricity}
#'   \item{\code{HC8B}}{Radio}
#'   \item{\code{HC8C}}{Television}
#'   \item{\code{HC8D}}{Non-mobile phone}
#'   \item{\code{HC8E}}{Refrigerator}
#'   \item{\code{HC8F}}{Gas}
#'   \item{\code{HC8G}}{Computer}
#'   \item{\code{HC8H}}{Air conditioner}
#'   \item{\code{HC8I}}{Washing machine/ Dryer}
#'   \item{\code{HC8J}}{Air cooler/ Fan}
#'   \item{\code{HC8K}}{Cooking range/ Micro wave}
#'   \item{\code{HC8L}}{Sewing/ Knitting Machine}
#'   \item{\code{HC8M}}{Iron}
#'   \item{\code{HC8N}}{Water filter}
#'   \item{\code{HC8O}}{Dunky pump/ Turbine}
#'   \item{\code{HC9A}}{Watch}
#'   \item{\code{HC9B}}{Mobile telephone}
#'   \item{\code{HC9C}}{Bicycle}
#'   \item{\code{HC9D}}{Motorcycle or scooter}
#'   \item{\code{HC9E}}{Animal-drawn cart}
#'   \item{\code{HC9F}}{Bus or truck}
#'   \item{\code{HC9G}}{Boat with motor}
#'   \item{\code{HC9H}}{Car or Van}
#'   \item{\code{HC9I}}{Tractor trolley}
#'   \item{\code{HC10}}{Household owns the dwelling}
#'   \item{\code{HC11}}{Any household member own land that can be used for agriculture}
#'   \item{\code{HC12}}{Acres of agricultural land members of household owns}
#'   \item{\code{HC13}}{Household own any animals}
#'   \item{\code{HC14A}}{Cattle, milk cows, buffaloes or bulls}
#'   \item{\code{HC14B}}{Horses, donkeys, or mules or camels}
#'   \item{\code{HC14C}}{Goats}
#'   \item{\code{HC14D}}{Sheep}
#'   \item{\code{HC14E}}{Chickens/ducks/turkey}
#'   \item{\code{HC15}}{Any household member have account in Bank, PO or National Saving Centre}
#'   \item{\code{WS1}}{Main source of drinking water}
#'   \item{\code{WS2}}{Main source of water used for other purposes (if bottled water used for drinking)}
#'   \item{\code{WS3}}{Location of the water source}
#'   \item{\code{WS4}}{Time (in minutes) to get water and come back}
#'   \item{\code{WS5}}{Person collecting water}
#'   \item{\code{WS6}}{Treat water to make safer for drinking}
#'   \item{\code{WS7A}}{Water treatment: Boil}
#'   \item{\code{WS7B}}{Water treatment: Add bleach/chlorine}
#'   \item{\code{WS7C}}{Water treatment: Strain it through a cloth}
#'   \item{\code{WS7D}}{Water treatment: Use water filter}
#'   \item{\code{WS7E}}{Water treatment: Solar disinfection}
#'   \item{\code{WS7F}}{Water treatment: Let it stand and settle}
#'   \item{\code{WS7X}}{Water treatment: Other}
#'   \item{\code{WS7Z}}{Water treatment: DK}
#'   \item{\code{WS8}}{Type of toilet facility}
#'   \item{\code{WS9}}{Toilet facility shared}
#'   \item{\code{WS10}}{Toilet shared with other household or with general public}
#'   \item{\code{WS11}}{Households using this toilet facility}
#'   \item{\code{RM1}}{Family member working outside village/city/country}
#'   \item{\code{RM2}}{Number of HH member working outside}
#'   \item{\code{RM3A}}{Other village/city}
#'   \item{\code{RM3B}}{Other district}
#'   \item{\code{RM3C}}{Other province}
#'   \item{\code{RM3D}}{Overseas}
#'   \item{\code{RM3Z}}{DK}
#'   \item{\code{RM4}}{HH recieved any remittances during last year}
#'   \item{\code{RM5}}{Amount recevied inside the country during past year}
#'   \item{\code{RM5A}}{Amount recieved from overseas during the past year}
#'   \item{\code{PB1}}{Any HH member recieved any pension benefits during last year}
#'   \item{\code{PB2A}}{Source of penstion : Government}
#'   \item{\code{PB2B}}{Source of penstion : EOBI}
#'   \item{\code{PB2X}}{Source of penstion : Others}
#'   \item{\code{PB2Z}}{Source of penstion : DK}
#'   \item{\code{SN1}}{HH received any benifit from Government}
#'   \item{\code{SN2A}}{Zakat (Guzara allowance, Health care, Marriage grant, Training from VTI)}
#'   \item{\code{SN2B}}{Bait-ul-Maal}
#'   \item{\code{SN2C}}{Sasta Ration}
#'   \item{\code{SN2D}}{BISP}
#'   \item{\code{SN2E}}{Watan Card}
#'   \item{\code{SN2X}}{Other}
#'   \item{\code{SN2Z}}{DK}
#'   \item{\code{SN3}}{HH recieved any cash donations from Zakat or other means during past year}
#'   \item{\code{SN4}}{Amount received from Zakat during part year}
#'   \item{\code{SN6}}{HH purchased consumable items from utility store}
#'   \item{\code{SN7}}{Mode of purchase from utility store}
#'   \item{\code{SN8}}{Government initiatives are benifiting the low income groups}
#'   \item{\code{HW1}}{Place where household members most often wash their hands}
#'   \item{\code{HW2}}{Water available at the place for handwashing}
#'   \item{\code{HW3A}}{Soap or detergent present at place of handwashing}
#'   \item{\code{HW3BA}}{Bar soap}
#'   \item{\code{HW3BB}}{Detergent (Powder / Liquid / Paste)}
#'   \item{\code{HW3BC}}{Liquid soap}
#'   \item{\code{HW3BD}}{Ash / Mud / Sand}
#'   \item{\code{HW4}}{Soap/other material available for washing hands}
#'   \item{\code{HW5A}}{Soap/other material available for washing hands}
#'   \item{\code{HW5BA}}{Bar soap}
#'   \item{\code{HW5BB}}{Detergent (Powder / Liquid / Paste)}
#'   \item{\code{HW5BC}}{Liquid soap}
#'   \item{\code{HW5BD}}{Ash / Mud / Sand}
#'   \item{\code{SI1}}{Salt iodization test outcome}
#'   \item{\code{SI3}}{When buying salt, asking for Iodised salt with Handi Logo}
#'   \item{\code{SI4}}{Salt that provided for the test, bought in sealed package}
#'   \item{\code{SI5}}{Brand of Salt that provided for the test}
#'   \item{\code{SI6}}{Take a small sample of salt for further testing of Iodine content in the lab}
#'   \item{\code{SI7}}{Collect one cup of Salt and sealed in plastic bag provided}
#'   \item{\code{SI8CN}}{Salt sample ID - Cluster Number}
#'   \item{\code{SI8HH}}{Salt Sample ID - household number}
#'   \item{\code{HHSEX}}{Sex of household head}
#'   \item{\code{division}}{Division}
#'   \item{\code{hh7r}}{District}
#'   \item{\code{hh6r}}{Area}
#'   \item{\code{suburban}}{}
#'   \item{\code{SIRes}}{Result of HH interview among those selected for salt test sample}
#'   \item{\code{stratum}}{Straum (Original)}
#'   \item{\code{helevel}}{Education of household head}
#'   \item{\code{wscore}}{Combined wealth score}
#'   \item{\code{windex5}}{Wealth index quintile}
#'   \item{\code{wscoreu}}{Urban wealth score}
#'   \item{\code{windex5u}}{Urban wealth index quintile}
#'   \item{\code{wscorer}}{Rural wealth score}
#'   \item{\code{windex5r}}{Rural wealth index quintile}
#'   \item{\code{hhweight}}{Household sample weight}
#'   \item{\code{stweight}}{Salt testing's sample weight} 
#'   }
#'   
#' @author 
#'  \enumerate{
#'   \item{Muhammad Yaseen } {(\email{myaseen208@@gmail.com})}
#'   \item{Muhammad Usman } {(\email{usmann75@hotmail.com})}
#'   }
#'
#' @references \enumerate{
#' \item UNICEF MICS (\url{http://www.mics.unicef.org/surveys}).
#'  }
#'
#' @importFrom data.table data.table
#' 
#' @examples
#' data(MICS2014HH)
NULL
