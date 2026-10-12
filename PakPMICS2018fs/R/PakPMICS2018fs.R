#' @title Multiple Indicator Cluster Survey (MICS) 2018 Children Age 5-17 Questionnaire Data for Punjab, Pakistan
#' @name  PakPMICS2018fs
#' @docType data
#' @keywords datasets
#' @usage data(PakPMICS2018fs)
#' @description \code{PakPMICS2018fs} provides data set and function for exploration of Multiple Indicator Cluster Survey (MICS) 2017-18 Children Age 5-17 questionnaire data for Punjab, Pakistan. The results of the present survey are critically important for the purposes of SDG monitoring, as the survey produces information on 32 global SDG indicators. The data was collected from 53,840 households selected at the second stage with systematic random sampling out of a sample of 2,692 clusters selected using Probability Proportional to size sampling. Six questionnaires were used in the survey:
#' 1. a household questionnaire to collect basic demographic information on all de jure household members (usual residents), the household, and the dwelling;
#' 2. a water quality testing questionnaire administered in three households in each cluster of the sample;
#' 3. a questionnaire for individual women administered in each household to all women age 15-49 years;
#' 4. a questionnaire for individual men administered in every second household to all men age 15-49 years;
#' 5. an under-5 questionnaire, administered to mothers (or caretakers) of all children under 5 living in the household; and
#' 6. a questionnaire for children age 5-17 years, administered to the mother (or caretaker) of one randomly selected child age 5-17 years living in the household.
#' @format A data frame with 37052 rows and 276 variables:
#' \describe{
#'    \item{\code{HH1}}{Cluster number}
#'    \item{\code{HH2}}{Household number}
#'    \item{\code{LN}}{Line number}
#'    \item{\code{FS1}}{Cluster number}
#'    \item{\code{FS2}}{Household number}
#'    \item{\code{FS3}}{Child's line number}
#'    \item{\code{FSINT}}{Interviewer number}
#'    \item{\code{FS4}}{Mother / Caretaker's line number}
#'    \item{\code{FS5}}{Interviewer number}
#'    \item{\code{FS6}}{Supervisor?s number}
#'    \item{\code{FS7D}}{Day of interview}
#'    \item{\code{FS7M}}{Month of interview}
#'    \item{\code{FS7Y}}{Year of interview}
#'    \item{\code{FS9}}{First interview with this respondent}
#'    \item{\code{FS10}}{Consent}
#'    \item{\code{FS17}}{Result of interview for children 5-17}
#'    \item{\code{FS8H}}{Start of interview - Hour}
#'    \item{\code{FS8M}}{Start of interview - Minutes}
#'    \item{\code{FS11H}}{End of interview - Hour}
#'    \item{\code{FS11M}}{End of interview - Minutes}
#'    \item{\code{FS12}}{Language of the Questionnaire}
#'    \item{\code{FS13}}{Language of the Interview}
#'    \item{\code{FS14}}{Native language of the Respondent}
#'    \item{\code{FS15}}{Translator used}
#'    \item{\code{FSHINT}}{Household interviewer}
#'    \item{\code{CB2M}}{Month of birth of child}
#'    \item{\code{CB2Y}}{Year of birth of child}
#'    \item{\code{CB3}}{Age of child}
#'    \item{\code{CB4}}{Ever attended school or early childhood programme}
#'    \item{\code{CB5A}}{Highest level of education attended}
#'    \item{\code{CB5B}}{Highest grade attended at that level}
#'    \item{\code{CB6}}{Ever completed that grade/year}
#'    \item{\code{CB7}}{Attended school or early childhood programme during current school year}
#'    \item{\code{CB8A}}{Level of education attended current school year}
#'    \item{\code{CB8B}}{Grade of education attended current school year}
#'    \item{\code{CB9}}{Attended school or early childhood programme during previous school year}
#'    \item{\code{CB10A}}{Level of education attended previous school year}
#'    \item{\code{CB10B}}{Grade of education attended previous school year}
#'    \item{\code{CB11}}{Covered by any health insurance}
#'    \item{\code{CB12A}}{Health insurance - PUBLIC HEALTH INSURANCE}
#'    \item{\code{CB12B}}{Health insurance - Health insurance through employer}
#'    \item{\code{CB12C}}{Health insurance - Social security}
#'    \item{\code{CB12D}}{Health insurance - Other privately purchased commercial health insurance}
#'    \item{\code{CB12X}}{Health insurance - Other}
#'    \item{\code{CB12NR}}{Health insurance - MIssing}
#'    \item{\code{CL1A}}{Worked or helped on garden in past week}
#'    \item{\code{CL1B}}{Helped in family business in past week}
#'    \item{\code{CL1C}}{Produced or sold articles in past week}
#'    \item{\code{CL1X}}{Engaged in any other activity for income in past week}
#'    \item{\code{CL3}}{Hours worked in past week}
#'    \item{\code{CL4}}{Activities required carrying heavy loads}
#'    \item{\code{CL5}}{Activities required working with dangerous tools or heavy machinery}
#'    \item{\code{CL6A}}{Description of work:  exposed to dust, fumes, or gas}
#'    \item{\code{CL6B}}{Description of work:  exposed to extreme temperatures or humidity}
#'    \item{\code{CL6C}}{Description of work:  exposed to loud noise or vibration}
#'    \item{\code{CL6D}}{Description of work:  required to work at heights}
#'    \item{\code{CL6E}}{Description of work:  required to work with chemicals}
#'    \item{\code{CL6X}}{Description of work:  exposed to other}
#'    \item{\code{CL7}}{Fetched water in past week}
#'    \item{\code{CL8}}{Hours spent fetching water  in past week}
#'    \item{\code{CL9}}{Collected firewood  in past week}
#'    \item{\code{CL10}}{Hours spent collecting firewood  in past week}
#'    \item{\code{CL11A}}{Household chores in past week:  shopping}
#'    \item{\code{CL11B}}{Household chores in past week:  cooking}
#'    \item{\code{CL11C}}{Household chores in past week:  washing dishes  or cleaning around the house}
#'    \item{\code{CL11D}}{Household chores in past week:  washing/Iron clothes}
#'    \item{\code{CL11E}}{Household chores in past week:  caring for children}
#'    \item{\code{CL11F}}{Household chores in past week:  caring for old or sick}
#'    \item{\code{CL11X}}{Household chores in past week:  other household tasks}
#'    \item{\code{CL13}}{Number of hours engaged in past week}
#'    \item{\code{FCD2A}}{Took away privileges}
#'    \item{\code{FCD2B}}{Explained why behaviour was wrong}
#'    \item{\code{FCD2C}}{Shook child}
#'    \item{\code{FCD2D}}{Shouted, yelled or screamed at child}
#'    \item{\code{FCD2E}}{Gave child something else to do}
#'    \item{\code{FCD2F}}{Spanked, hit or slapped child on bottom with bare hand}
#'    \item{\code{FCD2G}}{Hit child on the bottom or elsewhere with belt, brush, stick, etc.}
#'    \item{\code{FCD2H}}{Called child dumb, lazy or another name}
#'    \item{\code{FCD2I}}{Hit or slapped child on the face, head or ears}
#'    \item{\code{FCD2J}}{Hit or slapped child on the hand, arm or leg}
#'    \item{\code{FCD2K}}{Beat child up as hard as one could}
#'    \item{\code{FCD5}}{Child needs to be physically punished to be brought up properly}
#'    \item{\code{FCF1}}{Child wear glasses or contact lenses}
#'    \item{\code{FCF2}}{Child uses hearing aid}
#'    \item{\code{FCF3}}{Child uses any equipment or receive assistance for walking}
#'    \item{\code{FCF6}}{Child has difficulty seeing}
#'    \item{\code{FCF8}}{Child has difficulty hearing sounds like people voices or music}
#'    \item{\code{FCF10}}{Without using equipment or assistance child has difficulty walking 100 meters}
#'    \item{\code{FCF11}}{Without using equipment or assistance child has difficulty walking 500 meters}
#'    \item{\code{FCF12}}{When using equipment or assistance child has difficulty walking 100 meters}
#'    \item{\code{FCF13}}{When using equipment or assistance child has difficulty walking 500 meters}
#'    \item{\code{FCF14}}{Compared with children of the same age, child has difficulty walking 100 meters}
#'    \item{\code{FCF15}}{Compared with children of the same age, child has difficulty walking 500 meters}
#'    \item{\code{FCF16}}{Child has difficulty with self-care such as feeding or dressing}
#'    \item{\code{FCF17}}{Child has difficulty being understood by people inside of this household}
#'    \item{\code{FCF18}}{Child has difficulty being understood by people outside of this household}
#'    \item{\code{FCF19}}{Compared with children of the same age, child has difficulty learning things}
#'    \item{\code{FCF20}}{Compared with children of the same age, child has difficulty remembering things}
#'    \item{\code{FCF21}}{Child has difficulty concentrating on an activity that he/she enjoys}
#'    \item{\code{FCF22}}{Child has difficulty accepting changes in his/her routine}
#'    \item{\code{FCF23}}{Compared with children of the same age, child have difficulty controlling his/her behaviour}
#'    \item{\code{FCF24}}{Child has difficulty making friends}
#'    \item{\code{FCF25}}{How often child seems very anxious, nervous or worried}
#'    \item{\code{FCF26}}{How often child seems very sad or depressed}
#'    \item{\code{PR3}}{Number of children's books or picture books for child}
#'    \item{\code{PR5}}{Child ever had homework}
#'    \item{\code{PR6}}{Anyone helps child with homework}
#'    \item{\code{PR7}}{School have a School Governing Body in which parents can participate}
#'    \item{\code{PR8}}{Attended a PTA / SMC meeting in the last 12 months}
#'    \item{\code{PR9A}}{At meeting - discussed a plan for addressing key education isses faced by child's school}
#'    \item{\code{PR9B}}{At meeting - discussed school budget or use of funds received by child's school}
#'    \item{\code{PR10}}{In the last 12 months - received a student or student report card}
#'    \item{\code{PR11A}}{In the last 12 months gone to school for - a school assembly, a school celebration or a sport event}
#'    \item{\code{PR11B}}{In the last 12 months gone to school for - to discuss child's progress with child's teachers}
#'    \item{\code{PR12A}}{In the last 12 months school closed due to - natural disasters}
#'    \item{\code{PR12B}}{In the last 12 months school closed due to - man made disasters}
#'    \item{\code{PR12C}}{In the last 12 months school closed due to - teacher strike or teacher absenteesim}
#'    \item{\code{PR12X}}{In the last 12 months school closed due to - any other reasons}
#'    \item{\code{PR13}}{In the last 12 months, child was unable to attend class due to teacher being absent}
#'    \item{\code{PR15}}{Contacted the school officials or school governing bodies when this happened}
#'    \item{\code{FL1}}{Consent}
#'    \item{\code{FL2H}}{Start of interview with child - Hour}
#'    \item{\code{FL2M}}{Start of interview with child - Minutes}
#'    \item{\code{FL3}}{Consent}
#'    \item{\code{FL4A}}{You are not alone with the child unless they are at least visible to an adult known to the child}
#'    \item{\code{FL4B}}{You have engaged the child in conversation and built rapport e.g. using an Icebreaker}
#'    \item{\code{FL4C}}{The child is sat comfortably, able to use the Reading & Numbers Book without difficulty while you can see which page is open}
#'    \item{\code{FL6A}}{child read books}
#'    \item{\code{FL6B}}{someone read books to child}
#'    \item{\code{FL7}}{Language child speaks most of the time at home}
#'    \item{\code{FL9}}{Language teacher use most of the time when teaching child in class}
#'    \item{\code{FL10}}{Like to read story}
#'    \item{\code{FL13}}{Giving the Child book - Manual introduction}
#'    \item{\code{FL14}}{Child read every word in the practice correctly}
#'    \item{\code{FL15}}{Reading done: How old is Mano?}
#'    \item{\code{FL16}}{Mano is 5 years old}
#'    \item{\code{FL17}}{Another question: Who is older: Mano or Motie?}
#'    \item{\code{FL18}}{Motie is older than Mano. Motie is 6 and Mano is 5.}
#'    \item{\code{FLINTRO}}{INTRO}
#'    \item{\code{FL19W1}}{Word1}
#'    \item{\code{FL19W2}}{Word2}
#'    \item{\code{FL19W3}}{Word3}
#'    \item{\code{FL19W4}}{Word4}
#'    \item{\code{FL19W5}}{Word5}
#'    \item{\code{FL19W6}}{Word6}
#'    \item{\code{FL19W7}}{Word7}
#'    \item{\code{FL19W8}}{Word8}
#'    \item{\code{FL19W9}}{Word9}
#'    \item{\code{FL19W10}}{Word10}
#'    \item{\code{FL19W11}}{Word11}
#'    \item{\code{FL19W12}}{Word12}
#'    \item{\code{FL19W13}}{Word13}
#'    \item{\code{FL19W14}}{Word14}
#'    \item{\code{FL19W15}}{Word15}
#'    \item{\code{FL19W16}}{Word16}
#'    \item{\code{FL19W17}}{Word17}
#'    \item{\code{FL19W18}}{Word18}
#'    \item{\code{FL19W19}}{Word19}
#'    \item{\code{FL19W20}}{Word20}
#'    \item{\code{FL19W21}}{Word21}
#'    \item{\code{FL19W22}}{Word22}
#'    \item{\code{FL19W23}}{Word23}
#'    \item{\code{FL19W24}}{Word24}
#'    \item{\code{FL19W25}}{Word25}
#'    \item{\code{FL19W26}}{Word26}
#'    \item{\code{FL19W27}}{Word27}
#'    \item{\code{FL19W28}}{Word28}
#'    \item{\code{FL19W29}}{Word29}
#'    \item{\code{FL19W30}}{Word30}
#'    \item{\code{FL19W31}}{Word31}
#'    \item{\code{FL19W32}}{Word32}
#'    \item{\code{FL19W33}}{Word33}
#'    \item{\code{FL19W34}}{Word34}
#'    \item{\code{FL19W35}}{Word35}
#'    \item{\code{FL19W36}}{Word36}
#'    \item{\code{FL19W37}}{Word37}
#'    \item{\code{FL19W38}}{Word38}
#'    \item{\code{FL19W39}}{Word39}
#'    \item{\code{FL19W40}}{Word40}
#'    \item{\code{FL19W41}}{Word41}
#'    \item{\code{FL19W42}}{Word42}
#'    \item{\code{FL19W43}}{Word43}
#'    \item{\code{FL19W44}}{Word44}
#'    \item{\code{FL19W45}}{Word45}
#'    \item{\code{FL19W46}}{Word46}
#'    \item{\code{FL19W47}}{Word47}
#'    \item{\code{FL19W48}}{Word48}
#'    \item{\code{FL19W49}}{Word49}
#'    \item{\code{FL19W50}}{Word50}
#'    \item{\code{FL19W51}}{Word51}
#'    \item{\code{FL19W52}}{Word52}
#'    \item{\code{FL19W53}}{Word53}
#'    \item{\code{FL19W54}}{Word54}
#'    \item{\code{FL19W55}}{Word55}
#'    \item{\code{FL19W56}}{Word56}
#'    \item{\code{FL19W57}}{Word57}
#'    \item{\code{FL19W58}}{Word58}
#'    \item{\code{FL19W59}}{Word59}
#'    \item{\code{FL19W60}}{Word60}
#'    \item{\code{FL19W61}}{Word61}
#'    \item{\code{FL19W62}}{Word62}
#'    \item{\code{FL19W63}}{Word63}
#'    \item{\code{FL19W64}}{Word64}
#'    \item{\code{FL19W65}}{Word65}
#'    \item{\code{FL19W66}}{Word66}
#'    \item{\code{FL19W67}}{Word67}
#'    \item{\code{FL19W68}}{Word68}
#'    \item{\code{FL19W69}}{Word69}
#'    \item{\code{FL19W70}}{Word70}
#'    \item{\code{FL19W71}}{Word71}
#'    \item{\code{FL19W72}}{Word72}
#'    \item{\code{FL20A}}{Total number of words attempted}
#'    \item{\code{FL20B}}{Total number of words incorrect or missed}
#'    \item{\code{FL21}}{How well did child read the story}
#'    \item{\code{FL22A}}{Story comprehension: 1 (What class is Amjad in)}
#'    \item{\code{FL22B}}{Story comprehension: 2 (What did Amjad see on the way home)}
#'    \item{\code{FL22C}}{Story comprehension: 3 (Why did Amjad start crying)}
#'    \item{\code{FL22D}}{Story comprehension: 4 (Where did Amjad fall (down))}
#'    \item{\code{FL22E}}{Story comprehension: 5 (Why was Amjad happy)}
#'    \item{\code{FL23A}}{Child recognizes symbol: 9}
#'    \item{\code{FL23B}}{Child recognizes symbol: 12}
#'    \item{\code{FL23C}}{Child recognizes symbol: 30}
#'    \item{\code{FL23D}}{Child recognizes symbol: 48}
#'    \item{\code{FL23E}}{Child recognizes symbol: 74}
#'    \item{\code{FL23F}}{Child recognizes symbol: 731}
#'    \item{\code{FL24A}}{Child identities bigger of two numbers: 7-5}
#'    \item{\code{FL24B}}{Child identities bigger of two numbers: 11-24}
#'    \item{\code{FL24C}}{Child identities bigger of two numbers: 58-49}
#'    \item{\code{FL24D}}{Child identities bigger of two numbers: 65-67}
#'    \item{\code{FL24E}}{Child identities bigger of two numbers: 146-154}
#'    \item{\code{FL25A}}{Child adds numbers correctly: 3+2}
#'    \item{\code{FL25B}}{Child adds numbers correctly: 8+6}
#'    \item{\code{FL25C}}{Child adds numbers correctly: 7+3}
#'    \item{\code{FL25D}}{Child adds numbers correctly: 13+6}
#'    \item{\code{FL25E}}{Child adds numbers correctly: 12+24}
#'    \item{\code{FL26}}{Manual intro}
#'    \item{\code{FL27A}}{Child identifies next number: 5-6-7-X}
#'    \item{\code{FL27B}}{Child identifies next number: 14-15-X-17}
#'    \item{\code{FL27C}}{Child identifies next number: 20-X-40-50}
#'    \item{\code{FL27D}}{Child identifies next number: 2-4-6-X}
#'    \item{\code{FL27E}}{Child identifies next number: 5-8-11-X}
#'    \item{\code{FL28}}{Result of interview with selected child (7-14 years)}
#'    \item{\code{HH4}}{Supervisor number}
#'    \item{\code{HH6}}{Area}
#'    \item{\code{HL4}}{Sex}
#'    \item{\code{ED5A}}{Highest level of education attended}
#'    \item{\code{ED5B}}{Highest grade attended at that level}
#'    \item{\code{FSAGE}}{Age}
#'    \item{\code{FSDOI}}{Date of interview child 5-17 (CMC)}
#'    \item{\code{FSDOB}}{Date of birth of child 5-17 (CMC)}
#'    \item{\code{HH56}}{Number of CF questionnaires completed (age 5-17 years)}
#'    \item{\code{fsweight}}{Children 5-17's sample weight}
#'    \item{\code{caretakerdis}}{Mother's functional disabilities (age 18-49 years)}
#'    \item{\code{wscore}}{Combined wealth score}
#'    \item{\code{windex5}}{Wealth index quintile}
#'    \item{\code{windex10}}{Percentile Group of com1}
#'    \item{\code{wscoreu}}{Urban wealth score}
#'    \item{\code{windex5u}}{Urban wealth index quintile}
#'    \item{\code{windex10u}}{Percentile Group of urb1}
#'    \item{\code{wscorer}}{Rural wealth score}
#'    \item{\code{windex5r}}{Rural wealth index quintile}
#'    \item{\code{windex10r}}{Percentile Group of rur1}
#'    \item{\code{division}}{Division}
#'    \item{\code{majorcity}}{majorcity}
#'    \item{\code{hh6r}}{Area}
#'    \item{\code{suburban}}{ }
#'    \item{\code{hh7}}{Districts}
#'    \item{\code{fselevel}}{Child's education}
#'    \item{\code{fsinsurance}}{Health insurance}
#'    \item{\code{fsdisability}}{Functional difficulties}
#'    \item{\code{melevel}}{Mother's education}
#'    \item{\code{schage}}{Age at beginning of school year}
#'    \item{\code{HH52}}{Number of children age 5-17 years}
#'    \item{\code{fshweight}}{Children 5-17's household sample weight}
#'    \item{\code{stratum}}{stratum}
#'    \item{\code{psu}}{psu}
#'   }
#'
#' @author
#'  \enumerate{
#'   \item{Muhammad Yaseen } {(\email{myaseen208@@gmail.com})}
#'   \item{Muhammad Usman } {(\email{usmann75@hotmail.com})}
#'   }
#'
#' @references
#' \enumerate{
#'   \item UNICEF MICS (\url{http://www.mics.unicef.org/surveys}).
#'  }
#'
#' @import tibble
#'
#' @examples
#' data(PakPMICS2018fs)
NULL
