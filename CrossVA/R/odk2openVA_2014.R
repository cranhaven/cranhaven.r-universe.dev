#' Map VA records to InterVA5 and InSilico (with option data.type = "WHO2016").
#'
#' \code{odk2openVA_2014} transforms data collected with the 2014 WHO VA instrument
#'   (form id: va_who_2014_final10) to serve as the input to the InterVA5 and
#'   InSilicoVA alogrithms for coding cause of death.
#'
#' @param odk A dataframe, obtained from reading an ODK Briefcase
#'   export of records collected with the WHO questionnaire.
#' 
#' @param id_col A character string of the column name (in odk) with the
#' unique ID for each death.
#'
#' @examples
#' \dontrun{
#' record_f_name <- system.file("sample", "who2014_odk_export.csv", package = "CrossVA")
#' records <- read.csv(record_f_name, stringsAsFactors = FALSE)
#' output <- odk2openVA_2014(records)
#' }
#'
#' @importFrom stringi stri_endswith_fixed
#' 
#' @export
#'
odk2openVA_2014 <- function (odk, id_col = "meta.instanceID") {

    ## Input Data
    odkNames <- tolower(names(odk))

    ## Output Variables
    whoNames <- c("id3A280", "id3A280", "id1A110", "id1A110", "id1A220",
      "id1A220", "id1A220", "id1A220", "id1A220", "id1A220", "id1A220",
      "id1A220", "id1A220", "id1A220", "id1A220", "id1A220", "id1A220",
      "id1A220", "id1A600", "id3E100", "id3E115", NA, "id3E310", "id3E510",
      "id3E320", "id3E340", "id3E400", "id3E112", "id3E520", "id3E104",
      "id3E106", "id3E108", "id3E111", "id3E500", "id3E530", "id3E330",
      "id3E113", "id3E102", "id3D285", "id3D290", "id3D292", "id3D294",
      "id3D296", "id3D298", "id3D299", "id3D300", NA, "id3D310", "id3D320",
      "id3D325", "id3D330", "id3A300", "id3A300", "id3A310", "id3A100",
      "id3A110", "id3A120", "id3A130", "id3A135", "id3A140", "id3A150",
      "id3A160", "id3A170", "id3A180", "id3A190", "id3A200", "id3A210",
      "id3A220", "id3A230", "id3A240", "id3A250", "id3A260", "id3A270",
      "id3B100", "id3B110", "id3B110", "id3B110", NA, "id3B115", NA, "id3B120",
      "id3B130", "id3B140", "id3B140", "id3B150", "id3B155", "id3B160",
      "id3B170", "id3B242", "id3B244", "id3B246", "id3B190", "id3B200",
      "id3B200", "id3B210", "id3B220", "id3B220", "id3B230", "id3B240",
      "id3B250", "id3B260", NA, "id3B270", "id3B272", "id3B274", "id3B280",
      "id3B290", "id3B290", "id3B290", "id3B292", "id3B294", "id3B296",
      "id3B300", "id3B305", "id3B310", NA, "id3B315", "id3B320", "id3B325",
      "id3B330", NA, "id3B340", "id3B350", "id3B350", "id3B355", "id3B355",
      "id3B360", "id3B370", "id3B370", "id3B375", "id3B380", "id3B390",
      "id3B390", "id3B400", "id3B405", "id3B407", "id3B407", "id3B409",
      "id3B410", "id3B420", "id3B430", NA, "id3B440", "id3B445", "id3B450",
      "id3B455", "id3B460", "id3B465", "id3B470", "id3B470", "id3B480",
      "id3B490", "id3B500", "id3B510", "id3B520", "id3B530", "id3B535",
      "id3B537", "id3B542", "id3B544", "id3B546", "id3B560", "id3B570",
      "id3B570", "id3B575", "id3B575", "id3B575", "id3B575", "id3B580",
      "id3B590", "id3B592", "id3B594", NA, "id3B596", "id3B600", "id3B610",
      "id3B620", "id3B630", "id3B640", "id3B650", "id3B652", "id3B656",
      "id3B658", "id3B660", "id3B665", "id3B670", "id3B680", "id3B690",
      "id3B700", "id3B710", "id3B724", "id3B730", "id3B731", "id3B731",
      "id3B731", "id3B731", "id3B731", "id3B731", "id3B731", "id3B732",
      "id3B734", "id3B740", "id3B740", "id3B745", "id3B750", "id3B755",
      "id3B760", "id3B770", "id3B780", "id3B790", "id3D340", NA, "id3D345",
      "id3D350", "id3D360", "id3D370", "id3D380", "id3D390", "id3D400", NA,
      "id3D410", "id3D420", "id3D430", NA, "id3D435", "id3D440", "id3D445",
      "id3D450", "id3D455", "id3B720", "id3B722", "id3B798", NA, "id3B800",
      "id3B810", "id3B820", "id3B830", "id3B840", "id3B850", "id3C105",
      "id3C110", "id3C120", "id3C125", "id3C135", "id3C210", "id3C212",
      "id3C200", "id3C120", "id3C213", "id3C215", "id3C220", "id3C230",
      "id3C230", "id3C240", "id3C260", "id3C270", "id3C280", "id3C290",
      "id3C310", "id3C320", "id3C330", "id3C340", "id3C350", "id3C360",
      "id3C365", "id3C370", "id3C380", "id3C390", "id3C393", "id3C395",
      "id3C400", "id3C400", "id3C400", "id3C430", "id3C440", "id3C450",
      "id3C460", "id3C470", "id3C480", "id3D100", "id3D102", "id3D104",
      "id3D106", "id3D108a", "id3D155", "id3D155", "id3D155", "id3D165",
      "id3D180", "id3D190", "id3D201", "id3D200", "id3D210", "id3D210",
      "id3D210", "id3D215", "id3D221", "id3D230", "id3D240", "id3D241",
      "id3D242", NA, "id3D251", NA, "id3D253", "id3D254", NA, "id3D258",
      "id3D259", "id3D260", "id3D261", "id3D265", "id3D267", "id3D267", NA,
      "id3D269", NA, "id3D271", "id3D273", "id3D275", NA, "id3D276", "id3D277",
      "id3D278", NA, "id3D280", "id3D600", "id3F100", "id3F110", "id3F120",
      "id3F120", "id3F130", "id3G110", "id3G120", "id3G130", "id3G140",
      "id3G150", "id3G160", "id3G165", "id3G170", "id3G180", "id3G190",
      "id3H100", "id4A100", "id4A110", "id4A120", "id4A130", "id4A140",
      "id4A150", "id4A160", "id4A170", "id4A180", "id4A190")
    whoNames <- tolower(whoNames)

    iv5Names <- c("i004a", "i004b", "i019a", "i019b", "i022a", "i022b", "i022c", "i022d",
                  "i022e", "i022f", "i022g", "i022h", "i022i", "i022j", "i022k", "i022l",
                  "i022m", "i022n", "i059o", "i077o", "i079o", "i082o", "i083o", "i084o",
                  "i085o", "i086o", "i087o", "i089o", "i090o", "i091o", "i092o", "i093o",
                  "i094o", "i095o", "i096o", "i098o", "i099o", "i100o", "i104o", "i105o",
                  "i106a", "i107o", "i108a", "i109o", "i110o", "i111o", "i112o", "i113o",
                  "i114o", "i115o", "i116o", "i120a", "i120b", "i123o", "i125o", "i127o",
                  "i128o", "i129o", "i130o", "i131o", "i132o", "i133o", "i134o", "i135o",
                  "i136o", "i137o", "i138o", "i139o", "i140o", "i141o", "i142o", "i143o",
                  "i144o", "i147o", "i148a", "i148b", "i148c", "i149o", "i150a", "i151a",
                  "i152o", "i153o", "i154a", "i154b", "i155o", "i156o", "i157o", "i158o",
                  "i159o", "i161a", "i165a", "i166o", "i167a", "i167b", "i168o", "i169a",
                  "i169b", "i170o", "i171o", "i172o", "i173a", "i174o", "i175o", "i176a",
                  "i178a", "i181o", "i182a", "i182b", "i182c", "i183a", "i184a", "i185o",
                  "i186o", "i187o", "i188o", "i189o", "i190o", "i191o", "i192o", "i193o",
                  "i194o", "i195o", "i197a", "i197b", "i199a", "i199b", "i200o", "i201a",
                  "i201b", "i203a", "i204o", "i205a", "i205b", "i207o", "i208o", "i209a",
                  "i209b", "i210o", "i211a", "i212o", "i213o", "i214o", "i215o", "i216a",
                  "i217o", "i218o", "i219o", "i220o", "i221a", "i221b", "i222o", "i223o",
                  "i224o", "i225o", "i226o", "i227o", "i228o", "i229o", "i230o", "i231o",
                  "i232a", "i233o", "i234a", "i234b", "i235a", "i235b", "i235c", "i235d",
                  "i236o", "i237o", "i238o", "i239o", "i240o", "i241o", "i242o", "i243o",
                  "i244o", "i245o", "i246o", "i247o", "i248a", "i249o", "i250a", "i251o",
                  "i252o", "i253o", "i254o", "i255o", "i256o", "i257o", "i258o", "i259o",
                  "i260a", "i260b", "i260c", "i260d", "i260e", "i260f", "i260g", "i261o",
                  "i262a", "i263a", "i263b", "i264o", "i265o", "i266a", "i267o", "i268o",
                  "i269o", "i270o", "i271o", "i272o", "i273o", "i274a", "i275o", "i276o",
                  "i277o", "i278o", "i279o", "i281o", "i282o", "i283o", "i284o", "i285a",
                  "i286o", "i287o", "i288o", "i289o", "i290o", "i294o", "i295o", "i296o",
                  "i297o", "i298o", "i299o", "i300o", "i301o", "i302o", "i303a", "i304o",
                  "i305o", "i306o", "i309o", "i310o", "i312o", "i313o", "i314o", "i315o",
                  "i316o", "i317o", "i318o", "i319a", "i319b", "i320o", "i321o", "i322o",
                  "i323o", "i324o", "i325o", "i326o", "i327o", "i328o", "i329o", "i330o",
                  "i331o", "i332a", "i333o", "i334o", "i335o", "i336o", "i337a", "i337b",
                  "i337c", "i338o", "i340o", "i342o", "i343o", "i344o", "i347o", "i354o",
                  "i355a", "i356o", "i357o", "i358a", "i360a", "i360b", "i360c", "i361o",
                  "i362o", "i363o", "i364o", "i365o", "i367a", "i367b", "i367c", "i368o",
                  "i369o", "i370o", "i371o", "i372o", "i373o", "i376o", "i377o", "i382a",
                  "i383o", "i384o", "i385a", "i387o", "i388o", "i389o", "i391o", "i393o",
                  "i394a", "i394b", "i395o", "i396o", "i397o", "i398o", "i399o", "i400o",
                  "i401o", "i402o", "i403o", "i404o", "i405o", "i406o", "i408o", "i411o",
                  "i412o", "i413o", "i414a", "i415a", "i418o", "i419o", "i420o", "i421o",
                  "i422o", "i423o", "i424o", "i425o", "i426o", "i427o", "i428o", "i450o",
                  "i451o", "i452o", "i453o", "i454o", "i455o", "i456o", "i457o", "i458o",
                  "i459o")

    iv5Out <- matrix(".", nrow=nrow(odk), ncol=353)

    # check for missing indicators
    tmpMat <- matrix(sapply(whoNames, stri_endswith_fixed, str = odkNames), nrow = length(odkNames))
    indexData <- apply(tmpMat, 2, which)
    warnZeroMatch <- which(sapply(indexData, length) == 0)
    warnZeroMatch <- warnZeroMatch[!is.na(whoNames[warnZeroMatch])] # remove NA's
    if (length(warnZeroMatch) > 0) {
        cat(
            paste("Expecting indicator(s) with name(s): ",
                  whoNames[unique(warnZeroMatch)],
                  sep = ""),
            sep = "\n"
        )
        stop("Problem with data: please add above columns to your data frame")
    }

    numNA <- 0        # used for tracking columns with NAs
    indexNA <- NULL
    flagNonNumeric <- function(x) {
        return(tryCatch(as.numeric(x),
                        error = function(c) -9999,
                        warning = function(c) -9999)
               )
    }

    # function for creating simple Y/N indicators
    qYesNo <- c(20, 21, 23:25, 28:35, 37:40, 42, 44:46, 48:51, 54:74, 81, 82,
                85:89, 92, 95, 98:100, 103, 106, 112:115, 118:120, 122, 127,
                131, 134, 135, 138, 140, 143, 145:148, 151:160, 162, 169:172,
                174:180, 182, 184:192, 200, 204, 205, 207:211, 213, 215:219,
                221:223, 225:232, 234:238, 240:242, 244:247, 249:251, 254:265,
                267:270, 274:280, 282, 288:290, 292, 296:301, 303, 305, 306,
                308:312, 316, 318:320, 322:324, 326:329, 333:353)

    tmpMat <- matrix(
        sapply(
            whoNames[qYesNo],
            stri_endswith_fixed,
            str = odkNames
        ),
        nrow = length(odkNames)
    )
    indexData <- apply(tmpMat, 2, which)
     if (is.list(indexData)) {
        dups <- lapply(indexData, function(x) length(x) > 1)
        tmpNames <- whoNames[qYesNo]
        cat(
            paste("Duplicate column names containing:",
                  tmpNames[unlist(dups)],
                  sep = " "),
            sep = "\n"
        )
        stop("Problem with data: please remove or rename one of the duplicate columns.")
    }
    iv5Out[ , qYesNo] <- as.matrix(odk[ , indexData])
    iv5Out[iv5Out=="yes" | iv5Out=="Yes" | iv5Out=="YES"] <- "y"
    iv5Out[iv5Out=="no" | iv5Out=="No" | iv5Out=="NO"] <- "n"
    iv5Out[iv5Out=="dk" | iv5Out=="DK" | iv5Out=="Doesn't Know"] <- "."
    iv5Out[iv5Out=="Doesn't know" | iv5Out=="doesn't know"] <- "."
    iv5Out[iv5Out=="does not know" | iv5Out=="Does Not Know" | iv5Out=="Does not know"] <- "."
    iv5Out[iv5Out=="ref" | iv5Out=="Ref" | iv5Out=="REF"] <- "."
    iv5Out[iv5Out==""] <- "."
    iv5Out[is.na(iv5Out)] <- "."

    # Step through iv5 indicators to create new values
    #1) Did s(he) die during the wet season? d wet & 2) Did s(he) die during the dry season? d dry
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[1]))
    ## table(odk[ , indexData])
    iv5Out[tolower(odk[ , indexData])=="wet" | tolower(odk[ , indexData])=="wet season", 1] <- "y" ## wet season
    iv5Out[tolower(odk[ , indexData])=="dry" | tolower(odk[ , indexData])=="dry season", 1] <- "n"

    iv5Out[tolower(odk[ , indexData])=="wet" | tolower(odk[ , indexData])=="wet season", 2] <- "n" ## dry season
    iv5Out[tolower(odk[ , indexData])=="dry" | tolower(odk[ , indexData])=="dry season", 2] <- "y"

    #3) Was he male? male & ##4) Was he female? female
    indexData_sex <- which(stri_endswith_fixed(odkNames, whoNames[3]))
    iv5Out[tolower(odk[ , indexData_sex])=="male",   3] <- "y"
    iv5Out[tolower(odk[ , indexData_sex])=="female", 3] <- "n"

    iv5Out[tolower(odk[ , indexData_sex])=="male",   4] <- "n"
    iv5Out[tolower(odk[ , indexData_sex])=="female", 4] <- "y"

    # age
    indexData1y <- which(stri_endswith_fixed(odkNames, "ageinyears"))
    nonNumeric <- lapply(odk[ , indexData1y], flagNonNumeric)
    nonNumeric <- unlist(nonNumeric)
    containsNA <- ifelse(
        sum(nonNumeric < 0, na.rm = TRUE) > 0,
        1, 0)
    numNA <- numNA + containsNA
    if (containsNA > 0) indexNA <- c(indexNA, odkNames[indexData1y])
    nonNumeric[nonNumeric < 0] <- NA
    odk[is.na(nonNumeric), indexData1y] <- NA
    odk[, indexData1y] <- as.numeric(odk[, indexData1y])

    indexData1m <- which(stri_endswith_fixed(odkNames, "ageinmonths"))
    nonNumeric <- lapply(odk[ , indexData1m], flagNonNumeric)
    nonNumeric <- unlist(nonNumeric)
    containsNA <- ifelse(
        sum(nonNumeric < 0, na.rm = TRUE) > 0,
        1, 0)
    numNA <- numNA + containsNA
    if (containsNA > 0) indexNA <- c(indexNA, odkNames[indexData1m])
    nonNumeric[nonNumeric < 0] <- NA
    odk[is.na(nonNumeric), indexData1m] <- NA
    odk[, indexData1m] <- as.numeric(odk[, indexData1m])

    indexData1d <- which(stri_endswith_fixed(odkNames, "ageindays"))
    nonNumeric <- lapply(odk[ , indexData1d], flagNonNumeric)
    nonNumeric <- unlist(nonNumeric)
    containsNA <- ifelse(
        sum(nonNumeric < 0, na.rm = TRUE) > 0,
        1, 0)
    numNA <- numNA + containsNA
    if (containsNA > 0) indexNA <- c(indexNA, odkNames[indexData1d])
    nonNumeric[nonNumeric < 0] <- NA
    odk[is.na(nonNumeric), indexData1d] <- NA
    odk[, indexData1d] <- as.numeric(odk[, indexData1d])

    indexData2 <- which(stri_endswith_fixed(odkNames, "age_group"))

    indexData3 <- which(stri_endswith_fixed(odkNames, "age_adult"))
    nonNumeric <- lapply(odk[ , indexData3], flagNonNumeric)
    nonNumeric <- unlist(nonNumeric)
    containsNA <- ifelse(
        sum(nonNumeric < 0, na.rm = TRUE) > 0,
        1, 0)
    numNA <- numNA + containsNA
    if (containsNA > 0) indexNA <- c(indexNA, odkNames[indexData3])
    nonNumeric[nonNumeric < 0] <- NA
    odk[is.na(nonNumeric), indexData3] <- NA
    odk[, indexData3] <- as.numeric(odk[, indexData3])

    indexData4  <- which(stri_endswith_fixed(odkNames, "age_child_unit"))
    indexData4d <- which(stri_endswith_fixed(odkNames, "age_child_days"))
    nonNumeric <- lapply(odk[ , indexData4d], flagNonNumeric)
    nonNumeric <- unlist(nonNumeric)
    containsNA <- ifelse(
        sum(nonNumeric < 0, na.rm = TRUE) > 0,
        1, 0)
    numNA <- numNA + containsNA
    if (containsNA > 0) indexNA <- c(indexNA, odkNames[indexData4d])
    nonNumeric[nonNumeric < 0] <- NA
    odk[is.na(nonNumeric), indexData4d] <- NA
    odk[, indexData4d] <- as.numeric(odk[, indexData4d])

    indexData4m <- which(stri_endswith_fixed(odkNames, "age_child_months"))
    nonNumeric <- lapply(odk[ , indexData4m], flagNonNumeric)
    nonNumeric <- unlist(nonNumeric)
    containsNA <- ifelse(
        sum(nonNumeric < 0, na.rm = TRUE) > 0,
        1, 0)
    numNA <- numNA + containsNA
    if (containsNA > 0) indexNA <- c(indexNA, odkNames[indexData4m])
    nonNumeric[nonNumeric < 0] <- NA
    odk[is.na(nonNumeric), indexData4m] <- NA
    odk[, indexData4m] <- as.numeric(odk[, indexData4m])

    indexData4y <- which(stri_endswith_fixed(odkNames, "age_child_years"))
    nonNumeric <- lapply(odk[ , indexData4y], flagNonNumeric)
    nonNumeric <- unlist(nonNumeric)
    containsNA <- ifelse(
        sum(nonNumeric < 0, na.rm = TRUE) > 0,
        1, 0)
    numNA <- numNA + containsNA
    if (containsNA > 0) indexNA <- c(indexNA, odkNames[indexData4y])
    nonNumeric[nonNumeric < 0] <- NA
    odk[is.na(nonNumeric), indexData4y] <- NA
    odk[, indexData4y] <- as.numeric(odk[, indexData4y])

    indexData5d <- which(stri_endswith_fixed(odkNames, "age_neonate_days"))
    nonNumeric <- lapply(odk[ , indexData5d], flagNonNumeric)
    nonNumeric <- unlist(nonNumeric)
    containsNA <- ifelse(
        sum(nonNumeric < 0, na.rm = TRUE) > 0,
        1, 0)
    numNA <- numNA + containsNA
    if (containsNA > 0) indexNA <- c(indexNA, odkNames[indexData5d])
    nonNumeric[nonNumeric < 0] <- NA
    odk[is.na(nonNumeric), indexData5d] <- NA
    odk[, indexData5d] <- as.numeric(odk[, indexData5d])

    indexData5h <- which(stri_endswith_fixed(odkNames, "age_neonate_hours"))
    nonNumeric <- lapply(odk[ , indexData5h], flagNonNumeric)
    nonNumeric <- unlist(nonNumeric)
    containsNA <- ifelse(
        sum(nonNumeric < 0, na.rm = TRUE) > 0,
        1, 0)
    numNA <- numNA + containsNA
    if (containsNA > 0) indexNA <- c(indexNA, odkNames[indexData5h])
    nonNumeric[nonNumeric < 0] <- NA
    odk[is.na(nonNumeric), indexData5h] <- NA
    odk[, indexData5h] <- as.numeric(odk[, indexData5h])

    indexData5m <- which(stri_endswith_fixed(odkNames, "age_neonate_minutes"))
    nonNumeric <- lapply(odk[ , indexData5m], flagNonNumeric)
    nonNumeric <- unlist(nonNumeric)
    containsNA <- ifelse(
        sum(nonNumeric < 0, na.rm = TRUE) > 0,
        1, 0)
    numNA <- numNA + containsNA
    if (containsNA > 0) indexNA <- c(indexNA, odkNames[indexData5m])
    nonNumeric[nonNumeric < 0] <- NA
    odk[is.na(nonNumeric), indexData5m] <- NA
    odk[, indexData5m] <- as.numeric(odk[, indexData5m])

    indexData_isNeonatal <- which(stri_endswith_fixed(odkNames, "isneonatal"))
    indexData_isChild <- which(stri_endswith_fixed(odkNames, "ischild"))
    indexData_isAdult <- which(stri_endswith_fixed(odkNames, "isadult"))

    #5) Was s(he) aged 65 years or more at death? 65+
    iv5Out[odk[ , indexData1y]>=65, 5] <- "y"
    iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="adult" &
           odk[ , indexData3]>=65, 5] <- "y"

    #6) Was s(he) aged 50 to 64 years at death? 50 to 64
    iv5Out[odk[ , indexData1y]< 65 & odk[ , indexData1y]>=50, 6] <- "y"
    iv5Out[is.na(odk[ , indexData1y]) & odk[ ,indexData2]=="adult" &
           odk[ ,indexData3]< 65 & odk[ ,indexData3]>=50, 6] <- "y"

    #7) Was s(he) aged 15 to 49 years at death? 15-49
    iv5Out[odk[ , indexData1y]< 50 & odk[ , indexData1y]>=15, 7] <- "y"
    iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="adult" &
           odk[ , indexData3]< 50 & odk[ , indexData3]>=15, 7] <- "y"

    #8) Was s(he) aged 5-14 years at death? 5-14 (adult or child)
    iv5Out[odk[ , indexData1y]< 15 & odk[ , indexData1y]>= 5, 8] <- "y"
    iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="adult" &
           odk[ , indexData3]< 15 & odk[ , indexData3]>=5, 8] <- "y"
    iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="child" &
           odk[ , indexData4]=="days" & odk[ , indexData4d]< 15*365.25 &
           odk[ , indexData4d]>=5*365.25, 8] <- "y"
    iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="child" &
           odk[ , indexData4]=="months" & odk[ , indexData4m]< 15*12 &
           odk[ , indexData4m]>=5*12, 8] <- "y"
    iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="child" &
           odk[ , indexData4]=="years" & odk[ , indexData4y]< 15 &
           odk[ , indexData4y]>=5, 8] <- "y"

    #9) Was s(he) aged 1 to 4 years at death? 1 to 4 (child)
    iv5Out[odk[ , indexData1y]<  5 & odk[ , indexData1y]>=1, 9] <- "y"
    iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="child" &
           odk[ , indexData4]=="days" & odk[ , indexData4d]< 5*365.25 &
           odk[ , indexData4d]>=1*365.25, 9] <- "y"
    iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="child" &
           odk[ , indexData4]=="months" & odk[ , indexData4m]< 5*12 &
           odk[ , indexData4m]>=1*12, 9] <- "y"
    iv5Out[is.na(odk[ , indexData1y]) & odk[ , indexData2]=="child" &
           odk[ , indexData4]=="years" & odk[ , indexData4y]< 5 &
           odk[ , indexData4y]>=1, 9] <- "y"

    #10) Was s(he) aged 1 to 11 months at death? 1-11 months (child or neonate?)
    iv5Out[odk[ , indexData1d]< 365.25 & odk[ , indexData1d]>=28, 10] <- "y"
    iv5Out[is.na(odk[ , indexData1d]) & odk[ , indexData2]=="child" &
           odk[ , indexData4]=="days" & odk[ , indexData4d]< 365.25 &
           odk[ , indexData4d]>=28, 10] <- "y"
    iv5Out[is.na(odk[ , indexData1m]) & odk[ , indexData2]=="child" &
           odk[ , indexData4]=="months" & odk[ , indexData4m]< 12 &
           odk[ , indexData4m]>=1, 10] <- "y"

    #11) Was s(he) aged < 1 month (28 days) at death? 0 - 27 days (neonate)
    iv5Out[odk[ , indexData1d]< 28, 11] <- "y"
    iv5Out[is.na(odk[ , indexData1d]) & odk[ , indexData2]=="neonate", 11] <- "y"

    #12) Was s(he) a live baby who died within 24 hours of birth? day0 iv5Names[12]
    iv5Out[odk[ , indexData1d]< 1, 12] <- "y"
    ageDays <- odk[ , indexData5d]
    ageHours <- odk[ , indexData5h]/24
    ageHours[is.na(ageHours)] <- 0
    ageMinutes <- odk[ , indexData5m]/(24*12)
    ageMinutes[is.na(ageMinutes)] <- 0
    ageNeonate <- ageDays + ageHours + ageMinutes
    iv5Out[odk[ , indexData2]=="neonate" & !is.na(odk[ , indexData5d]) &
           ageNeonate< 1, 12] <- "y"

    #13) Was s(he) a baby who died between 24 and 48 hours of birth? day1 iv5Names[13]
    iv5Out[odk[ , indexData2]=="neonate" & !is.na(odk[ , indexData5d]) &
           ageNeonate< 2 & ageNeonate>=1, 13] <- "y"

    #14)  Was s(he) a baby who died more than 48 hours from birth, but within the first week? day2-6 iv5Names[14]
    iv5Out[odk[ , indexData2]=="neonate" & !is.na(odk[ , indexData5d]) &
           ageNeonate< 7 & ageNeonate>=2, 14] <- "y"

    #15) Was s(he) a baby who died after the first week, but within the first month? wk2-4 iv5Names[15]
    iv5Out[odk[ , indexData2]=="neonate" & !is.na(odk[ , indexData5d]) &
           ageNeonate< 28 & ageNeonate>=7, 15] <- "y"

    # Finish coding age (5-15) -- if only one age has "y", recode all others to "n"
    ## e.g., if age 65 == "y", then age 50-64 == "n" and age 15-49 == "n" etc.
    indexData6 <- iv5Out[ , 5:15] != "y"             ## identify elements in age columns that do not equal "y"
    ##indexData7 <- rowSums(iv5Out[ , 5:15] == "y")  ## identify with rows/records only have 1 "y" for all age columns
    indexData7 <- rowSums(iv5Out[ , 5:15] == "y", na.rm = TRUE)  ## identify which rows/records only have 1 "y" for all age columns
    ## Now recode all "n"
    iv5Out[indexData7 == 1, 5:15][ indexData6[indexData7 == 1, ] ] <- "n"
    ## extra steps for less than a month #11 & first few weeks #12 - #15
    y11_12 <- iv5Out[, 11] == "y" & iv5Out[, 12] == "y"
    y11_13 <- iv5Out[, 11] == "y" & iv5Out[, 13] == "y"
    y11_14 <- iv5Out[, 11] == "y" & iv5Out[, 14] == "y"
    y11_15 <- iv5Out[, 11] == "y" & iv5Out[, 15] == "y"
    
    iv5Out[indexData7 == 2 & y11_12, 5:15][ indexData6[indexData7 == 2 & y11_12, ] ] <- "n"
    iv5Out[indexData7 == 2 & y11_13, 5:15][ indexData6[indexData7 == 2 & y11_13, ] ] <- "n"
    iv5Out[indexData7 == 2 & y11_14, 5:15][ indexData6[indexData7 == 2 & y11_14, ] ] <- "n"
    iv5Out[indexData7 == 2 & y11_15, 5:15][ indexData6[indexData7 == 2 & y11_15, ] ] <- "n"

    #16) Was she a woman aged 12-19 years at death? f-19
    iv5Out[ , 16] <- ifelse(odk[ , indexData_sex]=="female" &
                            odk[ , indexData1y]< 20 &
                            odk[ , indexData1y]>= 12, "y", ".")
    iv5Out[odk[ , indexData_sex]=="female" & odk[ , indexData1y]< 12, 16] <- "n"
    iv5Out[odk[ , indexData_sex]=="female" & odk[ , indexData1y]> 19, 16] <- "n"

    iv5Out[odk[ , indexData_sex]=="female" & is.na(odk[ , indexData1y]) &
           odk[ , indexData2]=="adult" &
           odk[ , indexData3]< 20 & odk[ , indexData3]>=12, 16] <- "y"
    iv5Out[odk[ , indexData_sex]=="female" & is.na(odk[ , indexData1y]) &
           odk[ , indexData2]=="adult" & odk[ , indexData3]< 12, 16] <- "n"
    iv5Out[odk[ , indexData_sex]=="female" & is.na(odk[ , indexData1y]) &
           odk[ , indexData2]=="adult" & odk[ , indexData3]> 19, 16] <- "n"
    iv5Out[odk[ , indexData2]=="neonate", 16] <- "n"
    iv5Out[odk[ , indexData_isNeonatal] == 1, 16] <- "n"
    iv5Out[odk[ , indexData_isChild] == 1 & odk[ , indexData1y] < 12, 16] <- "n"
    iv5Out[odk[ , indexData_isChild] == 1 & odk[ , indexData4y] < 12, 16] <- "n"
    iv5Out[odk[ , indexData_isChild] == 1 &
           odk[ , indexData4m] < 12*12, 16] <- "n"
    iv5Out[odk[ , indexData_isChild] == 1 &
           odk[ , indexData4d] < 12*365.25, 16] <- "n"
    iv5Out[odk[ , indexData2] == "child" & odk[ , indexData1y] < 12, 16] <- "n"
    iv5Out[odk[ , indexData2] == "child" & odk[ , indexData4y] < 12, 16] <- "n"
    iv5Out[odk[ , indexData2] == "child" &
           odk[ , indexData4m] < 12*12, 16] <- "n"
    iv5Out[odk[ , indexData2] == "child" &
           odk[ , indexData4d] < 12*365.25, 16] <- "n"
    iv5Out[odk[ , indexData_sex]=="male", 16] <- "n"

    #17) Was she a woman aged 20-34 years at death? f20-34
    iv5Out[ , 17] <- ifelse(odk[ , indexData_sex]=="female" &
                            odk[ , indexData1y]< 35 &
                            odk[ , indexData1y]>= 20, "y", ".")
    iv5Out[odk[ , indexData_sex]=="female" & odk[ , indexData1y]< 20, 17] <-  "n"
    iv5Out[odk[ , indexData_sex]=="female" & odk[ , indexData1y]> 34, 17] <-  "n"

    iv5Out[odk[ , indexData_sex]=="female" & is.na(odk[ , indexData1y]) &
           odk[ , indexData2]=="adult" & odk[ , indexData3]< 35 &
           odk[ , indexData3]>=20, 17] <- "y"
    iv5Out[odk[ , indexData_sex]=="female" & is.na(odk[ , indexData1y]) &
           odk[ , indexData2]=="adult" & odk[ , indexData3]< 20, 17] <- "n"
    iv5Out[odk[ , indexData_sex]=="female" & is.na(odk[ , indexData1y]) &
           odk[ , indexData2]=="adult" & odk[ , indexData3]> 34, 17] <- "n"

    iv5Out[odk[ , indexData2]=="neonate", 17] <- "n"
    iv5Out[odk[ , indexData2]=="child", 17] <- "n"
    iv5Out[odk[ , indexData_isNeonatal] == 1, 17] <- "n"
    iv5Out[odk[ , indexData_isChild] == 1, 17] <- "n"
    iv5Out[odk[ , indexData_sex]=="male", 17] <- "n"

    #18) Was she a woman aged 35 to 49 years at death? f35-49
    iv5Out[ , 18] <- ifelse(odk[ , indexData_sex]=="female" &
                            odk[ , indexData1y]< 50 &
                            odk[ , indexData1y]>= 35, "y", ".")
    iv5Out[odk[ , indexData_sex]=="female" & odk[ , indexData1y]< 35, 18] <-  "n"
    iv5Out[odk[ , indexData_sex]=="female" & odk[ , indexData1y]> 49, 18] <-  "n"

    iv5Out[odk[ , indexData_sex]=="female" & is.na(odk[ , indexData1y]) &
           odk[ , indexData2]=="adult" & odk[ , indexData3]< 50 &
           odk[ , indexData3]>=35, 18] <- "y"
    iv5Out[odk[ , indexData_sex]=="female" & is.na(odk[ , indexData1y]) &
           odk[ , indexData2]=="adult" & odk[ , indexData3]< 35, 18] <- "n"
    iv5Out[odk[ , indexData_sex]=="female" & is.na(odk[ , indexData1y]) &
           odk[ , indexData2]=="adult" & odk[ , indexData3]> 49, 18] <- "n"

    iv5Out[odk[ , indexData2]=="neonate", 18] <- "n"
    iv5Out[odk[ , indexData2]=="child", 18] <- "n"
    iv5Out[odk[ , indexData_isNeonatal] == 1, 18] <- "n"
    iv5Out[odk[ , indexData_isChild] == 1, 18] <- "n"
    iv5Out[odk[ , indexData_sex]=="male", 18] <- "n"

    #19) Was she married at the time of death? married
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[19]))
    ## table(odk[ , indexData])
    iv5Out[odk[ , indexData_sex]=="female" & tolower(odk[ , indexData])=="married", 19] <- "y"
    iv5Out[odk[ , indexData_sex]=="female" & tolower(odk[ , indexData])=="single", 19] <- "n"
    iv5Out[odk[ , indexData_sex]=="female" & tolower(odk[ , indexData])=="partner", 19] <- "n"
    iv5Out[odk[ , indexData_sex]=="female" & tolower(odk[ , indexData])=="divorced", 19] <- "n"
    iv5Out[odk[ , indexData_sex]=="female" & tolower(odk[ , indexData])=="widowed", 19] <- "n"
    iv5Out[odk[ , indexData_sex]=="female" & tolower(odk[ , indexData])=="too_young_to_be_married", 19] <- "n"
    iv5Out[odk[ , indexData_sex]=="female" & tolower(odk[ , indexData])=="child", 19] <- "n"
    iv5Out[odk[ , indexData_sex]=="male", 19] <- "n"

    #22) Was (s)he injured in a non-road transport accident? inj nrta (NOT INCLUDED)
    #26) Was (s)he injured by the bite or sting of a venomous animal? inj venom
    indexData1 <- which(stri_endswith_fixed(odkNames, whoNames[26]))
    indexData2 <- which(stri_endswith_fixed(odkNames, "id3e400"))
    iv5Out[odk[ , indexData1] == "y" & odk[ , indexData2] == "snake", 26] <- "y"
    iv5Out[odk[ , indexData1] == "y" & odk[ , indexData2] == "insect_or_scorpion", 26] <- "y"

    #27) Was (s)he injured by an animal or insect (non-venomous) inj anim
    iv5Out[odk[ , indexData2] == "dog", 27] <- "y"

    #36) Was the injury accidental? inj accid
    indexData1 <- which(stri_endswith_fixed(odkNames, whoNames[36]))
    indexData2 <- which(stri_endswith_fixed(odkNames, "id3e335"))
    iv5Out[odk[ , indexData1] == "y" | odk[ , indexData2] == "y", 36] <- "y"

    #41) Was it more than 5 minutes after birth before the baby first cried? cry 5+m
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[41]))
    iv5Out[odk[ , indexData]> 5, 41] <- "y"
    iv5Out[odk[ , indexData]<=5, 41] <- "n"

    #43) Did the baby stop crying more than a day before (s)he died? cry st 1+d
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[43]))
    iv5Out[odk[ , indexData]> 24, 43] <- "y"
    iv5Out[odk[ , indexData]<=24, 43] <- "n"

    #47) Did the baby have a breathing problem?	brth prob (NOT INCLUDED)

    #52) Did the final illness last less than 3 weeks? ill <3w.
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[52]))
    iv5Out[odk[ , indexData]< 21, 52] <- "y"
    iv5Out[odk[ , indexData]>=21, 52] <- "n"

    #53) Did the final illness last at least 3 weeks? ill 3+w
    iv5Out[odk[ , indexData]>=21, 53] <- "y"
    iv5Out[odk[ , indexData]< 21, 53] <- "n"

    #75) Did the fever last less than a week before death? fev <1w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[75]))
    iv5Out[odk[ , indexData]< 7, 75] <- "y"
    iv5Out[odk[ , indexData]>=7, 75] <- "n"

    #76) Did the fever last at least one week, but less than 2 weeks before death? fev 1-2w
    iv5Out[odk[ , indexData]>= 7 & odk[ , indexData]< 14, 76] <- "y"
    iv5Out[odk[ , indexData]<  7,                         76] <- "n"

    #77) Did the fever last at least 2 weeks before death? fev 2+w
    iv5Out[odk[ , indexData]>= 14, 77] <- "y"
    iv5Out[odk[ , indexData]<  14, 77] <- "n"

    #78) Did the fever continue until death? fev death (NOT INCLUDED)

    #79) Was the fever severe? fev sev
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[79]))
    iv5Out[tolower(odk[ , indexData])=="severe",   79] <- "y"
    iv5Out[tolower(odk[ , indexData])=="mild",     79] <- "n"
    iv5Out[tolower(odk[ , indexData])=="moderate", 79] <- "n"

    #80) Was the fever continuous? fev cont (NOT INCLUDED)

    #83) Did the cough last less than 3 weeks before death? cou <3w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[83]))
    iv5Out[odk[ , indexData[1]]< 21, 83] <- "y"
    iv5Out[odk[ , indexData[1]]>=21, 83] <- "n"

    #84) Did the cough last at least 3 weeks before death? cou 3+w
    iv5Out[odk[ , indexData[1]]>=21, 84] <- "y"
    iv5Out[odk[ , indexData[1]]< 21, 84] <- "n"

    #90) Did the difficult breathing last for at least 3 days before death? dif br 3d
    indexDatad <- which(stri_endswith_fixed(odkNames, whoNames[90]))
    iv5Out[odk[ , indexDatad]>=3, 90] <- "y"
    iv5Out[odk[ , indexDatad]< 3, 90] <- "n"

    #91) Was the difficult breathing continuous during this period? dif br con
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[91]))
    iv5Out[tolower(odk[ , indexData])=="continuous", 91] <- "y"
    iv5Out[tolower(odk[ , indexData])=="on_and_off", 91] <- "n"

    #93) Did the fast breathing last for less than two weeks before death? br fs <2w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[93]))
    iv5Out[odk[ , indexData]< 14, 93] <- "y"
    iv5Out[odk[ , indexData]>=14, 93] <- "n"

    #94) Did the fast breathing last for at least 2 weeks before death? br fs 2+w
    iv5Out[odk[ , indexData]>=14, 94] <- "y"
    iv5Out[odk[ , indexData]< 14, 94] <- "n"

    #96) Did the breathlessness last for less than 2 weeks before death? brl <2w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[96]))
    iv5Out[odk[ , indexData]< 2, 96] <- "y"
    iv5Out[odk[ , indexData]>=2, 96] <- "n"

    #97) Did the breathlessness last for at least 2 weeks before death? brl 2+w
    iv5Out[odk[ , indexData]>=2, 97] <- "y"
    iv5Out[odk[ , indexData]< 2, 97] <- "n"

    #101) Did his/her breathing sound like wheezing or grunting? whz grun
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[101]))

    iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "grunting"), 101] <- "y"
    iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "stridor"),  101] <- "y"
    iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "wheezing"), 101] <- "y"

    iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "no"), 101] <- "n"

    #102) During the illness that led to death, did (s)he have chest pain? ch pain (NOT INCLUDED)

    #104) Did (s)he experience chest pain at least 3 days before death? chp 3d
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[104]))
    iv5Out[odk[ , indexData]>=3, 104] <- "y"
    iv5Out[odk[ , indexData]< 3, 104] <- "n"

    #105) Did the chest pain last for at least 30 minutes? chp 30m
    indexDatam <- which(stri_endswith_fixed(odkNames, whoNames[105]))
    iv5Out[odk[ , indexDatam]>=30, 105] <- "y"
    iv5Out[odk[ , indexDatam]< 30, 105] <- "n"

    #107) Did (s)he have diarrhoea for less than 2 weeks before death? drr <2w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[107]))
    iv5Out[odk[ , indexData]< 14, 107] <- "y"
    iv5Out[odk[ , indexData]>=14, 107] <- "n"

    #108) Did (s)he have diarrhoea for at least 2 weeks but less than 4 weeks before death? drr 2-4w
    iv5Out[odk[ , indexData]< 28 & odk[ , indexData]>=14, 108] <- "y"
    iv5Out[odk[ , indexData]< 14,                         108] <- "n"
    iv5Out[odk[ , indexData]>=28,                         108] <- "n"

    #109) Did (s)he have diarrhoea for at least 4 weeks before death? drr 4+w
    iv5Out[odk[ , indexData]>=28, 109] <- "y"
    iv5Out[odk[ , indexData]< 28, 109] <- "n"

    #110) Did the baby or child have at least 4 stools on the day that loose liquid stools were most frequent? 4+ stls
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[110]))
    iv5Out[odk[ , indexData]>= 4 & odk[ , indexData]< 999, 110] <- "y"
    iv5Out[odk[ , indexData]<  4, 110] <- "n"

    #111) Did the frequent loose or liquid stools start at least 3 days before death? drr 3d
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[111]))
    iv5Out[odk[ , indexData]>= 3 & odk[ , indexData]< 999, 111] <- "y"
    iv5Out[odk[ , indexData]<  3, 111] <- "n"

    #116)Did (s)he vomit in the week preceding the death? vom bd (NOT INCLUDED)

    #117) Did (s)he vomit for at least 3 days before death?	vom 3d
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[117]))
    iv5Out[odk[ , indexData]>= 3 & odk[ , indexData]< 999, 117] <- "y"
    iv5Out[odk[ , indexData]<  3, 117] <- "n"

    #121) Did (s)he have abdominal pain? abd pain (NOT INCLUDED)

    #123) Did (s)he have severe abdominal pain for less than 2 weeks before death? abd p <2w
    indexData  <- which(stri_endswith_fixed(odkNames, whoNames[123]))
    iv5Out[odk[ , indexData]< 14, 123] <- "y"
    iv5Out[odk[ , indexData]>=14, 123] <- "n"

    #124) Did (s)he have severe abdominal pain for at least 2 weeks before death? abd p 2+w
    iv5Out[odk[ , indexData]>=14, 124] <- "y"
    iv5Out[odk[ , indexData]< 14, 124] <- "n"

    #125) Was the pain in the upper abdomen? abd p up
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[125]))
    iv5Out[tolower(odk[ , indexData])=="upper_abdomen",       125] <- "y"
    iv5Out[tolower(odk[ , indexData])=="lower_abdomen",       125] <- "n"

    #126) Was the pain in the lower abdomen? abd p lo
    iv5Out[tolower(odk[ , indexData])=="upper_abdomen",       126] <- "n"
    iv5Out[tolower(odk[ , indexData])=="lower_abdomen",       126] <- "y"

    #128) Did (s)he have a more than usually protruding abdomen for less than 2 weeks before death? abd pr <2w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[128]))
    iv5Out[odk[ , indexData]< 14, 128] <- "y"
    iv5Out[odk[ , indexData]>=14, 128] <- "n"

    #129) Did (s)he have a more than usually protruding abdomen for at least 2 weeks before death? abd pr 2+w
    iv5Out[odk[ , indexData]>=14, 129] <- "y"
    iv5Out[odk[ , indexData]< 14, 129] <- "n"

    #130) Did (s)he develop the protruding abdomen rapidly? abd pr rap
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[130]))
    iv5Out[tolower(odk[ , indexData])=="rapidly", 130] <- "y"
    iv5Out[tolower(odk[ , indexData])=="slowly",  130] <- "n"

    #132) Did (s)he have a mass in the abdomen for less than 2 weeks before death? ab ms <2w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[132]))
    iv5Out[odk[ , indexData]< 14, 132] <- "y"
    iv5Out[odk[ , indexData]>=14, 132] <- "n"

    #133) Did (s)he have a mass in the abdomen for at least 2 weeks before death? ab ms 2+w
    iv5Out[odk[ , indexData]< 14, 133] <- "n"
    iv5Out[odk[ , indexData]>=14, 133] <- "y"

    #136) Did (s)he have a stiff neck for less than one week before death? st n <1w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[136]))
    iv5Out[odk[ , indexData]< 7, 136] <- "y"
    iv5Out[odk[ , indexData]>=7, 136] <- "n"

    #137) Did (s)he have a stiff neck for at least one week before death? st n 1+w
    iv5Out[odk[ , indexData]>=7, 137] <- "y"
    iv5Out[odk[ , indexData]< 7, 137] <- "n"

    #139) Did (s)he have a painful neck for at least one week before death? pa n 1+w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[139]))
    iv5Out[odk[ , indexData]>=7, 139] <- "y"
    iv5Out[odk[ , indexData]< 7, 139] <- "n"

    #141) Did (s)he have mental confusion for at least 3 months before death? menc 3+m
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[141]))
    iv5Out[odk[ , indexData]>=3, 141] <- "y"
    iv5Out[odk[ , indexData]< 3, 141] <- "n"

    #142) Did (s)he have mental confusion for at least 3 months before death? menc 3+m (NOT INCLUDED)

    #144) Was (s)he unsconscious for at least 6 hours before death?	unc 6+h
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[144]))
    iv5Out[odk[ , indexData]>=6, 144] <- "y"
    iv5Out[odk[ , indexData]< 6, 144] <- "n"

    #149) Did the convulsions last for less than 10 minutes?	conv <10m
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[149]))
    iv5Out[odk[ , indexData]< 10, 149] <- "y"
    iv5Out[odk[ , indexData]>=10, 149] <- "n"

    #150) Did the convulsions last for at least 10 minutes?	conv 10+m
    iv5Out[odk[ , indexData]< 10, 150] <- "n"
    iv5Out[odk[ , indexData]>=10, 150] <- "y"

    #161) Did the ulcer ooze pus for at least 2 weeks?	sk ul 2+w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[161]))
    iv5Out[odk[ , indexData]>=14, 161] <- "y"
    iv5Out[odk[ , indexData]< 14, 161] <- "n"

    #163) Did (s)he have the skin rash for less than one week?	sk ra <1w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[163]))
    iv5Out[odk[ , indexData]< 7, 163] <- "y"
    iv5Out[odk[ , indexData]>=7, 163] <- "n"

    #164) Did (s)he have the skin rash for at least one week?	sk ra 1+w
    iv5Out[odk[ , indexData]>=7, 164] <- "y"
    iv5Out[odk[ , indexData]< 7, 164] <- "n"

    #165) Did (s)he have a rash on the face?	sk ra face
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[165]))
    iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "face"),                165] <- "y"
    iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), negate = TRUE, "face"), 165] <- "n"
    iv5Out[tolower(odk[ , indexData])=="",                                         165] <- "."

    #166) Did (s)he have a rash on the trunk or abdomen?	sk ra abd
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[166]))
    iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "abdomen|trunk"),                166] <- "y"
    iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), negate = TRUE, "abdomen|trunk"), 166] <- "n"
    iv5Out[tolower(odk[ , indexData])=="",                                                166] <- "."

    #167) Did (s)he have a rash on the extremities?	sk ra ext
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[167]))
    iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "extremities"),                167] <- "y"
    iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), negate = TRUE, "extremities"), 167] <- "n"
    iv5Out[tolower(odk[ , indexData])=="",                                                167] <- "."

    #168) Did (s)he have a rash everywhere?	sk ra all
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[168]))
    iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), "everywhere"),                168] <- "y"
    iv5Out[stri_endswith_fixed(tolower(odk[ , indexData]), negate = TRUE, "everywhere"), 168] <- "n"
    iv5Out[tolower(odk[ , indexData])=="",                                               168] <- "."

    #173) During the illness that led to death, did he/she have areas of the skin with redness and swelling? sk red
    #     (NOT INCLUDED)

    #181) Did (s)he have puffiness of the face for at least one week before death?	sw p f 1+w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[181]))
    iv5Out[odk[ , indexData]>=7, 181] <- "y"
    iv5Out[odk[ , indexData]< 7, 181] <- "n"

    #183) Did the swelling last for at least 3 days before death?	sw lf 3+d
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[183]))
    iv5Out[odk[ , indexData]>=3, 183] <- "y"
    iv5Out[odk[ , indexData]< 3, 183] <- "n"

    #193) Was only the right side of the body paralysed?	par rs
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[193]))
    iv5Out[tolower(odk[ , indexData])=="right_side", 193] <- "y"
    iv5Out[tolower(odk[ , indexData])!="right_side", 193] <- "n"
    iv5Out[tolower(odk[ , indexData])=="",           193] <- "."

    #194) Was only the left side of the body paralysed?	par ls
    iv5Out[tolower(odk[ , indexData])=="left_side", 194] <- "y"
    iv5Out[tolower(odk[ , indexData])!="left_side", 194] <- "n"
    iv5Out[tolower(odk[ , indexData])=="",          194] <- "."

    #195) Was only the lower part of the body paralysed?	par lo
    iv5Out[tolower(odk[ , indexData])=="lower_part_of_body", 195] <- "y"
    iv5Out[tolower(odk[ , indexData])!="lower_part_of_body", 195] <- "n"
    iv5Out[tolower(odk[ , indexData])=="",                   195] <- "."

    #196) Was only the upper part of the body paralysed?	par up
    iv5Out[tolower(odk[ , indexData])=="upper_part_of_body", 196] <- "y"
    iv5Out[tolower(odk[ , indexData])!="upper_part_of_body", 196] <- "n"
    iv5Out[tolower(odk[ , indexData])=="",                   196] <- "."

    #197) Was only one leg paralysed?	par leg
    iv5Out[tolower(odk[ , indexData])=="one_leg_only", 197] <- "y"
    iv5Out[tolower(odk[ , indexData])!="one_leg_only", 197] <- "n"
    iv5Out[tolower(odk[ , indexData])=="",             197] <- "."

    #198) Was only one arm paralysed?	par arm
    iv5Out[tolower(odk[ , indexData])=="one_arm_only", 198] <- "y"
    iv5Out[tolower(odk[ , indexData])!="one_arm_only", 198] <- "n"
    iv5Out[tolower(odk[ , indexData])=="",             198] <- "."

    #199) Was the entire body paralysed?	par all
    iv5Out[tolower(odk[ , indexData])=="whole_body", 199] <- "y"
    iv5Out[tolower(odk[ , indexData])!="whole_body", 199] <- "n"
    iv5Out[tolower(odk[ , indexData])=="",           199] <- "."

    #201) Did (s)he have difficulty swallowing for at least one week before death?	swa 1+w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[201]))
    iv5Out[odk[ , indexData]>=7, 201] <- "y"
    iv5Out[odk[ , indexData]< 7, 201] <- "n"

    #202) Did (s)he have difficulty with swallowing solids?	swa sol
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[202]))
    iv5Out[tolower(odk[ , indexData])=="solids",  202] <- "y"
    iv5Out[tolower(odk[ , indexData])=="both",    202] <- "y"
    iv5Out[tolower(odk[ , indexData])=="liquids", 202] <- "n"

    #203) Did (s)he have difficulty with swallowing liquids?	swa liq
    iv5Out[tolower(odk[ , indexData])=="solids",  203] <- "n"
    iv5Out[tolower(odk[ , indexData])=="both",    203] <- "y"
    iv5Out[tolower(odk[ , indexData])=="liquids", 203] <- "y"

    #206) Did (s)he have the yellow discolouration for at least 3 weeks before death?	yell 3+w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[206]))
    iv5Out[odk[ , indexData]>=21, 206] <- "y"
    iv5Out[odk[ , indexData]< 21, 206] <- "n"

    #212) Did the baby ever suckle in a normal way? suck ev (NOT INCLUDED)

    #214) Did the baby stop suckling on the 2nd day of life or later?	suck st d1
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[214]))
    iv5Out[odk[ , indexData[1]]>=2, 214] <-  "y"
    iv5Out[odk[ , indexData[1]]< 2, 214] <-  "n"

    #220) During the illness that led to death, did the baby become unresponsive or unconscious? unresp
    #     (NOT INCLUDED)

    #224) Was the baby more than 3 days old when it started feeling cold to touch?	cold 3+d
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[224]))
    iv5Out[odk[ , indexData]> 3, 224] <- "y"
    iv5Out[odk[ , indexData]<=3, 224] <- "n"

    #224) Was the baby more than 3 days old when it started feeling cold to touch? cold 3+d (NOT INCLUDED)

    #233) During the illness that led to death, did she have excessive vaginal bleeding in between menstrual periods? men betw
    #     (NOT INCLUDED)

    #239) Had her period been overdue for at least 4 weeks?	men l 4+w
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[239]))
    iv5Out[odk[ , indexData]>=4, 239] <- "y"
    iv5Out[odk[ , indexData]< 4, 239] <- "n"

    #243) Was she, or had she been, pregnant for less than 6 months when she died?	d <6m pr
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[243]))
    iv5Out[odk[ , indexData]< 6, 243] <- "y"
    iv5Out[odk[ , indexData]>=6, 243] <- "n"

    #248) Did she die within 6 weeks of childbirth? d <6w chb
    indexData_a <- which(stri_endswith_fixed(odkNames, whoNames[248]))
    indexData_b <- which(stri_endswith_fixed(odkNames, "id3c212"))
    iv5Out[odk[ , indexData_a] == "y" & odk[ , indexData_b] == "y", 248] <- "y"
    iv5Out[odk[ , indexData_a] == "n" | odk[ , indexData_b] == "n", 248] <- "n"

    #252) Did she die during or after her first pregnancy?	1st pr.
    #     WHO question is "How many births, including stillbirths, did she/the mother have before this baby?"
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[252]))
    iv5Out[odk[ , indexData]==0, 252] <-  "y"
    iv5Out[odk[ , indexData]> 0, 252] <-  "n"

    #253) Did she have four or more pregnancies before this one?	4+ pr
    iv5Out[odk[ , indexData]>=4, 253] <- "y"
    iv5Out[odk[ , indexData]< 4, 253] <- "n"

    #266) Did her labour last longer than 24 hours?	lab 24+h
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[266]))
    iv5Out[odk[ , indexData]> 24, 266] <- "y"
    iv5Out[odk[ , indexData]<=24, 266] <- "n"

    #271) Did the mother deliver at a health facility or clinic?	del hfac
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[271]))
    iv5Out[tolower(odk[ , indexData])=="hospital",                         271] <- "y"
    iv5Out[tolower(odk[ , indexData])=="other_health_facility",            271] <- "y"
    iv5Out[tolower(odk[ , indexData])=="home",                             271] <- "n"
    iv5Out[tolower(odk[ , indexData])=="on_route_to_hospital_or_facility", 271] <- "n"
    iv5Out[tolower(odk[ , indexData])=="other",                            271] <- "n"

    #272) Did the mother deliver at home?	del home
    iv5Out[tolower(odk[ , indexData])=="hospital",                         272] <- "n"
    iv5Out[tolower(odk[ , indexData])=="other_health_facility",            272] <- "n"
    iv5Out[tolower(odk[ , indexData])=="home",                             272] <- "y"
    iv5Out[tolower(odk[ , indexData])=="on_route_to_hospital_or_facility", 272] <- "n"
    iv5Out[tolower(odk[ , indexData])=="other",                            272] <- "n"

    #273) Did the mother deliver elsewhere (not at a health facility nor at home)?	del else
    iv5Out[tolower(odk[ , indexData])=="hospital",                         273] <- "n"
    iv5Out[tolower(odk[ , indexData])=="other_health_facility",            273] <- "n"
    iv5Out[tolower(odk[ , indexData])=="home",                             273] <- "n"
    iv5Out[tolower(odk[ , indexData])=="on_route_to_hospital_or_facility", 273] <- "y"
    iv5Out[tolower(odk[ , indexData])=="other",                            273] <- "y"

    #281) If the child was part of a multiple birth, was it born first?	mult fir
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[281]))
    iv5Out[tolower(odk[ , indexData])=="first",           281] <- "y"
    iv5Out[tolower(odk[ , indexData])=="second_or_later", 281] <- "n"

    #283) Did the child's mother die during or shortly after the delivery?	moth d del
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[283]))
    iv5Out[tolower(odk[ , indexData])=="after_delivery",   283] <- "y"
    iv5Out[tolower(odk[ , indexData])=="during_delivery" , 283] <- "y"

    #284) Did the child's mother die in the baby's first year of life?	moth d y1
    indexDatam <- which(stri_endswith_fixed(odkNames, whoNames[284]))
    indexDatad <- which(stri_endswith_fixed(odkNames, "id3d108b"))
    nMonths <- odk[ , indexDatam]
    nDays   <- odk[ , indexDatad]
    nMonths[is.na(nMonths) & !is.na(nDays)] <- 0
    nDays[is.na(nDays) & !is.na(nMonths)] <- 0
    iv5Out[nMonths + nDays/30.4 <=12, 284] <- "y"
    iv5Out[nMonths + nDays/30.4 > 12, 284] <- "n"

    #285) Was the baby born in a health facility or clinic?	born fac
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[285]))
    iv5Out[tolower(odk[ , indexData])=="hospital",                         285] <- "y"
    iv5Out[tolower(odk[ , indexData])=="other_health_facility",            285] <- "y"
    iv5Out[tolower(odk[ , indexData])=="home",                             285] <- "n"
    iv5Out[tolower(odk[ , indexData])=="other",                            285] <- "n"
    iv5Out[tolower(odk[ , indexData])=="on_route_to_hospital_or_facility", 285] <- "n"

    #286) Was the baby born at home?	born home
    iv5Out[tolower(odk[ , indexData])=="hospital",                         286] <- "n"
    iv5Out[tolower(odk[ , indexData])=="other_health_facility",            286] <- "n"
    iv5Out[tolower(odk[ , indexData])=="home",                             286] <- "y"
    iv5Out[tolower(odk[ , indexData])=="other",                            286] <- "n"
    iv5Out[tolower(odk[ , indexData])=="on_route_to_hospital_or_facility", 286] <- "n"

    #287) Was the baby born somewhere else (e.g. on the way to a clinic)?	born on way
    iv5Out[tolower(odk[ , indexData])=="hospital",                         287] <- "n"
    iv5Out[tolower(odk[ , indexData])=="other_health_facility",            287] <- "n"
    iv5Out[tolower(odk[ , indexData])=="home",                             287] <- "n"
    iv5Out[tolower(odk[ , indexData])=="other",                            287] <- "y"
    iv5Out[tolower(odk[ , indexData])=="on_route_to_hospital_or_facility", 287] <- "y"

    #291) At birth, was the baby very much smaller than usual, (weighing under 1 kg)? bwt vsmall
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[291]))
    iv5Out[odk[ , indexData]< 1000, 291] <- "y"
    iv5Out[odk[ , indexData]>=1000, 291] <- "n"

    #293) Was the baby born during the ninth month (at least 37 weeks) of pregnancy?	gest 9m
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[293]))
    iv5Out[odk[ , indexData]>=9, 293] <- "y"
    iv5Out[odk[ , indexData]< 9, 293] <- "n"

    #294) Was the baby born during the eighth month (34 to 37 weeks) of pregnancy?	gest 8m
    iv5Out[odk[ , indexData]> 8, 294] <- "n"
    iv5Out[odk[ , indexData]==8, 294] <- "y"
    iv5Out[odk[ , indexData]< 8, 294] <- "n"

    #295) Was the baby born before the eighth month (less than 34 weeks) of pregnancy?	gest 7m
    iv5Out[odk[ , indexData]>=8, 295] <- "n"
    iv5Out[odk[ , indexData]< 8, 295] <- "y"

    #302) Was the baby moving in the last few days before the birth? bab mov (NOT INCLUDED)

    #304) Did labour and delivery take more than 24 hours? lab 24+h (NOT INCLUDED)

    #307) Was the liquor a green or brown colour when the waters broke?	liq gr-br (NOT INCLUDED)

    #313) Was this baby born from the mother's first pregnancy?	born 1st pr
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[313]))
    iv5Out[odk[ , indexData]==0, 313] <- "y"
    iv5Out[odk[ , indexData]> 0, 313] <- "n"

    #314) Did the baby's mother have four or more births before this one?	born 4+ pr
    iv5Out[odk[ , indexData]>=4, 314] <- "y"
    iv5Out[odk[ , indexData]< 4, 314] <- "n"

    #315) During labour, did the baby's mother suffer from fever? moth fev (NOT INCLUDED)

    #317) Did the baby's mother have diabetes mellitus?	moth diab (NOT INCLUDED)

    #321) Did the baby's mother have severe anaemia? moth anaem (NOT INCLUDED)

    #325) Was the umbilical cord delivered first? cord first (NOT INCLUDED)

    #330) Did (s)he smoke tobacco?	tobac ns
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[330]))
    iv5Out[tolower(odk[ , indexData])=="chewing_tobacco",       330] <- "n"
    iv5Out[tolower(odk[ , indexData])=="cigarettes",            330] <- "y"
    iv5Out[tolower(odk[ , indexData])=="pipe",                  330] <- "y"

    #331) Did (s)he use non-smoking tobacco?	tobac ns
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[331]))
    iv5Out[tolower(odk[ , indexData])=="chewing_tobacco",       331] <- "y"
    iv5Out[tolower(odk[ , indexData])=="cigarettes",            331] <- "n"
    iv5Out[tolower(odk[ , indexData])=="pipe",                  331] <- "n"
    iv5Out[tolower(odk[ , indexData])=="local_form_of_tobacco", 331] <- "n"
    iv5Out[tolower(odk[ , indexData])=="other",                 331] <- "n"

    #332) Did (s)he smoke at least 10 cigarettes daily?	cigs >10
    indexData <- which(stri_endswith_fixed(odkNames, whoNames[332]))
    iv5Out[odk[ , indexData]>=10, 332] <- "y"
    iv5Out[odk[ , indexData]< 10, 332] <- "n"

    # check for NAs
    if (numNA > 0) {
        warning("Found unexpected input values (coded as missing)", call. = FALSE)
        cat("Unexpected values found in: ", sep = "\n")
        cat(paste(indexNA), sep = ", ")
        cat("\n")
    }

    numNA <- colSums(is.na(iv5Out))
    indexNA <- which(numNA > 0)
    if (length(indexNA) > 0) {
        warning("NA's included in output", call. = FALSE)
        cat(
            paste("odk2openVA produced NA's in the following columns",
                  " (this may cause errors with openVA)",
                  sep = ""),
            sep = "\n"
            )
        cat(
            paste(iv5Names[indexNA], ## subtract 1 since ID is omitted
                  " Probably associated with WHO column containing: ",
                  whoNames[indexNA], ## subtract 1 since ID is omitted
                  sep = ""),
            sep = "\n"
        )
    }

    # Add ID as first column
    indexID <- which(stri_endswith_fixed(odkNames, tolower(id_col)))
    if (length(indexID)) {
        iv5Out <- cbind(as.character(odk[, indexID]), iv5Out)
    } else {
        message("Did not find id_col, so assigning row numbers for IDs.",
                call. = FALSE)
        iv5Out <- cbind(as.character(1:nrow(iv5Out)), iv5Out)
    }

    # Attach column names
    colnames(iv5Out) <- c("ID", iv5Names)

    # That's all folks!
    return(as.data.frame(iv5Out, stringsAsFactors = FALSE))
}
