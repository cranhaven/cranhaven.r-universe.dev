#'@title Demographics
#'
#'@description Prints participant count, age mean and SD, and gender ratio, from
#'  given dataset.
#'@param data_per_subject Data frame from which demographics are to be
#'  calculated. Should contain columns named as "\code{age}" and as
#'  "\code{gender}" (or, alternatively, "\code{sex}"). Alternatively, these
#'  columns can be specified via the \code{gender_col} and \code{age_col}
#'  parameters. The \code{age} column must contain only numbers or \code{NA},
#'  while \code{gender} column must contain only \code{1} (= male) or \code{2}
#'  (= female), either as numbers or as strings, or \code{NA}. Alternatively,
#'  different gender coding can be set via the parameters \code{male} and
#'  \code{female} (but \code{1}/\code{2} will be checked for first in any case).
#'@param group_by Optionally the name(s) of column(s) from the data frame
#'  provided as \code{data_per_subject} to group by.
#'@param gender_col Optionally the name of column from the data frame that
#'  contains the gender (sex) information.
#'@param age_col Optionally the name of column from the data frame that contains
#'  the age information.
#'@param male Alternative code for male: by default, it is the string
#'  \code{"male"}. Whatever string is given, its abbreviations will also be
#'  accepted (e.g. \code{"m"}). (Lettercases do not matter, e.g. \code{Male} or
#'  \code{MALE} are both evaluated same as \code{male}.)
#'@param female Alternative code for female: by default, it is the string
#'  \code{"female"}. Whatever string is given, its abbreviations will also be
#'  accepted (e.g. \code{"fem"}). (Lettercases do not matter.)
#'@param percent Logical. If \code{TRUE}, gender ratios (and the "unknown"
#'  ratios based on \code{NA} values) are presented as percentage. If
#'  \code{FALSE}, they are presented as counts (i.e., numbers of subjects).
#'@param round_perc Number \code{\link[=ro]{to round}} to, when using
#'  percentages.
#'@param show_fem Logical or \code{NULL}. If \code{TRUE}, the numbers of both
#'  male and female are displayed. If \code{FALSE}, only the number of males is
#'  displayed. If \code{NULL} (default), only the number of males is displayed
#'  when there are no unknown cases, but both numbers are displayed when there
#'  are any unknown cases.
#'@param age_range Logical, \code{FALSE} by default. If \code{TRUE}, also
#'  displays age range per group (minimum and maximum ages).
#'@param age_min If numeric given, removes all ages below (exclusive!) the given
#'  number before any age calculation.#'
#'@param age_max If numeric given, removes all ages above (exclusive!) the given
#'  number before any age calculation.
#'
#'@details If \code{gender_col} and/or \code{age_col} are not specified, the
#'  function will first look for columns named precisely "\code{age}" and as
#'  "\code{gender}". If either is not found, the function looks for the same
#'  names but with any lettercase (e.g. "\code{AGE}" or "\code{Gender}"). If
#'  still no "\code{gender}" column is found, the function looks for
#'  "\code{sex}" column in the same manner. If no column is found for either,
#'  all related values will be counted as "unknown" (\code{NA}).
#'
#'  If \code{NA} values are found in either the \code{age} or \code{gender}
#'  column, the ratio (or count) of unknown cases will be displayed everywhere.
#'  Otherwise it will simply not be displayed anywhere.
#'
#' @examples
#' # below is an illustrative example dataset
#' # (the "subject" and "measure_x" columns are not used in the function)
#' dat = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     conditions = c('x', 'y', 'x', 'y', 'y', 'x', 'x', 'x', 'y', 'x'),
#'     gender = c(2, 2, 1, 2, 1, 2, 2, 2, 1, 1),
#'     age = c(6, 7, 8.5, 6, 5, 16.5, 17, 16, 45.8, 77),
#'     measure_x = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85),
#'     stringsAsFactors = TRUE
#' )
#'
#' # print demographics (age and gender) per "conditions":
#' dems_neat(dat, group_by = 'conditions')
#'
#' # replace unlikely ages with NAs
#' dems_neat(dat,
#'           group_by = 'conditions',
#'           age_min = 8,
#'           age_max = 50)
#'
#' # remove only high values, and display age ranges
#' dems_neat(dat,
#'           group_by = 'conditions',
#'           age_max = 45,
#'           age_range = TRUE)
#'
#' # another dataset, with some missing values
#' dat = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     conditions = c('x', 'y', 'x', 'y', 'y', 'x', 'x', 'x', 'y', 'x'),
#'     gender = c(2, 2, NA, NA, 1, 1, 1, 2, NA, NA),
#'     age = c(6, 7, 8.5, 6, 5, 16, NA, 16, 45, 77),
#'     measure_x = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85),
#'     stringsAsFactors = TRUE
#' )
#' # again print demographics per "conditions":
#' dems_neat(dat, group_by = 'conditions')
#'
#' # another dataset, with no "age"/"gender" columns
#' dat = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     conditions = c('x', 'y', 'x', 'y', 'y', 'x', 'x', 'x', 'y', 'x'),
#'     geschlecht = c(2, 2, NA, NA, 1, 1, 1, 2, NA, NA),
#'     alter = c(6, 7, 8.5, 6, 5, 16, NA, 16, 45, 77),
#'     measure_y = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85),
#'     stringsAsFactors = TRUE
#' )
#'
#' # the following will return "unknowns"
#' dems_neat(dat, group_by = 'conditions')
#'
#' # gender column specified
#' dems_neat(dat, group_by = 'conditions', gender_col = 'geschlecht')
#'
#' # both columns specified
#' dems_neat(dat,
#'           group_by = 'conditions',
#'           age_col = 'alter',
#'           gender_col = 'geschlecht')
#'
#' @export

dems_neat = function(data_per_subject,
                     group_by = NULL,
                     gender_col = NULL,
                     age_col = NULL,
                     male = 'male',
                     female = 'female',
                     percent = FALSE,
                     round_perc = 0,
                     show_fem = NULL,
                     age_range = FALSE,
                     age_min = NULL,
                     age_max = NULL) {
    validate_args(
        match.call(),
        list(
            val_arg(data_per_subject, c('df')),
            val_arg(group_by, c('char', 'null')),
            val_arg(gender_col, c('char', 'null')),
            val_arg(age_col, c('char', 'null')),
            val_arg(male, c('char'), 1),
            val_arg(female, c('char'), 1),
            val_arg(percent, c('bool'), 1),
            val_arg(round_perc, c('num'), 1),
            val_arg(show_fem, c('bool', 'null'), 1),
            val_arg(age_range, c('bool'), 1),
            val_arg(age_min, c('num', 'null'), 1),
            val_arg(age_max, c('num', 'null'), 1)
        )
    )
    s_dat = data_per_subject
    if (!is.null(age_col)) {
        s_dat$age = s_dat[[age_col]]
    } else if (!'age' %in% names(s_dat)) {
        names(s_dat)[tolower(names(s_dat)) == 'age'] = 'age'
    }
    if (!is.null(gender_col)) {
        s_dat$gender = s_dat[[gender_col]]
    } else if (!'gender' %in% names(s_dat)) {
        names(s_dat)[tolower(names(s_dat)) == 'gender'] = 'gender'
        if (!'gender' %in% names(s_dat)) {
            names(s_dat)[names(s_dat) == 'sex'] = 'gender'
            if (!'gender' %in% names(s_dat)) {
                names(s_dat)[tolower(names(s_dat)) == 'sex'] = 'gender'
            }
        }
    }
    if (!'age' %in% names(s_dat)) {
        message('No column for "age" found.')
        s_dat$age = NA
    }
    if (!'gender' %in% names(s_dat)) {
        message('No column for "sex/gender" found.')
        s_dat$gender = NA
    }
    s_dat$gender = tolower(as.character(s_dat$gender))
    if (all(s_dat$gender == '1' |
            s_dat$gender == '2' |
            is.na(s_dat$gender)) == FALSE) {
        s_dat$gender[substring(male, 1, nchar(s_dat$gender)) == s_dat$gender] = '1'
        s_dat$gender[substring(female, 1, nchar(s_dat$gender)) == s_dat$gender] = '2'
        if (all(s_dat$gender == '1' |
                s_dat$gender == '2' |
                is.na(s_dat$gender)) == FALSE) {
            invals = (!s_dat$gender %in% c('1', '2'))
            s_dat$gender[invals] = NA
            message('Warning: ',
                    sum(invals),
                    ' values (unknown gender codes) converted to NA.')
            # '(The "gender" column should only contain the values 1 (male) or 2 (female) or NA. ',
            # 'Alternatively, it must only contain the values "male" and "female", or abbreviations of these,',
            # 'or, finally, alternatives given for the function parameters "male" and/or "female".)'
        }
    }
    if (is.null(group_by)) {
        s_dat$neat_cond = 0
    } else {
        s_dat$neat_cond = as.factor(eval(parse(
            text = paste0(
                'with(data = s_dat, paste(',
                paste(group_by, collapse = ','),
                ', sep = "_"))'
            )
        )))
    }
    s_dat$age = as.numeric(as.character(s_dat$age))

    age_feed = ''
    if (!is.null(age_min)) {
        age_low = s_dat$age < age_min
        if (sum(age_low, na.rm = T) > 0) {
            s_dat$age[age_low] = NA
            age_feed = paste0(sum(age_low, na.rm = T), ' below min')
        }
    }
    if (!is.null(age_max)) {
        age_high = s_dat$age > age_max
        if (sum(age_high, na.rm = T) > 0) {
            s_dat$age[age_high] = NA
            if (age_feed == '') {
                age_feed = paste0(sum(age_high, na.rm = T), ' below max')
            } else {
                age_feed = paste0(age_feed,
                                  ', and ',
                                  sum(age_high, na.rm = T),
                                  ' below max')
            }
        }
    }
    if (age_feed != '') {
        message('Ages replaced with NA: ', age_feed, '.')
    }

    age_imposs = (s_dat$age < 0) | (s_dat$age > 150)
    if (sum(age_imposs, na.rm = T) > 0) {
        message(
            'Impossible ages (',
            sum(age_imposs, na.rm = T),
            ')! ',
            paste(sort(s_dat$age[age_imposs]), collapse = ', ')
        )
    }
    s_dat$gender = factor(s_dat$gender, levels = c('1', '2'))
    gender = as.data.frame.matrix(stats::xtabs( ~ neat_cond + gender, s_dat))
    if (!'1' %in% colnames(gender)) {
        gender = data.frame('1' = 0, gender)
    } else if (!'2' %in% colnames(gender)) {
        gender[['2']] = 0
    }
    gender$ratio = gender[['1']] / (gender[['1']] + gender[['2']]) * 100
    gender$neat_cond = row.names(gender)

    age = do.call(data.frame,
                  stats::aggregate(s_dat$age, by = list(s_dat$neat_cond), function(x)
                      c(
                          count = length(x),
                          mean = mean(x, na.rm = TRUE),
                          sd = stats::sd(x, na.rm = TRUE),
                          min = min(x, na.rm = TRUE),
                          max = max(x, na.rm = TRUE)
                      )))
    names(age)[names(age) == "Group.1"] <- "neat_cond"
    age_gend = merge(age, gender, by = 'neat_cond')
    if (sum(is.na(s_dat$age)) > 0 | sum(is.na(s_dat$gender)) > 0) {
        if (is.null(show_fem)) {
            show_fem = TRUE
        }
        na_age = do.call(data.frame,
                         stats::aggregate(s_dat$age, by = list(s_dat$neat_cond), function(x)
                             c(
                                 count = sum(is.na(x)),
                                 percent = 100 * sum(is.na(x)) / length(x)
                             )))
        na_gender = do.call(data.frame,
                            stats::aggregate(s_dat$gender, by = list(s_dat$neat_cond), function(x)
                                c(
                                    count = sum(is.na(x)),
                                    percent = 100 * sum(is.na(x)) / length(x)
                                )))
        if (percent == TRUE) {
            age_gend$age_missing = paste0(' [',
                                          ro(na_age$x.percent, round_perc),
                                          '% unknown]')
            age_gend$gender_missing = paste0(' [',
                                             ro(na_gender$x.percent, round_perc),
                                             '% unknown]')
        } else {
            age_gend$age_missing = paste0(' [', na_age$x.count, ' unknown]')
            age_gend$gender_missing = paste0(' [',
                                             na_gender$x.count,
                                             ' unknown]')
        }
        age_gend$age_missing = paste0(age_gend$age_missing)
        age_gend$age_missing = ifelse(startsWith(age_gend$age_missing, ' [0'),
                                      '',
                                      age_gend$age_missing)
        age_gend$gender_missing = ifelse(startsWith(age_gend$gender_missing, ' [0'),
                                         '',
                                         age_gend$gender_missing)
    } else {
        if (is.null(show_fem)) {
            show_fem = FALSE
        }
        age_gend$age_missing = ""
        age_gend$gender_missing = ""
    }
    if (percent != FALSE) {
        for (i in 1:nrow(age_gend)) {
            row <- age_gend[i, ]
            thefems = ''
            if (show_fem == TRUE) {
                thefems = paste0(', ', ro(100 - row[9], round_perc), '% female')
            }
            if (age_range == TRUE) {
                age_r = paste0(' [', row[5], 'CHAR_MINUS', row[6], ']')
            } else {
                age_r = ''
            }
            prnt(
                'Group < ',
                row[[1]],
                ' >: ',
                row[2],
                ' subjects (age = ',
                ro(row[3], 1),
                'CHAR_PLUSMIN',
                ro(row[4], 1),
                age_r,
                row[10],
                '; ',
                ro(row[9], round_perc),
                "% male",
                thefems,
                row[11],
                ")"
            )
        }
    } else {
        for (i in 1:nrow(age_gend)) {
            row <- age_gend[i, ]
            thefems = ''
            if (show_fem == TRUE) {
                thefems = paste0(', ', row[8], ' female')
            }
            if (age_range == TRUE) {
                age_r = paste0(' [', row[5], 'CHAR_MINUS', row[6], ']')
            } else {
                age_r = ''
            }
            prnt(
                'Group < ',
                row[[1]],
                ' >: ',
                row[2],
                ' subjects (age = ',
                ro(row[3], 1),
                'CHAR_PLUSMIN',
                ro(row[4], 1),
                age_r,
                row[10],
                '; ',
                row[7],
                " male",
                thefems,
                row[11],
                ")"
            )
        }
    }
}
