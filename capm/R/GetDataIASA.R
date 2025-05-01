#' Get initial values and paramters for IASA model from survey data
#' @description Calculates initial values and paramters for the IASA model, from survey data collectected with the questionnaire described by Bquero at al., 2018.
#' @param data \code{\link{data.frame}} with survey data.
#' @param sex.col name or index of the column with the *sex* variable.
#' @param female.label string with the *female* category in \code{sex.col}.
#' @param male.label string with the *male* category in \code{sex.col}.
#' @param sterilized.col name or index of the column with the *sterilized* variable.
#' @param sterilized.label string with the *sterilized* category (ex. yes) in \code{sterilized.col}.
#' @param sterilized.ly.col name or index of the column with *sterilized last year* variable.
#' @param sterilized.ly.label string with the *sterilized during the last year* category (ex. yes) in \code{sterilized.ly.col}.
#' @param births.ly.col name or index of the column with the *births during the last year* variable.
#' @param species3.col name or index of the column with the *species* variable form the third questionnaire's section (animals that left the household).
#' @param species.label string with the *species* category of interest (ex. dog) in \code{species3.col}.
#' @param sex3.col name or index of the column with the *sex* variable form the third questionnaire's section (animals that left the household).
#' @param fate.col name or index of the column with the *fate* variable.
#' @param died.label string with the *died* category in \code{fate.col}.
#' @param lost.label string with the *lost* category in \code{fate.col}.
#' @param acquisition.col name or index of the column with the *acquisition* variable.
#' @param acquired.ly.col name or index of the column with the *acquisition during the last year* variable.
#' @param acquired.ly.label string with the *acquisition during the last year* category (ex. yes) in \code{acquisition.ly.col}.
#' @param adopted.label string with the *adopted* category in \code{acquistion.col}.
#' @param bought.label string with the *bought* category in \code{acquisition.col}.
#' @param acquisition.source.col name or index of the column with the *source* variable (ex. city).
#' @param acquired.sterilized.col name or index of the column with the *sterilized when acquired* variable.
#' @param destination.label string with the *destination* category (ex. city) in \code{acquisition.source.col}.
#' @param total.estimate number representing the estimated total population size.
#' @param k1.scale scale to define the carrying capacity of the owned population as k1 = k.scale * total.estimate.
#' @param h1 number representing the mean harem size in the owned population.
#' @param N2.scale scale to define the unowned population size as N2 = N2.sclae * total.estimate.
#' @param f2.scale scale to define the female unowned population size as f2 = f2.scale * f1.
#' @param fs2.scale scale to define the sterilized female unowned population size as fs2 = fs2.scale * fs1.
#' @param m2.scale scale to define the sterimlized male unowned population as m2 = m2.scale * m1.
#' @param ms2.scale scale to define the sterilized male unowned population size as ms2 = ms2.scale * ms1.
#' @param b2.scale scale to define the birth function of the unowned population as b2 = f2 \* b1 / f1 \* b2.scale.
#' @param df2.scale scale to define the death rate of the female unowned population as df2 = df2.scale * df1.
#' @param dm2.scale scale to define the death rate of the male unowned population as dm2 = dm2.scale * dm1.
#' @param sf2.scale scale to define the sterilized female unowned population size as sf2 = sf2 = sf2.scale * sf1.
#' @param sm2.scale scale to define the sterilized male unowned population size as sm2 = sm2.scale * sm1.
#' @param k2.scale scale to define the carrying capacity of the unowned population as k2 = k2.scale * N2.
#' @param h2 number representing the mean harem size in the unowned population.
#' @return \code{\link{list}} with two vectors: \code{init} (initial values) and \code{pars} (parameters).
#' @details If column and category names in \code{data} match arguments' defaults, the function call is simplified as in the example below. 
#' @references Baquero, O. S., Marconcin, S., Rocha, A., & Garcia, R. D. C. M. (2018). Companion animal demography and population management in Pinhais, Brazil. Preventive Veterinary Medicine.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @export
#' @examples
#' data(dogs)
#' GetDataIASA(dogs, destination.label = "Pinhais", total.estimate = 50444)
#' 
GetDataIASA <- function(data = NULL, sex.col = "sex", female.label = "female", male.label = "male", sterilized.col = "sterilized", sterilized.label = "yes", sterilized.ly.col = "sterilized_ly", sterilized.ly.label = "yes", births.ly.col = "births_ly", species3.col = "species3", species.label = "dog", sex3.col = "sex2", fate.col = "fate", died.label = "died", lost.label = "lost", acquisition.col = "acquisition", acquired.ly.col = "acquired_ly", acquired.ly.label = "yes", adopted.label = "adopted", bought.label = "bought", acquisition.source.col = "acquisition_city", acquired.sterilized.col = "acquired_sterilized", destination.label = NULL, total.estimate = NULL, k1.scale = 5, h1 = 1, N2.scale = 0.05, f2.scale = 0.9, fs2.scale = 0.1, m2.scale = 0.95, ms2.scale = 0.05, b2.scale = 1.5, df2.scale = 1.2, dm2.scale = 1.2, sf2.scale = 0.3, sm2.scale = 0.3, k2.scale = 2, h2 = 0.5) {
  
  # Workaround to the "no visible binding for global variable" note.
  . <- sterilized <- Xhat <- species3 <- sterilized.ly <- 
    acquisition.source <- acquired.sterilized <- acquired.ly <- c()
  
  if (is.numeric(sex.col)) {
    names(data)[sex.col] <- "sex"
  } else {
    names(data)[names(data) == sex.col] <- "sex"
  }
  sex <- "sex"
  if (is.numeric(sterilized.col)) {
    names(data)[sterilized.col] <- "sterilized"
  } else {
    names(data)[names(data) == sterilized.col] <- "sterilized"
  }
  if (is.numeric(sterilized.ly.col)) {
    names(data)[sterilized.ly.col] <- "sterilized.ly"
  } else {
    names(data)[names(data) == sterilized.ly.col] <- "sterilized.ly"
  }
  if (is.numeric(births.ly.col)) {
    names(data)[births.ly.col] <- "births.ly"
  } else {
    names(data)[names(data) == births.ly.col] <- "births.ly"
  }
  if (is.numeric(species3.col)) {
    names(data)[species3.col] <- "species3"
  } else {
    names(data)[names(data) == species3.col] <- "species3"
  }
  if (is.numeric(sex3.col)) {
    names(data)[sex3.col] <- "sex3"
  } else {
    names(data)[names(data) == sex3.col] <- "sex3"
  }
  if (is.numeric(fate.col)) {
    names(data)[fate.col] <- "fate"
  } else {
    names(data)[names(data) == fate.col] <- "fate"
  }
  if (is.numeric(acquisition.col)) {
    names(data)[acquisition.col] <- "acquisition"
  } else {
    names(data)[names(data) == acquisition.col] <- "acquisition"
  }
  if (is.numeric(acquired.ly.col)) {
    names(data)[acquired.ly.col] <- "acquired.ly"
  } else {
    names(data)[names(data) == acquired.ly.col] <- "acquired.ly"
  }
  if (is.numeric(acquired.ly.label)) {
    names(data)[acquired.ly.label] <- "acquired.ly.label"
  } else {
    names(data)[names(data) == acquired.ly.label] <- "acquired.ly.label"
  }
  if (is.numeric(acquisition.source.col)) {
    names(data)[acquisition.source.col] <- "acquisition.source"
  } else {
    names(data)[names(data) == acquisition.source.col] <- "acquisition.source"
  }
  if (is.numeric(acquired.sterilized.col)) {
    names(data)[acquired.sterilized.col] <- "acquired.sterilized"
  } else {
    names(data)[names(data) == acquired.sterilized.col] <- "acquired.sterilized"
  }
  
  
  # Totals by sex (f1 and m1) and reproductive status (fs1 and ms1)
  Nhat <- total.estimate
  tot_sex <- data %>%
    filter(!is.na(sex)) %>%
    group_by(sex) %>%
    summarise(Xhat = round(Nhat * n() / nrow(.)))
  f1 <- tot_sex[1, 2][[1]]
  m1 <- tot_sex[2, 2][[1]]
  
  tot_sex_ster <- data %>%
    filter(!is.na(sex) & !is.na(sterilized)) %>%
    group_by(sex, sterilized) %>%
    summarise(Xhat = round(n())) %>%
    ungroup() %>%
    mutate(Xhat = Xhat / rep(c(sum(Xhat[1:2]), sum(Xhat[3:4])), each = 2))
  fs1 <- tot_sex_ster[2, 3][[1]] * f1
  ms1 <- tot_sex_ster[4, 3][[1]] * m1
  
  f1 <- f1 - fs1
  m1 <- m1 - ms1
  
  # births
  b1 <- round(f1 * sum(data$births.ly, na.rm = TRUE) /
                sum(data$sex == female.label, na.rm = TRUE))
  
  # Deadth reates (df1 and dm1)
  data3 <- filter(data, species3 == species.label)
  df1 <-
    sum(data3$fate == died.label & data3$sex3 == female.label,
        na.rm = TRUE) /
    sum(c(data$sex == female.label, data$sex3 == female.label),
        na.rm = TRUE)
  dm1 <-
    sum(data3$fate == died.label & data3$sex3 == male.label,
        na.rm = TRUE) /
    sum(c(data$sex == male.label, data$sex3 == male.label),
        na.rm = TRUE)
  
  # Sterilization rates (sf1 and sm1)
  (ster <- data %>%
      group_by(sex, sterilized, sterilized.ly) %>%
      summarise(ster = n()) %>%
      na.omit(.) %>%
      filter(!(sterilized == sterilized.label &
                 sterilized.ly != sterilized.ly.label)))
  (ster_n <- ster %>%
      group_by(sex) %>%
      summarise(n = sum(ster)))
  (ster <- ster %>%
      left_join(ster_n, by = sex) %>%
      mutate(ster = ster / n) %>%
      filter(sterilized.ly == sterilized.ly.label))
  sf1 <- ster[ster$sex == female.label, ]$ster
  sm1 <- ster[ster$sex == male.label, ]$ster
  
  # Carrying capacity (k1)
  # Very high to desconsider its effect
  k1 <- round(total.estimate * k1.scale) 
  
  # (h1)
  h1 <- h1
  
  # Abandonment rate (a)
  a <- sum(data3$fate == lost.label, na.rm = TRUE) / nrow(data)
  
  # Adoption rate (alpha)
  alpha <- sum(data$acquired.ly == acquired.ly.label &
                 data$acquisition == adopted.label, na.rm = TRUE) / nrow(data)
  
  # Immigration rate (v)
  v <- sum(data$acquired.ly == acquired.ly.label &
             (data$acquisition.source != destination.label |
                data$acquisition == bought.label), na.rm = TRUE) / nrow(data)
  
  # Proportion of sterilized immigrants (z)
  (z <- data %>%
      filter(acquired.ly == acquired.ly.label &
               (acquisition.source != destination.label |
                  acquisition.source == destination.label &
                  acquisition.source == bought.label)) %>%
      group_by(acquired.sterilized) %>%
      summarise(n = n() / nrow(.)))
  z <- z[2, 2][[1]]
  
  ## Unowned dogs
  
  N2 <- Nhat * N2.scale
  f2 <- round((N2 / 2) * f2.scale)
  fs2 <- round((N2 / 2) * fs2.scale)
  m2 <- round((N2 / 2) * m2.scale)
  ms2 <- round((N2 / 2) * ms2.scale)
  
  b2 <- round(f2 * b1 / f1 * b2.scale)
  df2 <- df1 * df2.scale
  dm2 <- dm1 * dm2.scale
  sf2 <- sf1 * sf2.scale
  sm2 <- sm1 * sm2.scale
  k2 <- round(N2 * k2.scale)
  h2 <- h2
  
  pars <- c(b1 = b1, b2 = b2, df1 = df1, dm1 = dm1,
            df2 = df2, dm2 = dm2, sf1 = sf1, sf2 = sf2,
            sm1 = sm1, sm2 = sm2, k1 = k1, k2 = k2, h1 = h1,
            h2 = h2,  a = a, alpha = alpha, v = v, z = z)
  
  init <- c(f1 = f1, fs1 = fs1, m1 = m1, ms1 = ms1,
            f2 = f2, fs2 = fs2, m2 = m2, ms2 = ms2)
  
  list(pars = pars, init = init)
}