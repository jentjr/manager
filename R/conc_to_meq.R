#' Function to convert chemical data from mg/L to meq/L
#'
#' @param df data frame in wide format i.e, column names are major ions
#' @param magnesium magnesium
#' @param calcium calcium
#' @param sodium sodium
#' @param potassium potassium
#' @param chloride chloride
#' @param sulfate sulfate
#' @param alkalinity Total Alkalinity
#' 
#' @export

conc_to_meq <- function(df,
                        magnesium = "Magnesium, dissolved",
                        calcium = "Calcium, dissolved",
                        sodium = "Sodium, dissolved",
                        potassium = "Potassium, dissolved",
                        chloride = "Chloride, total",
                        sulfate = "Sulfate, total",
                        alkalinity = "Alkalinity, total (lab)") {

  # TODO: add ... feature and a data base of elements perhaps from phreeqc.
  # add check that units supplied are in mg/L

  # molecular weights (mg/mmol)
  calcium_fwt <- 40.078
  magnesium_fwt <- 24.305
  sodium_fwt <- 22.990
  potassium_fwt <- 39.098
  sulfur_fwt <- 32.06
  oxygen_fwt <- 15.999
  hydrogen_fwt <- 1.008
  carbon_fwt <- 12.011
  chloride_fwt <- 35.45

  # absolute value of charge (meq/mmol)
  calcium_chrg <- 2
  magnesium_chrg <- 2
  sodium_chrg <- 1
  potassium_chrg <- 1
  sulfate_chrg <- 2
  carbonate_chrg <- 2
  bicarbonate_chrg <- 1
  chloride_chrg <- 1

  # equivalent weight (mg/meq)
  magnesium_ew <- magnesium_chrg / magnesium_fwt
  calcium_ew <- calcium_chrg / calcium_fwt
  sodium_ew <- sodium_chrg / sodium_fwt
  potassium_ew <- potassium_chrg / potassium_fwt
  chloride_ew <- chloride_chrg / chloride_fwt
  sulfate_ew <- sulfate_chrg / (sulfur_fwt + 4 * oxygen_fwt)
  carbonate_ew <- carbonate_chrg / (carbon_fwt + 3 * oxygen_fwt)
  bicarbonate_ew <- bicarbonate_chrg / (hydrogen_fwt + carbon_fwt + 3 * oxygen_fwt)
  total_alk_ew <- carbonate_ew + bicarbonate_ew

  # conversion from mg/L to meq/L
  df <- df %>%
    mutate_at(vars(magnesium), funs(. * magnesium_ew)) %>%
    mutate_at(vars(calcium), funs(. * calcium_ew)) %>%
    mutate_at(vars(sodium), funs(. * sodium_ew)) %>%
    mutate_at(vars(potassium), funs(. * potassium_ew)) %>%
    mutate_at(vars(chloride), funs(. * chloride_ew)) %>%
    mutate_at(vars(sulfate), funs(. * sulfate_ew)) %>%
    mutate_at(vars(alkalinity), funs(. * total_alk_ew))

  return(df)

}
