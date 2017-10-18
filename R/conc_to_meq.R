#' Function to convert chemical data from mg/L to meq/L
#' 
#' @param df data frame
#' @param Mg Magnesium
#' @param Ca Calcium
#' @param Na Sodium
#' @param K Potassium
#' @param Cl Chloride
#' @param SO4 Sulfate
#' @param Alk Total Alkalinity
#' @export

conc_to_meq <- function(df, 
                        Mg = "Magnesium, dissolved", 
                        Ca = "Calcium, dissolved", 
                        Na = "Sodium, dissolved", 
                        K = "Potassium, dissolved", 
                        Cl = "Chloride, total", 
                        SO4 = "Sulfate, total", 
                        Alk = "Alkalinity, total (lab)") {
  
  # TODO: add ... feature and a data base of elements perhaps from phreeqc.
  # add check that units supplied are in mg/L
  
  # formuala weights
  Ca_fwt <- 40.078
  Mg_fwt <- 24.305
  Na_fwt <- 22.990
  K_fwt <- 39.098
  S_fwt <- 32.06
  O_fwt <- 15.999
  H_fwt <- 1.008
  C_fwt <- 12.011
  Cl_fwt <- 35.45
  
  # absolute value of charge
  Ca_chrg <- 2
  Mg_chrg <- 2
  Na_chrg <- 1
  K_chrg <- 1
  SO4_chrg <- 2
  CO3_chrg <- 2
  HCO3_chrg <- 1
  Cl_chrg <- 1
  
  #molar mass
  Mg_mol <- Mg_fwt*Mg_chrg
  Ca_mol <- Ca_fwt*Ca_chrg
  Na_mol <- Na_fwt*Na_chrg
  K_mol <- K_fwt*K_chrg
  Cl_mol <- Cl_fwt*Cl_chrg
  SO4_mol <- (S_fwt + 4*O_fwt)*SO4_chrg
  CO3_mol <- (C_fwt + 3*O_fwt)*CO3_chrg
  HCO3_mol <- (H_fwt + C_fwt + 3*O_fwt)*HCO3_chrg
  total_alk_mol <- CO3_mol + HCO3_mol
  
  # conversion 
  df <- df %>%
    mutate_at(vars(Mg), funs(./Mg_mol)) %>%
    mutate_at(vars(Ca), funs(./Ca_mol)) %>%
    mutate_at(vars(Na), funs(./Na_mol)) %>%
    mutate_at(vars(K), funs(./K_mol)) %>%
    mutate_at(vars(Cl), funs(./Cl_mol)) %>%
    mutate_at(vars(SO4), funs(./SO4_mol)) %>%
    mutate_at(vars(Alk), funs(./total_alk_mol))
  
  return(df)
  
}