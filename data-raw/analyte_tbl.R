analyte_tbl <- tibble::tribble(
  ~analyte                                                                                                                                                                   , ~aliases , ~std_unit , ~ion_class , ~charge , ~molecular_weight ,

  #===========================================================================
  # Major cations
  #===========================================================================

  "calcium"                                                                                                                                                                  ,
  list(.add_fraction_aliases(c("ca", "calcium")))                                                                                                                            ,
  "mg/L"                                                                                                                                                                     ,
  "cation"                                                                                                                                                                   ,
    2                                                                                                                                                                        ,
   40.078                                                                                                                                                                    ,

  "magnesium"                                                                                                                                                                ,
  list(.add_fraction_aliases(c("mg", "magnesium")))                                                                                                                          ,
  "mg/L"                                                                                                                                                                     ,
  "cation"                                                                                                                                                                   ,
    2                                                                                                                                                                        ,
   24.305                                                                                                                                                                    ,

  "sodium"                                                                                                                                                                   ,
  list(.add_fraction_aliases(c("na", "sodium")))                                                                                                                             ,
  "mg/L"                                                                                                                                                                     ,
  "cation"                                                                                                                                                                   ,
    1                                                                                                                                                                        ,
   22.990                                                                                                                                                                    ,

  "potassium"                                                                                                                                                                ,
  list(.add_fraction_aliases(c("k", "potassium")))                                                                                                                           ,
  "mg/L"                                                                                                                                                                     ,
  "cation"                                                                                                                                                                   ,
    1                                                                                                                                                                        ,
   39.098                                                                                                                                                                    ,

  #===========================================================================
  # Major anions
  #===========================================================================

  "chloride"                                                                                                                                                                 ,
  list(.add_fraction_aliases(c("cl", "chloride")))                                                                                                                           ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -1                                                                                                                                                                        ,
   35.453                                                                                                                                                                    ,

  "fluoride"                                                                                                                                                                 ,
  list(.add_fraction_aliases(c("f", "fluoride")))                                                                                                                            ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -1                                                                                                                                                                        ,
   18.998                                                                                                                                                                    ,

  "bromide"                                                                                                                                                                  ,
  list(.add_fraction_aliases(c("br", "bromide")))                                                                                                                            ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -1                                                                                                                                                                        ,
   79.904                                                                                                                                                                    ,

  "iodide"                                                                                                                                                                   ,
  list(c("i", "iodide"))                                                                                                                                                     ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -1                                                                                                                                                                        ,
  126.904                                                                                                                                                                    ,

  "sulfate"                                                                                                                                                                  ,
  list(.add_fraction_aliases(c("so4", "sulfate")))                                                                                                                           ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -2                                                                                                                                                                        ,
   96.056                                                                                                                                                                    ,

  "nitrate"                                                                                                                                                                  ,
  list(c("no3", "nitrate"))                                                                                                                                                  ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -1                                                                                                                                                                        ,
   62.005                                                                                                                                                                    ,

  "nitrite"                                                                                                                                                                  ,
  list(c("no2", "nitrite"))                                                                                                                                                  ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -1                                                                                                                                                                        ,
   46.005                                                                                                                                                                    ,

  "phosphate"                                                                                                                                                                ,
  list(c("po4", "phosphate"))                                                                                                                                                ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -3                                                                                                                                                                        ,
   94.970                                                                                                                                                                    ,

  #===========================================================================
  # Alkalinity / carbonate system
  #===========================================================================

  "bicarbonate"                                                                                                                                                              ,
  list(c("bicarbonate", "hco3", "alkalinity", "alkalinity as caco3", "alkalinity, total", "alkalinity total", "total alkalinity", "alkalinity total lab", "alkalinity lab")) ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -1                                                                                                                                                                        ,
   61.0168                                                                                                                                                                   ,

  "carbonate"                                                                                                                                                                ,
  list(c("carbonate", "co3"))                                                                                                                                                ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -2                                                                                                                                                                        ,
   60.008                                                                                                                                                                    ,

  #===========================================================================
  # Trace metals
  #===========================================================================

  "antimony"                                                                                                                                                                 ,
  list(.add_fraction_aliases(c("sb", "antimony")))                                                                                                                           ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "arsenic"                                                                                                                                                                  ,
  list(.add_fraction_aliases(c("as", "arsenic")))                                                                                                                            ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "barium"                                                                                                                                                                   ,
  list(.add_fraction_aliases(c("ba", "barium")))                                                                                                                             ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "beryllium"                                                                                                                                                                ,
  list(.add_fraction_aliases(c("be", "beryllium")))                                                                                                                          ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "boron"                                                                                                                                                                    ,
  list(.add_fraction_aliases(c("b", "boron")))                                                                                                                               ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "cadmium"                                                                                                                                                                  ,
  list(.add_fraction_aliases(c("cd", "cadmium")))                                                                                                                            ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "chromium"                                                                                                                                                                 ,
  list(.add_fraction_aliases(c("cr", "chromium")))                                                                                                                           ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "cobalt"                                                                                                                                                                   ,
  list(.add_fraction_aliases(c("co", "cobalt")))                                                                                                                             ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "copper"                                                                                                                                                                   ,
  list(.add_fraction_aliases(c("cu", "copper")))                                                                                                                             ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "iron"                                                                                                                                                                     ,
  list(.add_fraction_aliases(c("fe", "iron")))                                                                                                                               ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "lead"                                                                                                                                                                     ,
  list(.add_fraction_aliases(c("pb", "lead")))                                                                                                                               ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "lithium"                                                                                                                                                                  ,
  list(.add_fraction_aliases(c("li", "lithium")))                                                                                                                            ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "manganese"                                                                                                                                                                ,
  list(.add_fraction_aliases(c("mn", "manganese")))                                                                                                                          ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "mercury"                                                                                                                                                                  ,
  list(.add_fraction_aliases(c("hg", "mercury")))                                                                                                                            ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "molybdenum"                                                                                                                                                               ,
  list(.add_fraction_aliases(c("mo", "molybdenum")))                                                                                                                         ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "nickel"                                                                                                                                                                   ,
  list(.add_fraction_aliases(c("ni", "nickel")))                                                                                                                             ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "selenium"                                                                                                                                                                 ,
  list(.add_fraction_aliases(c("se", "selenium")))                                                                                                                           ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "silver"                                                                                                                                                                   ,
  list(.add_fraction_aliases(c("ag", "silver")))                                                                                                                             ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "thallium"                                                                                                                                                                 ,
  list(.add_fraction_aliases(c("tl", "thallium")))                                                                                                                           ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "vanadium"                                                                                                                                                                 ,
  list(.add_fraction_aliases(c("v", "vanadium")))                                                                                                                            ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "zinc"                                                                                                                                                                     ,
  list(.add_fraction_aliases(c("zn", "zinc")))                                                                                                                               ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  #===========================================================================
  # General chemistry
  #===========================================================================

  "hardness"                                                                                                                                                                 ,
  list(c("hardness", "total hardness", "hardness as caco3"))                                                                                                                 ,
  "mg/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "tds"                                                                                                                                                                      ,
  list(c("tds", "total dissolved solids"))                                                                                                                                   ,
  "mg/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "ammonia"                                                                                                                                                                  ,
  list(c("ammonia", "nh3"))                                                                                                                                                  ,
  "mg/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  #===========================================================================
  # Radiochemistry
  #===========================================================================

  "radium-226"                                                                                                                                                               ,
  list(c("radium-226", "ra-226"))                                                                                                                                            ,
  "pCi/L"                                                                                                                                                                    ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "radium-228"                                                                                                                                                               ,
  list(c("radium-228", "ra-228"))                                                                                                                                            ,
  "pCi/L"                                                                                                                                                                    ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "gross alpha"                                                                                                                                                              ,
  list(c("gross alpha"))                                                                                                                                                     ,
  "pCi/L"                                                                                                                                                                    ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "gross beta"                                                                                                                                                               ,
  list(c("gross beta"))                                                                                                                                                      ,
  "pCi/L"                                                                                                                                                                    ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_
) %>%
  dplyr::mutate(
    meq_factor = dplyr::if_else(
      !is.na(charge) &
        !is.na(molecular_weight),
      abs(charge) / molecular_weight,
      NA_real_
    )
  ) %>%
  dplyr::mutate(
    aliases = purrr::map(
      aliases,
      \(x) {
        if (is.list(x)) x[[1]] else x
      }
    )
  )
