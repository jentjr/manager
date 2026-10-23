analyte_tbl <- tibble::tribble(
  ~analyte_id                                                                                                                                                                , ~analyte , ~aliases , ~std_unit , ~ion_class , ~charge , ~molecular_weight ,

  #===========================================================================
  # Field Parameters
  #===========================================================================

  "pH"                                                                                                                                                                       ,
  "pH"                                                                                                                                                                       ,
  list("pH")                                                                                                                                                                 ,
  "pH"                                                                                                                                                                       ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "orp"                                                                                                                                                                      ,
  "orp"                                                                                                                                                                      ,
  list("orp", "oxidation reduction potential")                                                                                                                               ,
  "mV"                                                                                                                                                                       ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "do"                                                                                                                                                                       ,
  "do"                                                                                                                                                                       ,
  list("do", "dissolved oxygen")                                                                                                                                             ,
  "mg/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "conductivity"                                                                                                                                                             ,
  "conductivity"                                                                                                                                                             ,
  list("conductivity", "specific conductivity", "specific conductance")                                                                                                      ,
  "uS/cm"                                                                                                                                                                    ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "temperature"                                                                                                                                                              ,
  "temperature"                                                                                                                                                              ,
  list("temp", "temperature", "temperature, field")                                                                                                                          ,
  "degC"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  #===========================================================================
  # Major cations
  #===========================================================================

  "calcium"                                                                                                                                                                  ,
  "calcium"                                                                                                                                                                  ,
  list(.add_fraction_aliases(c("ca", "calcium")))                                                                                                                            ,
  "mg/L"                                                                                                                                                                     ,
  "cation"                                                                                                                                                                   ,
    2                                                                                                                                                                        ,
   40.078                                                                                                                                                                    ,

  "magnesium"                                                                                                                                                                ,
  "magnesium"                                                                                                                                                                ,
  list(.add_fraction_aliases(c("mg", "magnesium")))                                                                                                                          ,
  "mg/L"                                                                                                                                                                     ,
  "cation"                                                                                                                                                                   ,
    2                                                                                                                                                                        ,
   24.305                                                                                                                                                                    ,

  "sodium"                                                                                                                                                                   ,
  "sodium"                                                                                                                                                                   ,
  list(.add_fraction_aliases(c("na", "sodium")))                                                                                                                             ,
  "mg/L"                                                                                                                                                                     ,
  "cation"                                                                                                                                                                   ,
    1                                                                                                                                                                        ,
   22.990                                                                                                                                                                    ,

  "potassium"                                                                                                                                                                ,
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
  "chloride"                                                                                                                                                                 ,
  list(.add_fraction_aliases(c("cl", "chloride")))                                                                                                                           ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -1                                                                                                                                                                        ,
   35.453                                                                                                                                                                    ,

  "fluoride"                                                                                                                                                                 ,
  "fluoride"                                                                                                                                                                 ,
  list(.add_fraction_aliases(c("f", "fluoride")))                                                                                                                            ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -1                                                                                                                                                                        ,
   18.998                                                                                                                                                                    ,

  "bromide"                                                                                                                                                                  ,
  "bromide"                                                                                                                                                                  ,
  list(.add_fraction_aliases(c("br", "bromide")))                                                                                                                            ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -1                                                                                                                                                                        ,
   79.904                                                                                                                                                                    ,

  "iodide"                                                                                                                                                                   ,
  "iodide"                                                                                                                                                                   ,
  list(c("i", "iodide"))                                                                                                                                                     ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -1                                                                                                                                                                        ,
  126.904                                                                                                                                                                    ,

  "sulfate"                                                                                                                                                                  ,
  "sulfate"                                                                                                                                                                  ,
  list(.add_fraction_aliases(c("so4", "sulfate")))                                                                                                                           ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -2                                                                                                                                                                        ,
   96.056                                                                                                                                                                    ,

  "nitrate"                                                                                                                                                                  ,
  "nitrate"                                                                                                                                                                  ,
  list(c("no3", "nitrate"))                                                                                                                                                  ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -1                                                                                                                                                                        ,
   62.005                                                                                                                                                                    ,

  "nitrite"                                                                                                                                                                  ,
  "nitrite"                                                                                                                                                                  ,
  list(c("no2", "nitrite"))                                                                                                                                                  ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -1                                                                                                                                                                        ,
   46.005                                                                                                                                                                    ,

  "phosphate"                                                                                                                                                                ,
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
  "bicarbonate"                                                                                                                                                              ,
  list(c("bicarbonate", "hco3", "alkalinity", "alkalinity as caco3", "alkalinity, total", "alkalinity total", "total alkalinity", "alkalinity total lab", "alkalinity lab")) ,
  "mg/L"                                                                                                                                                                     ,
  "anion"                                                                                                                                                                    ,
   -1                                                                                                                                                                        ,
   61.0168                                                                                                                                                                   ,

  "carbonate"                                                                                                                                                                ,
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
  "antimony"                                                                                                                                                                 ,
  list(.add_fraction_aliases(c("sb", "antimony")))                                                                                                                           ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "arsenic"                                                                                                                                                                  ,
  "arsenic"                                                                                                                                                                  ,
  list(.add_fraction_aliases(c("as", "arsenic")))                                                                                                                            ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "barium"                                                                                                                                                                   ,
  "barium"                                                                                                                                                                   ,
  list(.add_fraction_aliases(c("ba", "barium")))                                                                                                                             ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "beryllium"                                                                                                                                                                ,
  "beryllium"                                                                                                                                                                ,
  list(.add_fraction_aliases(c("be", "beryllium")))                                                                                                                          ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "boron"                                                                                                                                                                    ,
  "boron"                                                                                                                                                                    ,
  list(.add_fraction_aliases(c("b", "boron")))                                                                                                                               ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "cadmium"                                                                                                                                                                  ,
  "cadmium"                                                                                                                                                                  ,
  list(.add_fraction_aliases(c("cd", "cadmium")))                                                                                                                            ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "chromium"                                                                                                                                                                 ,
  "chromium"                                                                                                                                                                 ,
  list(.add_fraction_aliases(c("cr", "chromium")))                                                                                                                           ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "cobalt"                                                                                                                                                                   ,
  "cobalt"                                                                                                                                                                   ,
  list(.add_fraction_aliases(c("co", "cobalt")))                                                                                                                             ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "copper"                                                                                                                                                                   ,
  "copper"                                                                                                                                                                   ,
  list(.add_fraction_aliases(c("cu", "copper")))                                                                                                                             ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "iron"                                                                                                                                                                     ,
  "iron"                                                                                                                                                                     ,
  list(.add_fraction_aliases(c("fe", "iron")))                                                                                                                               ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "lead"                                                                                                                                                                     ,
  "lead"                                                                                                                                                                     ,
  list(.add_fraction_aliases(c("pb", "lead")))                                                                                                                               ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "lithium"                                                                                                                                                                  ,
  "lithium"                                                                                                                                                                  ,
  list(.add_fraction_aliases(c("li", "lithium")))                                                                                                                            ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "manganese"                                                                                                                                                                ,
  "manganese"                                                                                                                                                                ,
  list(.add_fraction_aliases(c("mn", "manganese")))                                                                                                                          ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "mercury"                                                                                                                                                                  ,
  "mercury"                                                                                                                                                                  ,
  list(.add_fraction_aliases(c("hg", "mercury")))                                                                                                                            ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "molybdenum"                                                                                                                                                               ,
  "molybdenum"                                                                                                                                                               ,
  list(.add_fraction_aliases(c("mo", "molybdenum")))                                                                                                                         ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "nickel"                                                                                                                                                                   ,
  "nickel"                                                                                                                                                                   ,
  list(.add_fraction_aliases(c("ni", "nickel")))                                                                                                                             ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "selenium"                                                                                                                                                                 ,
  "selenium"                                                                                                                                                                 ,
  list(.add_fraction_aliases(c("se", "selenium")))                                                                                                                           ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "silver"                                                                                                                                                                   ,
  "silver"                                                                                                                                                                   ,
  list(.add_fraction_aliases(c("ag", "silver")))                                                                                                                             ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "thallium"                                                                                                                                                                 ,
  "thallium"                                                                                                                                                                 ,
  list(.add_fraction_aliases(c("tl", "thallium")))                                                                                                                           ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "vanadium"                                                                                                                                                                 ,
  "vanadium"                                                                                                                                                                 ,
  list(.add_fraction_aliases(c("v", "vanadium")))                                                                                                                            ,
  "ug/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "zinc"                                                                                                                                                                     ,
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
  "hardness"                                                                                                                                                                 ,
  list(c("hardness", "total hardness", "hardness as caco3"))                                                                                                                 ,
  "mg/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "tds"                                                                                                                                                                      ,
  "tds"                                                                                                                                                                      ,
  list(c("tds", "total dissolved solids"))                                                                                                                                   ,
  "mg/L"                                                                                                                                                                     ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "ammonia"                                                                                                                                                                  ,
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
  "radium-226"                                                                                                                                                               ,
  list(c("radium-226", "ra-226"))                                                                                                                                            ,
  "pCi/L"                                                                                                                                                                    ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "radium-228"                                                                                                                                                               ,
  "radium-228"                                                                                                                                                               ,
  list(c("radium-228", "ra-228"))                                                                                                                                            ,
  "pCi/L"                                                                                                                                                                    ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "gross alpha"                                                                                                                                                              ,
  "gross alpha"                                                                                                                                                              ,
  list(c("gross alpha"))                                                                                                                                                     ,
  "pCi/L"                                                                                                                                                                    ,
  NA_character_                                                                                                                                                              ,
  NA_real_                                                                                                                                                                   ,
  NA_real_                                                                                                                                                                   ,

  "gross beta"                                                                                                                                                               ,
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

# --------------------------------------------------------------
# Enrich analyte metadata
# --------------------------------------------------------------

analyte_tbl <- analyte_tbl %>%
  dplyr::mutate(
    short_name = dplyr::case_when(
      analyte == "calcium" ~ "Ca",
      analyte == "magnesium" ~ "Mg",
      analyte == "sodium" ~ "Na",
      analyte == "potassium" ~ "K",

      analyte == "chloride" ~ "Cl",
      analyte == "fluoride" ~ "F",
      analyte == "bromide" ~ "Br",
      analyte == "iodide" ~ "I",
      analyte == "sulfate" ~ "SO4",
      analyte == "nitrate" ~ "NO3",
      analyte == "nitrite" ~ "NO2",
      analyte == "phosphate" ~ "PO4",
      analyte == "bicarbonate" ~ "HCO3",
      analyte == "carbonate" ~ "CO3",

      analyte == "antimony" ~ "Sb",
      analyte == "arsenic" ~ "As",
      analyte == "barium" ~ "Ba",
      analyte == "beryllium" ~ "Be",
      analyte == "boron" ~ "B",
      analyte == "cadmium" ~ "Cd",
      analyte == "chromium" ~ "Cr",
      analyte == "cobalt" ~ "Co",
      analyte == "copper" ~ "Cu",
      analyte == "iron" ~ "Fe",
      analyte == "lead" ~ "Pb",
      analyte == "lithium" ~ "Li",
      analyte == "manganese" ~ "Mn",
      analyte == "mercury" ~ "Hg",
      analyte == "molybdenum" ~ "Mo",
      analyte == "nickel" ~ "Ni",
      analyte == "selenium" ~ "Se",
      analyte == "silver" ~ "Ag",
      analyte == "thallium" ~ "Tl",
      analyte == "vanadium" ~ "V",
      analyte == "zinc" ~ "Zn",

      analyte == "hardness" ~ "Hardness",
      analyte == "tds" ~ "TDS",
      analyte == "ammonia" ~ "NH3",

      analyte == "radium-226" ~ "Ra-226",
      analyte == "radium-228" ~ "Ra-228",
      analyte == "gross alpha" ~ "Gross Alpha",
      analyte == "gross beta" ~ "Gross Beta",

      TRUE ~ analyte
    ),

    casrn = dplyr::case_when(
      analyte == "calcium" ~ "7440-70-2",
      analyte == "magnesium" ~ "7439-95-4",
      analyte == "sodium" ~ "7440-23-5",
      analyte == "potassium" ~ "7440-09-7",

      analyte == "chloride" ~ "16887-00-6",
      analyte == "fluoride" ~ "16984-48-8",
      analyte == "bromide" ~ "24959-67-9",

      analyte == "antimony" ~ "7440-36-0",
      analyte == "arsenic" ~ "7440-38-2",
      analyte == "barium" ~ "7440-39-3",
      analyte == "beryllium" ~ "7440-41-7",
      analyte == "boron" ~ "7440-42-8",
      analyte == "cadmium" ~ "7440-43-9",
      analyte == "chromium" ~ "7440-47-3",
      analyte == "cobalt" ~ "7440-48-4",
      analyte == "copper" ~ "7440-50-8",
      analyte == "iron" ~ "7439-89-6",
      analyte == "lead" ~ "7439-92-1",
      analyte == "lithium" ~ "7439-93-2",
      analyte == "manganese" ~ "7439-96-5",
      analyte == "mercury" ~ "7439-97-6",
      analyte == "molybdenum" ~ "7439-98-7",
      analyte == "nickel" ~ "7440-02-0",
      analyte == "selenium" ~ "7782-49-2",
      analyte == "silver" ~ "7440-22-4",
      analyte == "thallium" ~ "7440-28-0",
      analyte == "vanadium" ~ "7440-62-2",
      analyte == "zinc" ~ "7440-66-6",

      TRUE ~ NA_character_
    ),

    analyte_group = dplyr::case_when(
      analyte %in%
        c(
          "calcium",
          "magnesium",
          "sodium",
          "potassium",
          "chloride",
          "fluoride",
          "bromide",
          "iodide",
          "sulfate",
          "nitrate",
          "nitrite",
          "phosphate",
          "bicarbonate",
          "carbonate"
        ) ~ "Major Ions",

      analyte %in%
        c(
          "antimony",
          "arsenic",
          "barium",
          "beryllium",
          "boron",
          "cadmium",
          "chromium",
          "cobalt",
          "copper",
          "iron",
          "lead",
          "lithium",
          "manganese",
          "mercury",
          "molybdenum",
          "nickel",
          "selenium",
          "silver",
          "thallium",
          "vanadium",
          "zinc"
        ) ~ "Trace Metals",

      analyte %in%
        c(
          "radium-226",
          "radium-228",
          "gross alpha",
          "gross beta"
        ) ~ "Radionuclides",

      analyte %in%
        c(
          "pH",
          "orp",
          "do",
          "conductivity",
          "temperature"
        ) ~ "Field Parameters",

      TRUE ~ "General Chemistry"
    ),

    subcategory = dplyr::case_when(
      analyte %in%
        c(
          "calcium",
          "magnesium",
          "sodium",
          "potassium"
        ) ~ "Cation",

      analyte %in%
        c(
          "chloride",
          "fluoride",
          "bromide",
          "iodide",
          "sulfate",
          "nitrate",
          "nitrite",
          "phosphate"
        ) ~ "Anion",

      analyte %in%
        c(
          "bicarbonate",
          "carbonate"
        ) ~ "Carbonate System",

      analyte_group == "Trace Metals" ~ "Metals",

      analyte_group == "Radionuclides" ~
        "Radiochemistry",

      analyte == "ammonia" ~ "Nutrients",

      analyte_group == "Field Parameters" ~
        "Field Measurements",

      TRUE ~ "General Chemistry"
    ),

    is_major_ion = analyte_group == "Major Ions",

    is_trace_metal = analyte_group == "Trace Metals",

    is_radionuclide = analyte_group == "Radionuclides",

    is_general_chemistry = analyte_group == "General Chemistry",

    is_field_parameter = analyte_id %in%
      c(
        "ph",
        "orp",
        "specific conductance",
        "conductivity",
        "temperature",
        "dissolved oxygen",
        "do",
        "turbidity"
      ),

    regulatory_class = dplyr::case_when(
      is_field_parameter ~ "Field Parameters",
      is_radionuclide ~ "Radiochemistry",
      is_trace_metal ~ "Metals",
      is_major_ion ~ "Major Ions",
      is_general_chemistry ~
        "General Chemistry",
      TRUE ~ "Other"
    )
  ) %>%
  dplyr::mutate(
    meq_factor = dplyr::if_else(
      !is.na(charge) &
        !is.na(molecular_weight),
      abs(charge) / molecular_weight,
      NA_real_
    )
  ) %>%
  dplyr::relocate(
    analyte_id,
    analyte,
    short_name,
    casrn,
    aliases,
    analyte_group,
    subcategory,
    regulatory_class,
    std_unit,
    ion_class,
    charge,
    molecular_weight,
    meq_factor,
    is_major_ion,
    is_trace_metal,
    is_radionuclide,
    is_general_chemistry,
    is_field_parameter
  )

validate_analyte_tbl <- function(x) {
  required <- c(
    "analyte_id",
    "analyte",
    "aliases",
    "short_name",
    "std_unit",
    "analyte_group"
  )

  missing <- setdiff(
    required,
    names(x)
  )

  if (length(missing) > 0) {
    rlang::abort(
      paste(
        "Missing analyte_tbl columns:",
        paste(missing, collapse = ", ")
      )
    )
  }

  if (anyNA(x$analyte_id)) {
    rlang::abort(
      "'analyte_id' cannot contain NA."
    )
  }

  if (anyNA(x$analyte)) {
    rlang::abort(
      "'analyte' cannot contain NA."
    )
  }

  if (anyNA(x$std_unit)) {
    rlang::abort(
      "'std_unit' cannot contain NA."
    )
  }

  if (!is.character(x$analyte_id)) {
    rlang::abort(
      "'analyte_id' must be character."
    )
  }

  if (!is.character(x$analyte)) {
    rlang::abort(
      "'analyte' must be character."
    )
  }

  id_dupes <- x %>%
    dplyr::count(analyte_id) %>%
    dplyr::filter(n > 1)

  if (nrow(id_dupes) > 0) {
    rlang::abort(
      paste(
        "Duplicate analyte_id values detected:",
        paste(
          id_dupes$analyte_id,
          collapse = ", "
        )
      )
    )
  }

  dupes <- x %>%
    dplyr::count(analyte) %>%
    dplyr::filter(n > 1)

  if (nrow(dupes) > 0) {
    rlang::abort(
      paste(
        "Duplicate analytes detected:",
        paste(
          dupes$analyte,
          collapse = ", "
        )
      )
    )
  }

  if (
    !all(
      purrr::map_lgl(
        x$aliases,
        ~ is.character(.x) &&
          length(.x) > 0
      )
    )
  ) {
    rlang::abort(
      "'aliases' must be a list-column of non-empty character vectors."
    )
  }

  invisible(TRUE)
}
