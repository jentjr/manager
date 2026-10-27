#' Analyte Lookup Table
#'
#' Master analyte reference table used for analyte normalization,
#' classification, unit standardization, and metadata enrichment.
#'
#' @format A tibble containing analyte metadata.
#'
#' @examples
#' data(analyte_tbl)
#'
#' head(analyte_tbl)
"analyte_tbl"

analyte_tbl <- tibble::tribble(
  ~analyte_id         ,
  ~analyte            ,
  ~normalized_analyte ,
  ~short_name         ,
  ~casrn              ,
  ~std_unit           ,
  ~ion_class          ,
  ~charge             ,
  ~molecular_weight   ,

  # Field Parameters

  "ph"                ,
  "pH"                ,
  "ph"                ,
  "pH"                ,
  NA_character_       ,
  "pH"                ,
  NA_character_       ,
  NA_real_            ,
  NA_real_            ,

  "orp"               ,
  "ORP"               ,
  "orp"               ,
  "ORP"               ,
  NA_character_       ,
  "mV"                ,
  NA_character_       ,
  NA_real_            ,
  NA_real_            ,

  "do"                ,
  "Dissolved Oxygen"  ,
  "do"                ,
  "DO"                ,
  NA_character_       ,
  "mg/L"              ,
  NA_character_       ,
  NA_real_            ,
  NA_real_            ,

  "conductivity"      ,
  "Conductivity"      ,
  "conductivity"      ,
  "Cond"              ,
  NA_character_       ,
  "uS/cm"             ,
  NA_character_       ,
  NA_real_            ,
  NA_real_            ,

  "temperature"       ,
  "Temperature"       ,
  "temperature"       ,
  "Temp"              ,
  NA_character_       ,
  "degC"              ,
  NA_character_       ,
  NA_real_            ,
  NA_real_            ,

  # Major Ions

  "calcium"           ,
  "Calcium"           ,
  "calcium"           ,
  "Ca"                ,
  "7440-70-2"         ,
  "mg/L"              ,
  "cation"            ,
    2                 ,
   40.078             ,

  "magnesium"         ,
  "Magnesium"         ,
  "magnesium"         ,
  "Mg"                ,
  "7439-95-4"         ,
  "mg/L"              ,
  "cation"            ,
    2                 ,
   24.305             ,

  "sodium"            ,
  "Sodium"            ,
  "sodium"            ,
  "Na"                ,
  "7440-23-5"         ,
  "mg/L"              ,
  "cation"            ,
    1                 ,
   22.990             ,

  "potassium"         ,
  "Potassium"         ,
  "potassium"         ,
  "K"                 ,
  "7440-09-7"         ,
  "mg/L"              ,
  "cation"            ,
    1                 ,
   39.098             ,

  "chloride"          ,
  "Chloride"          ,
  "chloride"          ,
  "Cl"                ,
  "16887-00-6"        ,
  "mg/L"              ,
  "anion"             ,
   -1                 ,
   35.453             ,

  "fluoride"          ,
  "Fluoride"          ,
  "fluoride"          ,
  "F"                 ,
  "16984-48-8"        ,
  "mg/L"              ,
  "anion"             ,
   -1                 ,
   18.998             ,

  "bromide"           ,
  "Bromide"           ,
  "bromide"           ,
  "Br"                ,
  "24959-67-9"        ,
  "mg/L"              ,
  "anion"             ,
   -1                 ,
   79.904             ,

  "iodide"            ,
  "Iodide"            ,
  "iodide"            ,
  "I"                 ,
  NA_character_       ,
  "mg/L"              ,
  "anion"             ,
   -1                 ,
  126.904             ,

  "sulfate"           ,
  "Sulfate"           ,
  "sulfate"           ,
  "SO4"               ,
  NA_character_       ,
  "mg/L"              ,
  "anion"             ,
   -2                 ,
   96.056             ,

  "nitrate"           ,
  "Nitrate"           ,
  "nitrate"           ,
  "NO3"               ,
  NA_character_       ,
  "mg/L"              ,
  "anion"             ,
   -1                 ,
   62.005             ,

  "nitrite"           ,
  "Nitrite"           ,
  "nitrite"           ,
  "NO2"               ,
  NA_character_       ,
  "mg/L"              ,
  "anion"             ,
   -1                 ,
   46.005             ,

  "phosphate"         ,
  "Phosphate"         ,
  "phosphate"         ,
  "PO4"               ,
  NA_character_       ,
  "mg/L"              ,
  "anion"             ,
   -3                 ,
   94.970             ,

  "bicarbonate"       ,
  "Bicarbonate"       ,
  "bicarbonate"       ,
  "HCO3"              ,
  NA_character_       ,
  "mg/L"              ,
  "anion"             ,
   -1                 ,
   61.0168            ,

  "carbonate"         ,
  "Carbonate"         ,
  "carbonate"         ,
  "CO3"               ,
  NA_character_       ,
  "mg/L"              ,
  "anion"             ,
   -2                 ,
   60.008             ,

  # Trace Metals

  "arsenic"           , "Arsenic"                , "arsenic"     , "As"          , "7440-38-2"   , "ug/L"  , NA , NA , NA ,
  "antimony"          , "Antimony"               , "antimony"    , "Sb"          , "7440-36-0"   , "ug/L"  , NA , NA , NA ,
  "barium"            , "Barium"                 , "barium"      , "Ba"          , "7440-39-3"   , "ug/L"  , NA , NA , NA ,
  "beryllium"         , "Beryllium"              , "beryllium"   , "Be"          , "7440-41-7"   , "ug/L"  , NA , NA , NA ,
  "boron"             , "Boron"                  , "boron"       , "B"           , "7440-42-8"   , "ug/L"  , NA , NA , NA ,
  "cadmium"           , "Cadmium"                , "cadmium"     , "Cd"          , "7440-43-9"   , "ug/L"  , NA , NA , NA ,
  "chromium"          , "Chromium"               , "chromium"    , "Cr"          , "7440-47-3"   , "ug/L"  , NA , NA , NA ,
  "cobalt"            , "Cobalt"                 , "cobalt"      , "Co"          , "7440-48-4"   , "ug/L"  , NA , NA , NA ,
  "copper"            , "Copper"                 , "copper"      , "Cu"          , "7440-50-8"   , "ug/L"  , NA , NA , NA ,
  "iron"              , "Iron"                   , "iron"        , "Fe"          , "7439-89-6"   , "ug/L"  , NA , NA , NA ,
  "lead"              , "Lead"                   , "lead"        , "Pb"          , "7439-92-1"   , "ug/L"  , NA , NA , NA ,
  "lithium"           , "Lithium"                , "lithium"     , "Li"          , "7439-93-2"   , "ug/L"  , NA , NA , NA ,
  "manganese"         , "Manganese"              , "manganese"   , "Mn"          , "7439-96-5"   , "ug/L"  , NA , NA , NA ,
  "mercury"           , "Mercury"                , "mercury"     , "Hg"          , "7439-97-6"   , "ug/L"  , NA , NA , NA ,
  "molybdenum"        , "Molybdenum"             , "molybdenum"  , "Mo"          , "7439-98-7"   , "ug/L"  , NA , NA , NA ,
  "nickel"            , "Nickel"                 , "nickel"      , "Ni"          , "7440-02-0"   , "ug/L"  , NA , NA , NA ,
  "selenium"          , "Selenium"               , "selenium"    , "Se"          , "7782-49-2"   , "ug/L"  , NA , NA , NA ,
  "silver"            , "Silver"                 , "silver"      , "Ag"          , "7440-22-4"   , "ug/L"  , NA , NA , NA ,
  "thallium"          , "Thallium"               , "thallium"    , "Tl"          , "7440-28-0"   , "ug/L"  , NA , NA , NA ,
  "vanadium"          , "Vanadium"               , "vanadium"    , "V"           , "7440-62-2"   , "ug/L"  , NA , NA , NA ,
  "zinc"              , "Zinc"                   , "zinc"        , "Zn"          , "7440-66-6"   , "ug/L"  , NA , NA , NA ,

  # General Chemistry

  "hardness"          , "Hardness"               , "hardness"    , "Hardness"    , NA_character_ , "mg/L"  , NA , NA , NA ,
  "tds"               , "Total Dissolved Solids" , "tds"         , "TDS"         , NA_character_ , "mg/L"  , NA , NA , NA ,
  "ammonia"           , "Ammonia"                , "ammonia"     , "NH3"         , NA_character_ , "mg/L"  , NA , NA , NA ,

  # Radiochemistry

  "radium_226"        , "Radium-226"             , "radium_226"  , "Ra-226"      , NA_character_ , "pCi/L" , NA , NA , NA ,
  "radium_228"        , "Radium-228"             , "radium_228"  , "Ra-228"      , NA_character_ , "pCi/L" , NA , NA , NA ,
  "gross_alpha"       , "Gross Alpha"            , "gross_alpha" , "Gross Alpha" , NA_character_ , "pCi/L" , NA , NA , NA ,
  "gross_beta"        , "Gross Beta"             , "gross_beta"  , "Gross Beta"  , NA_character_ , "pCi/L" , NA , NA , NA
) |>
  dplyr::mutate(
    fraction = NA_character_,
    meq_factor = dplyr::if_else(
      !is.na(charge) &
        !is.na(molecular_weight),
      abs(charge) / molecular_weight,
      NA_real_
    ),
    analyte_group = dplyr::case_when(
      analyte_id %in%
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

      analyte_id %in%
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

      analyte_id %in%
        c(
          "radium_226",
          "radium_228",
          "gross_alpha",
          "gross_beta"
        ) ~ "Radionuclides",

      analyte_id %in%
        c(
          "ph",
          "orp",
          "do",
          "conductivity",
          "temperature"
        ) ~ "Field Parameters",

      TRUE ~ "General Chemistry"
    ),

    subcategory = dplyr::case_when(
      analyte_id %in%
        c(
          "calcium",
          "magnesium",
          "sodium",
          "potassium"
        ) ~ "Cation",

      analyte_id %in%
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

      analyte_id %in%
        c(
          "bicarbonate",
          "carbonate"
        ) ~ "Carbonate System",

      analyte_group == "Trace Metals" ~ "Metals",

      analyte_group == "Radionuclides" ~
        "Radiochemistry",

      analyte_id == "ammonia" ~ "Nutrients",

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
        "do",
        "conductivity",
        "temperature"
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
  ) |>
  dplyr::relocate(
    analyte_id,
    analyte,
    normalized_analyte,
    fraction,
    short_name,
    casrn,
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
