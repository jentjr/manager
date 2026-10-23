standards_tbl <- standards_schema(
  tibble::tribble(
    ~analyte                    , ~standard_type              , ~standard_value , ~standard_unit , ~medium       , ~source , ~provenance                                   , ~reference                      , ~effective_date , ~notes , ~priority ,

    "Antimony"                  , "Maximum Contaminant Level" ,               6 , "ug/L"         , "groundwater" , "EPA"   , "National Primary Drinking Water Regulations" , "40 CFR 141"                    , NA              , NA     , 100L      ,
    "Arsenic"                   , "Maximum Contaminant Level" ,              10 , "ug/L"         , "groundwater" , "EPA"   , "National Primary Drinking Water Regulations" , "40 CFR 141"                    , NA              , NA     , 100L      ,
    "Barium"                    , "Maximum Contaminant Level" ,            2000 , "ug/L"         , "groundwater" , "EPA"   , "National Primary Drinking Water Regulations" , "40 CFR 141"                    , NA              , NA     , 100L      ,
    "Beryllium"                 , "Maximum Contaminant Level" ,               4 , "ug/L"         , "groundwater" , "EPA"   , "National Primary Drinking Water Regulations" , "40 CFR 141"                    , NA              , NA     , 100L      ,
    "Cadmium"                   , "Maximum Contaminant Level" ,               5 , "ug/L"         , "groundwater" , "EPA"   , "National Primary Drinking Water Regulations" , "40 CFR 141"                    , NA              , NA     , 100L      ,
    "Chromium"                  , "Maximum Contaminant Level" ,             100 , "ug/L"         , "groundwater" , "EPA"   , "National Primary Drinking Water Regulations" , "40 CFR 141"                    , NA              , NA     , 100L      ,

    "Cobalt"                    , "Regional Screening Level"  ,               6 , "ug/L"         , "groundwater" , "EPA"   , "Regional Screening Levels"                   , "EPA Regional Screening Levels" , NA              , NA     , 100L      ,

    "Fluoride"                  , "Maximum Contaminant Level" ,               4 , "mg/L"         , "groundwater" , "EPA"   , "National Primary Drinking Water Regulations" , "40 CFR 141"                    , NA              , NA     , 100L      ,

    "Lead"                      , "Regional Screening Level"  ,              15 , "ug/L"         , "groundwater" , "EPA"   , "Regional Screening Levels"                   , "EPA Regional Screening Levels" , NA              , NA     , 100L      ,

    "Lithium"                   , "Regional Screening Level"  ,              40 , "ug/L"         , "groundwater" , "EPA"   , "Regional Screening Levels"                   , "EPA Regional Screening Levels" , NA              , NA     , 100L      ,

    "Mercury"                   , "Maximum Contaminant Level" ,               2 , "ug/L"         , "groundwater" , "EPA"   , "National Primary Drinking Water Regulations" , "40 CFR 141"                    , NA              , NA     , 100L      ,

    "Molybdenum"                , "Regional Screening Level"  ,             100 , "ug/L"         , "groundwater" , "EPA"   , "Regional Screening Levels"                   , "EPA Regional Screening Levels" , NA              , NA     , 100L      ,

    "Selenium"                  , "Maximum Contaminant Level" ,              50 , "ug/L"         , "groundwater" , "EPA"   , "National Primary Drinking Water Regulations" , "40 CFR 141"                    , NA              , NA     , 100L      ,
    "Thallium"                  , "Maximum Contaminant Level" ,               2 , "ug/L"         , "groundwater" , "EPA"   , "National Primary Drinking Water Regulations" , "40 CFR 141"                    , NA              , NA     , 100L      ,

    "Radium-226 and Radium-228" , "Maximum Contaminant Level" ,               5 , "pCi/L"        , "groundwater" , "EPA"   , "National Primary Drinking Water Regulations" , "40 CFR 141"                    , NA              , NA     , 100L
  ),
  analyte = analyte,
  standard_type = standard_type,
  standard_value = standard_value,
  standard_unit = standard_unit,
  medium = medium,
  source = source,
  provenance = provenance,
  reference = reference,
  effective_date = effective_date,
  notes = notes,
  priority = priority
)
