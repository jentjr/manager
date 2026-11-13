#' Analyte Alias Lookup Table
#'
#' Lookup table mapping alternate analyte names to standardized
#' analyte definitions contained in [analyte_tbl].
#'
#' @format A tibble containing analyte aliases.
#'
#' @examples
#' data(analyte_alias_tbl)
#'
#' head(analyte_alias_tbl)
"analyte_alias_tbl"

analyte_alias_tbl <- tibble::tribble(
  ~analyte_id       , ~alias                               ,

  "gw_elev"         , "groundwater elevation"              ,
  "gw_elev"         , "groundwater elevation ref msl"      ,
  "gw_elev"         , "groundwater elevation (ref msl)"    ,

  "gw_depth"        , "groundwater depth"                  ,
  "gw_depth"        , "groundwater depth from toc"         ,
  "gw_depth"        , "groundwater, depth from toc"        ,

  "ph_field"        , "ph"                                 ,
  "ph_field"        , "ph (field)"                         ,
  "ph_field"        , "ph field"                           ,
  "ph_lab"          , "ph (lab)"                           ,
  "ph_lab"          , "ph lab"                             ,

  "turbidity_field" , "turbidity"                          ,
  "turbidity_field" , "turbidity (field)"                  ,
  "turbidity_field" , "turbidity, field"                   ,
  "turbidity_field" , "turbidity field"                    ,
  "turbidity_lab"   , "turbidity (lab)"                    ,
  "turbidity_lab"   , "turbidity lab"                      ,

  "orp"             , "orp"                                ,
  "orp"             , "oxidation reduction potential"      ,

  "do"              , "do"                                 ,
  "do"              , "dissolved oxygen"                   ,

  "conductivity"    , "conductivity"                       ,
  "conductivity"    , "specific conductivity"              ,
  "conductivity"    , "specific conductance"               ,
  "conductivity"    , "Specific Conductance @ 25C (field)" ,

  "temperature"     , "temperature"                        ,
  "temperature"     , "temp"                               ,
  "temperature"     , "temperature field"                  ,
  "temperature"     , "temperature (celsius)"              ,
  "temperature"     , "temperature fahrenheit"             ,

  "calcium"         , "ca"                                 ,
  "calcium"         , "calcium"                            ,

  "magnesium"       , "mg"                                 ,
  "magnesium"       , "magnesium"                          ,

  "sodium"          , "na"                                 ,
  "sodium"          , "sodium"                             ,

  "potassium"       , "k"                                  ,
  "potassium"       , "potassium"                          ,

  "chloride"        , "cl"                                 ,
  "chloride"        , "chloride"                           ,

  "fluoride"        , "f"                                  ,
  "fluoride"        , "fluoride"                           ,

  "bromide"         , "br"                                 ,
  "bromide"         , "bromide"                            ,

  "iodide"          , "i"                                  ,
  "iodide"          , "iodide"                             ,

  "sulfate"         , "so4"                                ,
  "sulfate"         , "sulfate"                            ,

  "nitrate"         , "no3"                                ,
  "nitrate"         , "nitrate"                            ,
  "nitrate"         , "nitrate nitrogen"                   ,
  "nitrate"         , "nitrate nitrogen total"             ,

  "nitrite"         , "no2"                                ,
  "nitrite"         , "nitrite"                            ,
  "nitrite"         , "nitrite nitrogen"                   ,
  "nitrite"         , "nitrite nitrogen total"             ,

  "nitrate_nitrite" , "nitrate + nitrite"                  ,
  "nitrate_nitrite" , "nitrate + nitrite total"            ,

  "silica"          , "sio2"                               ,

  "phosphate"       , "po4"                                ,
  "phosphate"       , "phosphate"                          ,
  "phosphate"       , "phosphorus total"                   ,
  "phosphate"       , "phosphorus, total"                  ,

  "bicarbonate"     , "bicarbonate"                        ,
  "bicarbonate"     , "hco3"                               ,
  "bicarbonate"     , "alkalinity"                         ,
  "bicarbonate"     , "alkalinity as caco3"                ,
  "bicarbonate"     , "alkalinity total"                   ,
  "bicarbonate"     , "alkalinity total lab"               ,
  "bicarbonate"     , "alkalinity total (lab)"             ,
  "bicarbonate"     , "alkalinity, total (lab)"            ,

  "carbonate"       , "carbonate"                          ,
  "carbonate"       , "co3"                                ,

  "aluminum"        , "al"                                 ,
  "aluminum"        , "aluminum"                           ,

  "arsenic"         , "as"                                 ,
  "arsenic"         , "arsenic"                            ,
  "arsenic"         , "arsenic dis"                        ,

  "antimony"        , "sb"                                 ,
  "antimony"        , "antimony"                           ,

  "barium"          , "ba"                                 ,
  "barium"          , "barium"                             ,

  "beryllium"       , "be"                                 ,
  "beryllium"       , "beryllium"                          ,

  "boron"           , "b"                                  ,
  "boron"           , "boron"                              ,

  "cadmium"         , "cd"                                 ,
  "cadmium"         , "cadmium"                            ,

  "chromium"        , "cr"                                 ,
  "chromium"        , "chromium"                           ,

  "hex_chromium"    , "Chromium, hexavalent"               ,

  "cobalt"          , "co"                                 ,
  "cobalt"          , "cobalt"                             ,

  "copper"          , "cu"                                 ,
  "copper"          , "copper"                             ,

  "iron"            , "fe"                                 ,
  "iron"            , "iron"                               ,

  "lead"            , "pb"                                 ,
  "lead"            , "lead"                               ,

  "lithium"         , "li"                                 ,
  "lithium"         , "lithium"                            ,

  "manganese"       , "mn"                                 ,
  "manganese"       , "manganese"                          ,

  "mercury"         , "hg"                                 ,
  "mercury"         , "mercury"                            ,

  "molybdenum"      , "mo"                                 ,
  "molybdenum"      , "molybdenum"                         ,

  "nickel"          , "ni"                                 ,
  "nickel"          , "nickel"                             ,

  "selenium"        , "se"                                 ,
  "selenium"        , "selenium"                           ,

  "silver"          , "ag"                                 ,
  "silver"          , "silver"                             ,

  "strontium"       , "sr"                                 ,
  "strontium"       , "strontium"                          ,

  "thallium"        , "tl"                                 ,
  "thallium"        , "thallium"                           ,

  "tin"             , "sn"                                 ,
  "tin"             , "tin"                                ,

  "uranium"         , "u"                                  ,
  "uranium"         , "uranium"                            ,

  "vanadium"        , "v"                                  ,
  "vanadium"        , "vanadium"                           ,

  "zinc"            , "zn"                                 ,
  "zinc"            , "zinc"                               ,

  "hardness"        , "hardness"                           ,
  "hardness"        , "hardness total"                     ,
  "hardness"        , "hardness total as caco3"            ,
  "hardness"        , "hardness total mg l as caco3"       ,

  "tds"             , "tds"                                ,
  "tds"             , "total dissolved solids"             ,

  "ammonia"         , "ammonia"                            ,
  "ammonia"         , "nh3"                                ,

  "radium_226"      , "radium 226"                         ,
  "radium_226"      , "ra 226"                             ,
  "radium_226"      , "radium-226"                         ,

  "radium_228"      , "radium 228"                         ,
  "radium_228"      , "ra 228"                             ,
  "radium_228"      , "radium-228"                         ,

  "gross_alpha"     , "gross alpha"                        ,

  "gross_beta"      , "gross beta"
)
