standards_df <- tibble::tribble(
    ~param_name, ~standard_type, ~standard_value, ~standard_unit,
    "Antimony", "Maximum Contaminant Legel", 6, "ug/L",
    "Arsenic", "Maximum Contaminant Level", 10, "ug/L",
    "Barium", "Maximum Contaminant Level", 2000, "ug/L",
    "Beryllium", "Maximum Contaminant Level", 4, "ug/L",
    "Cadmium", "Maximum Contaminant Level", 5, "ug/L",
    "Chromium", "Maximum Contaminant Level", 100, "ug/L",
    "Cobalt", "Regional Screening Level", 6, "ug/L",
    "Fluoride", "Maximum Contaminant Level", 4, "mg/L",
    "Lead", "Regional Screening Level", 15, "ug/L",
    "Lithium", "Regional Screening Level", 40, "ug/L",
    "Mercury", "Maximum Contaminant Level", 2, "ug/L",
    "Molybdenum", "Regional Screening Level", 100, "ug/L",
    "Selenium", "Maximum Contaminant Level", 50, "ug/L",
    "Thallium", "Maximum Contaminant Level", 2, "ug/L",
    "Radium-226 and Radium-228", "Maximum Contaminant Level", 5, "pCi/L"
)

usethis::use_data(standards_df, internal=TRUE, overwrite = TRUE)
