# Module UI Function
userFileInput <- function(id, label = "File Input") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    
    selectInput(
      ns('fileInputType'), "Data Input Type", 
      choices = c(csv = "csv", 
                  excel = "excel",
                  manages = "manages")
    ),
    
    conditionalPanel(
      sprintf("input['%s'] == 'csv'", ns("fileInputType")),
      
      fileInput(ns("csvfile"), label),
      
      checkboxInput(
        ns("csvheader"),
        label = "Has Header", 
        value = TRUE
        ),
      
      radioButtons(
        ns("csvsep"),
        label = "Separator",
        choices = c(Comma = ",",
          Semicolon = ";",
          Tab = "\t"),
        selected = ','
      ),
      
      radioButtons(
        ns("csvquote"),
        label = "Quote",
        choices = c(
          None = "",
          'Double Quote' =  '\"',
          'Single Quote' = "\'"
        ),
        selected = '\"'
      ),
      
      selectInput(
        ns('location_id'), "Location ID Column", 
        choices = colnames(data)
      ),
      
      selectInput(
        ns('sample_date'), "Sample Date Column", 
        choices = colnames(data)
      ),
      
      textInput(
        ns("date_format"), 
        label = "Date format", 
        value = "%m/%d/%Y"
        ),
      
      selectInput(
        ns('param_name'), "Constituent Column", 
        choices = colnames(data)
      ),
      
      selectInput(
        ns('lt_measure'), "Detection Column", 
        choices = colnames(data)
      ),
      
      selectInput(
        ns('analysis_result'), "Results Column", 
        choices = colnames(data)
      ),
      
      selectInput(
        ns('default_unit'), "Units Column", 
        choices = colnames(data)
      )
    ),
    
    conditionalPanel(
      sprintf("input['%s'] == 'excel'", ns("fileInputType")),
      textInput(
        ns("excelsheet"),
        label = "Sheet Name",
        value = "Sheet1"
      ),
      fileInput(ns("excelfile"), label)
    ),
    
    conditionalPanel(
      sprintf("input['%s'] == 'manages'", ns("fileInputType")),
      fileInput(ns("managesfile"), label)
    )
  )
}