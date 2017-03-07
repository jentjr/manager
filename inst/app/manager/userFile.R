# Module server function
userFile <- function(input, output, session, stringsAsFactors) {
  
  userFileReact <- reactive({

    tidy_message = 
"Please upload a file in the following format: 
\n\n
location_id | sample_date | param_name     | lt_measure | analysis_result | default_unit
------------| ------------| ---------------| -----------| ----------------| ------------
MW-1        | 2008-01-01  | Arsenic, diss  |      <     |     0.01        |       ug/L
------------| ------------| ---------------| -----------| ----------------| ------------
MW-1        | 2008-01-01  | Boron, diss    |            |     0.24        |       mg/L 
\n\n
Check the boxes to the left, and MANAGER will try to do the rest."
    
    if (input$fileInputType == 'csv') {
      
      validate(need(input$csvfile,
               message  = tidy_message))
      
      inputdata <- readr::read_delim(
        file = input$csvfile$datapath,
        col_names = input$csvheader,
        delim = input$csvsep,
        quote = input$csvquote
        )
      
    }
    
    if (input$fileInputType == 'excel') {
      
      validate(need(input$excelfile$datapath, 
                    message = tidy_message))
      
      inputdata <- readxl::read_excel(
        input$excelfile$datapath
        
      )
    }
    
    if (input$fileInputType == 'manages') {
      
      validate(need(input$managesfile$datapath,
                    message = "Please upload a MANAGES Site.mdb file \n\n
                    Only works when running locally for now..."))
      
      inputdata <- manager::connect_manages(
        path = input$managesfile$datapath,
        sheet = input$excelsheet
        )
      
    }
    return(inputdata)
  })
}