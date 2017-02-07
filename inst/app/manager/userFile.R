# Module server function
userFile <- function(input, output, session, stringsAsFactors) {
  
  userFileReact <- reactive({

    if (input$fileInputType == 'csv') {
      
      validate(need(input$csvfile,
               message  = 
"Please upload a .csv file in the following format: \n\n
location_id | sample_date | param_name | lt_measure | analysis_result | default_unit
-------------- | ---------------- | ----------------- | -------------- | ------------------ | --------------
MW-1         | 2008-01-01  | Arsenic, diss  |      <            |     0.01             |       ug/L
-------------- | ---------------- | ----------------- | -------------- | ------------------ | --------------
MW-1         | 2008-01-01  | Boron, diss     |                   |     0.24             |       mg/L "))
      
      inputdata <- manager::from_csv(input$csvfile$datapath,
                                     input$csv_date)
      
    }
    
    if (input$fileInputType == 'manages') {
      
      validate(need(input$managesfile$datapath,
                    message = "Please upload a MANAGES Site.mdb file \n\n
                    Only works when running locally for now..."))
      
      inputdata <- manager::connect_manages(input$managesfile$datapath)
      
    }
    return(inputdata)
  })
}