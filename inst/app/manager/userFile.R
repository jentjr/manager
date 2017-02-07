# Module server function
userFile <- function(input, output, session, stringsAsFactors) {
  
  userFileReact <- reactive({
    
    validate(need(input$file, 
      message = 
"Please upload a data set. It can either be a MANAGES Site.mdb file, or a .csv file in the following format: \n\n
location_id | sample_date | param_name | lt_measure | analysis_result | default_unit
-------------- | ---------------- | ----------------- | -------------- | ------------------ | --------------
MW-1         | 2008-01-01  | Arsenic, diss  |      <            |     0.01             |       ug/L
-------------- | ---------------- | ----------------- | -------------- | ------------------ | --------------
MW-1         | 2008-01-01  | Boron, diss     |                   |     0.24             |       mg/L ")
    )
    input$file
  })
  
  dataframe <- reactive({
    
    if (input$fileInputType == 'csv') {

      read.csv(userFileReact()$datapath,
               header = input$heading,
               quote = input$quote,
               stringsAsFactors = stringsAsFactors)
    }

    if (input$fileInputType == 'manages') {
      
      connect_manages(userFileReact()$datapath)
      
    }
    
  })
  
  return(dataframe)

}