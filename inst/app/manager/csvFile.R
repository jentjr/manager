# Module server function
csvFile <- function(input, output, session, stringsAsFactors) {
  
  userFile <- reactive({
    
    validate(need(input$file, 
      message = 
"Please upload a data set. The data should be in the following format: \n\n
location_id | sample_date | param_name | lt_measure | analysis_result | default_unit
-------------- | ---------------- | ----------------- | -------------- | ------------------ | --------------
MW-1         | 2008-01-01  | Arsenic, diss  |      <            |     0.01             |       ug/L
-------------- | ---------------- | ----------------- | -------------- | ------------------ | --------------
MW-1         | 2008-01-01  | Boron, diss     |                   |     0.24             |       mg/L ")
    )
    input$file
  })
  
  dataframe <- reactive({
    read.csv(userFile()$datapath,
      header = input$heading,
      quote = input$quote,
      stringsAsFactors = stringsAsFactors)
  })
  
  return(dataframe)
}