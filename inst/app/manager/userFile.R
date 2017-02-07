# Module server function
userFile <- function(input, output, session, stringsAsFactors) {
  
  userFileReact <- reactive({
    
#     validate(need(input$managesfile, 
#       message = 
# "Please upload a data set. It can either be a MANAGES Site.mdb file, or a .csv file in the following format: \n\n
# location_id | sample_date | param_name | lt_measure | analysis_result | default_unit
# -------------- | ---------------- | ----------------- | -------------- | ------------------ | --------------
# MW-1         | 2008-01-01  | Arsenic, diss  |      <            |     0.01             |       ug/L
# -------------- | ---------------- | ----------------- | -------------- | ------------------ | --------------
# MW-1         | 2008-01-01  | Boron, diss     |                   |     0.24             |       mg/L ")
#     )
    
    if (input$fileInputType == 'csv') {
      
      userdata <- from_csv(input$csvfile$datapath)
      
    }
    
    if (input$fileInputType == 'manages') {
      
      userdata <- connect_manages(input$managesfile$datapath)
      
    }
    return(userdata)
  })
}