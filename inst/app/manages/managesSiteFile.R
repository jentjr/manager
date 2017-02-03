# Module server function
managesSiteFile <- function(input, output, session) {
  
  userFile <- reactive({
    
    validate(need(input$file,
      message = "Please upload MANAGES Site.mdb File"
      )
    )
    input$file
  })
  
  dataframe <- reactive({
    connect_manages(userFile()$datapath)
  })
  
  return(dataframe)
}