# Module server function
userFile <- function(input, output, session, stringsAsFactors) {
  
  userFileReact <- reactive({
    
      validate(need(input$managesfile$datapath,
                    message = "Please upload a MANAGES 3.X Site.mdb file\n\n
                    Only works when running locally for now..."))
      
      inputdata <- manager::read_manages3(
         input$managesfile$datapath
        )
      
    return(inputdata)
  })
}