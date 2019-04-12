library(shiny)
library(DT)
library(dplyr)

mass <- read_csv("mass.csv")

shinyApp(
  ui = fluidPage(
    
    DTOutput('editableTable'),
    uiOutput("p")
  ),
  
  
  server = function(input, output, session) {
    
    
    #name the dataframe
    dta <- mass
   
    dta$Date <- Sys.time() + seq_len(nrow(dta))
    
    output$editableTable <- renderDT(dta,
                                     selection = 'none',
                                     editable = TRUE)
    
    proxy <- dataTableProxy('x1')
    
    observeEvent(
      input$x1_cell_edit, 
      {
      info <- input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      dta[i, j] <<- DT::coerceValue(v, dta[i, j])
      replaceData(proxy, dta, resetPaging = FALSE)  # important
    })
    
    output$p <- renderUI({
      tagList(input$x1_cell_edit,
      dta[1, 6])
    })
  }
)