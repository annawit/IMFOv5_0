library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(
    
    DTOutput('editableIrisTable')
  ),
  
  
  server = function(input, output, session) {
    
    
    #name the dataframe
    dta <- iris
    
    
    dta$Date <- Sys.time() + seq_len(nrow(dta))
    
    output$editableIrisTable <- renderDT(dta, selection = 'none', editable = TRUE)
    
    proxy <- dataTableProxy('x1')
    
    observeEvent(input$x1_cell_edit, {
      info <- input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      dta[i, j] <<- DT::coerceValue(v, dta[i, j])
      replaceData(proxy, dta, resetPaging = FALSE)  # important
    })
  }
)