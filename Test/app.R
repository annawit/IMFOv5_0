library(shiny)
library(DT)
library(dplyr)

mass <- read_csv("mass.csv")

shinyApp(
  ui = fluidPage(
    
    DTOutput('editableTable'),
    textOutput("p")
  ),
  
  
  server = function(input, output, session) {
    
    
    #name the dataframe
    dta <- mass %>% 
      filter(`2015 Weight` > 400000)
   
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
    
    output$p <- renderText({
      input$x1_cell_edit
      dta[1, 6]
    })
  }
)