library(shiny)
library(DT)

mass <- read_csv("mass.csv")

shinyApp(
  ui = fluidPage(
    DTOutput('x1'),
    textOutput("text"),
    textOutput("text2")
  ),
  
  
  server = function(input, output, session) {
    x <- data.frame(mass)
    
    y <- reactive({
      input$x1_cell_edit
      x
    })
    
    x$Date <- Sys.time() + seq_len(nrow(x))
    
    output$x1 <- renderDT(x, selection = 'none', editable = TRUE)
    
    proxy <- dataTableProxy('x1')
    
    observeEvent(input$x1_cell_edit, {
      info <- input$x1_cell_edit
      str(info)
      i <- info$row
      j <- info$col
      v <- info$value
      x[i, j] <<- DT::coerceValue(v, x[i, j])
      replaceData(proxy, x, resetPaging = FALSE)  # important
    })
    
    output$text <- renderText({
      input$x1_cell_edit
      x[1, 6]
    })
    
    output$text2 <- renderText({
      input$x1_cell_edit
      y()[1, 6]
    })
    
  }
)