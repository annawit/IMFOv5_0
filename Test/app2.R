library(shiny)
library(DT)

# Adapted from
# https://github.com/rstudio/DT/pull/480

mass <- read_csv("mass.csv")

shinyApp(
  ui = fluidPage(
    
    # this function is the same as DT::renderDataTable
    DTOutput('x1'),
    
    #check output
    textOutput("text"),
    textOutput("text2"),
    plotOutput("plot"),
    plotOutput("plot2")
  ),
  
  
  server = function(input, output, session) {
    
    # it is the important that the data is a dataframe and not a list
    x <- data.frame(mass)
    
    y <- reactive({
      input$x1_cell_edit
      x
    })
    
    
    # I can't tell if this is optional. I decided to leave it because it
    # differentiates it from the original data in case people edit it
    # away from it's base case
    x$Date <- Sys.time() + seq_len(nrow(x))
    
    # This is the same as DT::renderDataTable. it needs a format date at the end
    # to make the date output pretty.
    output$x1 <- renderDT(x, 
                          selection = 'none',
                          editable = TRUE)
    
    # I'm not exactly how these next two work yet.
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
    
    # A plot test of the table
    output$plot <- renderPlot({
      input$x1_cell_edit
      hist(x$X2015.Weight)
    })
    
    output$plot2 <- renderPlot({
      input$x1_cell_edit
      hist(y()[,6])
    })
    
  }
)