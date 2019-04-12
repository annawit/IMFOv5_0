#for reactive data

library(shiny)
library(DT)
library(dplyr)

# Adapted from
# https://github.com/rstudio/DT/pull/480
# https://yihui.shinyapps.io/DT-edit/
# https://stackoverflow.com/questions/52332200/edit-specific-columns-of-datatable-in-r
# https://dev.to/awwsmm/reactive-datatables-in-r-with-persistent-filters-l26
# https://antoineguillot.wordpress.com/2017/03/01/three-r-shiny-tricks-to-make-your-shiny-app-shines-33-buttons-to-delete-edit-and-compare-datatable-rows/
# https://www.bryer.org/post/2018-22-26-dtedit/

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
    # x <- data.frame(mass)
    
    y <- reactive({
      input$x1_cell_edit
      df <- mass %>% 
        filter(Wasteshed == "Columbia") %>% 
        mutate(Date = Sys.time() + seq_len(nrow())
        )
      
      data.frame(df)
    })
    
    # yedit <- reactive({
    #   input$x1_cell_edit
    #   y()
    # })
    # 
    
    # I can't tell if this is optional. I decided to leave it because it
    # differentiates it from the original data in case people edit it
    # away from it's base case
    # x$Date <- Sys.time() + seq_len(nrow(x))
    
    # This is the same as DT::renderDataTable. it needs a format date at the end
    # to make the date output pretty.
    output$blah1 <- renderDT(x, 
                          selection = 'none',
                          editable = TRUE)
    
    # I'm not exactly how these next two work yet.
    # proxy <- dataTableProxy('x1')
    # 
    # observeEvent(input$x1_cell_edit, {
    #   info <- input$x1_cell_edit
    #   str(info)
    #   i <- info$row
    #   j <- info$col
    #   v <- info$value
    #   
    #   #limits editing to columns specified by j
    #   if ( j > 1 & j < 4) {
    #     x[i, j] <<- DT::coerceValue(v, x[i, j])
    #     replaceData(proxy, x, resetPaging = FALSE)  # important
    #   } else {}
    #   
    # })
    
    output$text <- renderText({
      input$x1_cell_edit
      y()[1, 1]
    })
    
    output$text2 <- renderText({
      input$x1_cell_edit
      y()[1, 6]
    })
    
    # A plot test of the table
    output$plot <- renderPlot({
      input$x1_cell_edit
      plot(X2015.Weight~Material, y())
    })
    
    output$plot2 <- renderPlot({
      input$x1_cell_edit
      hist(y()[,6])
    })
  
    

# reactive editable -------------------------------------------------------

    output$x1 <- renderDT(y(), 
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
      
      #limits editing to columns specified by j
      if (j == 6) {
        y()[i, j] <<- DT::coerceValue(v, y()[i, j])
        replaceData(proxy, y(), resetPaging = FALSE)  # important
      } else {}
      
    })    
    
    
  }
)