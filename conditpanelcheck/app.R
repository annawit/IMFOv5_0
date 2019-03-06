#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# data("faithful")
# view(faithful)


library(shiny)
library(shinyjs)


# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        actionButton("resetsliders", "Reset"),

           uiOutput("slider2")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
  output$slider2 <- renderUI({
    
    tw <- faithful %>% 
      select(waiting) %>% 
      pull(waiting)
    div(
      id = "panel",
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput("fun",
                 "How much fun?",
                 min = 0,
                 max = 100,
                 value = 50),
    sliderInput("habit",
                "How much habit?",
                min = 0,
                max = 100,
                value = 50),
    sliderInput("fate",
                "How much fate?",
                min = 0,
                max = 100,
                value = 33)
    )
    
   })
  
  observe({
    
    # input$fun
    # input$habit
    # 
    req(input$fun)
    
      if (input$fun == 0 & input$habit == 0) 
        {
        updateSliderInput(session = session,
                          inputId = "fate",
                          value = 99)

      } else {
        updateSliderInput(session = session, 
                          inputId = "fate", 
                          value = 
                            round((100 * 
                                     input$fate / (input$fun + 
                                                     input$habit + 
                                                     isolate(input$fate))),
                                  digits = 0)
        )
      }
    })
  
  observe({
      
    # input$fate
    # input$fun
    req(input$fate)
    
      if ((input$fun == 100 & input$fate == 0 & input$habit == 0) |
          (input$fun == 0 & input$fate == 100 & input$habit == 0)) {
        updateSliderInput(session = session,
                          inputId = "habit",
                          value = 0.1)
      } else {
        updateSliderInput(session = session, 
                          inputId = "habit", 
                          value = 
                            round((100 * 
                                     input$habit/(input$fun + 
                                                    isolate(input$habit) + 
                                                    input$fate)), 
                                  digits = 0)
                          )
      }
    })
  
  observe({
      
    # input$fate
    # input$habit
    req(input$habit)
    
    
      if ((input$habit == 0 & input$fate == 100 & input$fun == 0) | 
           (input$habit == 100 & input$fate == 0 & input$fun == 0) )
        { updateSliderInput(session = session, 
                          inputId = "fun", 
                          value = 0.1)
      } else {
        updateSliderInput(session = session, 
                          inputId = "fun", 
                          value = 
                            round((100 * 
                                     input$fun/(isolate(input$fun) + 
                                                  input$habit + 
                                                  input$fate)), 
                                  digits = 0)
                          )
      }
    })
  
  observeEvent(input$resetsliders, {
    reset("panel")
  }
    
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

