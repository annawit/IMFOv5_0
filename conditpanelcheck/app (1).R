#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
one <- 25
two <- 25
three <- 50

ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         
         sliderInput("one", "First", min = 0, max = 100, value = one),
         sliderInput("two", "Second", min = 0, max = 100, value = two),
         sliderInput("three", "Third", min = 0, max = 100, value = three)
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
   
  
   # observeEvent(input$one, {
   #   print("Observe 1")
   #   if (input$one != one) {
   #    two <<- ceiling(input$two / 2)
   #    three <<- ceiling(input$three / 2)
   #    updateSliderInput(session, "two", value = two)
   #    updateSliderInput(session, "three", value = three)
   #    print("Input 1 changed")
   #   }
   #  }
   # )
   
   
   observeEvent(input$one, {
     print("Observe 1")
     if (input$one != one) {
       two <<- ceiling(input$two + (one - input$one)*0.5)
       three <<- ceiling(input$three + (one - input$one)*0.5)
       updateSliderInput(session, "two", value = two)
       updateSliderInput(session, "three", value = three)
       print("Input 1 changed")
     }
   }
   )
   
   
   
#    observeEvent(input$two, {
#      print("Observe 2")
#      if (input$two != two) {
#        one <<- ceiling(input$one / 2)
#        three <<- ceiling(input$three / 2)
#        updateSliderInput(session, "one", value = one)
#        updateSliderInput(session, "three", value = three)
#        print("Input 2 changed")
#      }
#    })
   
      observeEvent(input$two, {
        print("Observe 2")
        if (input$two != two) {
          #takes value of one and adds half of the change in slider two
          one <<- ceiling(input$one + (two - input$two)*0.5)
          #takes value of three and adds half of the change in slider two
          three <<- ceiling(input$three + (two - input$two)*0.5)
          updateSliderInput(session, "one", value = one)
          updateSliderInput(session, "three", value = three)
          print("Input 2 changed")
        }
      })
#    
#    observeEvent(input$three, {
#      print("Observe 3")
#      if (input$three != three) {
#       one <<- ceiling(input$one / 2)
#       two <<- ceiling(input$two / 2)
#       updateSliderInput(session, "one", value = one)
#       updateSliderInput(session, "two", value = two)
#       print("Input 3 changed")
#      }
#    })
# }
      
         observeEvent(input$three, {
           print("Observe 3")
           if (input$three != three) {
            one <<- ceiling(input$one + (three - input$three)*0.5)
            two <<- ceiling(input$two + (three - input$three)*0.5)
            updateSliderInput(session, "one", value = one)
            updateSliderInput(session, "two", value = two)
            print("Input 3 changed")
           }
         })
      }



# Run the application 
shinyApp(ui = ui, server = server)

