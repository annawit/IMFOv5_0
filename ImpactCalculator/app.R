
library(shiny)
library(dplyr)

mass <- read_csv("mass.csv")

# brings in impact factors
impact <- read_csv("I1.csv")

# UI ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Impact Checker",
    tabPanel("Single Material",
      
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          pickerInput(inputId = "usermaterial",
                      label = "Select a waste material:",
                      choices = c("Cardboard", "Electronics",
                                  "Food", "Glass", "Paper", "Wood"),
                      selected = "Cardboard",
                      options = list(style = "btn-primary")),
          pickerInput(inputId = "userimpact",
                      label = "Select an impact:",
                      choices = unique(impact$Category),
                      selected = "Energy demand",
                      options = list(style = "btn-primary"))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          wellPanel(div(style = "height:300px;",
                        DT::dataTableOutput("df"))
          ),
          wellPanel(
            plotlyOutput("impactfactorplot")
          )
        )
      )
    ),
    

# Multi Panel -------------------------------------------------------------
tabPanel("Multiple materials",
             sidebarLayout(
               sidebarPanel(
                 pickerInput(inputId = "usermaterial2",
                             label = "Select a waste material:",
                             choices = unique(impact$Material),
                             selected = unique(impact$Material),
                             multiple = TRUE,
                             options = list(style = "btn-primary")),
                 pickerInput(inputId = "userimpact2",
                             label = "Select an impact:",
                             choices = unique(impact$Category),
                             selected = "Energy demand",
                             options = list(style = "btn-primary"))
               ),
               
               mainPanel(

                 wellPanel(
                   plotlyOutput("multimaterialplot")
                 ),
                 wellPanel(div(DT::dataTableOutput("df2"))
                 )
               )
             )
             
      
    )
  )
)

# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  df <- reactive({
    impact %>% 
      filter(Material == input$usermaterial) %>% 
      filter(Category == input$userimpact)
  })
  
  
  output$df <- DT::renderDataTable({
    df()
  })
   
  output$impactfactorplot <- renderPlotly({
    p <- plot_ly(df(),
                 x = ~Disposition,
                 y = ~Factor,
                 name = ~Disposition,
                 color = ~`Life Cycle Stage`,
                 type = "bar"
    )
    p %>% 
      layout(
        yaxis = list(title =  paste("Impact in", df()$Units[[1]])),
        xaxis = list(title = "")
      ) %>% 
      config(displaylogo = FALSE,
             collaborate = FALSE,
             modeBarButtonsToRemove = list(
               'sendDataToCloud',
               'zoom2d',
               'pan2d',
               'select2d',
               'lasso2d',
               'zoomIn2d',
               'zoomOut2d',
               'autoScale2d',
               'resetScale2d',
               'toggleSpikelines'
             ))
    
  })

# multi -------------------------------------------------------------------

  
  
  df2spread <- reactive({
    impact %>% 
      select(-"Life Cycle Stage") %>% 
      spread("Disposition", "Factor", fill = 0) %>% 
      filter(Material %in% input$usermaterial2) %>% 
      filter(Category %in% input$userimpact2)
      
  })
  
  # df2spread <- reactive({
  #   df2() %>% 
  #     select(-"Life Cycle Stage") %>% 
  #     spread("Disposition", "Factor", fill = 0)
  # })
  # 
  
  
  output$df2 <- DT::renderDataTable({
    df2spread()
  })
  
  output$multimaterialplot <- renderPlotly({
    p <- plot_ly(df2spread(),
                 x = ~Material,
                 type = 'bar',
                 y = ~Production,
                 name = "Production") %>% 
      add_trace(y = ~Landfilling,
                name = "Landfilling") %>% 
      add_trace(y = ~Combustion,
                name = "Combustion") %>% 
      add_trace(y = ~Recycling,
                name = "Recycling") %>% 
      add_trace(y = ~`Anaerobic Digestion`,
                name = "Anaerobic Digestion") %>% 
      add_trace(y = ~Composting,
                name = "Composting") %>% 
      add_trace(y = ~`Use as Aggregate`,
                name = "Use as Aggregate") %>% 
      layout(barmode = 'group')

    p %>% 
      layout(
        yaxis = list(title =  paste("Impact in", df()$Units[[1]])),
        xaxis = list(title = "")
      ) %>% 
      config(displaylogo = FALSE,
             collaborate = FALSE,
             modeBarButtonsToRemove = list(
               'sendDataToCloud',
               'zoom2d',
               'pan2d',
               'select2d',
               'lasso2d',
               'zoomIn2d',
               'zoomOut2d',
               'autoScale2d',
               'resetScale2d',
               'toggleSpikelines'
             ))
    
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

