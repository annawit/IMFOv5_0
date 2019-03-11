
library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(viridis)
library(shinyjs)
library(shinyBS)
library(packcircles)
library(ggiraph)
library(ggrepel)


mass <- read_csv("mass.csv")

I1 <- read_csv("I1.csv")

d <- read_csv("impact_dictionary.csv")

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  chooseSliderSkin("Modern"),
  useShinyjs(),
  
  navbarPage("Lifecycle Impacts Tool",
             
             # Introduction tab -------------------------------------------------------
             # tabPanel("Introduction",
             #          fluidPage(
             #            column(12,
             #                   align = "center",
             #                   h3("Welcome to the Lifecycle Impacts Tool!", align = "center"),
             #                   # h2("Interactive Visualizer!", align = "center"),
             #                   # div(img(src = 'greenpic.jpeg', height="50%", width="50%"), style = "text-align: center;"),
             #                   br(),
             #                   HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/-9JRowyICbo" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
             #                   br(),
             #                   br(),
             #                   div( style = "width:500px; text-align:center;",
             #                        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
             #                   )
             #            )
             #          )
             # ),

# Visualize Tab -----------------------------------------------------------

             
tabPanel("Visualize!",
         fluidPage(
           column(4,
                  wellPanel(
                    conditionalPanel(
                      condition = "input.usermaterial == `Cardboard`",
                      uiOutput("cardboardpanel")
                    ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Carpet`",
                      uiOutput("carpetpanel")
                    ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Electronics`",
                      uiOutput("electronicspanel")
                    ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Food`",
                      uiOutput("foodpanel")
                    ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Glass`",
                      uiOutput("glasspanel")
                    ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Trash`",
                      uiOutput("trashpanel")
                    ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Paper`",
                      uiOutput("paperpanel")
                    ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Plastic Film`",
                      uiOutput("plasticffilmpanel")
                    ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Rigid Plastic`",
                      uiOutput("rigidplasticpanel")
                    ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Scrap Metal`",
                      uiOutput("scrapmetalpanel")
                    ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Wood`",
                      uiOutput("woodpanel")
                    ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Yard Debris`",
                      uiOutput("yarddebrispanel")
                    ),
                    actionButton("resetsliders", "Reset")
                    
                  ),
                  wellPanel(
                    selectInput(inputId = "usermaterial",
                                label = "Select a material:",
                                choices = unique(mass$Material),
                                selected = "Rigid Plastic")
                  )
           ),
           column(4,
                  wellPanel(),
                  wellPanel(
                    selectInput(inputId = "userregion",
                                label = "Select a region:",
                                choices = unique(mass$Wasteshed),
                                selected = "Oregon total")
                  )),
           column(4,
                  wellPanel(),
                  wellPanel(
                    selectInput(inputId = "userimpact",
                                label = "Select an impact:",
                                choices = unique(I1$Category),
                                selected = "Global warming"))
           )
         )
                      ),
tabPanel("Context"),

# More NavbarMenu ---------------------------------------------------------


             navbarMenu("More",
                        tabPanel("Glossary"),
                        tabPanel("Resources"),
                        tabPanel("About")
             )
  )
)



server <- function(input, output, session) {

  usermass <- reactive({
    mass %>% 
      filter(Wasteshed == input$userregion) %>% 
      filter(Material == input$usermaterial)
  })

# Cardboard Panel ---------------------------------------------------------

  output$cardboardpanel <-  renderUI({
    
    tweight <- usermass() %>% 
      pull(`2015 Weight`)
    
    tagList(
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Mass, in Tons",
                  min = 0,
                  max = sum(tweight)*1.5,
                  value = sum(tweight)),
      sliderInput(inputId = "CBCombustion",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = tweight[1]/sum(tweight)),
      sliderInput(inputId = "CBLandfilling",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = tweight[2]/sum(tweight)),
      sliderInput(inputId = "CBRecycling",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = tweight[3]/sum(tweight))
    )
    
  })
  
  observeEvent({
    input$CBCombustion
    input$CBRecycling
  }, {
    updateSliderInput(session = session, 
                      inputId = "CBLandfilling", 
                      value = 
                        round((100 * 
                                 input$CBLandfilling/(input$CBCombustion + 
                                                        input$CBLandfilling + 
                                                        input$CBRecycling)), 
                              digits = 2)
    )
  })
  
  
  observeEvent({
    input$CBLandfilling
    input$CBRecycling
  },  {
    updateSliderInput(session = session,
                      inputId = "CBCombustion",
                      value = round((100 * 
                                       input$CBCombustion/(input$CBCombustion + 
                                                             input$CBLandfilling + 
                                                             input$CBRecycling)), 
                                    digits = 2)
    )
  })
  
  observeEvent({
    input$CBCombustion
    input$CBLandfilling
  },  {
    updateSliderInput(session = session,
                      inputId = "CBRecycling",
                      value = round((100 * input$CBRecycling/(input$CBCombustion + 
                                                                input$CBLandfilling + 
                                                                input$CBRecycling)), 
                                    digits = 2)
    )
  })

# Carpet Panel ------------------------------------------------------------


# Electronics Panel -------------------------------------------------------


# Food Panel --------------------------------------------------------------

  output$foodpanel <- renderUI({
    
    #tagList binds the items together for output in renderUI
    tagList(
      sliderInput(inputId = "Production",
                  label = "Total Mass, in Tons",
                  min = 0,
                  max = 100,
                  value = 50),
      sliderInput(inputId = "slider3AD",
                  label = "Anaerobic Digestion",
                  min   = 0,
                  max   = 100,
                  value = 50),
      sliderInput(inputId = "slider3cp",
                  label = "Composting",
                  min   = 0,
                  max   = 100,
                  value = 50),
      sliderInput(inputId = "slider3L",
                  label = "Landfilling",
                  min   = 0,
                  max   = 100,
                  value = 50)
    )
  })

# Glass -------------------------------------------------------------------


# Trash -------------------------------------------------------------------


# Paper -------------------------------------------------------------------


# Plastic Film ------------------------------------------------------------


# Rigid Plastic -----------------------------------------------------------

  output$rigidplasticpanel <-  renderUI({
    
    # https://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app
    
    # tweight <- oregonweights %>% 
    #   filter(Material == "Rigid Plastic") %>% 
    #   pull(`2015 Weight`)
    
    tagList(
      div(
        id = "panel",
        tweight <- usermass() %>% 
          pull(`2015 Weight`),
        setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
        br(),
        sliderInput(inputId = "Production",
                    label = "Total Mass, in Tons",
                    min = 0,
                    max = sum(tweight)*1.5,
                    value = sum(tweight)),
        sliderInput(inputId = "RPCombustion",
                    label = "% Combustion",
                    min = 0,
                    max = 100,
                    value = tweight[1]/sum(tweight)),
        sliderInput(inputId = "RPLandfilling",
                    label = "% Landfilling",
                    min = 0,
                    max = 100,
                    value = tweight[2]/sum(tweight)),
        sliderInput(inputId = "RPRecycling",
                    label = "% Recycling",
                    min = 0,
                    max = 100,
                    value = tweight[3]/sum(tweight))
      )
    )
    
  })
  
  observeEvent({
    input$RPCombustion
    input$RPRecycling
  }, {
    
      updateSliderInput(session = session, 
                        inputId = "RPLandfilling", 
                        value = 
                          round((100 * 
                                   input$RPLandfilling/(input$RPCombustion + 
                                                          input$RPLandfilling + 
                                                          input$RPRecycling)), 
                                digits = 2)
      )
                        
  })
  
  
  observeEvent({
    input$RPLandfilling
    input$RPRecycling
  },  {
    updateSliderInput(session = session,
                      inputId = "RPCombustion",
                      value = round((100 * 
                                       input$RPCombustion/(input$RPCombustion + 
                                                             input$RPLandfilling + 
                                                             input$RPRecycling)), 
                                    digits = 2)
    )
  })
  
  observeEvent({
    input$RPCombustion
    input$RPLandfilling
  },  {
    updateSliderInput(session = session,
                      inputId = "RPRecycling",
                      value = round((100 * input$RPRecycling/(input$RPCombustion + 
                                                                input$RPLandfilling + 
                                                                input$RPRecycling)), 
                                    digits = 2)
    )
  })

# Scrap Metal -------------------------------------------------------------


# Wood --------------------------------------------------------------------


# Yard Debris -------------------------------------------------------------


# Reset button ------------------------------------------------------------

observeEvent(input$resetsliders, {
  reset("panel")
})  
  
}

shinyApp(ui = ui, server = server)