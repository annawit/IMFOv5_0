
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
             tabPanel("Introduction",
                      fluidPage(
                        column(12,
                               align = "center",
                               h3("Welcome to the Lifecycle Impacts Tool!", align = "center"),
                               # h2("Interactive Visualizer!", align = "center"),
                               # div(img(src = 'greenpic.jpeg', height="50%", width="50%"), style = "text-align: center;"),
                               br(),
                               HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/-9JRowyICbo" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                               br(),
                               br(),
                               div( style = "width:500px; text-align:center;",
                                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                               )
                        )
                      )
             ),

# Visualize Tab -----------------------------------------------------------

             
tabPanel("Visualize!",
         fluidPage(
           column(4,
                  #fixedpanel allows the sliders to scroll with the page and might
                  #not be necessary in the final version, depending on what is included
                  #the first two arguments conform it to the column width, it is a little
                  #finicky
                  fixedPanel(
                    width = "30%",
                    left = 20,
                  wellPanel(
                    conditionalPanel(
                      condition = "input.usermaterial == `Cardboard`",
                      uiOutput("cardboardpanel")
                      ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Food`",
                      uiOutput("foodpanel")
                    ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Rigid Plastic`",
                      uiOutput("rppanel")
                    ),
                    actionButton("resetsliders", "Reset")
                    
                  ),
                  wellPanel(
                    selectInput(inputId = "usermaterial",
                                label = "Select a material:",
                                choices = unique(mass$Material),
                                selected = "Rigid Plastic")
                  )
                  )),
           column(4,
                  wellPanel(
                    selectInput(inputId = "userregion",
                                label = "",
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


# Cardboard Panel ---------------------------------------------------------

  output$cardboardpanel <-  renderUI({
    
    tweight <- mass %>% 
      filter(Wasteshed == input$userregion)
      filter(Material == input$usermaterials) %>% 
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


# paper -------------------------------------------------------------------


# Plastic Film ------------------------------------------------------------


# Rigid Plastic -----------------------------------------------------------

  output$rppanel <-  renderUI({
    
    # https://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app
    
    # tweight <- oregonweights %>% 
    #   filter(Material == "Rigid Plastic") %>% 
    #   pull(`2015 Weight`)
    
    tagList(
      div(
        id = "panel",
        tweight <- mass %>% 
          filter(Wasteshed == input$userregion) %>% 
          filter(Material == "Rigid Plastic") %>% 
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