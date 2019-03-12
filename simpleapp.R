
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

one <- 50
two <- 20
three <- 30


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
                    textOutput("vals"),
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
                    )
                    
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
  
  adjust <- function(x, y, z, deltax = 0, deltay = 0, deltaz = 0) {
    
    if (deltax != 0) {
      x <- x + deltax
      diff <- deltax / 2
      
      ydiff <- min(y, diff)
      zdiff <- deltax - ydiff #  min(z, deltax - ydiff)
      y <- ceiling(y - ydiff)
      z <- floor(z - zdiff)
      
      if (x == 100) {
        y = 0
        z = 0
      }
    }
    
    else if (deltay != 0) {
      y <- y + deltay
      diff <- deltay / 2
      
      xdiff <- min(x, diff)
      zdiff <- deltay - xdiff #min(z, deltay - xdiff)
      x <- ceiling(x - xdiff)
      z <- floor(z - zdiff)
      
      if (y == 100) {
        x = 0
        z = 0
      }
    }
    
    else if (deltaz != 0) {
      z <- z + deltaz
      diff <- deltaz / 2
      
      xdiff <- min(x, diff)
      ydiff <- deltaz - xdiff #min(z, deltaz - xdiff)
      x <- ceiling(x - xdiff)
      y <- floor(y - ydiff)
      
      if (z == 100) {
        x = 0
        y = 0
      }
    }
    
    return( c(x, y, z, x+y+z) )
  }
  
  
  
  
  usermass <- reactive({
    mass %>% 
      filter(Wasteshed == input$userregion) %>% 
      filter(Material == input$usermaterial)
  })
  
  tweight <- reactive({
    usermass() %>% 
      pull(`2015 Weight`)
  })

  one <- reactiveValues()
  two <- reactiveValues()
  three <- reactiveValues()
  four <- reactiveValues()
  five <- reactiveValues()
  
  
  oner <- reactive({
    one <- tweight()[[1]]/sum(tweight())
    one
  })
  
  twor <- reactive({
    two <- tweight()[[2]]/sum(tweight())
    two
  })
  
  threer <- reactive({
    three <- tweight()[[3]]/sum(tweight())
    three
  })
  
  fourr <- reactive({
    four <- tweight()[[4]]/sum(tweight())
    four
  })
  
  fiver <- reactive({
    five <- tweight()[[5]]/sum(tweight())
    five
  })
  
  output$vals <- renderText({
    paste(one, two, three)
  })
  
# Cardboard Panel ---------------------------------------------------------

  
  output$cardboardpanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Mass, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "one",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = one),
      sliderInput(inputId = "two",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two),
      sliderInput(inputId = "three",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = three),
      actionButton("cardboardreset", "Reset"),
    
    print(tweight())
    )
    
  })
  
  observeEvent(input$reset, {
    one <<- oner()
    two <<- twor()
    three <<- threer()
    
    updateSliderInput(session, "one", value = one)
    updateSliderInput(session, "two", value = two)
    updateSliderInput(session, "three", value = three)
  })
  
  observeEvent(input$one, {
    print("Observe 1")
    if (input$one != one) {
      delta <- input$one - one
      print(delta)
      changes <- adjust(one, two, three, deltax = delta)
      one <<- changes[[1]]
      two <<- changes[[2]]
      three <<- changes[[3]]
      
      updateSliderInput(session, "two", value = two)
      updateSliderInput(session, "three", value = three)
      print(changes)
    }
  }
  )
  
  observeEvent(input$two, {
    print("Observe 2")
    if (input$two != two) {
      delta <- input$two - two
      print(delta)
      changes <- adjust(one, two, three, deltay = delta)
      one <<- changes[[1]]
      two <<- changes[[2]]
      three <<- changes[[3]]
      
      updateSliderInput(session, "one", value = one)
      updateSliderInput(session, "three", value = three)
      print(changes)
    }
  })
  
  observeEvent(input$three, {
    print("Observe 3")
    if (input$three != three) {
      delta <- input$three - three
      print(delta)
      changes <- adjust(one, two, three, deltaz = delta)
      one <<- changes[[1]]
      two <<- changes[[2]]
      three <<- changes[[3]]
      
      updateSliderInput(session, "one", value = one)
      updateSliderInput(session, "two", value = two)
      print(changes)
    }
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