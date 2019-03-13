
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

# This function is used for the sliders

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
  
  # this is the initial dataframe created when the user
  # filters based on their region and material
  
  usermass <- reactive({
    mass %>% 
      filter(Wasteshed == input$userregion) %>% 
      filter(Material == input$usermaterial)
  })
  
  # this creates a vector of numbers from the weights of each
  # disposition.
  
  tweight <- reactive({
    usermass() %>% 
      pull(`2015 Weight`)
  })

  # initializes the reactive values for the sliders
  # pct is for the percent value of the sliders, which changes
  # st is for the starting value, which does not change and is used for the reset
  
  one <- reactiveValues(pct = 0, st = 0)
  two <- reactiveValues(pct = 0, st = 0)
  three <- reactiveValues(pct = 0, st = 0)
  
  # These observe event create the percent values for the sliders
  # one$pct is used over and over again as the sliders change, and becomes the 
  # value of the slider
  # one$st is the starting percentage of that disposition, and is used for the 
  # reset button
  
  observeEvent(input$usermaterial, {
    one$pct <- (tweight()[1]/sum(tweight()))*100
    one$st <- (tweight()[1]/sum(tweight()))*100
    print(one$pct)
  })
  
  observeEvent(input$usermaterial, {
    two$pct <- (tweight()[2]/sum(tweight()))*100
    two$st <- (tweight()[2]/sum(tweight()))*100
    print(two$pct)
  })
  
  observeEvent(input$usermaterial, {
    three$pct <- (tweight()[3]/sum(tweight()))*100
    three$st <- (tweight()[3]/sum(tweight()))*100
    print(three$pct)
  })
  
  #just a test of the output
  output$vals <- renderText({
    paste(one$pct, two$pct, three$pct, sum(one$pct, two$pct, three$pct))
  })
  
# Cardboard Panel ---------------------------------------------------------

  
  output$cardboardpanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "one",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = one$st),
      sliderInput(inputId = "two",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two$st),
      sliderInput(inputId = "three",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = three$st),
      actionButton("cardboardreset", "Reset")
    )
    
  })
  
  # watches for the cardboard reset button, and sets slider values back to original
  
  observeEvent(input$cardboardreset, {
    updateSliderInput(session, "one", value = one$st)
    updateSliderInput(session, "two", value = two$st)
    updateSliderInput(session, "three", value = three$st)
  })
  
  observeEvent(input$one, {
    print("Observe 1")
    if (input$one != one$pct) {
      delta <- input$one - one$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltax = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "two", value = two$pct)
      updateSliderInput(session, "three", value = three$pct)
      print(changes)
    }
  }
  )
  
  observeEvent(input$two, {
    print("Observe 2")
    if (input$two != two$pct) {
      delta <- input$two - two$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltay = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "one", value = one$pct)
      updateSliderInput(session, "three", value = three$pct)
      print(changes)
    }
  })
  
  observeEvent(input$three, {
    print("Observe 3")
    if (input$three != three$pct) {
      delta <- input$three - three$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltaz = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "one", value = one$pct)
      updateSliderInput(session, "two", value = two$pct)
      print(changes)
    }
  })
  
# Carpet Panel ------------------------------------------------------------

  output$carpetpanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "one",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "two",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two$pct),
      sliderInput(inputId = "three",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = three$pct),
      actionButton("carpetreset", "Reset")
    )
    
  })
  
  # watches for the cardboard reset button, and sets slider values back to original
  
  observeEvent(input$carpetreset, {
    updateSliderInput(session, "one", value = one$st)
    updateSliderInput(session, "two", value = two$st)
    updateSliderInput(session, "three", value = three$st)
  })
  
  observeEvent(input$one, {
    print("Observe 1")
    if (input$one != one$pct) {
      delta <- input$one - one$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltax = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "two", value = two$pct)
      updateSliderInput(session, "three", value = three$pct)
      print(changes)
    }
  }
  )
  
  observeEvent(input$two, {
    print("Observe 2")
    if (input$two != two$pct) {
      delta <- input$two - two$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltay = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "one", value = one$pct)
      updateSliderInput(session, "three", value = three$pct)
      print(changes)
    }
  })
  
  observeEvent(input$three, {
    print("Observe 3")
    if (input$three != three$pct) {
      delta <- input$three - three$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltaz = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "one", value = one$pct)
      updateSliderInput(session, "two", value = two$pct)
      print(changes)
    }
  })
  
# Electronics Panel -------------------------------------------------------


# Food Panel --------------------------------------------------------------

  output$foodpanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "one",
                  label = "% Anaerobic Digestion",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "two",
                  label = "% Composting",
                  min = 0,
                  max = 100,
                  value = two$pct),
      sliderInput(inputId = "three",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = three$pct),
      actionButton("foodreset", "Reset")
    )
    
  })
  
  # watches for the food reset button, and sets slider values back to original
  
  observeEvent(input$foodreset, {
    updateSliderInput(session, "one", value = one$st)
    updateSliderInput(session, "two", value = two$st)
    updateSliderInput(session, "three", value = three$st)
  })
  
  observeEvent(input$one, {
    print("Observe 1")
    if (input$one != one$pct) {
      delta <- input$one - one$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltax = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "two", value = two$pct)
      updateSliderInput(session, "three", value = three$pct)
      print(changes)
    }
  }
  )
  
  observeEvent(input$two, {
    print("Observe 2")
    if (input$two != two$pct) {
      delta <- input$two - two$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltay = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "one", value = one$pct)
      updateSliderInput(session, "three", value = three$pct)
      print(changes)
    }
  })
  
  observeEvent(input$three, {
    print("Observe 3")
    if (input$three != three$pct) {
      delta <- input$three - three$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltaz = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "one", value = one$pct)
      updateSliderInput(session, "two", value = two$pct)
      print(changes)
    }
  })

# Glass -------------------------------------------------------------------

  output$glasspanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "one",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "two",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = two$pct),
      sliderInput(inputId = "three",
                  label = "% Use as Aggregate",
                  min = 0,
                  max = 100,
                  value = three$pct),
      actionButton("glassreset", "Reset")
    )
    
  })
  
  # watches for the glass reset button, and sets slider values back to original
  
  observeEvent(input$cardboardreset, {
    updateSliderInput(session, "one", value = one$st)
    updateSliderInput(session, "two", value = two$st)
    updateSliderInput(session, "three", value = three$st)
  })
  
  observeEvent(input$one, {
    print("Observe 1")
    if (input$one != one$pct) {
      delta <- input$one - one$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltax = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "two", value = two$pct)
      updateSliderInput(session, "three", value = three$pct)
      print(changes)
    }
  }
  )
  
  observeEvent(input$two, {
    print("Observe 2")
    if (input$two != two$pct) {
      delta <- input$two - two$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltay = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "one", value = one$pct)
      updateSliderInput(session, "three", value = three$pct)
      print(changes)
    }
  })
  
  observeEvent(input$three, {
    print("Observe 3")
    if (input$three != three$pct) {
      delta <- input$three - three$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltaz = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "one", value = one$pct)
      updateSliderInput(session, "two", value = two$pct)
      print(changes)
    }
  })
  
# Trash -------------------------------------------------------------------


# Paper -------------------------------------------------------------------

  output$paperpanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "one",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "two",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two$pct),
      sliderInput(inputId = "three",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = three$pct),
      actionButton("paperreset", "Reset")
    )
    
  })
  
  # watches for the paper reset button, and sets slider values back to original
  
  observeEvent(input$paperreset, {
    updateSliderInput(session, "one", value = one$st)
    updateSliderInput(session, "two", value = two$st)
    updateSliderInput(session, "three", value = three$st)
  })
  
  observeEvent(input$one, {
    print("Observe 1")
    if (input$one != one$pct) {
      delta <- input$one - one$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltax = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "two", value = two$pct)
      updateSliderInput(session, "three", value = three$pct)
      print(changes)
    }
  }
  )
  
  observeEvent(input$two, {
    print("Observe 2")
    if (input$two != two$pct) {
      delta <- input$two - two$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltay = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "one", value = one$pct)
      updateSliderInput(session, "three", value = three$pct)
      print(changes)
    }
  })
  
  observeEvent(input$three, {
    print("Observe 3")
    if (input$three != three$pct) {
      delta <- input$three - three$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltaz = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "one", value = one$pct)
      updateSliderInput(session, "two", value = two$pct)
      print(changes)
    }
  })
  
# Plastic Film ------------------------------------------------------------

  output$plasticfilmpanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "one",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "two",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two$pct),
      sliderInput(inputId = "three",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = three$pct),
      actionButton("plasticfilmreset", "Reset")
    )
    
  })
  
  # watches for the plastic film reset button, and sets slider values back to original
  
  observeEvent(input$plasticfilmreset, {
    updateSliderInput(session, "one", value = one$st)
    updateSliderInput(session, "two", value = two$st)
    updateSliderInput(session, "three", value = three$st)
  })
  
  observeEvent(input$one, {
    print("Observe 1")
    if (input$one != one$pct) {
      delta <- input$one - one$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltax = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "two", value = two$pct)
      updateSliderInput(session, "three", value = three$pct)
      print(changes)
    }
  }
  )
  
  observeEvent(input$two, {
    print("Observe 2")
    if (input$two != two$pct) {
      delta <- input$two - two$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltay = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "one", value = one$pct)
      updateSliderInput(session, "three", value = three$pct)
      print(changes)
    }
  })
  
  observeEvent(input$three, {
    print("Observe 3")
    if (input$three != three$pct) {
      delta <- input$three - three$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltaz = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "one", value = one$pct)
      updateSliderInput(session, "two", value = two$pct)
      print(changes)
    }
  })

# Rigid Plastic -----------------------------------------------------------

  output$rigidplasticpanel <-  renderUI({
    
  #   # https://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app
  #   
  #   # tweight <- oregonweights %>% 
  #   #   filter(Material == "Rigid Plastic") %>% 
  #   #   pull(`2015 Weight`)
  #   
  #   tagList(
  #     div(
  #       id = "panel",
  #       tweight <- usermass() %>% 
  #         pull(`2015 Weight`),
  #       setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
  #       br(),
  #       sliderInput(inputId = "Production",
  #                   label = "Total Mass, in Tons",
  #                   min = 0,
  #                   max = sum(tweight)*1.5,
  #                   value = sum(tweight)),
  #       sliderInput(inputId = "RPCombustion",
  #                   label = "% Combustion",
  #                   min = 0,
  #                   max = 100,
  #                   value = tweight[1]/sum(tweight)),
  #       sliderInput(inputId = "RPLandfilling",
  #                   label = "% Landfilling",
  #                   min = 0,
  #                   max = 100,
  #                   value = tweight[2]/sum(tweight)),
  #       sliderInput(inputId = "RPRecycling",
  #                   label = "% Recycling",
  #                   min = 0,
  #                   max = 100,
  #                   value = tweight[3]/sum(tweight))
  #     )
  #   )
  #   
  # })
  # 
    
    tagList(
      
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "one",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "two",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two$pct),
      sliderInput(inputId = "three",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = three$pct),
      actionButton("rigidplasticreset", "Reset")
    )
    
  })
  
  # watches for the rigid plastic reset button, and sets slider values back to original
  
  observeEvent(input$rigidplasticreset, {
    updateSliderInput(session, "one", value = one$st)
    updateSliderInput(session, "two", value = two$st)
    updateSliderInput(session, "three", value = three$st)
  })
  
  observeEvent(input$one, {
    print("Observe 1")
    if (input$one != one$pct) {
      delta <- input$one - one$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltax = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "two", value = two$pct)
      updateSliderInput(session, "three", value = three$pct)
      print(changes)
    }
  }
  )
  
  observeEvent(input$two, {
    print("Observe 2")
    if (input$two != two$pct) {
      delta <- input$two - two$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltay = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "one", value = one$pct)
      updateSliderInput(session, "three", value = three$pct)
      print(changes)
    }
  })
  
  observeEvent(input$three, {
    print("Observe 3")
    if (input$three != three$pct) {
      delta <- input$three - three$pct
      print(delta)
      changes <- adjust(one$pct, two$pct, three$pct, deltaz = delta)
      one$pct <<- changes[1]
      two$pct <<- changes[2]
      three$pct <<- changes[3]
      
      updateSliderInput(session, "one", value = one$pct)
      updateSliderInput(session, "two", value = two$pct)
      print(changes)
    }
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