
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

# brings in main table of regions/waste types/masses
mass <- read_csv("mass.csv")

# brings in impact factors
I1 <- read_csv("I1.csv")


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
                    
                    # textOutput("vals"),
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
                      uiOutput("plasticfilmpanel")
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
                    selectInput(inputId = "usermaterial",
                                label = "Select a material:",
                                choices = unique(mass$Material),
                                selected = "Cardboard")
                  )
           ),
           column(4,
                  wellPanel(
                    plotlyOutput("massplot"),
                    hr(),
                    selectInput(inputId = "userregion",
                                label = "Select a region:",
                                choices = unique(mass$Wasteshed),
                                selected = "Oregon total"),
                  tableOutput("df"),
                  tableOutput("cbdf")
                  )
                  ),
           column(4,
                  wellPanel(
                    plotlyOutput("cbplot"),
                    hr(),
                    selectInput(inputId = "userimpact",
                                label = "Select an impact:",
                                choices = unique(I1$Category),
                                selected = "Global warming"),
                    DT::dataTableOutput("userimpact")
                  )
           )
         )
),
tabPanel("Context"),

# More NavbarMenu ---------------------------------------------------------


             navbarMenu("More",
                        tabPanel("Glossary",
                                 fluidPage(
                                   column(3, wellPanel()),
                                   column(9,
                                          wellPanel(
                                            includeMarkdown("Glossary.md")
                                          )
                                   )
                                 )
                        ),
                        tabPanel("Resources"),
                        tabPanel("About")
             )
  )
)




server <- function(input, output, session) {


# General -------------------------------------------------------------

  
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
  
  tdisp <- reactive({
    usermass() %>% 
      pull(Disposition)
  })

  # initializes the reactive values for the sliders
  # pct is for the percent value of the sliders, which changes
  # st is for the starting value, which does not change and is used for the reset
  
  one <- reactiveValues(pct = 0, st = 0)
  two <- reactiveValues(pct = 0, st = 0)
  three <- reactiveValues(pct = 0, st = 0)
  four <- reactiveValues(pct = 0, st = 0)
  
  # These observe event create the percent values for the sliders
  # The pct/st is left over from when I was trying to create a reset button
  
  observeEvent(input$usermaterial, {
    one$pct <- (tweight()[1]/sum(tweight()))
    one$st <- (tweight()[1]/sum(tweight()))
    print(one$pct)
  })
  
  observeEvent(input$usermaterial, {
    two$pct <- (tweight()[2]/sum(tweight()))
    two$st <- (tweight()[2]/sum(tweight()))
    print(two$pct)
  })
  
  observeEvent(input$usermaterial, {
    three$pct <- (tweight()[3]/sum(tweight()))
    three$st <- (tweight()[3]/sum(tweight()))
    print(three$pct)
  })
  
  observeEvent(input$usermaterial, {
    four$pct <- (tweight()[4]/sum(tweight()))
    four$st <- (tweight()[4]/sum(tweight()))
    print(four$pct)
  })
  
  #just a test of the output
  output$vals <- renderText({
    paste(tweight()[1], one$pct, two$pct, three$pct, four$pct, sum(one$pct, two$pct, three$pct, four$pct))
  })
  
  # creates reactive impact dataframe
  
  userimpact <- reactive({
    
    I1 %>% 
      filter(Category == input$userimpact) %>% 
      filter(Material == input$usermaterial)
    
  })
  
  output$userimpact <- DT::renderDataTable({
    userimpact()
  })
  
# Cardboard Panel ---------------------------------------------------------

  
  output$cardboardpanel <-  renderUI({
    
    tagList(
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      sliderInput(inputId = "Production",
                  label = "Total Waste, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "cbcslide",
                  label = paste("%", tdisp()[1]),
                  min = 0,
                  max = 1,
                  value = one$st),
      sliderInput(inputId = "cblslide",
                  label = paste("%", tdisp()[2]),
                  min = 0,
                  max = 1,
                  value = two$st),
      sliderInput(inputId = "cbrslide",
                  label = paste("%", tdisp()[3]),
                  min = 0,
                  max = 1,
                  value = three$st)
    )
  })
  
  observeEvent({
    input$cbcslide
    input$cblslide
  }, {
    updateSliderInput(session = session,
                      inputId = "cbrslide",
                      value = input$cbrslide/(input$cbcslide + input$cblslide + input$cbrslide)
                      )
  })
  
  observeEvent({
    input$cbcslide
    input$cbrslide
  }, {
    updateSliderInput(session = session,
                      inputId = "cblslide",
                      value = input$cblslide/(input$cbcslide + input$cblslide + input$cbrslide)
                      )
  })
  observeEvent({
    input$cbrslide
    input$cblslide
  }, {
    updateSliderInput(session = session,
                      inputId = "cbcslide",
                      value = input$cbcslide/(input$cbcslide + input$cblslide + input$cbrslide)
                      )
  })
  
  cbmassdf <- reactive({
    
    Disposition       <- c(tdisp()[1], tdisp()[2], tdisp()[3], "Production")
    `Initial Weight`  <- c(tweight()[1], tweight()[2], tweight()[3], sum(tweight()))
    `Scenario Weight` <- c(input$Production*input$cbcslide,
                           input$Production*input$cblslide,
                           input$Production*input$cbrslide,
                           input$Production)
    
    tibble(Disposition, `Initial Weight`, `Scenario Weight`) %>% 
      left_join(userimpact()) %>% 
      mutate(`Initial Impact` = `Initial Weight`*Factor,
             `New Scenario Impact` = `Scenario Weight`*Factor) %>% 
      select(Disposition, `Initial Weight`, `Scenario Weight`, `Initial Impact`, `New Scenario Impact`)
  # %>% 
  #     gather(key = `Scenario`, value = "Weight", c(`Initial Weight`, `Scenario Weight`))
  })
  
  cardboarddf <- reactive({
    cbmassdf() %>% 
      gather(key = "Variable", value = "Value", c(`Initial Weight`, `Scenario Weight`,
                                                  `Initial Impact`, `New Scenario Impact`))
  })
  
  
  output$cbdf <- renderTable({
    cbmassdf()
  }, digits = 0)
  
  output$df <- renderTable({
    cardboarddf()
  }, digits = 0)
  
  output$massplot <- renderPlotly({
    req(cardboarddf())
    #Add traces with a for loop
    #https://stackoverflow.com/questions/46583282/r-plotly-to-add-traces-conditionally-based-on-available-columns-in-dataframe
    massplot <- plot_ly(cardboarddf() %>%
                          filter(grepl("Weight", Variable)) %>%
                          spread("Disposition", "Value"),
                        x = ~Variable,
                        y = ~Combustion,
                        name = "Combustion weight",
                        marker = list(color = ("#A7B753")),
                        type = "bar") %>% 
      add_trace(y = ~Landfilling,
                name = "Landfilling weight",
                marker = list(color = ("#492F42"))) %>% 
      add_trace(y = ~Recycling,
                name = "Recycling weight",
                marker = list(color = ("#389476"))) %>% 
      layout(barmode = "relative")
    
    massplot %>% 
      layout(yaxis = list(overlaying = "y",
                          title = "Weight in tons"),
             xaxis = list(title = ""),
             legend = list(orientation = 'h')
      )
  })
  
  output$cbplot <- renderPlotly({
    req(cardboarddf())
    
    p <- plot_ly(cardboarddf() %>% 
                   filter(grepl("Impact", Variable)) %>% 
                   spread("Disposition", "Value") %>% 
                   mutate(Sum = rowSums(.[2:5])),
                 x = ~Variable,
                 y = ~Production,
                 name = "Production impact",
                 marker = list(color = ("#3A6276")),
                 type = "bar") %>% 
      add_trace(y = ~Combustion,
                name = "Combustion impact",
                marker = list(color = ("#A7B753"))) %>% 
      add_trace(y = ~Landfilling,
                name = "Landfilling impact",
                marker = list(color = ("#492F42"))) %>% 
      add_trace(y = ~Recycling,
                name = "Recycling impact",
                marker = list(color = ("#389476"))) %>% 
      layout(barmode = "relative")
    
    p %>% 
      add_trace(y = ~Sum,
                type = "scatter",
                mode = "line",
                name = "Net impact",
                marker = list(size = ~log(Sum),
                              color = ("#C59B44"))) %>% 
      layout(yaxis = list(overlaying = "y",
                          title = paste("Impact in", userimpact()$Units[[1]])),
             xaxis = list(title = ""),
             legend = list(orientation = 'h')
      )
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
      sliderInput(inputId = "ccslide",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "clslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two$pct),
      sliderInput(inputId = "crslide",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = three$pct)
    )
    
  })
  
  observeEvent({
    input$ccslide
    input$clslide
  }, {
    updateSliderInput(session = session,
                      inputId = "crslide",
                      value = round(100*input$crslide/(input$ccslide + input$clslide + input$crslide)))
  })
  
  observeEvent({
    input$ccslide
    input$crslide
  }, {
    updateSliderInput(session = session,
                      inputId = "clslide",
                      value = round(100*input$clslide/(input$ccslide + input$clslide + input$crslide)))
  })
  observeEvent({
    input$crslide
    input$clslide
  }, {
    updateSliderInput(session = session,
                      inputId = "ccslide",
                      value = round(100*input$ccslide/(input$ccslide + input$clslide + input$crslide)))
  })
  
# Electronics Panel -------------------------------------------------------

  output$electronicspanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "elslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "erslide",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = two$pct)
    )
  })
  
  observeEvent({
    input$erslide
  }, {
    updateSliderInput(session = session,
                      inputId = "elslide",
                      value = ceiling(100*input$elslide/(input$erslide + input$elslide)))
  })
  
  observeEvent({
    input$elslide
  }, {
    updateSliderInput(session = session,
                      inputId = "erslide",
                      value = ceiling(100*input$erslide/(input$erslide + input$elslide)))
  })


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
      sliderInput(inputId = "fadslide",
                  label = "% Anaerobic Digestion",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "fcslide",
                  label = "% Composting",
                  min = 0,
                  max = 100,
                  value = two$pct),
      sliderInput(inputId = "flslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = three$pct)
    )
    
  })
  
  observeEvent({
    input$fadslide
    input$flslide
  }, {
    updateSliderInput(session = session,
                      inputId = "fcslide",
                      value = ceiling(100*input$fcslide/(input$fcslide + input$fadslide + input$flslide)))
  })
  
  observeEvent({
    input$fadslide
    input$fcslide
  }, {
    updateSliderInput(session = session,
                      inputId = "flslide",
                      value = ceiling(100*input$flslide/(input$fcslide + input$fadslide + input$flslide)))
  })
  observeEvent({
    input$fcslide
    input$flslide
  }, {
    updateSliderInput(session = session,
                      inputId = "fadslide",
                      value = ceiling(100*input$fadslide/(input$fcslide + input$fadslide + input$flslide)))
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
      sliderInput(inputId = "glslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "grslide",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = two$pct),
      sliderInput(inputId = "guaslide",
                  label = "% Use as Aggregate",
                  min = 0,
                  max = 100,
                  value = three$pct)
    )
    
  })
  
  observeEvent({
    input$grslide
    input$glslide
  }, {
    updateSliderInput(session = session,
                      inputId = "guaslide",
                      value = ceiling(100*input$guaslide/(input$guaslide + input$glslide + input$grslide)))
  })
  
  observeEvent({
    input$guaslide
    input$grslide
  }, {
    updateSliderInput(session = session,
                      inputId = "glslide",
                      value = ceiling(100*input$glslide/(input$guaslide + input$glslide + input$grslide)))
  })
  observeEvent({
    input$guaslide
    input$glslide
  }, {
    updateSliderInput(session = session,
                      inputId = "grslide",
                      value = ceiling(100*input$grslide/(input$guaslide + input$glslide + input$grslide)))
  })
  
# Trash -------------------------------------------------------------------

  output$trashpanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "tcslide",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "tlslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two$pct)
    )
  })
  
  observeEvent({
    input$tcslide
  }, {
    updateSliderInput(session = session,
                      inputId = "tlslide",
                      value = ceiling(100*input$tlslide/(input$tcslide + input$tlslide)))
  })
  
  observeEvent({
    input$tlslide
  }, {
    updateSliderInput(session = session,
                      inputId = "tcslide",
                      value = ceiling(100*input$tcslide/(input$tcslide + input$tlslide)))
  })

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
      sliderInput(inputId = "pcslide",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "plslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two$pct),
      sliderInput(inputId = "prslide",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = three$pct)
    )
    
  })
  
  observeEvent({
    input$prslide
    input$plslide
  }, {
    updateSliderInput(session = session,
                      inputId = "pcslide",
                      value = ceiling(100*input$pcslide/(input$pcslide + input$plslide + input$prslide)))
  })
  
  observeEvent({
    input$pcslide
    input$prslide
  }, {
    updateSliderInput(session = session,
                      inputId = "plslide",
                      value = ceiling(100*input$plslide/(input$pcslide + input$plslide + input$prslide)))
  })
  observeEvent({
    input$pcslide
    input$plslide
  }, {
    updateSliderInput(session = session,
                      inputId = "prslide",
                      value = ceiling(100*input$prslide/(input$pcslide + input$plslide + input$prslide)))
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
      sliderInput(inputId = "pfcslide",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "oflslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two$pct),
      sliderInput(inputId = "pfrslide",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = three$pct)
    )
  })
  
  observeEvent({
    input$pfrslide
    input$pflslide
  }, {
    updateSliderInput(session = session,
                      inputId = "pfcslide",
                      value = floor(100*input$pfcslide/(input$pfcslide + input$pflslide + input$pfrslide)))
  })
  
  observeEvent({
    input$pfcslide
    input$pfrslide
  }, {
    updateSliderInput(session = session,
                      inputId = "pflslide",
                      value = floor(100*input$pflslide/(input$pfcslide + input$pflslide + input$pfrslide)))
  })
  observeEvent({
    input$pfcslide
    input$pflslide
  }, {
    updateSliderInput(session = session,
                      inputId = "pfrslide",
                      value = floor(100*input$pfrslide/(input$pfcslide + input$pflslide + input$pfrslide)))
  })

# Rigid Plastic -----------------------------------------------------------

  output$rigidplasticpanel <-  renderUI({

    tagList(
      
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "rpcslide",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "rplslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two$pct),
      sliderInput(inputId = "rprslide",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = three$pct)
    )
    
  })
  
  observeEvent({
    input$rprslide
    input$rplslide
  }, {
    updateSliderInput(session = session,
                      inputId = "rpcslide",
                      value = floor(100*input$rpcslide/(input$rpcslide + input$rplslide + input$rprslide)))
  })
  
  observeEvent({
    input$rpcslide
    input$rprslide
  }, {
    updateSliderInput(session = session,
                      inputId = "rplslide",
                      value = floor(100*input$rplslide/(input$rpcslide + input$rplslide + input$rprslide)))
  })
  observeEvent({
    input$rpcslide
    input$rplslide
  }, {
    updateSliderInput(session = session,
                      inputId = "rprslide",
                      value = floor(100*input$rprslide/(input$rpcslide + input$rplslide + input$rprslide)))
  })
  
# Scrap Metal -------------------------------------------------------------

  output$scrapmetalpanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "smlslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "smrslide",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = two$pct)
    )
  })
  
  observeEvent({
    input$smlslide
  }, {
    updateSliderInput(session = session,
                      inputId = "smrslide",
                      value = ceiling(100*input$smrslide/(input$smrslide + input$smlslide)))
  })
  
  observeEvent({
    input$smrslide
  }, {
    updateSliderInput(session = session,
                      inputId = "smlslide",
                      value = ceiling(100*input$smlslide/(input$smrslide + input$smlslide)))
  })

# Wood --------------------------------------------------------------------

  output$woodpanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "wcbslide",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "wcpslide",
                  label = "% Composting",
                  min = 0,
                  max = 100,
                  value = two$pct),
      sliderInput(inputId = "wlslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = three$pct),
      sliderInput(inputId = "wrslide",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = four$pct)
    )
    
  })
  
  observeEvent({
    input$wcbslide
    input$wcpslide
    input$wlslide
  }, {
    updateSliderInput(session = session,
                      inputId = "wrslide",
                      value = ceiling(100*input$wrslide/(input$wcbslide +
                                                         input$wcpslide +
                                                         input$wlslide +
                                                           input$wrslide)))
  })
  
  observeEvent({
    input$wcbslide
    input$wcpslide
    input$wrslide
  }, {
    updateSliderInput(session = session,
                      inputId = "wlslide",
                      value = ceiling(100*input$wlslide/(input$wcbslide +
                                                           input$wcpslide +
                                                           input$wlslide +
                                                           input$wrslide)))
  })
  
  observeEvent({
    input$wcbslide
    input$wlslide
    input$wrslide
  }, {
    updateSliderInput(session = session,
                      inputId = "wcpslide",
                      value = ceiling(100*input$wcpslide/(input$wcbslide +
                                                            input$wcpslide +
                                                            input$wlslide +
                                                            input$wrslide)))
  })
  
  observeEvent({
    input$wcpslide
    input$wlslide
    input$wrslide
  }, {
    updateSliderInput(session = session,
                      inputId = "wcbslide",
                      value = ceiling(100*input$wcbslide/(input$wcbslide +
                                                            input$wcpslide +
                                                            input$wlslide +
                                                            input$wrslide)))
  })
  

  
# Yard Debris -------------------------------------------------------------

  output$yarddebrispanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#3A6276", "#A7B753", "#492F42", "#389476"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "yadslide",
                  label = "% Anaerobic Digestion",
                  min = 0,
                  max = 100,
                  value = one$pct),
      sliderInput(inputId = "ycbslide",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = two$pct),
      sliderInput(inputId = "ycpslide",
                  label = "% Composting",
                  min = 0,
                  max = 100,
                  value = three$pct),
      sliderInput(inputId = "ylslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = four$pct)
    )
    
  })
  
  observeEvent({
    input$ycbslide
    input$ycpslide
    input$ylslide
  }, {
    updateSliderInput(session = session,
                      inputId = "yadslide",
                      value = ceiling(100*input$yadslide/(input$ycbslide +
                                                           input$ycpslide +
                                                           input$ylslide +
                                                           input$yadslide)))
  })
  
  observeEvent({
    input$ycbslide
    input$ycpslide
    input$yadslide
  }, {
    updateSliderInput(session = session,
                      inputId = "ylslide",
                      value = ceiling(100*input$ylslide/(input$ycbslide +
                                                           input$ycpslide +
                                                           input$ylslide +
                                                           input$yadslide)))
  })
  
  observeEvent({
    input$ycbslide
    input$ylslide
    input$yadslide
  }, {
    updateSliderInput(session = session,
                      inputId = "wcpslide",
                      value = ceiling(100*input$wcpslide/(input$ycbslide +
                                                            input$ycpslide +
                                                            input$ylslide +
                                                            input$yadslide)))
  })
  
  observeEvent({
    input$wcpslide
    input$wlslide
    input$wrslide
  }, {
    updateSliderInput(session = session,
                      inputId = "ycbslide",
                      value = ceiling(100*input$ycbslide/(input$ycbslide +
                                                            input$ycpslide +
                                                            input$ylslide +
                                                            input$yadslide)))
  })

  
}

shinyApp(ui = ui, server = server)