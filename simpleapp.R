
library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(viridis)
library(shinyjs)
library(shinyBS)

# brings in main table of regions/waste types/masses
mass <- read_csv("mass.csv")

# brings in impact factors
I1 <- read_csv("I1.csv")

context <- readRDS("impacts_by_LC_stage_data.Rdata")

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  chooseSliderSkin("Modern"),
  useShinyjs(),
  
  navbarPage("Lifecycle Impacts Tool: DRAFT",
             
             # Introduction tab -------------------------------------------------------
             tabPanel("Introduction",
                      fluidPage(
                        # setBackgroundImage("mtthhsfade.jpg"),
                        # setBackgroundImage("paintedfade.jpg"),
                        setBackgroundImage("southsilverfade.jpg"),
                        # setBackgroundColor("darkgreen"),
                        column(12,
                               align = "center",
                               wellPanel(
                                 # black background
                                 style = "background-color: rgba(0,0,0,0.6)",
                                 # smoke background
                                 # style = "background-color: rgba(248,245,240,0.85)",
                                 h3(style = "color: rgba(248,245,240)",
                                    "Lifecycle Impacts Tool", align = "center"),
                               # h2("Interactive Visualizer!", align = "center"),
                               # div(img(src = 'greenpic.jpeg', height="50%", width="50%"), style = "text-align: center;"),
                               br(),
                               div(
                                 style = "width: 560px; height: 315px",
                                 HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/roNLC7UbZao?start=309" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                               ),
                               br(),
                               br(),
                               div(style = "width:500px; text-align:center;",
                                    p(style = "color: rgba(248,245,240)", "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                               )
                        )
                        )
                      )
             ),

# Visualize Tab -----------------------------------------------------------

             
tabPanel("Visualize!",
         fluidPage(
           column(4,
                  wellPanel(
                    # black background
                    style = "background-color: rgba(0,0,0,0.6);
                    color: rgba(248,245,240)",
                    # style = "background-color: rgba(248,245,240,0.9)",
                    # uiOutput("sliders"),
                    
                    # textOutput("vals"),
                    conditionalPanel(
                      condition = "input.usermaterial == `Cardboard`",
                      uiOutput("cardboardpanel")
                    ),
                    # conditionalPanel(
                    #   condition = "input.usermaterial == `Carpet`"
                    #   # uiOutput("sliders")
                    #   # uiOutput("carpetpanel"
                    #            
                    # ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Electronics`",
                      uiOutput("electronicspanel")
                    ),
                    conditionalPanel(
                      condition = "input.usermaterial == `Food`",
                      uiOutput("foodpanel")
                    ),
                    # conditionalPanel(
                    #   condition = "input.usermaterial == `Glass`",
                    #   uiOutput("glasspanel")
                    # ),
                    # conditionalPanel(
                    #   condition = "input.usermaterial == `Trash`",
                    #   uiOutput("trashpanel")
                    # ),
                    # conditionalPanel(
                    #   condition = "input.usermaterial == `Paper`",
                    #   uiOutput("paperpanel")
                    # ),
                    # conditionalPanel(
                    #   condition = "input.usermaterial == `Plastic Film`",
                    #   uiOutput("plasticfilmpanel")
                    # ),
                    # conditionalPanel(
                    #   condition = "input.usermaterial == `Rigid Plastic`",
                    #   uiOutput("rigidplasticpanel")
                    # ),
                    # conditionalPanel(
                    #   condition = "input.usermaterial == `Scrap Metal`",
                    #   uiOutput("scrapmetalpanel")
                    # ),
                    # conditionalPanel(
                    #   condition = "input.usermaterial == `Wood`",
                    #   uiOutput("woodpanel")
                    # ),
                    # conditionalPanel(
                    #   condition = "input.usermaterial == `Yard Debris`",
                    #   uiOutput("yarddebrispanel")
                    # ),
                    selectInput(inputId = "usermaterial",
                                label = "Select a material:",
                                choices = c("Cardboard", "Electronics", "Food"),
                                selected = "Cardboard"),
                    tableOutput("ttable")
                  )
           ),
           column(4,
                  wellPanel(
                    style = "background-color: rgba(0,0,0,0.6);
                    color: rgba(248,245,240)",
                    # style = "background-color: rgba(248,245,240,0.9)",
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
                    style = "background-color: rgba(0,0,0,0.6);
                    color: rgba(248,245,240)",
                    # style = "background-color: rgba(248,245,240,0.9)",
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

# Context tab -------------------------------------------------------------

tabPanel("Context",
         fluidPage(
           fixedPanel(width = "22%",
                      left = "30px",
                         wellPanel(
                           style = "background-color: rgba(0,0,0,0.6);
                    color: rgba(248,245,240)",
                           selectInput(inputId = "contextregion",
                                       label = "Select a region:",
                                       choices = unique(context$wasteshed),
                                       selected = "Global warming")
                         )),
           column(width = 9, offset = 3, 
             wellPanel(
               style = "background-color: rgba(0,0,0,0.6);
                    color: rgba(248,245,240)"))
         )
),

# More NavbarMenu ---------------------------------------------------------


             navbarMenu("More",
                        tabPanel("Glossary",
                                 fluidPage(
                                   column(3, wellPanel(
                                     style = "background-color: rgba(0,0,0,0.6);
                    color: rgba(248,245,240)"
                                   )),
                                   column(9,
                                          wellPanel(
                                            style = "background-color: rgba(0,0,0,0.6);
                    color: rgba(248,245,240)",
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
  
  t <- reactive({
    usermass() %>% 
      select(Disposition, `2015 Weight`) %>% 
      mutate(Percent = `2015 Weight`/sum(`2015 Weight`)*100)
  })
  
 output$ttable <- renderTable({
    t()
  })

  # initializes the reactive values for the sliders
  # st is for the starting value
  
  one <- reactiveValues(st = 0)
  two <- reactiveValues(st = 0)
  three <- reactiveValues(st = 0)
  four <- reactiveValues(st = 0)
  
  # These observe event create the percent values for the sliders
  # The pct/st is left over from when I was trying to create a reset button
  
  observeEvent(input$usermaterial, {
    one$st  <- (tweight()[1]/sum(tweight()))*100
    print(one$st)
  })
  
  observeEvent(input$usermaterial, {
    two$st <- (tweight()[2]/sum(tweight()))*100
    print(two$st)
  })
  
  observeEvent(input$usermaterial, {
    three$st <- (tweight()[3]/sum(tweight()))*100
    print(three$st)
  })
  
  observeEvent(input$usermaterial, {
    four$st <- (tweight()[4]/sum(tweight()))*100
    print(four$st)
  })
  
  #just a test of the output
  output$vals <- renderText({
    paste(tweight()[1])
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
    #dynamic number of sliders
    #https://stackoverflow.com/questions/35579439/dynamic-number-of-sliders-in-shiny
    
    tagList(
      setSliderColor(c("#237698", "#B1CA54", "#564D65", "#248F79"), c(1, 2, 3, 4)),
      sliderInput(inputId = "Production",
                  label = "Total Waste, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "cbcslide",
                  label = paste("%", tdisp()[1]),
                  min = 0,
                  max = 100,
                  post = " %",
                  value = one$st),
      sliderInput(inputId = "cblslide",
                  label = paste("%", tdisp()[2]),
                  min = 0,
                  max = 100,
                  post = " %",
                  value = two$st),
      sliderInput(inputId = "cbrslide",
                  label = paste("%", tdisp()[3]),
                  min = 0,
                  max = 100,
                  post = " %",
                  value = three$st)
    )
  })
  
  observeEvent({
    input$cbcslide
    input$cblslide
  }, {
    updateSliderInput(session = session,
                      inputId = "cbrslide",
                      value = 100*input$cbrslide/(input$cbcslide + input$cblslide + input$cbrslide)
                      )
  })
  
  observeEvent({
    input$cbcslide
    input$cbrslide
  }, {
    updateSliderInput(session = session,
                      inputId = "cblslide",
                      value = 100*input$cblslide/(input$cbcslide + input$cblslide + input$cbrslide)
                      )
  })
  observeEvent({
    input$cbrslide
    input$cblslide
  }, {
    updateSliderInput(session = session,
                      inputId = "cbcslide",
                      value = 100*input$cbcslide/(input$cbcslide + input$cblslide + input$cbrslide)
                      )
  })
  
  sliderweights <- reactive({
    c(input$Production*input$cbcslide/100,
      input$Production*input$cblslide/100,
      input$Production*input$cbrslide/100,
      input$Production)
  })
  
  cbmassdf <- reactive({
    
    Disposition       <- c(tdisp()[1], tdisp()[2], tdisp()[3], "Production")
    `Initial Weight`  <- c(tweight()[1], tweight()[2], tweight()[3], sum(tweight()))
    `Scenario Weight` <- sliderweights()
    # `Scenario Weight` <- c(input$Production*input$cbcslide,
    #                        input$Production*input$cblslide,
    #                        input$Production*input$cbrslide,
    #                        input$Production)
    
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
                        marker = list(color = ("#B1CA54")),
                        type = "bar") %>% 
      add_trace(y = ~Landfilling,
                name = "Landfilling weight",
                marker = list(color = ("#564D65"))) %>% 
      add_trace(y = ~Recycling,
                name = "Recycling weight",
                marker = list(color = ("#248F79"))) %>% 
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
                 marker = list(color = ("#237698")),
                 type = "bar") %>% 
      add_trace(y = ~Combustion,
                name = "Combustion impact",
                marker = list(color = ("#B1CA54"))) %>% 
      add_trace(y = ~Landfilling,
                name = "Landfilling impact",
                marker = list(color = ("#564D65"))) %>% 
      add_trace(y = ~Recycling,
                name = "Recycling impact",
                marker = list(color = ("#248F79"))) %>% 
      layout(barmode = "relative")
    
    p %>% 
      add_trace(y = ~Sum,
                type = "scatter",
                mode = "line",
                name = "Net impact",
                marker = list(size = ~log(Sum),
                              color = ("#cf9f35"))) %>% 
      layout(yaxis = list(overlaying = "y",
                          title = paste("Impact in", userimpact()$Units[[1]])),
             xaxis = list(title = ""),
             legend = list(orientation = 'h')
      )
  })
  
  
# Carpet Panel ------------------------------------------------------------

  # output$carpetpanel <-  renderUI({
  #   
  #   tagList(
  #     # 3A6276
  #     setSliderColor(c("#237698", "#B1CA54", "#564D65", "#248F79"), c(1, 2, 3, 4)),
  #     br(),
  #     sliderInput(inputId = "Production",
  #                 label = "Total Weight, in Tons",
  #                 min = 0,
  #                 max = sum(tweight())*1.5,
  #                 value = sum(tweight())),
  #     sliderInput(inputId = "ccslide",
  #                 label = paste("%", tdisp()[1]),
  #                 min = 0,
  #                 max = 100,
  #                 value = one$st),
  #     sliderInput(inputId = "clslide",
  #                 label = paste("%", tdisp()[2]),
  #                 min = 0,
  #                 max = 100,
  #                 value = two$st),
  #     sliderInput(inputId = "crslide",
  #                 label = paste("%", tdisp()[3]),
  #                 min = 0,
  #                 max = 100,
  #                 value = three$st)
  #   )
  #   
  # })
  # 
  # observeEvent({
  #   input$ccslide
  #   input$clslide
  # }, {
  #   updateSliderInput(session = session,
  #                     inputId = "crslide",
  #                     value = round(100*input$crslide/(input$ccslide + input$clslide + input$crslide)))
  # })
  # 
  # observeEvent({
  #   input$ccslide
  #   input$crslide
  # }, {
  #   updateSliderInput(session = session,
  #                     inputId = "clslide",
  #                     value = round(100*input$clslide/(input$ccslide + input$clslide + input$crslide)))
  # })
  # observeEvent({
  #   input$crslide
  #   input$clslide
  # }, {
  #   updateSliderInput(session = session,
  #                     inputId = "ccslide",
  #                     value = round(100*input$ccslide/(input$ccslide + input$clslide + input$crslide)))
  # })
  
# Electronics Panel -------------------------------------------------------

  output$electronicspanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#237698", "#B1CA54", "#564D65", "#248F79"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      div(
        mapply(FUN = function(x, y){
          sliderInput(
            inputId = paste0("Electric", x),
            label = paste("%", x),
            min = 0,
            max = 100,
            value = y)
        }, x = t()$Disposition,
        y = t()$Percent,
        SIMPLIFY = FALSE, USE.NAMES = FALSE
        )
      )
    )
  })
  
  observeEvent({
    input$ElectricRecycling
  }, {
    updateSliderInput(session = session,
                      inputId = "ElectricLandfilling",
                      value = 100*input$ElectricLandfilling/(input$ElectricRecycling + input$ElectricLandfilling))
  })
  
  observeEvent({
    input$ElectricLandfilling
  }, {
    updateSliderInput(session = session,
                      inputId = "ElectricRecycling",
                      value = 100*input$ElectricRecycling/(input$ElectricRecycling + input$ElectricLandfilling))
  })


# Food Panel --------------------------------------------------------------

  output$foodpanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#237698", "#B1CA54", "#564D65", "#248F79"), c(1, 2, 3, 4)),
      br(),
      sliderInput(inputId = "Production",
                  label = "Total Weight, in Tons",
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      div(
        mapply(FUN = function(x, y){
          sliderInput(
            inputId = paste0("Food", x),
            label = paste("%", x),
            min = 0,
            max = 100,
            value = y)
        }, x = t()$Disposition,
        y = t()$Percent,
        SIMPLIFY = FALSE, USE.NAMES = FALSE
        )
      )
    )
    
  })
  
  observeEvent({
    input$`FoodAnaerobic Digestion`
    input$FoodLandfilling
  }, {
    updateSliderInput(session = session,
                      inputId = "FoodCombustion",
                      value = 100*input$FoodCombustion/(input$FoodCombustion + input$`FoodAnaerobic Digestion` + input$FoodLandfilling))
  })
  
  observeEvent({
    input$`FoodAnaerobic Digestion`
    input$FoodCombustion
  }, {
    updateSliderInput(session = session,
                      inputId = "FoodLandfilling",
                      value = 100*input$FoodLandfilling/(input$FoodCombustion + input$`FoodAnaerobic Digestion` + input$FoodLandfilling))
  })
  observeEvent({
    input$FoodCombustion
    input$FoodLandfilling
  }, {
    updateSliderInput(session = session,
                      inputId = "\`FoodAnaerobic Digestion`",
                      value = 100*input$`FoodAnaerobic Digestion`/(input$FoodCombustion + input$`FoodAnaerobic Digestion` + input$FoodLandfilling))
  })

# Glass -------------------------------------------------------------------

  output$glasspanel <-  renderUI({
    
    tagList(
      
      setSliderColor(c("#237698", "#B1CA54", "#564D65", "#248F79"), c(1, 2, 3, 4)),
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
                  value = one$st),
      sliderInput(inputId = "grslide",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = two$st),
      sliderInput(inputId = "guaslide",
                  label = "% Use as Aggregate",
                  min = 0,
                  max = 100,
                  value = three$st)
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
      
      setSliderColor(c("#237698", "#B1CA54", "#564D65", "#248F79"), c(1, 2, 3, 4)),
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
                  value = one$st),
      sliderInput(inputId = "tlslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two$st)
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
      
      setSliderColor(c("#237698", "#B1CA54", "#564D65", "#248F79"), c(1, 2, 3, 4)),
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
                  value = one$st),
      sliderInput(inputId = "plslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two$st),
      sliderInput(inputId = "prslide",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = three$st)
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
      
      setSliderColor(c("#237698", "#B1CA54", "#564D65", "#248F79"), c(1, 2, 3, 4)),
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
                  value = one$st),
      sliderInput(inputId = "oflslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two$st),
      sliderInput(inputId = "pfrslide",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = three$st)
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
      
      setSliderColor(c("#237698", "#B1CA54", "#564D65", "#248F79"), c(1, 2, 3, 4)),
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
                  value = one$st),
      sliderInput(inputId = "rplslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = two$st),
      sliderInput(inputId = "rprslide",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = three$st)
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
      
      setSliderColor(c("#237698", "#B1CA54", "#564D65", "#248F79"), c(1, 2, 3, 4)),
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
                  value = one$st),
      sliderInput(inputId = "smrslide",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = two$st)
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
      
      setSliderColor(c("#237698", "#B1CA54", "#564D65", "#248F79"), c(1, 2, 3, 4)),
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
                  value = one$st),
      sliderInput(inputId = "wcpslide",
                  label = "% Composting",
                  min = 0,
                  max = 100,
                  value = two$st),
      sliderInput(inputId = "wlslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = three$st),
      sliderInput(inputId = "wrslide",
                  label = "% Recycling",
                  min = 0,
                  max = 100,
                  value = four$st)
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
      
      setSliderColor(c("#237698", "#B1CA54", "#564D65", "#248F79"), c(1, 2, 3, 4)),
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
                  value = one$st),
      sliderInput(inputId = "ycbslide",
                  label = "% Combustion",
                  min = 0,
                  max = 100,
                  value = two$st),
      sliderInput(inputId = "ycpslide",
                  label = "% Composting",
                  min = 0,
                  max = 100,
                  value = three$st),
      sliderInput(inputId = "ylslide",
                  label = "% Landfilling",
                  min = 0,
                  max = 100,
                  value = four$st)
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

  

# Test UI -----------------------------------------------------------------
# sliders <- function()
# {
#   pvars <- t()
#   sliderInput(inputId = paste0("range", pvars[i, 1]),
#               label = pvars[i],
#               min = 0,
#               max = 1,
#               value = 0.5
#   )
# }
# 
#   
# output$sliders <- renderUI({
#   pvars <- tdisp()
#   # pvars <- t()
#   lapply(seq(pvars), function(i) {
#     sliderInput(inputId = paste0("range", pvars[i]),
#                 label = pvars[i],
#                 min = 0,
#                 max = 100,
#                 value = 50
#                 )
#   }
#     )
# })

  # output$sliders <- renderUI({
  #   # pvars <- tdisp()
  #   
  #   mapply(t()$names, t()$values, function(i, j) {
  #     sliderInput(inputId = paste0("range", i),
  #                 label = i,
  #                 min = 0,
  #                 max = 100,
  #                 value = j
  #     )
  #   }
  #   )
  # })
  
  
  output$sliders <- renderUI({

    div(
    mapply(FUN = function(x, y){
             sliderInput(
               inputId = paste0("%", x),
               label = x,
               min = 0,
               max = 100,
               value = y)
             
           }, x = t()$Disposition,
           y = t()$Percent,
           SIMPLIFY = FALSE, USE.NAMES = FALSE
    ))
    
    
  })
    
  

# Context plots -----------------------------------------------------------

regionalcontext <- reactive({
  context %>% 
    filter(wasteshed %in% input$contextregion)
}) 
  
  renderPlotly({
    tt <- plot_ly(data = regionalcontext,
                  y = ~appMaterial,
                  x = ~Disposal,
                  name = "Disposal",
                  type = "bar") %>%
      add_trace(x = ~Recovery,
                name = "Recovery") %>% 
      layout(barmode = "relative")

    # Net impact
    Bakertrnet <- Bakertr %>% 
      filter(umbDisp == "Net") %>% 
      select(wasteshed, appMaterial, umbDisp, impact, tons)
    
    bb <- plot_ly(data = Bakertrnet,
                  y = ~appMaterial,
                  x = ~impact,
                  name = "Net Impact",
                  type = "bar") %>%
      layout(barmode = "relative")
    bb
    
    sub2 <- subplot(tt, bb)
    sub2
  })
  
  
  
}

shinyApp(ui = ui, server = server)