
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

#brings in data for context page
context <- readRDS("impacts_by_LC_stage_data.Rdata")


ui <- fluidPage(
  # points to a CSS sheet that customizes the look of the app
  # based off of the bootstrap shinytheme "Sandstone"
  includeCSS("www/sandstone2.css"),

  #changes look of sliders
  chooseSliderSkin("Modern"),
  # useShinyjs(),
  
  #creates a page with a navigation bar at the top
  navbarPage("Solid Waste Visualizer (SWaV): DRAFT",
             #collabsible optimizes for phones/small screens
             collapsible = TRUE,
             # if we want to have a DEQ footer
             # footer = img(src = 'DEQbwhz80.png',
             #              height = "50px", align = "center"),
             
# Introduction tab -------------------------------------------------------
tabPanel("Introduction",
         fluidPage(
           # Alternate backgrounds, can be switched out
           # setBackgroundImage("mtthhsfade.jpg"),
           # setBackgroundImage("paintedfade.jpg"),
           setBackgroundImage("southsilverfade.jpg"),
           # setBackgroundColor("darkgreen"),
           column(12,
                  align = "center",
                  wellPanel(
                    # background matches nav bar
                    # using an rgba background allows you to adjust
                    # opacity of the well panel without it transferring to 
                    # child divs
                    # style = "background-color: rgba(62,63,58,0.85)",
                    # light background
                    # style = "background-color: rgba(248,245,240,0.85)",
                    h3(style = "color: rgba(248,245,240)",
                       "Welcome to the Solid Waste Visualizer!", align = "center"),
                    # To place a picture:
                    # div(img(src = 'greenpic.jpeg', height="50%", width="50%"), style = "text-align: center;"),
                    br(),
                    div(
                      style = "width: 560px; height: 315px; background-color: rgba(0,0,0)",
                      h3("Video coming soon")),
                    # To place a video:
                    #   HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/roNLC7UbZao?start=309" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                    # ),
                    br(),
                    br(),
                    div(style = "width:600px; text-align:center;",
                        p(style = "color: rgba(248,245,240)",
                          "Watch the video to learn more,
                          or start exploring the data interactively
                          in the tabs above.")
                    )
                  )
           )
         )
),

# Visualize Tab -----------------------------------------------------------

             
tabPanel("Visualize!",
         fluidPage(
           fluidRow(
             column(4,
                    wellPanel(
                      # style = "background-color: rgba(62,63,58,0.855);",
                              div(style = "text-align:center; color: rgba(248,245,240)",
                                  p("The sliders to the left are set to Oregon waste weights from 2016."),
                                  p("Move the sliders to explore the data.")))),
             column(4,
                    wellPanel(style = "background-color: rgba(62,63,58,0.85);",
                              div(style = "text-align:center; color: rgba(248,245,240)",
                                  p("The weights of the different end of life treatments are shown below, with 2016 weights on the left and the new slider weights on the right.")))),
             column(4,
                    wellPanel(style = "background-color: rgba(62,63,58,0.85);",
                              div(style = "text-align:center; color: rgba(248,245,240)",
                                  p("The environmental impacts of the different end-of-life treatments are shown below."))))
           )
                    ,
           fluidRow(
             column(4,
                    wellPanel(
                      # black background
                      style = "background-color: rgba(62,63,58,0.85);
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

# dormant slider panels ----------------------------------------------

                      
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

# usermaterial input ------------------------------------------

                      pickerInput(inputId = "usermaterial",
                                  label = "Select a waste material:",
                                  choices = c("Cardboard", "Electronics", "Food"),
                                  selected = "Cardboard",
                                  options = list(style = "btn-secondary"))
                    )
             ),
             column(4,
                    wellPanel(
                      style = "background-color: rgba(62,63,58,0.85);
                    color: rgba(248,245,240)",
                      # style = "background-color: rgba(248,245,240,0.9)",
                      htmlOutput("materialtext"),
                      br(),
                      conditionalPanel(
                        condition = "input.usermaterial == `Cardboard`",
                        plotlyOutput("massplot")
                      ),
                      conditionalPanel(
                        condition = "input.usermaterial == `Electronics`",
                        plotlyOutput("elmassplot")
                      ),
                      conditionalPanel(
                        condition = "input.usermaterial == `Food`",
                        plotlyOutput("foodmassplot")
                      ),
                      br(),
                      pickerInput(inputId = "userregion",
                                  label = "Select a region:",
                                  choices = unique(mass$Wasteshed),
                                  selected = "Oregon total",
                                  options = list(style = "btn-secondary"))
                    )
             ),
             column(4,
                    wellPanel(
                      style = "background-color: rgba(62,63,58,0.85);
                    color: rgba(248,245,240)",
                      # style = "background-color: rgba(248,245,240,0.9)",
                      htmlOutput("impacttext"),
                      br(),
                      conditionalPanel(
                        condition = "input.usermaterial == `Cardboard`",
                        plotlyOutput("cbplot")
                      ),
                      conditionalPanel(
                        condition = "input.usermaterial == `Electronics`",
                        plotlyOutput("elplot")
                      ),
                      conditionalPanel(
                        condition = "input.usermaterial == `Food`",
                        plotlyOutput("foodplot")
                      ),
                      br(),
                      pickerInput(inputId = "userimpact",
                                  label = "Select an impact:",
                                  choices = unique(I1$Category),
                                  selected = "Energy demand",
                                  options = list(style = "btn-secondary"))
                    )
             )
           ),
fluidRow(
  column(12,
         wellPanel(style = "background-color: rgba(62,63,58,0.85);",
                   div(style = "text-align:center; color: rgba(248,245,240)",
                       p("Explore different materials, regions, and impacts in using the dropdown menus. Additional options will be available in the final version.")
                       # p("The data from the current view above is shown in the table below."),
                       #   p("This includes the initial 2016 weight, the new weight given by the slider (including some difference due to rounding), as well as the life cycle stage, the impact category, the units the impact is described in, the factor (i.e., the quantity we used to multiply the weight by to get the overall impact) and the overall impact for the 2016 impact and the new one created with the sliders."),
                       # p("This table can be downloaded in both Excel and CSV formats. Any plots are also downloadable by hovering over the upper right-hand corner.")
                       
         )))),

# tables ------------------------------------------------------------------


           fluidRow(
             column(12,
               wellPanel(
                 # style = "background-color: rgba(62,63,58,0.85);
                 #    color: rgba(248,245,240)",
                 style = "background-color: rgba(248,245,240,0.9)",
                 conditionalPanel(
                   condition = "input.usermaterial == `Cardboard`",
                   DT::dataTableOutput("cbdf")
                 ),
                 conditionalPanel(
                   condition = "input.usermaterial == `Electronics`",
                   DT::dataTableOutput("eldf")
                   
                 ),
                 conditionalPanel(
                   condition = "input.usermaterial == `Food`",
                   DT::dataTableOutput("fddf")
                   
                 )
                 
                 # tableOutput("ttable"),
                 # tableOutput("df"),
                 # DT::dataTableOutput("userimpact")
                 )))
           
)
),

# Context tab -------------------------------------------------------------

tabPanel("Context",
         fluidPage(
           fixedPanel(width = "22%",
                      left = "30px",
                         wellPanel(
                           style = "background-color: rgba(62,63,58,0.85);
                    color: rgba(248,245,240)",
                           pickerInput(inputId = "contextregion",
                                       label = "Select a region:",
                                       choices = unique(context$wasteshed),
                                       selected = "Global warming",
                                       options = list(style = "btn-secondary"))
                         )),
           column(width = 9, offset = 3, 
             wellPanel(
               style = "background-color: rgba(62,63,58,0.85);
                    color: rgba(248,245,240)",
               plotlyOutput("contextplot")
               ),
             br(),
             br(),
             br())
         )
),

# More NavbarMenu ---------------------------------------------------------


             navbarMenu("More",
                        tabPanel("Glossary",
                                 fluidPage(
                                   column(3, wellPanel(
                                     style = "background-color: rgba(62,63,58,0.85);
                    color: rgba(248,245,240)"
                                   )),
                                   column(9,
                                          wellPanel(
                                            style = "background-color: rgba(62,63,58,0.85);
                    color: rgba(248,245,240)",
                                            includeMarkdown("Glossary.md")
                                          )
                                   )
                                 )
                        ),

# Resources tab -----------------------------------------------------------

tabPanel("Resources",
         wellPanel(
           style = "background-color: rgba(62,63,58,0.85);
                    color: rgba(248,245,240)",
           tags$div(
             tags$ul(
               tags$li(a(href = "https://www.epa.gov/warm",
                         "EPA's Waste Reduction Model (WARM)"),
                       p("Another environmental impact calculator")),
               tags$li(a(href = "https://www.footprintcalculator.org/",
                         "Global Footprint Network's Footprint Calculator"),
                       p("An ecological footprint calculator for lifestyle choices.")
               )
             )))),

# About tab ---------------------------------------------------------------

tabPanel("About",
         fluidPage(
           fluidRow(
             column(4,
                    wellPanel(
                      style = "background-color: rgba(62,63,58,0.85);
                    color: rgba(248,245,240)",
                      tags$div(
                        p("This is placeholder text.")
                      )
                    )
             ),
             column(4,
                    wellPanel(
                      style = "background-color: rgba(62,63,58,0.85);
                    color: rgba(248,245,240)",
                      tags$div(
                        p("This is placeholder text.")
                      )
                    )
             ),
             column(4,
                    wellPanel(
                      style = "background-color: rgba(62,63,58,0.85);
                    color: rgba(248,245,240)",
                      tags$div(
                        p("This is placeholder text.")
                      )
                    )
             )
           ))
)
)
)
)




server <- function(input, output, session) {


# General -------------------------------------------------------------

  output$materialtext <- renderText({
    paste("<B>", input$usermaterial, "Weights</B>")
  })
  
  output$impacttext <- renderText({
    paste("<B>", tools::toTitleCase(input$userimpact), "Impact from", input$usermaterial, "</B>")
  })
  
  # this is the initial dataframe created when the user
  # filters based on their region and material
  
  usermass <- reactive({
    req(input$userregion)
    req(input$usermaterial)
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
      # https://stackoverflow.com/questions/36906265/how-to-color-sliderbar-sliderinput
      # why coloring dynamic sliders doesnt work the second time
      # setSliderColor(c("#0B3C49", "#B1CA54", "#564D65", "#587B7F"), c(1, 2, 3, 4)),
      sliderInput(inputId = "Production",
                  label =  paste("Total", input$usermaterial, "Waste for the", input$userregion, " region, in Tons"),
                  min = 0,
                  max = sum(tweight())*1.5,
                  post = " tons",
                  value = sum(tweight())),
      sliderInput(inputId = "cbcslide",
                  label = paste("%", tdisp()[1]),
                  min = 0,
                  max = 100,
                  post = " %",
                  value = t()$Percent[1]
                    # one$st
                  ),
      sliderInput(inputId = "cblslide",
                  label = paste("%", tdisp()[2]),
                  min = 0,
                  max = 100,
                  post = " %",
                  value = t()$Percent[2]
                    # two$st
                  ),
      sliderInput(inputId = "cbrslide",
                  label = paste("%", tdisp()[3]),
                  min = 0,
                  max = 100,
                  post = " %",
                  value = t()$Percent[3]
                    # three$st
                    )
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
  
  cbdf <- reactive({
    Disposition       <- c(tdisp()[1], tdisp()[2], tdisp()[3], "Production")
    `Initial Weight`  <- c(tweight()[1], tweight()[2], tweight()[3], sum(tweight()))
    `Scenario Weight` <- sliderweights()
    
    tibble(Disposition, `Initial Weight`, `Scenario Weight`) %>% 
      left_join(userimpact()) %>% 
      mutate(`Initial Impact` = `Initial Weight`*Factor,
             `Scenario Impact` = `Scenario Weight`*Factor)
  })
  
  cbmassdf <- reactive({
    cbdf() %>%
      select(Disposition, `Initial Weight`, `Scenario Weight`, `Initial Impact`, `Scenario Impact`)
    
  })
  
  cardboarddf <- reactive({
    if(is.null(cbmassdf())) {
      return(NULL)
    }
    cbmassdf() %>% 
      gather(key = "Variable", value = "Value", c(`Initial Weight`, `Scenario Weight`,
                                                  `Initial Impact`, `Scenario Impact`))
  })
  
  
  output$cbdf <- DT::renderDataTable({
    DT::datatable(
      data = cbdf(),
      style = "bootstrap",
      extensions = "Buttons",
      options = list(dom = 'Bfrtip',
                     pageLength = 10,
                     compact = TRUE,
                     nowrap = TRUE,
                     scrollX = TRUE,
                     buttons = c('excel', 'csv')
      ),
      rownames = FALSE,
      filter = "bottom"
      ) %>% 
      DT::formatRound(
        columns =  c(names(cbdf())),
        digits = 0)
  })
  
  output$df <- renderTable({
    cardboarddf()
  }, digits = 0)
  
  output$massplot <- renderPlotly({
    req(input$userregion)
    req(input$usermaterial)
    req(cardboarddf())
    #Add traces with a for loop
    #https://stackoverflow.com/questions/46583282/r-plotly-to-add-traces-conditionally-based-on-available-columns-in-dataframe
    massplot <- plot_ly(cardboarddf() %>%
                          filter(grepl("Weight", Variable)) %>%
                          spread("Disposition", "Value"),
                        x = ~Variable,
                        y = ~Combustion,
                        name = "Combustion",
                        marker = list(color = ("#898952")),
                        type = "bar") %>% 
      add_trace(y = ~Landfilling,
                name = "Landfilling",
                marker = list(color = ("#564D65"))) %>% 
      add_trace(y = ~Recycling,
                name = "Recycling",
                marker = list(color = ("#587B7F"))) %>% 
      layout(barmode = "relative")
    
    massplot %>% 
      layout(paper_bgcolor = "#f8f5f0",
             plot_bgcolor = "#f8f5f0",
             yaxis = list(overlaying = "y",
                          zerolinecolor = "#cf9f35",
                          title = "Tons",
                          titlefont = list(size = 18)),
             xaxis = list(title = "",
                          tickfont = list(size = 18)),
             legend = list(orientation = 'h'),
             font = list(
               family = "Roboto, sans-serif",
               size = 14,
               color = "#cf9f35"),
             margin = list(b = 100,
                           l = 70,
                           r = 30,
                           t = 30,
                           pad = 4)) %>% 
      # modebar reference
      # https://github.com/plotly/plotly.js/blob/master/src/components/modebar/buttons.js
      config(displaylogo = FALSE,
             collaborate = FALSE,
             modeBarButtonsToRemove = list(
               'sendDataToCloud',
               # 'toImage',
               'zoom2d',
               'pan2d',
               'select2d',
               'lasso2d',
               'zoomIn2d',
               'zoomOut2d',
               'autoScale2d',
               'resetScale2d',
               # 'hoverClosestCartesian',
               # 'hoverCompareCartesian'
               'toggleSpikelines'
             ))
      # layout(paper_bgcolor = "#f8f5f0",
      #        plot_bgcolor = "#f8f5f0",
      #        yaxis = list(overlaying = "y",
      #                     title = "Weight in tons"),
      #        xaxis = list(title = ""),
      #        legend = list(orientation = 'h')
      # )
  })
  
  output$cbplot <- renderPlotly({
    req(input$userimpact)
    req(input$usermaterial)
    req(cardboarddf())
    
    p <- plot_ly(cardboarddf() %>% 
                   filter(grepl("Impact", Variable)) %>% 
                   spread("Disposition", "Value") %>% 
                   mutate(Sum = rowSums(.[2:5])),
                 x = ~Variable,
                 y = ~Production,
                 name = "Production",
                 marker = list(color = ("#0B3C49")),
                 type = "bar") %>% 
      add_trace(y = ~Combustion,
                name = "Combustion",
                marker = list(color = ("#898952"))) %>% 
      add_trace(y = ~Landfilling,
                name = "Landfilling",
                marker = list(color = ("#564D65"))) %>% 
      add_trace(y = ~Recycling,
                name = "Recycling",
                marker = list(color = ("#587B7F"))) %>% 
      layout(barmode = "relative")
    
    p %>% 
      add_trace(y = ~Sum,
                type = "scatter",
                mode = "line",
                name = "Net impact",
                marker = list(size = "18",
                  # size = ~log(Sum),
                              color = ("#cf9f35"))) %>% 
      layout(
        paper_bgcolor = "#f8f5f0",
        plot_bgcolor = "#f8f5f0",
        xaxis = list(title = "",
                     tickfont = list(size = 18)),
        yaxis = list(
          overlaying = "y",
          zerolinecolor = "#cf9f35",
          title = paste("Impact in", userimpact()$Units[[1]]),
          titlefont = list(size = 20)
        ),
        legend = list(orientation = 'h'),
        font = list(family = "Roboto, sans-serif",
                    size = 14,
                    color = "#cf9f35"),
        margin = list(
          b = 100,
          l = 90,
          r = 30,
          t = 30,
          pad = 5
        )
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
      # layout(paper_bgcolor = "#f8f5f0",
      #        plot_bgcolor = "#f8f5f0",
      #        yaxis = list(overlaying = "y",
      #                     title = paste("Impact in", userimpact()$Units[[1]])),
      #        xaxis = list(title = ""),
      #        legend = list(orientation = 'h')
      # )
  })
  
  
# Carpet Panel ------------------------------------------------------------

  # output$carpetpanel <-  renderUI({
  #   
  #   tagList(
  #     # 3A6276
  #     setSliderColor(c("#0B3C49", "#B1CA54", "#564D65", "#587B7F"), c(1, 2, 3, 4)),
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
      sliderInput(inputId = "ElectricProduction",
                  label =  paste("Total", input$usermaterial, "Waste for the", input$userregion, " region, in Tons"),
                  min = 0,
                  max = sum(tweight())*1.5,
                  value = sum(tweight())),
      sliderInput(inputId = "ElectricLandfilling",
                  label = paste("%", tdisp()[1]),
                  min = 0,
                  max = 100,
                  post = " %",
                  value = t()$Percent[1]
      ),
      sliderInput(inputId = "ElectricRecycling",
                  label = paste("%", tdisp()[2]),
                  min = 0,
                  max = 100,
                  post = " %",
                  value = t()$Percent[2]
      )
    )
  })
  
  observeEvent({
    input$ElectricRecycling
  }, {
    updateSliderInput(session = session,
                      inputId = "ElectricLandfilling",
                      value = 100*input$ElectricLandfilling/
                        (input$ElectricRecycling + input$ElectricLandfilling))
  })
  
  observeEvent({
    input$ElectricLandfilling
  }, {
    updateSliderInput(session = session,
                      inputId = "ElectricRecycling",
                      value = 100*input$ElectricRecycling/
                        (input$ElectricRecycling + input$ElectricLandfilling))
  })
  
  elsliderweights <- reactive({
    c(input$ElectricProduction*input$ElectricLandfilling/100,
      input$ElectricProduction*input$ElectricRecycling/100,
      input$ElectricProduction)
  })
  
  eldf <- reactive({
    Disposition       <- c(tdisp()[1], tdisp()[2], "Production")
    `Initial Weight`  <- c(tweight()[1], tweight()[2], sum(tweight()))
    `Scenario Weight` <- elsliderweights()
    
    tibble(Disposition, `Initial Weight`, `Scenario Weight`) %>% 
      left_join(userimpact()) %>% 
      mutate(`Initial Impact` = `Initial Weight`*Factor,
             `Scenario Impact` = `Scenario Weight`*Factor)
  })
  
  output$eldf <- DT::renderDataTable({
    DT::datatable(
      data = eldf(),
      style = "bootstrap",
      extensions = "Buttons",
      options = list(dom = 'Bfrtip',
                     pageLength = 10,
                     compact = TRUE,
                     nowrap = TRUE,
                     scrollX = TRUE,
                     buttons = c('excel', 'csv')
      ),
      rownames = FALSE,
      filter = "bottom"
    ) %>% 
      DT::formatRound(
        columns =  c(names(eldf())),
        digits = 0)
  })
  
  elmassdf <- reactive({
    eldf() %>% 
      select(Disposition, `Initial Weight`, `Scenario Weight`,
             `Initial Impact`, `Scenario Impact`)
  })
  
  electronicdf <- reactive({
    elmassdf() %>% 
      gather(key = "Variable", value = "Value",
             c(`Initial Weight`, `Scenario Weight`,
               `Initial Impact`, `Scenario Impact`))
  })
  
  output$elmassplot <- renderPlotly({
    req(electronicdf())
    massplot <- plot_ly(electronicdf() %>%
                          filter(grepl("Weight", Variable)) %>%
                          spread("Disposition", "Value"),
                        x = ~Variable,
                        y = ~Landfilling,
                        name = "Landfilling weight",
                        marker = list(color = ("#564D65")),
                        type = "bar") %>% 
      add_trace(y = ~Recycling,
                name = "Recycling weight",
                marker = list(color = ("#587B7F"))) %>% 
      layout(barmode = "relative")
    
    massplot %>% 
      layout(paper_bgcolor = "#f8f5f0",
             plot_bgcolor = "#f8f5f0",
             yaxis = list(overlaying = "y",
                          zerolinecolor = "#cf9f35",
                          title = "Tons",
                          titlefont = list(size = 18)),
             xaxis = list(title = "",
                          tickfont = list(size = 18)),
             legend = list(orientation = 'h'),
             font = list(
               family = "Roboto, sans-serif",
               size = 14,
               color = "#cf9f35"),
             margin = list(b = 100,
                           l = 70,
                           r = 30,
                           t = 30,
                           pad = 4))%>% 
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
  
  output$elplot <- renderPlotly({
    
    p <- plot_ly(electronicdf() %>% 
                   filter(grepl("Impact", Variable)) %>% 
                   spread("Disposition", "Value") %>% 
                   mutate(Sum = rowSums(.[2:3])),
                 x = ~Variable,
                 y = ~Production,
                 name = "Production impact",
                 marker = list(color = ("#0B3C49")),
                 type = "bar") %>% 
      add_trace(y = ~Landfilling,
                name = "Landfilling impact",
                marker = list(color = ("#564D65"))) %>% 
      add_trace(y = ~Recycling,
                name = "Recycling impact",
                marker = list(color = ("#587B7F"))) %>% 
      layout(barmode = "relative")
    
    p %>% 
      add_trace(y = ~Sum,
                type = "scatter",
                mode = "line",
                name = "Net impact",
                marker = list(size = ~log(Sum),
                              color = ("#cf9f35"))) %>% 
      layout(
        paper_bgcolor = "#f8f5f0",
        plot_bgcolor = "#f8f5f0",
        xaxis = list(title = "",
                     tickfont = list(size = 18)),
        yaxis = list(
          overlaying = "y",
          zerolinecolor = "#cf9f35",
          title = paste("Impact in", userimpact()$Units[[1]]),
          titlefont = list(size = 20)
        ),
        legend = list(orientation = 'h'),
        font = list(family = "Roboto, sans-serif",
                    size = 14,
                    color = "#cf9f35"),
        margin = list(
          b = 100,
          l = 90,
          r = 30,
          t = 30,
          pad = 5
        )
      )%>% 
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

# Food Panel --------------------------------------------------------------

  output$foodpanel <-  renderUI({
    
      tagList(

        sliderInput(inputId = "FoodProduction",
                    label = paste("Total", input$usermaterial, "Waste for the", input$userregion, " region, in Tons"),
                    min = 0,
                    max = sum(tweight())*1.5,
                    value = sum(tweight())),
        sliderInput(inputId = "FoodAnaerobic Digestion",
                    label = paste("%", tdisp()[1]),
                    min = 0,
                    max = 100,
                    post = " %",
                    value = t()$Percent[1]
        ),
        sliderInput(inputId = "FoodComposting",
                    label = paste("%", tdisp()[2]),
                    min = 0,
                    max = 100,
                    post = " %",
                    value = t()$Percent[2]
        ),
        sliderInput(inputId = "FoodLandfilling",
                    label = paste("%", tdisp()[3]),
                    min = 0,
                    max = 100,
                    post = " %",
                    value = t()$Percent[3]
        )
      )
  })
  
  observeEvent({
    input$`FoodAnaerobic Digestion`
    input$FoodLandfilling
  }, {
    updateSliderInput(session = session,
                      inputId = "FoodComposting",
                      value = 100*input$FoodComposting /
                        (input$FoodComposting + 
                           input$`FoodAnaerobic Digestion` + 
                           input$FoodLandfilling))
  })
  
  observeEvent({
    input$`FoodAnaerobic Digestion`
    input$FoodComposting
  }, {
    updateSliderInput(session = session,
                      inputId = "FoodLandfilling",
                      value = 100*input$FoodLandfilling /
                        (input$FoodComposting + 
                           input$`FoodAnaerobic Digestion` + 
                           input$FoodLandfilling))
  })
  
  observeEvent({
    input$FoodComposting
    input$FoodLandfilling
  }, {
    updateSliderInput(session = session,
                      inputId = "\`FoodAnaerobic Digestion`",
                      value = 100*input$`FoodAnaerobic Digestion`/ 
                        (input$FoodComposting + 
                           input$`FoodAnaerobic Digestion` + 
                           input$FoodLandfilling))
  })
  
  foodsliderweights <- reactive({
    c(input$FoodProduction*input$`FoodAnaerobic Digestion`/100,
      input$FoodProduction*input$FoodComposting/100,
      input$FoodProduction*input$FoodLandfilling/100,
      input$FoodProduction)
  })
  
  fddf <- reactive({
    Disposition       <- c(tdisp()[1], tdisp()[2], tdisp()[3], "Production")
    `Initial Weight`  <- c(tweight()[1], tweight()[2], tweight()[3], sum(tweight()))
    `Scenario Weight` <- foodsliderweights()
    
    tibble(Disposition, `Initial Weight`, `Scenario Weight`) %>% 
      left_join(userimpact()) %>% 
      mutate(`Initial Impact` = `Initial Weight`*Factor,
             `Scenario Impact` = `Scenario Weight`*Factor)
  })
  
  
  foodmassdf <- reactive({
    fddf() %>% 
      select(Disposition, `Initial Weight`, `Scenario Weight`,
             `Initial Impact`, `Scenario Impact`)
  })
  
  fooddf <- reactive({
    foodmassdf() %>% 
      gather(key = "Variable", value = "Value",
             c(`Initial Weight`, `Scenario Weight`,
               `Initial Impact`, `Scenario Impact`))
  })
  
  output$fddf <- DT::renderDataTable({
    DT::datatable(
      data = fddf(),
      style = "bootstrap",
      extensions = "Buttons",
      options = list(dom = 'Bfrtip',
                     pageLength = 10,
                     compact = TRUE,
                     nowrap = TRUE,
                     scrollX = TRUE,
                     buttons = c('excel', 'csv')
      ),
      rownames = FALSE,
      filter = "bottom"
    ) %>% 
      DT::formatRound(
        columns =  c(names(fddf())),
        digits = 0)
  })
  
  output$foodmassplot <- renderPlotly({
    req(input$userregion)
    req(input$usermaterial)
    #Add traces with a for loop
    #https://stackoverflow.com/questions/46583282/r-plotly-to-add-traces-conditionally-based-on-available-columns-in-dataframe
    massplot <- plot_ly(fooddf() %>%
                          filter(grepl("Weight", Variable)) %>%
                          spread("Disposition", "Value"),
                        x = ~Variable,
                        y = ~Composting,
                        name = "Composting weight",
                        marker = list(color = ("#A57548")),
                        type = "bar") %>% 
      add_trace(y = ~Landfilling,
                name = "Landfilling weight",
                marker = list(color = ("#564D65"))) %>% 
      add_trace(y = ~`Anaerobic Digestion`,
                name = "Anaerobic Digestion weight",
                marker = list(color = ("#726E60"))) %>% 
      layout(barmode = "relative")
    
    massplot %>% 
      layout(paper_bgcolor = "#f8f5f0",
             plot_bgcolor = "#f8f5f0",
             yaxis = list(overlaying = "y",
                          zerolinecolor = "#cf9f35",
                          title = "Tons",
                          titlefont = list(size = 18)),
             xaxis = list(title = "",
                          tickfont = list(size = 18)),
             legend = list(orientation = 'h'),
             font = list(
               family = "Roboto, sans-serif",
               size = 14,
               color = "#cf9f35"),
             margin = list(b = 100,
                           l = 70,
                           r = 30,
                           t = 30,
                           pad = 4))%>% 
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
  
  output$foodplot <- renderPlotly({
    req(input$userimpact)
    req(input$usermaterial)
    
    p <- plot_ly(fooddf() %>% 
                   filter(grepl("Impact", Variable)) %>% 
                   spread("Disposition", "Value") %>% 
                   mutate(Sum = rowSums(.[2:5])),
                 x = ~Variable,
                 y = ~Production,
                 name = "Production impact",
                 marker = list(color = ("#0B3C49")),
                 type = "bar") %>% 
      add_trace(y = ~Composting,
                name = "Composting impact",
                marker = list(color = ("#A57548"))) %>% 
      add_trace(y = ~Landfilling,
                name = "Landfilling impact",
                marker = list(color = ("#564D65"))) %>% 
      add_trace(y = ~`Anaerobic Digestion`,
                name = "Anaerobic Digestion impact",
                marker = list(color = ("#726E60"))) %>% 
      layout(barmode = "relative")
    
    p %>% 
      add_trace(y = ~Sum,
                type = "scatter",
                mode = "line",
                name = "Net impact",
                marker = list(size = ~log(Sum),
                              color = ("#cf9f35"))) %>% 
      layout(
        paper_bgcolor = "#f8f5f0",
        plot_bgcolor = "#f8f5f0",
        xaxis = list(title = "",
                     tickfont = list(size = 18)),
        yaxis = list(
          overlaying = "y",
          zerolinecolor = "#cf9f35",
          title = paste("Impact in", userimpact()$Units[[1]]),
          titlefont = list(size = 20)
        ),
        legend = list(orientation = 'h'),
        font = list(family = "Roboto, sans-serif",
                    size = 14,
                    color = "#cf9f35"),
        margin = list(
          b = 100,
          l = 90,
          r = 30,
          t = 30,
          pad = 5
        )
      )%>% 
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

# Glass -------------------------------------------------------------------

#   output$glasspanel <-  renderUI({
#     
#     tagList(
#       
#       setSliderColor(c("#0B3C49", "#B1CA54", "#564D65", "#587B7F"), c(1, 2, 3, 4)),
#       br(),
#       sliderInput(inputId = "Production",
#                   label = "Total Weight, in Tons",
#                   min = 0,
#                   max = sum(tweight())*1.5,
#                   value = sum(tweight())),
#       sliderInput(inputId = "glslide",
#                   label = "% Landfilling",
#                   min = 0,
#                   max = 100,
#                   value = one$st),
#       sliderInput(inputId = "grslide",
#                   label = "% Recycling",
#                   min = 0,
#                   max = 100,
#                   value = two$st),
#       sliderInput(inputId = "guaslide",
#                   label = "% Use as Aggregate",
#                   min = 0,
#                   max = 100,
#                   value = three$st)
#     )
#     
#   })
#   
#   observeEvent({
#     input$grslide
#     input$glslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "guaslide",
#                       value = ceiling(100*input$guaslide/(input$guaslide + input$glslide + input$grslide)))
#   })
#   
#   observeEvent({
#     input$guaslide
#     input$grslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "glslide",
#                       value = ceiling(100*input$glslide/(input$guaslide + input$glslide + input$grslide)))
#   })
#   observeEvent({
#     input$guaslide
#     input$glslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "grslide",
#                       value = ceiling(100*input$grslide/(input$guaslide + input$glslide + input$grslide)))
#   })
#   
# # Trash -------------------------------------------------------------------
# 
#   output$trashpanel <-  renderUI({
#     
#     tagList(
#       
#       setSliderColor(c("#0B3C49", "#B1CA54", "#564D65", "#587B7F"), c(1, 2, 3, 4)),
#       br(),
#       sliderInput(inputId = "Production",
#                   label = "Total Weight, in Tons",
#                   min = 0,
#                   max = sum(tweight())*1.5,
#                   value = sum(tweight())),
#       sliderInput(inputId = "tcslide",
#                   label = "% Combustion",
#                   min = 0,
#                   max = 100,
#                   value = one$st),
#       sliderInput(inputId = "tlslide",
#                   label = "% Landfilling",
#                   min = 0,
#                   max = 100,
#                   value = two$st)
#     )
#   })
#   
#   observeEvent({
#     input$tcslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "tlslide",
#                       value = ceiling(100*input$tlslide/(input$tcslide + input$tlslide)))
#   })
#   
#   observeEvent({
#     input$tlslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "tcslide",
#                       value = ceiling(100*input$tcslide/(input$tcslide + input$tlslide)))
#   })
# 
# # Paper -------------------------------------------------------------------
# 
#   output$paperpanel <-  renderUI({
#     
#     tagList(
#       
#       setSliderColor(c("#0B3C49", "#B1CA54", "#564D65", "#587B7F"), c(1, 2, 3, 4)),
#       br(),
#       sliderInput(inputId = "Production",
#                   label = "Total Weight, in Tons",
#                   min = 0,
#                   max = sum(tweight())*1.5,
#                   value = sum(tweight())),
#       sliderInput(inputId = "pcslide",
#                   label = "% Combustion",
#                   min = 0,
#                   max = 100,
#                   value = one$st),
#       sliderInput(inputId = "plslide",
#                   label = "% Landfilling",
#                   min = 0,
#                   max = 100,
#                   value = two$st),
#       sliderInput(inputId = "prslide",
#                   label = "% Recycling",
#                   min = 0,
#                   max = 100,
#                   value = three$st)
#     )
#     
#   })
#   
#   observeEvent({
#     input$prslide
#     input$plslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "pcslide",
#                       value = ceiling(100*input$pcslide/(input$pcslide + input$plslide + input$prslide)))
#   })
#   
#   observeEvent({
#     input$pcslide
#     input$prslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "plslide",
#                       value = ceiling(100*input$plslide/(input$pcslide + input$plslide + input$prslide)))
#   })
#   observeEvent({
#     input$pcslide
#     input$plslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "prslide",
#                       value = ceiling(100*input$prslide/(input$pcslide + input$plslide + input$prslide)))
#   })
#   
# # Plastic Film ------------------------------------------------------------
# 
#   output$plasticfilmpanel <-  renderUI({
#     
#     tagList(
#       
#       setSliderColor(c("#0B3C49", "#B1CA54", "#564D65", "#587B7F"), c(1, 2, 3, 4)),
#       br(),
#       sliderInput(inputId = "Production",
#                   label = "Total Weight, in Tons",
#                   min = 0,
#                   max = sum(tweight())*1.5,
#                   value = sum(tweight())),
#       sliderInput(inputId = "pfcslide",
#                   label = "% Combustion",
#                   min = 0,
#                   max = 100,
#                   value = one$st),
#       sliderInput(inputId = "oflslide",
#                   label = "% Landfilling",
#                   min = 0,
#                   max = 100,
#                   value = two$st),
#       sliderInput(inputId = "pfrslide",
#                   label = "% Recycling",
#                   min = 0,
#                   max = 100,
#                   value = three$st)
#     )
#   })
#   
#   observeEvent({
#     input$pfrslide
#     input$pflslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "pfcslide",
#                       value = floor(100*input$pfcslide/(input$pfcslide + input$pflslide + input$pfrslide)))
#   })
#   
#   observeEvent({
#     input$pfcslide
#     input$pfrslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "pflslide",
#                       value = floor(100*input$pflslide/(input$pfcslide + input$pflslide + input$pfrslide)))
#   })
#   observeEvent({
#     input$pfcslide
#     input$pflslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "pfrslide",
#                       value = floor(100*input$pfrslide/(input$pfcslide + input$pflslide + input$pfrslide)))
#   })
# 
# # Rigid Plastic -----------------------------------------------------------
# 
#   output$rigidplasticpanel <-  renderUI({
# 
#     tagList(
#       
#       setSliderColor(c("#0B3C49", "#B1CA54", "#564D65", "#587B7F"), c(1, 2, 3, 4)),
#       br(),
#       sliderInput(inputId = "Production",
#                   label = "Total Weight, in Tons",
#                   min = 0,
#                   max = sum(tweight())*1.5,
#                   value = sum(tweight())),
#       sliderInput(inputId = "rpcslide",
#                   label = "% Combustion",
#                   min = 0,
#                   max = 100,
#                   value = one$st),
#       sliderInput(inputId = "rplslide",
#                   label = "% Landfilling",
#                   min = 0,
#                   max = 100,
#                   value = two$st),
#       sliderInput(inputId = "rprslide",
#                   label = "% Recycling",
#                   min = 0,
#                   max = 100,
#                   value = three$st)
#     )
#     
#   })
#   
#   observeEvent({
#     input$rprslide
#     input$rplslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "rpcslide",
#                       value = floor(100*input$rpcslide/(input$rpcslide + input$rplslide + input$rprslide)))
#   })
#   
#   observeEvent({
#     input$rpcslide
#     input$rprslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "rplslide",
#                       value = floor(100*input$rplslide/(input$rpcslide + input$rplslide + input$rprslide)))
#   })
#   observeEvent({
#     input$rpcslide
#     input$rplslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "rprslide",
#                       value = floor(100*input$rprslide/(input$rpcslide + input$rplslide + input$rprslide)))
#   })
#   
# # Scrap Metal -------------------------------------------------------------
# 
#   output$scrapmetalpanel <-  renderUI({
#     
#     tagList(
#       
#       setSliderColor(c("#0B3C49", "#B1CA54", "#564D65", "#587B7F"), c(1, 2, 3, 4)),
#       br(),
#       sliderInput(inputId = "Production",
#                   label = "Total Weight, in Tons",
#                   min = 0,
#                   max = sum(tweight())*1.5,
#                   value = sum(tweight())),
#       sliderInput(inputId = "smlslide",
#                   label = "% Landfilling",
#                   min = 0,
#                   max = 100,
#                   value = one$st),
#       sliderInput(inputId = "smrslide",
#                   label = "% Recycling",
#                   min = 0,
#                   max = 100,
#                   value = two$st)
#     )
#   })
#   
#   observeEvent({
#     input$smlslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "smrslide",
#                       value = ceiling(100*input$smrslide/(input$smrslide + input$smlslide)))
#   })
#   
#   observeEvent({
#     input$smrslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "smlslide",
#                       value = ceiling(100*input$smlslide/(input$smrslide + input$smlslide)))
#   })
# 
# # Wood --------------------------------------------------------------------
# 
#   output$woodpanel <-  renderUI({
#     
#     tagList(
#       
#       setSliderColor(c("#0B3C49", "#B1CA54", "#564D65", "#587B7F"), c(1, 2, 3, 4)),
#       br(),
#       sliderInput(inputId = "Production",
#                   label = "Total Weight, in Tons",
#                   min = 0,
#                   max = sum(tweight())*1.5,
#                   value = sum(tweight())),
#       sliderInput(inputId = "wcbslide",
#                   label = "% Combustion",
#                   min = 0,
#                   max = 100,
#                   value = one$st),
#       sliderInput(inputId = "wcpslide",
#                   label = "% Composting",
#                   min = 0,
#                   max = 100,
#                   value = two$st),
#       sliderInput(inputId = "wlslide",
#                   label = "% Landfilling",
#                   min = 0,
#                   max = 100,
#                   value = three$st),
#       sliderInput(inputId = "wrslide",
#                   label = "% Recycling",
#                   min = 0,
#                   max = 100,
#                   value = four$st)
#     )
#     
#   })
#   
#   observeEvent({
#     input$wcbslide
#     input$wcpslide
#     input$wlslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "wrslide",
#                       value = ceiling(100*input$wrslide/(input$wcbslide +
#                                                          input$wcpslide +
#                                                          input$wlslide +
#                                                            input$wrslide)))
#   })
#   
#   observeEvent({
#     input$wcbslide
#     input$wcpslide
#     input$wrslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "wlslide",
#                       value = ceiling(100*input$wlslide/(input$wcbslide +
#                                                            input$wcpslide +
#                                                            input$wlslide +
#                                                            input$wrslide)))
#   })
#   
#   observeEvent({
#     input$wcbslide
#     input$wlslide
#     input$wrslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "wcpslide",
#                       value = ceiling(100*input$wcpslide/(input$wcbslide +
#                                                             input$wcpslide +
#                                                             input$wlslide +
#                                                             input$wrslide)))
#   })
#   
#   observeEvent({
#     input$wcpslide
#     input$wlslide
#     input$wrslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "wcbslide",
#                       value = ceiling(100*input$wcbslide/(input$wcbslide +
#                                                             input$wcpslide +
#                                                             input$wlslide +
#                                                             input$wrslide)))
#   })
#   
# 
#   
# # Yard Debris -------------------------------------------------------------
# 
#   output$yarddebrispanel <-  renderUI({
#     
#     tagList(
#       
#       setSliderColor(c("#0B3C49", "#B1CA54", "#564D65", "#587B7F"), c(1, 2, 3, 4)),
#       br(),
#       sliderInput(inputId = "Production",
#                   label = "Total Weight, in Tons",
#                   min = 0,
#                   max = sum(tweight())*1.5,
#                   value = sum(tweight())),
#       sliderInput(inputId = "yadslide",
#                   label = "% Anaerobic Digestion",
#                   min = 0,
#                   max = 100,
#                   value = one$st),
#       sliderInput(inputId = "ycbslide",
#                   label = "% Combustion",
#                   min = 0,
#                   max = 100,
#                   value = two$st),
#       sliderInput(inputId = "ycpslide",
#                   label = "% Composting",
#                   min = 0,
#                   max = 100,
#                   value = three$st),
#       sliderInput(inputId = "ylslide",
#                   label = "% Landfilling",
#                   min = 0,
#                   max = 100,
#                   value = four$st)
#     )
#     
#   })
#   
#   observeEvent({
#     input$ycbslide
#     input$ycpslide
#     input$ylslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "yadslide",
#                       value = ceiling(100*input$yadslide/(input$ycbslide +
#                                                            input$ycpslide +
#                                                            input$ylslide +
#                                                            input$yadslide)))
#   })
#   
#   observeEvent({
#     input$ycbslide
#     input$ycpslide
#     input$yadslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "ylslide",
#                       value = ceiling(100*input$ylslide/(input$ycbslide +
#                                                            input$ycpslide +
#                                                            input$ylslide +
#                                                            input$yadslide)))
#   })
#   
#   observeEvent({
#     input$ycbslide
#     input$ylslide
#     input$yadslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "wcpslide",
#                       value = ceiling(100*input$wcpslide/(input$ycbslide +
#                                                             input$ycpslide +
#                                                             input$ylslide +
#                                                             input$yadslide)))
#   })
#   
#   observeEvent({
#     input$wcpslide
#     input$wlslide
#     input$wrslide
#   }, {
#     updateSliderInput(session = session,
#                       inputId = "ycbslide",
#                       value = ceiling(100*input$ycbslide/(input$ycbslide +
#                                                             input$ycpslide +
#                                                             input$ylslide +
#                                                             input$yadslide)))
#   })

  

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
  
output$contextplot <- renderPlotly({
  
  sfont <- list(
    family = "Roboto, sans-serif",
    color = "#cf9f35",
    size = 16
  )
  largefont <- list(
    family = "Roboto, sans-serif",
    size = 18,
    color = "#cf9f35"
  )
  xaxlist <- list(
    title = "Waste, in Tons",
    titlefont = largefont,
    showticklabels = TRUE,
    zerolinecolor = "#cf9f35",
    # tickangle = 45,
    tickfont = sfont
  )
  xax2list <- list(
    title = "Global Warming Potential, in kg CO<sub>2</sub> eq.",
    titlefont = largefont,
    showticklabels = TRUE,
    zerolinecolor = "#cf9f35",
    # tickangle = 45,
    tickfont = sfont
  )
  yaxlist <- list(
    showticklabels = TRUE,
    tickfont = sfont
  )
  
    tt <- plot_ly(data = regionalcontext()%>% 
                    filter(!umbDisp %in% c("Net", "Production")) %>%
                    select(wasteshed, appMaterial, umbDisp, impact, tons) %>% 
                    spread(key = umbDisp, value = tons),
                  y = ~appMaterial,
                  x = ~Disposal,
                  name = "Disposal",
                  color = I("#564D65"),  
                  type = "bar") %>%
      add_trace(x = ~Recovery,
                color = I("#587B7F"),
                name = "Recovery") %>% 
      layout(barmode = "relative",
             xaxis = xaxlist)

    # Net impact
    net <- regionalcontext() %>% 
      filter(umbDisp == "Net") %>% 
      select(wasteshed, appMaterial, umbDisp, impact, tons)
    
    bb <- plot_ly(data = net,
                  y = ~appMaterial,
                  x = ~impact,
                  name = "Net Impact",
                  color = I("#cf9f35"),
                  type = "bar") %>%
      layout(barmode = "relative",
             xaxis = xax2list)
    
    sub2 <- subplot(tt, bb, shareY = TRUE, titleX = TRUE, titleY = FALSE) %>% 
      layout(
        # paper_bgcolor = "#E2DADB",
        # plot_bgcolor = "#E2DADB",
        paper_bgcolor = "#f8f5f0",
        plot_bgcolor = "#f8f5f0",
        yaxis = yaxlist,
        font = sfont,
        margin = list(b = 80,
                      l = 120,
                      r = 30,
                      t = 30,
                      pad = 8)
             # legend = list(orientation = 'h')
             )
    sub2
  })
  
  
  
}

shinyApp(ui = ui, server = server)