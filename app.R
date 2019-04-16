
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
library(markdown)

# brings in main table of regions/waste types/masses
mass <- read_csv("data/mass.csv")

# brings in impact factors
I1 <- read_csv("data/I1.csv")

#brings in data for context page
context <- readRDS("data/impacts_by_LC_stage_data.RData")


ui <- fluidPage(
  # points to a CSS sheet that customizes the look of the app
  # based off of the bootstrap shinytheme "Sandstone"
  includeCSS("www/sandstone2.css"),

  #changes look of sliders
  chooseSliderSkin("Modern"),
  # useShinyjs(),
  
  #creates a page with a navigation bar at the top
  navbarPage("Waste Impact Calculator: DRAFT",
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
                       "Welcome to the Waste Impact Calculator!", align = "center"),
                    # To place a picture:
                    # div(img(src = 'greenpic.jpeg', height="50%", width="50%"), style = "text-align: center;"),
                    br(),
                    div(
                      style = "width: 560px; height: 315px; background-color: rgba(0,0,0)",
                    # To place a video:
                      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/Xd8y73b6Wn8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                    ),
                    
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

# Context tab -----------------------------------------------------------------

tabPanel("Context",
         fluidPage(
           fixedPanel(width = "22%",
                      left = "30px",
                      wellPanel(
                        pickerInput(inputId = "contextregion",
                                    label = "Select a region:",
                                    choices = unique(context$wasteshed),
                                    selected = "Metro",
                                    options = list(style = "btn-secondary")),
                        pickerInput(inputId = "contextimpact",
                                    label = "Select an impact:",
                                    choices = unique(context$impactCategory),
                                    selected = "Global warming",
                                    options = list(style = "btn-secondary"))
                      ),
                      wellPanel(
                        div(style = "text-align:left; height:200px;",
                            p("Select a region, above."),
                            p("The chart shows how tons of waste relate to life
                              cycle impacts of waste in the region."),
                            p("Comparing waste to impact may help you decide
                              which materials to prioritize for action.")
                        ))
           ),
           column(width = 9, offset = 3, 
                  wellPanel(
                    plotlyOutput("contextplot")),
                  wellPanel(
                    plotlyOutput("transparentbars"))
                  # wellPanel(
                  #   plotlyOutput("transparentbarspercent")
                  # )
                  
                  
           )
         )
),

# Visualize Tab -----------------------------------------------------------

tabPanel("Visualize!",
         fluidPage(
           fluidRow(
             column(4,
                    wellPanel(
                      pickerInput(inputId = "usermaterial",
                                  label = "Select a waste material:",
                                  choices = c("Cardboard", "Electronics",
                                              "Food", "Glass", "Paper", "Wood"),
                                  selected = "Cardboard",
                                  options = list(style = "btn-secondary"))),
                    wellPanel(
                      conditionalPanel(
                        condition = "input.usermaterial == `Cardboard`",
                        uiOutput("cardboardpanel")
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
                      # conditionalPanel(
                      #   condition = "input.usermaterial == `Trash`",
                      #   uiOutput("trashpanel")
                      # ),
                      conditionalPanel(
                        condition = "input.usermaterial == `Paper`",
                        uiOutput("paperpanel")
                      ),
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
                      conditionalPanel(
                        condition = "input.usermaterial == `Wood`",
                        uiOutput("woodpanel")
                      )
                      # conditionalPanel(
                      #   condition = "input.usermaterial == `Yard Debris`",
                      #   uiOutput("yarddebrispanel")
                      # ),
                    )
             ),
             
             # Viz Column 2 ------------------------------------------
             column(4,
                    wellPanel(pickerInput(inputId = "userregion",
                                          label = "Select a region:",
                                          choices = unique(mass$Wasteshed),
                                          selected = "Oregon total",
                                          options = list(style = "btn-secondary"))),
                    wellPanel(
                      style = "background-color: rgba(62,63,58,0.85);
                    color: rgba(248,245,240)",
                      # style = "background-color: rgba(248,245,240,0.9)",
                      htmlOutput("materialtext"),
                      br(),
                      conditionalPanel(
                        condition = "input.usermaterial == `Cardboard`",
                        plotlyOutput("cardboardmassplot")
                      ),
                      conditionalPanel(
                        condition = "input.usermaterial == `Electronics`",
                        plotlyOutput("elmassplot")
                      ),
                      conditionalPanel(
                        condition = "input.usermaterial == `Food`",
                        plotlyOutput("foodmassplot")
                      ),
                      conditionalPanel(
                        condition = "input.usermaterial == `Glass`",
                        plotlyOutput("glassmassplot")
                      ),
                      conditionalPanel(
                        condition = "input.usermaterial == `Paper`",
                        plotlyOutput("papermassplot")
                      ),
                      conditionalPanel(
                        condition = "input.usermaterial == `Wood`",
                        plotlyOutput("woodmassplot")
                      )
                      
                    )
             ),
             
             
             # Viz Column 3 ------------------------------------------------------------
             
             column(4,
                    wellPanel(pickerInput(inputId = "userimpact",
                                          label = "Select an impact:",
                                          choices = unique(I1$Category),
                                          selected = "Energy demand",
                                          options = list(style = "btn-secondary"))),
                    wellPanel(
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
                      conditionalPanel(
                        condition = "input.usermaterial == `Glass`",
                        plotlyOutput("glplot")
                      ),
                      conditionalPanel(
                        condition = "input.usermaterial == `Paper`",
                        plotlyOutput("ppplot")
                      ),
                      conditionalPanel(
                        condition = "input.usermaterial == `Wood`",
                        plotlyOutput("woodplot")
                      )
                      
                    )
             )
           ),
           
           # Text above tables
           fluidRow(
             column(12,
                    wellPanel(
                      div(align = "center",
                        p("View and download your results below, either in a detailed form or a summary that includes net impact."))))
             ),
           
# Tables ------------------------------------------------------------------
           
fluidRow(
  column(12,
         wellPanel(
           style = "background-color: rgba(248,245,240,0.9); color: rgba(62,63,58,0.85) ",
           conditionalPanel(
             condition = "input.usermaterial == `Cardboard`",
             DT::dataTableOutput("cbdf"),
             DT::dataTableOutput("cbimpact")
           ),
           conditionalPanel(
             condition = "input.usermaterial == `Electronics`",
             DT::dataTableOutput("eldf")
             
           ),
           conditionalPanel(
             condition = "input.usermaterial == `Food`",
             DT::dataTableOutput("fddf")
             
           ),
           conditionalPanel(
             condition = "input.usermaterial == `Glass`",
             DT::dataTableOutput("gldf")
           ),
           conditionalPanel(
             condition = "input.usermaterial == `Paper`",
             DT::dataTableOutput("ppdf")
           ),
           conditionalPanel(
             condition = "input.usermaterial == `Wood`",
             DT::dataTableOutput("wddf")
           )
           
           # tableOutput("ttable"),
           # tableOutput("df"),
           # DT::dataTableOutput("userimpact")
         )))
           
         )
),


# Glossary tab ---------------------------------------------------------

tabPanel("Glossary",
         fluidPage(
           column(3, wellPanel()),
           column(9,  wellPanel(
             includeMarkdown("Glossary.md")))
         )
),

# Resources tab -----------------------------------------------------------

tabPanel("Resources",
         fluidPage(
           column(5,
                  
                  wellPanel(
                    tags$div(
                      h3("Other calculators to explore"),
                      tags$ul(
                        tags$li(a(href = "https://www.epa.gov/warm",
                                  "EPA's Waste Reduction Model (WARM)"),
                                p("Another environmental impact calculator")),
                        tags$li(a(href = "https://www.footprintcalculator.org/",
                                  "Global Footprint Network's Footprint Calculator"),
                                p("An ecological footprint calculator for lifestyle choices.")
                        ),
                        tags$li(a(href = "https://www.montgomerycountymd.gov/sws/footprint/",
                                  "Environmental Footprint Calculator the Department of Environmental Protection in Mongomery County, MD"),
                                p("An ecological footprint calculator for the recycling of materials.")
                        ),
                        tags$li(a(href = "https://c.environmentalpaper.org/home",
                                  "Environmental Paper Network's Paper Calculator"),
                                p("A tool to assess and compare different environmental impacts of paper")),
                        tags$li(a(href = "http://www.gpi.org/recycling/carbon-calculator",
                                  "Glass Packaging Institute's Savings from Glass Recycling calculator"),
                                p("")
                      ))
                    ))
                  ),
           column(5
                  # wellPanel(
                  #   tags$div(
                  #     h3("Other"),
                  #     tags$ul(
                  #       tags$li(a(href = "",
                  #                 ""),
                  #               p("")),
                  #       tags$li(a(href = "",
                  #                 ""),
                  #               p(""),
                  #               br()
                  #       )
                  #     ))
                  # )
           )
         )
),


tabPanel("FAQs",
  fluidPage(
    fluidRow(
      column(8,
             wellPanel(
               tags$div(
                 h2("Frequently Asked Questions"),
                 br(),
                 h4("What about the stuff that's not in the solid waste stream?"),
                 p("This calculator only shows the impacts of things that have entered the solid waste stream at the end of life. It doesn't account for a lot of other things - for example, poured concrete, tennis shows sitting in your closet, or other things we haven't measured in a waste stream."),
                 h4("What about the use phase?"),
                 p("We aren't including use phase here, partly because those numbers can vary a lot. We limited the scope of this calculator to the production phase and the end-of-life phase.")
               )
             ))
  ))),

# About tab ---------------------------------------------------------------
tabPanel("About",
         fluidPage(
           fluidRow(
             column(4,
                    wellPanel(
                      div(style = "height:200px;",
                        h3("Materials Management"),
                        p("Materials management is an approach to serving human needs by using/reusing resources most productively and sustainably throughout their life cycles, generally minimizing the amount of materials involved and all the associated environmental impacts. This approach presents rich and transformative opportunities for DEQ and all Oregonians to better protect our environment."),
                        p(a(href = "https://www.oregon.gov/deq/mm/Pages/What-is-Materials-Management.aspx",
                            "Learn more"), "about Oregon DEQ's Materials Management Program.")
                      )
                    )
             ),
             column(4,
                    wellPanel(
                      div(style = "height:200px;",
                        h3("Oregon Department of Environmental Quality"),
                        p("The Oregon Department of Environmental Quality is a regulatory agency whose job is to protect the quality of Oregon's environment."),
                        p(a(href = "https://www.oregon.gov/deq",
                            "Learn more"), "about the Oregon DEQ and its other work around the state.")
                      )
                    )
             ),
             column(4,
                    wellPanel(
                      div(style = "height:200px;",
                        h3("Life Cycle Assessment"),
                        p("This calculator uses environmental impact factors derived from a Life Cycle Assessment (LCA)."),
                        p("For an overview of Life Cycle Assessments, check out Wikipedia's",
                          a(href = "https://en.wikipedia.org/wiki/Life-cycle_assessment",
                            "overview"), ".")
                      )
                    )
             )
           ),
           fluidRow(
             column(12)
           )
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
      sliderInput(inputId = "CardboardProduction",
                  label =  paste("Total", input$usermaterial, "Waste for the",
                                 input$userregion, " region, in Tons"),
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
                  ),
      sliderInput(inputId = "cblslide",
                  label = paste("%", tdisp()[2]),
                  min = 0,
                  max = 100,
                  post = " %",
                  value = t()$Percent[2]
                  ),
      sliderInput(inputId = "cbrslide",
                  label = paste("%", tdisp()[3]),
                  min = 0,
                  max = 100,
                  post = " %",
                  value = t()$Percent[3]
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
  
  cardboardsliderweights <- reactive({
    c(input$CardboardProduction*input$cbcslide/100,
      input$CardboardProduction*input$cblslide/100,
      input$CardboardProduction*input$cbrslide/100,
      input$CardboardProduction)
  })
  
  cbdf <- reactive({
    Disposition       <- c(tdisp()[1], tdisp()[2], tdisp()[3], "Production")
    `Initial Weight`  <- c(tweight()[1], tweight()[2], tweight()[3], sum(tweight()))
    `Scenario Weight` <- cardboardsliderweights()
    
    tibble(Disposition, `Initial Weight`, `Scenario Weight`) %>% 
      left_join(userimpact()) %>% 
      mutate(`Initial Impact` = `Initial Weight`*Factor,
             `Scenario Impact` = `Scenario Weight`*Factor,
             `Impact Difference` = `Initial Impact` - `Scenario Impact`)
  })
  
  cbmassdf <- reactive({
    cbdf() %>%
      select(Disposition, `Initial Weight`, `Scenario Weight`,
             `Initial Impact`, `Scenario Impact`)
    
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
  
  
  output$cardboardmassplot <- renderPlotly({
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
             yaxis = list(
               overlaying = "y",
               zerolinecolor = "#cf9f35",
               title = "Tons",
               titlefont = list(size = 18)),
             xaxis = list(title = "",
                          tickfont = list(size = 18)),
             legend = list(x = 100,
                           y = 0.5),
             font = list(
               family = "Open Sans, sans-serif",
               size = 14,
               color = "#cf9f35")

             ) %>% 
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
  })
  
  

  
  output$cbplot <- renderPlotly({
    
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
                marker = list(size = "22",
                              symbols = "x",
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
        legend = list(x = 100,
                      y = 0.5),
        font = list(family = "Open Sans, sans-serif",
                    size = 14,
                    color = "#cf9f35")
        # margin = list(
        #   b = 100,
        #   l = 90,
        #   r = 30,
        #   t = 30,
        #   pad = 5
        # )
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
  
  
# Carpet Panel ------------------------------------------------------------

  
  
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
                        name = "Landfilling",
                        marker = list(color = ("#564D65")),
                        type = "bar") %>% 
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
             legend = list(x = 100,
                           y = 0.5),
             font = list(
               family = "Open Sans, sans-serif",
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
                 name = "Production",
                 marker = list(color = ("#0B3C49")),
                 type = "bar") %>% 
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
                marker = list(size = "22",
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
        font = list(family = "Open Sans, sans-serif",
                    size = 14,
                    color = "#cf9f35"),
        legend = list(x = 100,
                      y = 0.5),
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
                    label = paste("Total", input$usermaterial, "Waste for the",
                                  input$userregion, " region, in Tons"),
                    min = 0,
                    max = sum(tweight())*1.5,
                    value = sum(tweight())
                    ),
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
                      value = 100 * input$FoodComposting / (input$FoodComposting +  input$`FoodAnaerobic Digestion` +  input$FoodLandfilling))
  })
  
  observeEvent({
    input$`FoodAnaerobic Digestion`
    input$FoodComposting
  }, {
    updateSliderInput(session = session,
                      inputId = "FoodLandfilling",
                      value = 100 * input$FoodLandfilling / (input$FoodComposting + input$`FoodAnaerobic Digestion` +  input$FoodLandfilling))
  })
  
  observeEvent({
    input$FoodComposting
    input$FoodLandfilling
  }, {
    updateSliderInput(session = session,
                      inputId = "\`FoodAnaerobic Digestion`",
                      value = 100 * input$`FoodAnaerobic Digestion` / (input$FoodComposting + input$`FoodAnaerobic Digestion` + input$FoodLandfilling))
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
                        name = "Composting",
                        marker = list(color = ("#A57548")),
                        type = "bar") %>% 
      add_trace(y = ~Landfilling,
                name = "Landfilling",
                marker = list(color = ("#564D65"))) %>% 
      add_trace(y = ~`Anaerobic Digestion`,
                name = "Anaerobic Digestion",
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
             font = list(
               family = "Open Sans, sans-serif",
               size = 14,
               color = "#cf9f35"),
             legend = list(x = 100,
                           y = 0.5),
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
                 name = "Production",
                 marker = list(color = ("#0B3C49")),
                 type = "bar") %>% 
      add_trace(y = ~Composting,
                name = "Composting",
                marker = list(color = ("#A57548"))) %>% 
      add_trace(y = ~Landfilling,
                name = "Landfilling",
                marker = list(color = ("#564D65"))) %>% 
      add_trace(y = ~`Anaerobic Digestion`,
                name = "Anaerobic Digestion",
                marker = list(color = ("#726E60"))) %>% 
      layout(barmode = "relative")
    
    p %>% 
      add_trace(y = ~Sum,
                type = "scatter",
                mode = "line",
                name = "Net impact",
                marker = list(size = "22",
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
        font = list(family = "Open Sans, sans-serif",
                    size = 14,
                    color = "#cf9f35"),
        legend = list(x = 100,
                      y = 0.5),
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

  output$glasspanel <-  renderUI({
    #dynamic number of sliders
    #https://stackoverflow.com/questions/35579439/dynamic-number-of-sliders-in-shiny
    
    tagList(
      # https://stackoverflow.com/questions/36906265/how-to-color-sliderbar-sliderinput
      # why coloring dynamic sliders doesnt work the second time
      # setSliderColor(c("#0B3C49", "#B1CA54", "#564D65", "#587B7F"), c(1, 2, 3, 4)),
      sliderInput(inputId = "GlassProduction",
                  label =  paste("Total", input$usermaterial, "Waste for the",
                                 input$userregion, " region, in Tons"),
                  min = 0,
                  max = sum(tweight())*1.5,
                  post = " tons",
                  value = sum(tweight())),
      sliderInput(inputId = "glslide",
                  label = paste("%", tdisp()[1]),
                  min = 0,
                  max = 100,
                  post = " %",
                  value = t()$Percent[1]
      ),
      sliderInput(inputId = "grslide",
                  label = paste("%", tdisp()[2]),
                  min = 0,
                  max = 100,
                  post = " %",
                  value = t()$Percent[2]
      ),
      sliderInput(inputId = "gaslide",
                  label = paste("%", tdisp()[3]),
                  min = 0,
                  max = 100,
                  post = " %",
                  value = t()$Percent[3]
      )
    )
  })
  
  observeEvent({
    input$glslide
    input$grslide
  }, {
    updateSliderInput(session = session,
                      inputId = "gaslide",
                      value = 100*input$gaslide/(input$glslide + input$grslide + input$gaslide)
    )
  })
  
  observeEvent({
    input$gaslide
    input$glslide
  }, {
    updateSliderInput(session = session,
                      inputId = "grslide",
                      value = 100*input$grslide/(input$glslide + input$grslide + input$gaslide)
    )
  })
  observeEvent({
    input$gaslide
    input$grslide
  }, {
    updateSliderInput(session = session,
                      inputId = "glslide",
                      value = 100*input$glslide/(input$glslide + input$grslide + input$gaslide)
    )
  })
  
  glsliderweights <- reactive({
    c(input$GlassProduction*input$glslide/100,
      input$GlassProduction*input$grslide/100,
      input$GlassProduction*input$gaslide/100,
      input$GlassProduction)
  })
  
  gldf <- reactive({
    Disposition       <- c(tdisp()[1], tdisp()[2], tdisp()[3], "Production")
    `Initial Weight`  <- c(tweight()[1], tweight()[2], tweight()[3], sum(tweight()))
    `Scenario Weight` <- glsliderweights()
    
    tibble(Disposition, `Initial Weight`, `Scenario Weight`) %>% 
      left_join(userimpact()) %>% 
      mutate(`Initial Impact` = `Initial Weight`*Factor,
             `Scenario Impact` = `Scenario Weight`*Factor)
  })
  
  glmassdf <- reactive({
    gldf() %>%
      select(Disposition, `Initial Weight`, `Scenario Weight`, `Initial Impact`, `Scenario Impact`)
    
  })
  
  glassdf <- reactive({
    if(is.null(glmassdf())) {
      return(NULL)
    }
    glmassdf() %>% 
      gather(key = "Variable", value = "Value", c(`Initial Weight`, `Scenario Weight`,
                                                  `Initial Impact`, `Scenario Impact`))
  })
  
  
  output$gldf <- DT::renderDataTable({
    DT::datatable(
      data = gldf(),
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
        columns =  c(names(gldf())),
        digits = 0)
  })
  
  output$glassmassplot <- renderPlotly({
    req(input$userregion)
    req(input$usermaterial)
    req(glassdf())
    #Add traces with a for loop
    #https://stackoverflow.com/questions/46583282/r-plotly-to-add-traces-conditionally-based-on-available-columns-in-dataframe
    massplot <- plot_ly(glassdf() %>%
                          filter(grepl("Weight", Variable)) %>%
                          spread("Disposition", "Value"),
                        x = ~Variable,
                        y = ~Landfilling,
                        name = "Landfilling",
                        marker = list(color = ("#564D65")),
                        type = "bar") %>% 
      add_trace(y = ~Recycling,
                name = "Recycling",
                marker = list(color = ("#587B7F"))) %>% 
      add_trace(y = ~`Use as Aggregate`,
                name = "Use as Aggregate",
                marker = list(color = ("#58700F"))) %>% 
      layout(barmode = "relative")
    
    massplot %>% 
      layout(paper_bgcolor = "#f8f5f0",
             plot_bgcolor = "#f8f5f0",
             yaxis = list(
               overlaying = "y",
               zerolinecolor = "#cf9f35",
               title = "Tons",
               titlefont = list(size = 18)),
             xaxis = list(title = "",
                          tickfont = list(size = 18)),
             legend = list(x = 100,
                           y = 0.5),
             font = list(
               family = "Open Sans, sans-serif",
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
  })
  
  
  output$glplot <- renderPlotly({
    
    p <- plot_ly(glassdf() %>%
                   filter(grepl("Impact", Variable)) %>%
                   spread("Disposition", "Value") %>%
                   mutate(Sum = rowSums(.[2:5])),
                 x = ~Variable,
                 y = ~Production,
                 name = "Production",
                 marker = list(color = ("#0B3C49")),
                 type = "bar") %>% 
      add_trace(y = ~Landfilling,
                name = "Landfilling",
                marker = list(color = ("#564D65"))) %>% 
      add_trace(y = ~Recycling,
                name = "Recycling",
                marker = list(color = ("#587B7F"))) %>%
      add_trace(y = ~`Use as Aggregate`,
                name = "Use as Aggregate",
                marker = list(color = ("#58700F"))) %>% 
      layout(barmode = "relative")
    
    p %>% 
      add_trace(y = ~Sum,
                type = "scatter",
                mode = "line",
                name = "Net impact",
                marker = list(size = "22",
                              symbols = "x",
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
        legend = list(x = 100,
                      y = 0.5),
        font = list(family = "Open Sans, sans-serif",
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

  })
# # Trash -------------------------------------------------------------------

  
# Paper -------------------------------------------------------------------

output$paperpanel <-  renderUI({

  tagList(
    sliderInput(inputId = "PaperProduction",
                label =  paste("Total", input$usermaterial, "Waste for the",
                               input$userregion, " region, in Tons"),
                min = 0,
                max = sum(tweight())*1.5,
                post = " tons",
                value = sum(tweight())
                ),
    sliderInput(inputId = "ppcslide",
                label = paste("%", tdisp()[1]),
                min = 0,
                max = 100,
                post = " %",
                value = t()$Percent[1]
                ),
    sliderInput(inputId = "pplslide",
                label = paste("%", tdisp()[2]),
                min = 0,
                max = 100,
                post = " %",
                value = t()$Percent[2]
                ),
    sliderInput(inputId = "pprslide",
                label = paste("%", tdisp()[3]),
                min = 0,
                max = 100,
                post = " %",
                value = t()$Percent[3]
                )
  )
})

  observeEvent({
    input$ppcslide
    input$pplslide
  }, {
    updateSliderInput(session = session,
                      inputId = "pprslide",
                      value = 100*input$pprslide/(input$ppcslide + input$pplslide + input$pprslide)
    )
  })

observeEvent({
  input$ppcslide
  input$pprslide
}, {
  updateSliderInput(session = session,
                    inputId = "pplslide",
                    value = 100*input$pplslide/(input$ppcslide + input$pplslide + input$pprslide)
  )
})
observeEvent({
  input$pprslide
  input$pplslide
}, {
  updateSliderInput(session = session,
                    inputId = "ppcslide",
                    value = 100*input$ppcslide/(input$ppcslide + input$pplslide + input$pprslide)
  )
})

papersliderweights <- reactive({
  c(input$PaperProduction*input$ppcslide/100,
    input$PaperProduction*input$pplslide/100,
    input$PaperProduction*input$pprslide/100,
    input$PaperProduction)
})

ppdf <- reactive({
  Disposition       <- c(tdisp()[1], tdisp()[2], tdisp()[3], "Production")
  `Initial Weight`  <- c(tweight()[1], tweight()[2], tweight()[3], sum(tweight()))
  `Scenario Weight` <- papersliderweights()

  tibble(Disposition, `Initial Weight`, `Scenario Weight`) %>%
    left_join(userimpact()) %>%
    mutate(`Initial Impact` = `Initial Weight`*Factor,
           `Scenario Impact` = `Scenario Weight`*Factor)
})

ppmassdf <- reactive({
  ppdf() %>%
    select(Disposition, `Initial Weight`, `Scenario Weight`,
           `Initial Impact`, `Scenario Impact`)

})

paperdf <- reactive({
  if(is.null(cbmassdf())) {
    return(NULL)
  }
  ppmassdf() %>%
    gather(key = "Variable",
           value = "Value",
           c(`Initial Weight`, `Scenario Weight`,
             `Initial Impact`, `Scenario Impact`))
})


output$ppdf <- DT::renderDataTable({
  DT::datatable(
    data = ppdf(),
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
      columns =  c(names(ppdf())),
      digits = 0)
})


output$papermassplot <- renderPlotly({
  req(input$userregion)
  req(input$usermaterial)
  req(paperdf())
  #Add traces with a for loop
  #https://stackoverflow.com/questions/46583282/r-plotly-to-add-traces-conditionally-based-on-available-columns-in-dataframe
  massplot <- plot_ly(paperdf() %>%
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
           yaxis = list(
             overlaying = "y",
             zerolinecolor = "#cf9f35",
             title = "Tons",
             titlefont = list(size = 18)),
           xaxis = list(title = "",
                        tickfont = list(size = 18)),
           # legend = list(x = 100,                            y = 0.5),
           font = list(
             family = "Open Sans, sans-serif",
             size = 14,
             color = "#cf9f35"),
           legend = list(x = 100,
                         y = 0.5)
           # ,
           # margin = list(b = 100,
           #               l = 70,
           #               r = 30,
           #               t = 30,
           #               pad = 4)
    ) %>%
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
})




output$ppplot <- renderPlotly({

  p <- plot_ly(paperdf() %>%
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
              marker = list(size = "22",
                            symbols = "x",
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
      # legend = list(x = 100,                            y = 0.5),
      font = list(family = "Open Sans, sans-serif",
                  size = 14,
                  color = "#cf9f35"),
      legend = list(x = 100,
                    y = 0.5)
      # margin = list(
      #   b = 100,
      #   l = 90,
      #   r = 30,
      #   t = 30,
      #   pad = 5
      # )
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

output$woodpanel <-  renderUI({
  
  tagList(
    sliderInput(inputId = "WoodProduction",
                label = paste("Total", input$usermaterial, "Waste for the",
                              input$userregion, " region, in Tons"),
                min = 0,
                max = sum(tweight())*1.5,
                value = sum(tweight())
                ),
    sliderInput(inputId = "WoodCombustion",
                label = paste("%", tdisp()[1]),
                min = 0,
                max = 100,
                post = " %",
                value = t()$Percent[1]
                ),
    sliderInput(inputId = "WoodComposting",
                label = paste("%", tdisp()[2]),
                min = 0,
                max = 100,
                post = " %",
                value = t()$Percent[2]
                ),
    sliderInput(inputId = "WoodLandfilling",
                label = paste("%", tdisp()[3]),
                min = 0,
                max = 100,
                post = " %",
                value = t()$Percent[3]
                ),
    sliderInput(inputId = "WoodRecycling",
                label = paste("%", tdisp()[4]),
                min = 0,
                max = 100,
                post = " %",
                value = t()$Percent[4]
                )
    )
})

observeEvent({
  input$WoodCombustion
  input$WoodLandfilling
  input$WoodRecycling
}, {
  updateSliderInput(session = session,
                    inputId = "WoodComposting",
                    value = 100 * input$WoodComposting / (input$WoodCombustion +
                                                            input$WoodComposting +
                                                            input$WoodLandfilling +
                                                            input$WoodRecycling))
  })

observeEvent({
  input$WoodComposting
  input$WoodLandfilling
  input$WoodRecycling
}, {
  updateSliderInput(session = session,
                    inputId = "WoodCombustion",
                    value = 100 * input$WoodCombustion / (input$WoodCombustion +
                                                            input$WoodComposting +
                                                            input$WoodLandfilling +
                                                            input$WoodRecycling))
})

observeEvent({
  input$WoodCombustion
  input$WoodComposting
  input$WoodRecycling
}, {
  updateSliderInput(session = session,
                    inputId = "WoodLandfilling",
                    value = 100 * input$WoodLandfilling / (input$WoodCombustion +
                                                             input$WoodComposting +
                                                             input$WoodLandfilling +
                                                             input$WoodRecycling))
})

observeEvent({
  input$WoodCombustion
  input$WoodComposting
  input$WoodLandfilling

}, {
  updateSliderInput(session = session,
                    inputId = "WoodRecycling",
                    value = 100 * input$WoodRecycling / (input$WoodCombustion +
                                                           input$WoodComposting +
                                                           input$WoodLandfilling +
                                                           input$WoodRecycling))
})

woodsliderweights <- reactive({
  c(input$WoodProduction*input$WoodCombustion/100,
    input$WoodProduction*input$WoodComposting/100,
    input$WoodProduction*input$WoodLandfilling/100,
    input$WoodProduction*input$WoodRecycling/100,
    input$WoodProduction)
})

wddf <- reactive({
  Disposition       <- c(tdisp()[1], tdisp()[2], tdisp()[3], tdisp()[4],
                         "Production")
  `Initial Weight`  <- c(tweight()[1], tweight()[2], tweight()[3], tweight()[4],
                         sum(tweight()))
  `Scenario Weight` <- woodsliderweights()

  tibble(Disposition, `Initial Weight`, `Scenario Weight`) %>%
    left_join(userimpact()) %>%
    mutate(`Initial Impact` = `Initial Weight`*Factor,
           `Scenario Impact` = `Scenario Weight`*Factor)
})


woodmassdf <- reactive({
  wddf() %>%
    select(Disposition, `Initial Weight`, `Scenario Weight`,
           `Initial Impact`, `Scenario Impact`)
})

wooddf <- reactive({
  woodmassdf() %>%
    gather(key = "Variable", value = "Value",
           c(`Initial Weight`, `Scenario Weight`,
             `Initial Impact`, `Scenario Impact`))
})

output$wddf <- DT::renderDataTable({
  DT::datatable(
    data = wddf(),
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
      columns =  c(names(wddf())),
      digits = 0)
})

output$woodmassplot <- renderPlotly({
  massplot <- plot_ly(wooddf() %>%
                        filter(grepl("Weight", Variable)) %>%
                        spread("Disposition", "Value"),
                      x = ~Variable,
                      y = ~Composting,
                      name = "Composting",
                      marker = list(color = ("#A57548")),
                      type = "bar") %>%
    add_trace(y = ~Landfilling,
              name = "Landfilling",
              marker = list(color = ("#564D65"))) %>%
    add_trace(y = ~Combustion,
              name = "Combustion",
              marker = list(color = ("#898952"))) %>%
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
           font = list(
             family = "Open Sans, sans-serif",
             size = 14,
             color = "#cf9f35"),
           legend = list(x = 100,
                         y = 0.5),
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

output$woodplot <- renderPlotly({
  req(input$userimpact)
  req(input$usermaterial)

  p <- plot_ly(wooddf() %>%
                 filter(grepl("Impact", Variable)) %>%
                 spread("Disposition", "Value") %>%
                 mutate(Sum = rowSums(.[2:6])),
               x = ~Variable,
               y = ~Production,
               name = "Production",
               marker = list(color = ("#0B3C49")),
               type = "bar") %>%
    add_trace(y = ~Composting,
              name = "Composting",
              marker = list(color = ("#A57548"))) %>%
    add_trace(y = ~Landfilling,
              name = "Landfilling",
              marker = list(color = ("#564D65"))) %>%
    add_trace(y = ~Combustion,
              name = "Combustion",
              marker = list(color = ("#898952"))) %>%
    add_trace(y = ~Recycling,
              name = "Recycling",
              marker = list(color = ("#587B7F"))) %>%
    layout(barmode = "relative")

  p %>%
    add_trace(y = ~Sum,
              type = "scatter",
              mode = "line",
              name = "Net impact",
              marker = list(size = "22",
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
      font = list(family = "Open Sans, sans-serif",
                  size = 14,
                  color = "#cf9f35"),
      legend = list(x = 100,
                    y = 0.5),
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

# End Wood Panel ----------------------------------------------------------



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
    family = "Open Sans, sans-serif",
    color = "#cf9f35",
    size = 16
  )
  largefont <- list(
    family = "Open Sans, sans-serif",
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
    title = "Global Warming Potential, in MTCO2E",
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
             # legend = list(x = 100,                            y = 0.5),
             )
    sub2
  })
  
  
output$transparentbars <- renderPlotly({
  
  transparentbars <- plot_ly(data = regionalcontext() %>% filter(umbDisp == "Net"),
                             x = ~appMaterial) %>% 
    add_trace(y = ~tons,
              yaxis = "y2", 
              type = "bar",
              opacity = 0.6,
              name = "Weight",
              color = I("#587079")) %>%
    add_trace(y = ~impact,
              type = "bar",
              opacity = 0.9,
              name = "Impact",
              color = I("#cf9f35")) %>%
    layout(
      barmode = "group",
      paper_bgcolor = "#f8f5f0",
      plot_bgcolor = "#f8f5f0",
      xaxis = list(title = "",
                   tickfont = list(size = 18)),
      yaxis = list(
        zerolinecolor = "#cf9f35",
        title = "Impact in MTCO2E",
        titlefont = list(size = 20)
      ),
      yaxis2 = list(title = "Weight in tons",
                    overlaying = "y",
                    side = "right"),
      legend = list(x = 0.5,
                    y = 100,
                    orientation = 'h'),
      font = list(family = "Open Sans, sans-serif",
                  size = 14,
                  color = "#cf9f35"),
      margin = list(
        b = 100,
        l = 90,
        r = 90,
        t = 30,
        pad = 18
      )
    )
  
  transparentbars
})

output$transparentbarspercent <- renderPlotly({
  
  transparentbarspercent <- plot_ly(data = regionalcontext() %>% 
                                      filter(umbDisp == "Net") %>% 
                                      mutate(Percent_Impact = impact/sum(impact),
                                             Percent_Tons = tons/sum(tons)),
                                    x = ~appMaterial) %>% 
    add_trace(y = ~Percent_Tons,
              yaxis = "y2", 
              type = "bar",
              opacity = 0.5,
              name = "Percent Weight",
              color = I("#587079")) %>%
    add_trace(y = ~Percent_Impact,
              type = "bar",
              opacity = 0.8,
              name = "Percent Impact",
              color = I("#cf9f35")) %>%
    layout(
      barmode = "group",
      paper_bgcolor = "#f8f5f0",
      plot_bgcolor = "#f8f5f0",
      xaxis = list(title = "",
                   tickfont = list(size = 18)),
      yaxis = list(
        zerolinecolor = "#cf9f35",
        title = "Percent of GHG impact",
        titlefont = list(size = 20)
      ),
      yaxis2 = list(title = "Percent weight",
                    overlaying = "y",
                    side = "right"),
      legend = list(x = 0.5,
                    y = 100,
                    orientation = 'h'),
      font = list(family = "Open Sans, sans-serif",
                  size = 14,
                  color = "#cf9f35"),
      margin = list(
        b = 100,
        l = 90,
        r = 30,
        t = 30,
        pad = 5
      )
    )
  
  transparentbarspercent
  
  
  
})

  
}

shinyApp(ui = ui, server = server)