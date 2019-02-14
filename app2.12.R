
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


m1 <- read_csv("imfoAppMassProfiles.csv")
mass <- m1 %>%
  mutate(`Umbrella Disposition` = ifelse(disposition %in% "landfilling", "Disposal", "Recovery")) %>% 
  mutate(Material = recode(material, "FoodWaste" = "Food Waste")) %>% 
  mutate(`Life Cycle Stage` = ifelse(LCstage %in% "endOfLifeTransport", "EOL Transport", "EOL")) %>% 
  filter(`Life Cycle Stage` != "EOL Transport") %>%
  mutate(`2015 Weight` = round(tons, digits = -2)) %>% 
  rename(Wasteshed = wasteshed, Disposition = disposition) %>% 
  select(Wasteshed, Material, Disposition, `Life Cycle Stage`, `Umbrella Disposition`, `2015 Weight`)


I <- read_csv("imfoAppImpactFactors.csv")

I1 <- I %>% 
  mutate(Material = recode(material, "FoodWaste" = "Food Waste")) %>% 
  mutate(`Life Cycle Stage` = ifelse(LCstage %in% "endOfLifeTransport", "EOL Transport", 
                                     ifelse(LCstage %in% "endOfLife", "EOL",
                                            ifelse(LCstage %in% "production", "Production",
                                                   "other")))
  ) %>% 
  rename(Disposition = disposition, `Impact Category` = impactCategory,
         `Impact Units` = impactUnits, `Impact Factor` = impactFactor,
         `Implied Miles` = impliedMiles) %>% 
  select(Material, Disposition, `Life Cycle Stage`, `Impact Category`,
         `Impact Units`, `Impact Factor`, `Implied Miles`)

tweight <- mass %>% 
  filter(Material %in% "Rigid Plastic Cont.") %>% 
  pull(`2015 Weight`)

Wastesheds <- sort(unique(mass$Wasteshed))
Materials <- sort(unique(mass$Material))
Dispositions <- sort(unique(mass$Disposition))

options(shiny.reactlog = TRUE)


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  chooseSliderSkin("Modern"),
  
  navbarPage("Material Impact Visualizer",

# Introduction tab -------------------------------------------------------
    tabPanel("Introduction",
             h2("Welcome to DEQ's Material Impact Visualizer!", align = "center"),
             div(img(src = 'greenpic.jpeg', height="50%", width="50%"), style = "text-align: center;"),
             br(),
             p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")),

# User Input Tab ---------------------------------------------------------------
tabPanel("Start here!",
         sidebarLayout(
           sidebarPanel(
             sliderInput(inputId = "Production",
                         label = "Production",
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
                         value = tweight[3]/sum(tweight)),
             uiOutput("RPLandfilling"),
             uiOutput("RPCombustion"),
             uiOutput("RPRecycling")
           ),
           mainPanel()
           
         )
),

tabPanel("Visualize More Impacts",
         sidebarLayout(
           sidebarPanel(
             selectInput(inputId = "selectedwasteshed",
                         label = "Select a wasteshed:",
                         choices = Wastesheds),
             bsTooltip("selectedwasteshed",
                       "Waste is counted within these regions. You can also choose Metro and Oregon as a whole.",
                       "right", options = list(container = "body")),
             br(),
             uiOutput("choose_materials"),
             br(),
             actionButton("submitbutton", "Submit"),
             br(),
             br(),
             materialSwitch(inputId = "materialconstraints",
                            label = "Keep original material weights?"),
             hr(),
             conditionalPanel(
               condition = "input.submitbutton == true",
               
               tags$div(class = "header",
                        tags$p(tags$b("Sliders are set to the 2015 weights for each material. Move sliders to generate a new scenario for this wasteshed.")),
                        tags$p("The overall weight for each material can be more or less than the 2015 amount, which would reflect a change in production.")),
               
               #scrolling well panel
               wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                         
                         # Sliders (UI) ------------------------------------------------------------
                         
                         conditionalPanel(
                           condition = "input$usermaterials %in% 'Cardboard'",
                           uiOutput("cardboardsliders")),
                         conditionalPanel(
                           condition = "input$usermaterials %in% 'Electronics'",
                           uiOutput("electricsliders")),
                         conditionalPanel(
                           condition = "input$usermaterials %in% 'Food'",
                           uiOutput("foodsliders")),
                         conditionalPanel(
                           condition = "input$usermaterials %in% 'Glass Containers'",
                           uiOutput("glasssliders")),
                         conditionalPanel(
                           condition = "input$usermaterials %in% 'Nonrecyclables'",
                           uiOutput("nonrecyclableslider")),
                         conditionalPanel(
                           condition = "input$usermaterials %in% 'Paper'",
                           uiOutput("papersliders")),
                         conditionalPanel(
                           condition = "input$usermaterials %in% 'Rigid Plastic Cont.'",
                           uiOutput("rigidplasticsliders")),
                         conditionalPanel(
                           condition = "input$usermaterials %in% 'Scrap Metal'",
                           uiOutput("metalsliders")),
                         conditionalPanel(
                           condition = "input$usermaterials %in% 'Wood'",
                           uiOutput("woodsliders")),
                         conditionalPanel(
                           condition = "input$usermaterials %in% 'Yard'",
                           uiOutput("yardsliders"))
               )
             )
    ),

# Main Panel --------------------------------------------------------------

    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Stacked",
                           tags$br(),
                           tags$div(class = "header",
                                    tags$p("Existing weights are shown in the
                                           light colors. Moving the sliders generates
                                           a new scenario, shown in the darker colors.")),
                                    plotOutput("weightsplot1")),
                  tabPanel("Side by side",
                           tags$br(),
                           tags$div(class = "header",
                                    tags$p("Existing weights are shown in the light colors.
                                           Moving the sliders generates a new scenario,
                                           shown in the darker colors.")),
                           plotOutput("weightsplot2")),
                  tabPanel("Impacts",
                           plotOutput("impactplot")),
                  tabPanel("Impacts Stacked",
                           plotOutput("impactplot_b")),
                  # tabPanel("impact2", plotOutput("impactplot2")),

# Tables tab --------------------------------------------------------------

tabPanel("Tables",
         hr(),
         h3("A reactive table created from the selected wasteshed 
                         and selected materials:"),
         hr(),
         DT::dataTableOutput("table1"),
         hr(),
         h3("A reactive table created from the above table and
                         the sliders:"),
         hr(),
         DT::dataTableOutput("table2"),
         hr(),
         h3("A reactive table created from a join of the above table and the
            impact data:"),
         hr(),
         DT::dataTableOutput("table3"),
         hr(),
         h3("A \"melted\" form of the above data, used for plots:"),
         hr(),
         DT::dataTableOutput("table4")),

# Download button ---------------------------------------------------------
                  
                  tabPanel("Data export",
                           selectInput("dataset",
                                       "Choose a dataset:",
                                       choices = c("Wide form", "Long form")),
                           
                           # Button
                           downloadButton("downloadData", "Download")
                  )
                  )
      # plotOutput("weightsplot2"),
      # plotOutput("impactplot"),
      # # DT::dataTableOutput("table1"),
      # DT::dataTableOutput("table2"),
      # DT::dataTableOutput("table3"),
      # DT::dataTableOutput("table4")
      # plotlyOutput("plot1")
    )
  )
),

# Glossary tab ------------------------------------------------------------


tabPanel("Glossary",
         navlistPanel(
           widths = c(2, 10),
           tabPanel("Materials"),
           tabPanel("Dispositions"),
           tabPanel("Impacts")
         ),
         mainPanel(
           width = 8,
           selectInput("impacttodefine",
                       label = "Choose an impact to define:",
                       choices = I1$`Impact Category`),
           verbatimTextOutput(outputId = "impactdefinition")
         )),
navbarMenu("More",
           tabPanel("Resources",
                    tags$div(
                      tags$ul(
                        tags$li(a(href = "https://www.epa.gov/warm", "EPA's Waste Reduction Model (WARM)"),
                                p("Another environmental impact calculator")),
                        tags$li(a(href = "https://www.footprintcalculator.org/", "Global Footprint Network's Footprint Calculator"),
                                p("An ecological footprint calculator for lifestyle choices.")
                      )
                    ))),
                    # a(href = "https://www.epa.gov/warm", "EPA's Waste Reduction Model (WARM)"),
                    # p("Another environmental impact calculator")),
           tabPanel("About",
                    h5("Built with",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "40px"), "by", img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                       ".")
                    )
           )
)
)




# Server ------------------------------------------------------------------

server <- function(input, output, session) {



# Simple ------------------------------------------------------------------

  observeEvent({
    input$RPCombustion
    input$RPRecycling
    }, {
      updateSliderInput(session = session, 
                        inputId = "RPLandfilling", 
                        value = 100 * input$RPLandfilling/(input$RPCombustion + input$RPLandfilling + input$RPRecycling))
    })
  
  # when air change, update water
  observeEvent({
    input$RPLandfilling
    input$RPCombustion
    },  {
      updateSliderInput(session = session,
                        inputId = "RPCombustion",
                        value = 100 * input$RPCombustion/(input$RPCombustion + input$RPLandfilling + input$RPRecycling))
    })
  
  observeEvent({
    input$RPCombustion
    input$RPLandfilling
    },  {
      updateSliderInput(session = session,
                        inputId = "RPRecycling",
                        value = 100 * input$RPRecycling/(input$RPCombustion + input$RPLandfilling + input$RPRecycling))
    })
# more --------------------------------------------------------------------

  
  values <- reactiveValues(starting = TRUE)
  session$onFlushed(function() {
    values$starting <- FALSE
  })
  
  #widget that filters dataframe to display chosen materials
  output$choose_materials <- renderUI({
    pickerInput(
      inputId = "usermaterials",
      label = "Select up to six materials:",
      choices = Materials,
      multiple = TRUE,
      options = list("max-options" = 6)
    )
    # selectInput(inputId = "usermaterials",
    #             label = 'Select up to six materials:',
    #             choices = Materials,
    #             multiple = TRUE,
    #             selectize = TRUE)
  })
  
  #creates a reactive dataframe once the submit button is clicked
  userwastemat <- eventReactive(input$submitbutton, {
    mass %>%
      filter(Wasteshed == input$selectedwasteshed) %>%
      filter(Material %in% input$usermaterials)
  })

  # Sliders ----------------------------------------------------------------
  
  output$cardboardsliders <- renderUI({
    tweight <- userwastemat() %>% 
      filter(Material %in% "Cardboard/Kraft") %>% 
      pull(`2015 Weight`)
    
    req(input$usermaterials == "Cardboard/Kraft")
    
    tagList(h4("Cardboard/Kraft"),
            sliderInput(inputId = "slider1cb",
                        label = "Combustion",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[1]),
            sliderInput(inputId = "slider1cp",
                        label = "Composting",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[2],
                        step = 1000),
            sliderInput(inputId = "slider1L",
                        label = "Landfilling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[3]),
            sliderInput(inputId = "slider1R",
                        label = "Recycling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[4],
                        step = 1000))
  })
  
  output$electricsliders <- renderUI({
    tweight <- userwastemat() %>% 
      filter(Material %in% "Electronics") %>% 
      pull(`2015 Weight`)
    
    req(input$usermaterials == "Electronics")
    
    tagList(h4("Electronics"),
    sliderInput(inputId = "slider2L",
                label = "Landfilling",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[1]),
    sliderInput(inputId = "slider2R",
                label = "Recycling",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[2])
    )
  })
  
  
  output$foodsliders <- renderUI({
    tweight <- userwastemat() %>% 
      filter(Material %in% "Food Waste") %>% 
      pull(`2015 Weight`)
    
    req(input$usermaterials == "Food Waste")
    
    tagList(h4("Food Waste"),
            sliderInput(inputId = "slider3AD",
                        label = "Anaerobic Digestion",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[1]),
            sliderInput(inputId = "slider3cb",
                        label = "Combustion",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[2]),
            sliderInput(inputId = "slider3cp",
                        label = "Composting",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[3]),
            sliderInput(inputId = "slider3L",
                        label = "Landfilling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[4])
    )
  })
  
  output$glasssliders <- renderUI({
    tweight <- userwastemat() %>% 
      filter(Material %in% "Glass Containers") %>% 
      pull(`2015 Weight`)
    
    req(input$usermaterials == "Glass Containers")
    tagList(h4("Glass Containers"),
            sliderInput(inputId = "slider4L",
                        label = "Landfilling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[1]),
            sliderInput(inputId = "slider4R",
                        label = "Recycling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[2]),
            sliderInput(inputId = "slider4U",
                        label = "useAsAggregate",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[3])
    )
  })
  
  output$nonrecyclableslider <- renderUI({
    tweight <- userwastemat() %>% 
      filter(Material %in% "Nonrecyclables") %>% 
      pull(`2015 Weight`)
    
    req(input$usermaterials == "Nonrecyclables")
    
    tagList(h4("Nonrecylables"),
            sliderInput(inputId = "slider5L",
                label = "Landfilling",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[1])
    )
  })
  
  output$papersliders <- renderUI({
    tweight <- userwastemat() %>% 
      filter(Material %in% "Paper Fiber") %>% 
      pull(`2015 Weight`)
    
    req(input$usermaterials == "Paper Fiber")
    
    tagList(h4("Paper Fiber"),
            sliderInput(inputId = "slider6cb",
                        label = "Combustion",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[1]
            ),
            sliderInput(inputId = "slider6L",
                        label = "Landfilling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[2]
            ),
            sliderInput(inputId = "slider6R",
                        label = "Recycling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[3]
            )
    )
  })
  
  
  output$rigidplasticsliders <- renderUI({
    tweight <- userwastemat() %>% 
      filter(Material %in% "Rigid Plastic Cont.") %>% 
      pull(`2015 Weight`)
    
    req(input$usermaterials == "Rigid Plastic Cont.")
    
    tagList(h4("Rigid Plastic"),
            sliderInput(inputId = "slider7cb",
                        label = "Combustion",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[1]),
            sliderInput(inputId = "slider7L",
                        label = "Landfilling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[2]),
            sliderInput(inputId = "slider7R",
                          label = "Recycling",
                          min   = 0,
                          max   = sum(tweight)*1.2,
                          value = tweight[3])
    )
  })
  
  output$metalsliders <- renderUI({
    tweight <- userwastemat() %>% 
      filter(Material %in% "Scrap Metal - Other") %>% 
      pull(`2015 Weight`)
    
    req(input$usermaterials == "Scrap Metal - Other")
    
    tagList(h4("Scrap Metal - Other"),
            sliderInput(inputId = "slider8L",
                        label = "Landfilling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[1]),
            sliderInput(inputId = "slider8R",
                        label = "Recycling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[2])
            )
  })
  
  output$woodsliders <- renderUI({
    tweight <- userwastemat() %>% 
      filter(Material %in% "Wood Waste") %>% 
      pull(`2015 Weight`)
    
    req(input$usermaterials == "Wood Waste")
    tagList(h4("Wood Waste"),
            sliderInput(inputId = "slider9cb",
                        label = "Combustion",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[1]
            ),
            sliderInput(inputId = "slider9cp",
                        label = "Composting",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[2]
            ),
            sliderInput(inputId = "slider9L",
                        label = "Landfilling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[3]
            ),
            sliderInput(inputId = "slider9R",
                        label = "Recycling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[4]
            )
    )
  })
  
  
  output$yardsliders <- renderUI({
    tweight <- userwastemat() %>% 
      filter(Material %in% "Yard Debris") %>% 
      pull(`2015 Weight`)
    
    req(input$usermaterials == "Yard Debris")
    tagList(h4("Yard Debris"),
            sliderInput(inputId = "slider10AD",
                        label = "Anaerobic Digestion",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[1]
            ),
            sliderInput(inputId = "slider10cb",
                        label = "Combustion",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[2]
            ),
            sliderInput(inputId = "slider10cp",
                        label = "Composting",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[3]
            ),
            sliderInput(inputId = "slider10L",
                        label = "Landfilling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[4]
            )
    )
  })

# End Sliders -------------------------------------------------------------


# Dataframes and tables ---------------------------------------------------

  
  output$table1 <- DT::renderDataTable({
    if (is.null(input$selectedwasteshed))
      return()
    userwastemat()
  })
  
  newnew <- reactive({
    req(userwastemat())
    
    df <- mass %>%
    
      filter(Wasteshed == input$selectedwasteshed) %>%
      filter(Material %in% input$usermaterials) %>%
      mutate(`New Weight` = `2015 Weight`)
    df$`New Weight` <- c(input$slider1cb, input$slider1cp,
                          input$slider1L, input$slider1R, 
                          input$slider2L, input$slider2R, 
                          input$slider3AD, input$slider3cb,
                          input$slider3cp, input$slider3L, 
                          input$slider4L, input$slider4R, input$slider4U,
                          input$slider5L, 
                          input$slider6cb, input$slider6L, input$slider6R, 
                          input$slider7cb, input$slider7L, input$slider7R, 
                          input$slider8L, input$slider8R, 
                          input$slider9cb, input$slider9cp,
                          input$slider9L, input$slider9R, 
                          input$slider10AD, input$slider10cb,
                          input$slider10cp, input$slider10L) 
    df
  })
  
  output$table2 <- DT::renderDataTable({
    newnew()
  })
  
  # allLC <- reactive({
  #   nT <- newnew() %>% 
  #     select(-`Life Cycle Stage`) %>% 
  #     mutate(`Life Cycle Stage` = "EOL Transportation")
  #   
  #   nP <- newnew() %>% 
  #     select(-`Life Cycle Stage`) %>% 
  #     mutate(`Life Cycle Stage` = "Production")
  #   
  #   nn <- newnew() %>% 
  #     rbind(nT) %>% 
  #     rbind(nP)
  #   
  #   nn
  # })
  
  newimpacts <- reactive({
    nT <- newnew() %>% 
      mutate(`Life Cycle Stage` = "EOL Transport")
    
    nP <- newnew() %>% 
      mutate(`Life Cycle Stage` = "Production",
             Disposition = "production")
    
    nn <- newnew() %>% 
      rbind(nT) %>% 
      rbind(nP) %>% 
      left_join(I1, by = c("Material", "Disposition",
                           "Life Cycle Stage")) %>% 
      mutate(`2015 Impact` = round(`2015 Weight`*`Impact Factor`),
             `New Impact` = round(`New Weight`*`Impact Factor`))
    
    
    nn
    # n <- newnew() %>% 
    #  left_join(I1, by = c("Material", "Disposition",
    #                       "Life Cycle Stage")) %>% 
    #  mutate(`2015 Impact` = round(`2015 Weight`*`Impact Factor`),
    #         `New Impact` = round(`New Weight`*`Impact Factor`))
    #  n
  })
  
  output$table3 <- DT::renderDataTable({
    newimpacts()
  })
  

  meltedusermass <- reactive({
    test <- newnew() %>% 
      select(-c(Wasteshed, `Umbrella Disposition`, `Life Cycle Stage`)) %>% 
      melt(id.vars = c('Material', 'Disposition'))
    # print(test)
  })
  
  # meltedimpacts <- reactive({
  #   t <- newimpacts() %>% 
  #     select(-c(`2015 Weight`, `New Weight`, `Impact Factor`)) %>% 
  #     melt(id.vars = c('Material', 'Disposition', `Impact Factor`, `Impact Units`)) %>% 
  #     filter(!is.na(`Impact Factor`))
  #   t
  # })
 
  meltedimpacts <- reactive({ 
  t <- newimpacts() %>% 
    select(Material, Disposition, `Life Cycle Stage`, `Umbrella Disposition`, `Impact Category`, `Impact Units`, `2015 Impact`, `New Impact`) %>% 
    gather(key = "Scenario", value = "Impact", -c(Material, Disposition, `Life Cycle Stage`, `Umbrella Disposition`, `Impact Category`, `Impact Units`))
  })
  
  output$table4 <- DT::renderDataTable({
    meltedimpacts()
  })


# Impacts plot ------------------------------------------------------------
  output$impactplot <- renderPlot({
  
    pl <- ggplot(meltedimpacts(),
                 aes(y = Impact,
                     x = Material,
                     fill = `Life Cycle Stage`,
                     alpha = Scenario
                 )) +
      geom_bar(position = "dodge",
               stat = "identity",
               width = 1) +
      theme_minimal(base_size = 18) +
      facet_wrap(~`Impact Category`, ncol = 3, scales = "free_y"
      ) +
      scale_fill_viridis_d(end = 0.4, direction = 1) 
    # +
    #   scale_alpha_discrete(range = c(0.5, 1))
    pl +
      theme(        
        axis.title.x = element_blank(),
        axis.title.y = element_text(
          margin = margin(t = 0, r = 20, b = 0, l = 0),
          # size = 16,
          vjust = -0.65),
        # axis.text = element_text(size = 16),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 50, hjust = 1
    )) +
      geom_hline(mapping = NULL, data = NULL, size = 1, yintercept = 0,
                 na.rm = FALSE, show.legend = NA)
  }, height = 750, width = 1000)
      
  #   pl <- ggplot(meltedimpacts(),
  #                aes(y = Impact,
  #                    x = Material,
  #                    fill = `Life Cycle Stage`,
  #                    alpha = Scenario
  #                    )) +
  #     geom_bar(position = "dodge",
  #              stat = "identity") +
  #     theme_bw(base_size = 16) +
  #     facet_wrap(~`Impact Category`, ncol = 3, scales = "free_y"
  #     ) +
  #     scale_fill_viridis_d(begin = 0.5, direction = -1) 
  #   # +
  #   #   scale_alpha_discrete(range = c(0.5, 1))
  #   pl + theme(axis.text.x = element_text(angle = 50, hjust = 1
  #   )) +
  #     # scale_y_continuous(limits = c(min(meltedimpacts()$Impact), max(meltedimpacts()$Impact))) +
  #     geom_hline(mapping = NULL, data = NULL, size = 1, yintercept = 0,
  #                na.rm = FALSE, show.legend = NA)
  # }, height = 750, width = 1000)

  
  output$impactplot_b <- renderPlot({
    
    pl <- ggplot(meltedimpacts() %>% 
                   filter(Scenario %in% "New Impact"),
                 aes(y = Impact,
                     x = Material,
                     fill = `Life Cycle Stage`)) +
      geom_bar(
        stat = "identity") +
      theme_minimal(base_size = 20) +
      facet_wrap(~`Impact Category`, ncol = 3, scales = "free_y"
      ) +
      scale_fill_viridis_d(end = 0.4, direction = 1) 
    # +
    #   scale_alpha_discrete(range = c(0.5, 1))
    pl + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 20, b = 0, l = 0),
        # size = 16,
        vjust = -0.65),
      # axis.text = element_text(size = 16),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 50, hjust = 1
    )) +
      # scale_y_continuous(limits = c(min(meltedimpacts()$Impact), max(meltedimpacts()$Impact))) +
      geom_hline(mapping = NULL, data = NULL, size = 1, yintercept = 0,
                 na.rm = FALSE, show.legend = NA)
  }, height = 750, width = 1000)
  
  
  
  
  
  output$impactplot2 <- renderPlot({
    
    pl <- ggplot(meltedimpacts(), aes(y = value, x = impactCategory, fill = impactCategory, alpha = variable)) +
      geom_bar(position = "dodge", stat = "identity") +
      theme_bw(base_size = 20) +
      facet_wrap(~Material, ncol = 3, scales = "free_y"
      ) +
      scale_fill_viridis_d(direction = -1, option = "A") +
      scale_alpha_discrete(range = c(0.5, 1))
    pl + theme(axis.text.x = element_text(angle = 50, hjust = 1
    )) +
      geom_hline(mapping = NULL, data = NULL, size = 1, yintercept = 0,
                 na.rm = FALSE, show.legend = NA)
  }, height = 750, width = 1200)
  
  
  

# Weights plot ------------------------------------------------------------

  
# output$weightsplot2 <- renderPlot({
#   if (values$starting)
#     return(NULL)
#   ggplot(meltedusermass(),
#          aes(y = value,
#              x = variable,
#              fill = Material,
#              alpha = variable)) +
#     geom_bar(stat = "identity") +
#     theme_bw(base_size = 16) +
#     theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
#     facet_wrap(~Material, nrow = 2) +
#     scale_fill_viridis_d(direction = -1) +
#     scale_alpha_discrete(range = c(0.5, 1)) +
#     scale_y_continuous(labels = scales::comma)
# })
  output$weightsplot1 <- renderPlot({
    req(meltedusermass())
    
  
    ggplot(meltedusermass(),
           aes(y = value,
               x = variable,
               fill = Disposition,
               alpha = variable)) +
      geom_bar(stat = "identity") +
      theme_minimal(base_size = 20) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_text(
          margin = margin(t = 0, r = 20, b = 0, l = 0),
          # size = 16,
          vjust = -0.65),
        # axis.text = element_text(size = 16),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 50, hjust = 1)) +
      labs(y = "Weight in Tons",
           alpha = "") +
      facet_wrap(~Material, nrow = 2) +
      scale_fill_viridis_d(direction = 1, end = 0.85) +
      scale_alpha_discrete(range = c(0.3, 1)) +
      scale_y_continuous(labels = scales::comma,
                         limits = c(0, 20000))
    
  }, height = 600, width = 1000)
  
  
output$weightsplot2 <- renderPlot({
  ggplot(meltedusermass(), aes(y = value,
                               x = Disposition,
                               fill = Material,
                               alpha = variable)) +
    geom_bar(position = "dodge", stat = "identity") +
    theme_minimal(base_size = 20) +
    labs(y = "Weight in Tons") +
    theme(
      axis.title.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 50, hjust = 1)) +
    facet_wrap(~Material, ncol = 3) +
    scale_fill_viridis_d(direction = 1) +
    scale_alpha_discrete(range = c(0.5, 1)) +
    scale_y_continuous(labels = scales::comma,
                       limits = c(0, 15000)) 
}, height = 600, width = 1000)





# Download button ---------------------------------------------------------

datasetInput <- reactive({
  switch(input$dataset,
         "Wide form" = newimpacts(),
         "Long form" = meltedimpacts())
})

output$table <- renderTable({
  datsetInput()
})

output$downloadData <- downloadHandler(
  filename = function() {
    paste(input$dataset, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(datasetInput(), file, row.names = FALSE)
  }
)
  
output$impactdefinition <- renderPrint({
  
})

  
}

shinyApp(ui = ui, server = server)
