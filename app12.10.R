
library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(plotly)
library(shinyWidgets)
library(viridis)
library(shinyjs)


# m1 <- read_csv("sampleInteractiveIMFOinput.csv")
m1 <- read_csv("imfoAppMassProfiles.csv")
mass <- m1 %>%
  mutate(Weight = round(tons, digits = -2)) %>% 
  select(-"tons")
# I1 <- read_csv("testBuildImpactFactors_small.csv")
I1 <- read_csv("imfoAppImpactFactors.csv")
wastesheds <- sort(unique(mass$wasteshed))
materials <- sort(unique(mass$material))

options(shiny.reactlog = TRUE)


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  # theme = "bootstrap.css",
  chooseSliderSkin("Modern"),
  # Application title
  tags$head(
    tags$style("
      @import url('//fonts.googleapis.com/css?family=Muli|Cabin:400,700');
               ")
  ),
  # titlePanel(h1("Impacts of Material Flows", 
  #               style = "font-family: 'Muli';
  #               font-weight: 500; line-height: 3.1; 
  #               color: #4d3a7d;")),
  
  # Sidebar with a slider input for number of bins 
  navbarPage("Material Impact Visualizer",
    tabPanel("Introduction"),
    tabPanel("Visualize Impacts",
             sidebarLayout(
    sidebarPanel(
                
      selectInput(inputId = "selectedwasteshed",
                  label = "Select a wasteshed:",
                  choices = wastesheds
                  ),
      uiOutput("choose_materials"),
      tags$div(class = "header",
               tags$p(tags$b("Sliders are set to the 2015 weights for each material. Move sliders to generate a new scenario for this wasteshed.")),
               tags$p("The overall weight for each material can be more or less than the 2015 amount, which would reflect a change in production.")),
      
      #scrolling well panel
      wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",

# Sliders (UI) ------------------------------------------------------------

      conditionalPanel(
        condition = "input$usermaterials %in% 'Cardboard'",
        uiOutput("slider1L"),
        uiOutput("slider1R")
        ),
      conditionalPanel(
        condition = "input$usermaterials %in% 'Electronics'",
        # h4(textOutput("mat_text2")),
        uiOutput("slider2L"),
        uiOutput("slider2R")
        ),
      conditionalPanel(
        condition = "input$usermaterials %in% 'Food'",
        # h4(textOutput("mat_text3")),
        uiOutput("slider3L"),
        uiOutput("slider3C"),
        uiOutput("slider3Cmp")
        ),
      conditionalPanel(
        condition = "input$usermaterials %in% 'Misc. nonrecyclables'",
        # h4(textOutput("mat_text4"),
           uiOutput("slider4L")),
      conditionalPanel(
        condition = "input$usermaterials %in% 'Paper'",
        uiOutput("slider5L"),
        uiOutput("slider5R")),
      conditionalPanel(
        condition = "input$usermaterials %in% 'Film'",
        uiOutput("slider6L"),
        uiOutput("slider6R")),
      conditionalPanel(
        condition = "input$usermaterials %in% 'Plastic Other'",
        uiOutput("slider7L"),
        uiOutput("slider7C"),
        uiOutput("slider7R")),
      conditionalPanel(
        condition = "input$usermaterials %in% 'Scrap'",
        uiOutput("slider8L"),
        uiOutput("slider8R")),
      conditionalPanel(
        condition = "input$usermaterials %in% 'Wood'",
        uiOutput("slider9L"),
        uiOutput("slider9C"),
        uiOutput("slider9Cmp"),
        uiOutput("slider9R")),
      conditionalPanel(
        condition = "input$usermaterials %in% 'Yard'",
        uiOutput("slider10L"),
        uiOutput("slider10C"),
        uiOutput("slider10Cmp"))
      )
    ),

# Main Panel --------------------------------------------------------------

    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Disposition weights in separate plots",
                           tags$br(),
                           tags$div(class = "header",
                                    tags$p("Existing weights are shown in the light colors. Moving the sliders generates a new scenario, shown in the darker colors.")),
                                    plotOutput("weightsplot3")),
                  # tabPanel("Total weight",
                  #          tags$br(),
                  #          tags$div(class = "header",
                  #                   tags$p("Existing weights are shown in the light colors. Moving the sliders generates a new scenario, shown in the darker colors.")),
                  #          plotOutput("weightsplot2")),
                  tabPanel("Stacked by disposition",
                           tags$br(),
                           tags$div(class = "header",
                                    tags$p("Existing weights are shown in the light colors. Moving the sliders generates a new scenario, shown in the darker colors.")),
                           plotOutput("weightsplot4")),
                  tabPanel("Impacts", plotOutput("impactplot")),
                  # tabPanel("impact2", plotOutput("impactplot2")),
                  tabPanel("Tables", 
                           DT::dataTableOutput("table2"),
                           DT::dataTableOutput("table3"),
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
tabPanel("Glossary"),
navbarMenu("More",
           tabPanel("Resources"),
           tabPanel("About"))
)
)



# Server ------------------------------------------------------------------

server <- function(input, output, session) {

  
  values <- reactiveValues(starting = TRUE)
  session$onFlushed(function() {
    values$starting <- FALSE
  })
  
  
  userwasteshed <- reactive({
    # req(input$wasteshed)
    mass %>%
      filter(wasteshed == input$selectedwasteshed)
  })
  
  userwastemat <- reactive({
    # req(input$wasteshed)
    userwasteshed() %>%
      filter(material %in% input$usermaterials)
  })
  
  #widget that filters dataframe to display chosen materials
  output$choose_materials <- renderUI({
    selectizeInput(inputId = "usermaterials",
                   label = "Select up to six materials:",
                   choices = unique(userwasteshed()$material),
                   options = list(placeholder = "Select materials",
                                  maxItems = 6))
    # checkboxGroupInput(inputId = "usermaterials",
    #                    label = "Choose materials:",
    #                    choices  = unique(userwasteshed()$material),
    #                    selected = unique(userwasteshed()$material),
    #                    inline = TRUE)
  })

  # observe({print(input$usermaterials)
  # })
  

  # Sliders ----------------------------------------------------------------
  
  output$slider1L <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Cardboard/Kraft") %>% 
      pull(Weight)
    
    
    req(input$usermaterials == "Cardboard/Kraft")
    
    tagList(h4("Cardboard"),
            sliderInput(inputId = "slider1L",
                  label = "Landfilling",
                  min   = 0,
                  max   = sum(tweight)*1.2,
                  value = tweight[1],
                  step = 1000
      )
    )
  })
  
  
  output$slider1R <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Cardboard/Kraft") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Cardboard/Kraft")
    
    sliderInput(inputId = "slider1R",
                label = "Recycling",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[2]
    )
  })
  
  # output$mat_text2 <- renderText({ 
  #   input$usermaterials[2]
  # })
  
  output$slider2L <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Electronics") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Electronics")
    
    tagList(h4("Electronics"),
    sliderInput(inputId = "slider2L",
                label = "Landfilling",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[1]
    )
    )
  })
  
  output$slider2R <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Electronics") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Electronics")
    
    sliderInput(inputId = "slider2R",
                label = "Recycling",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[2]
    )
  })
  
  # output$mat_text3 <- renderText({ 
  #   input$usermaterials[3]
  # })
  
  output$slider3L <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "FoodWaste") %>% 
      pull(Weight)
    
    req(input$usermaterials == "FoodWaste")
    tagList(h4("Food Waste"),
            sliderInput(inputId = "slider3L",
                label = "Landfilling",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[1]
    )
    )
  })
  
  output$slider3C <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "FoodWaste") %>% 
      pull(Weight)
    
    req(input$usermaterials == "FoodWaste")
    sliderInput(inputId = "slider3C",
                label = "Combustion",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[2]
    )
  })
  
  output$slider3Cmp <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "FoodWaste") %>% 
      pull(Weight)
    
    req(input$usermaterials == "FoodWaste")
    sliderInput(inputId = "slider3Cmp",
                label = "Composting",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[3]
    )
  })
  
  # output$mat_text4 <- renderText({ 
  #   input$usermaterials[4]
  # })
  
  output$slider4L <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Misc. nonrecyclables") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Misc. nonrecyclables")
    tagList(h4("Miscellaneous Non-recyclables"),
            sliderInput(inputId = "slider4L",
                label = "Landfilling",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[1]
    )
    )
  })
  
  # output$mat_text5 <- renderText({ 
  #   input$usermaterials[5]
  # })
  
  output$slider5L <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Paper Fiber") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Paper Fiber")
    tagList(h4("Paper Fiber"),
            sliderInput(inputId = "slider5L",
                        label = "Landfilling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[1]
            )
    )
  })
  
  output$slider5R <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Paper Fiber") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Paper Fiber")
    sliderInput(inputId = "slider5R",
                        label = "Recycling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[2]
            )
  })
  # output$mat_text6 <- renderText({ 
  #   input$usermaterials[6]
  # })
  
  output$slider6L <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Plastic Film") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Plastic Film")
    tagList(h4("Plastic Film"),
            sliderInput(inputId = "slider6L",
                        label = "Landfilling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[1]
            )
    )
  })
  
  output$slider6R <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Plastic Film") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Plastic Film")
    sliderInput(inputId = "slider6R",
                        label = "Recycling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[2]
            )
  })
  # output$mat_text7 <- renderText({ 
  #   input$usermaterials[7]
  # })
  
  output$slider7L <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Plastic Other") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Plastic Other")
    tagList(h4("Plastic Other"),
            sliderInput(inputId = "slider7L",
                        label = "Landfilling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[1]
            )
    )
  })
  
  
  output$slider7C <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Plastic Other") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Plastic Other")
    sliderInput(inputId = "slider7C",
                        label = "Combustion",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[2]
            )
  })
  
  output$slider7R <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Plastic Other") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Plastic Other")
    sliderInput(inputId = "slider7R",
                        label = "Recycling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[3]
            )
  })
  # output$mat_text8 <- renderText({ 
  #   input$usermaterials[8]
  # })
  
  output$slider8L <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Scrap Metal - Other") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Scrap Metal - Other")
    tagList(h4("Scrap Metal (Other)"),
            sliderInput(inputId = "slider8L",
                label = "Landfilling",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[1]
    )
    )
  })
  
  output$slider8R <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Scrap Metal - Other") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Scrap Metal - Other")
    sliderInput(inputId = "slider8R",
                label = "Recycling",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[2]
    )
  })
  # output$mat_text9 <- renderText({ 
  #   input$usermaterials[9]
  # })
  output$slider9L <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Wood Waste") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Wood Waste")
    tagList(h4("Wood Waste"),
            sliderInput(inputId = "slider9L",
                        label = "Landfilling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[1]
            )
    )
  })
  
  output$slider9C <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Wood Waste") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Wood Waste")
    sliderInput(inputId = "slider9C",
                label = "Combustion",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[2]
    )
  })
  
  output$slider9Cmp <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Wood Waste") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Wood Waste")
    sliderInput(inputId = "slider9Cmp",
                label = "Composting",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[3]
    )
  })
  
  output$slider9R <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Wood Waste") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Wood Waste")
    sliderInput(inputId = "slider9R",
                label = "Recycling",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[4]
    )
  })
  
  
  # output$mat_text10 <- renderText({ 
  #   input$usermaterials[10]
  # })
  
  output$slider10L <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Yard Debris") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Yard Debris")
    tagList(h4("Yard Debris"),
            sliderInput(inputId = "slider10L",
                        label = "Landfilling",
                        min   = 0,
                        max   = sum(tweight)*1.2,
                        value = tweight[1]
            )
    )
  })
  
  output$slider10C <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Yard Debris") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Yard Debris")
    sliderInput(inputId = "slider10C",
                label = "Combustion",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[2]
    )
  })
  
  output$slider10Cmp <- renderUI({
    tweight <- userwastemat() %>% 
      filter(material %in% "Yard Debris") %>% 
      pull(Weight)
    
    req(input$usermaterials == "Yard Debris")
    sliderInput(inputId = "slider10Cmp",
                label = "Composting",
                min   = 0,
                max   = sum(tweight)*1.2,
                value = tweight[3]
    )
  })

# End Sliders -------------------------------------------------------------

  output$table1 <- DT::renderDataTable({
    if (is.null(input$selectedwasteshed))
      return()
    
    userwastemat()
  })
  
  # observe({print(userwastemat)
  # })
  
  newnew <- reactive({
    df <- mass %>%
      filter(wasteshed == input$selectedwasteshed) %>%
      mutate(`Your Weight` = Weight)
    df$`Your Weight` <- c(input$slider1L, input$slider1R, input$slider2L, input$slider2R, input$slider3L, input$slider3C, input$slider3Cmp, input$slider4L, input$slider5L, input$slider5R, input$slider6L, input$slider6R, input$slider7L, input$slider7C, input$slider7R, input$slider8L, input$slider8R, input$slider9L, input$slider9C, input$slider9Cmp, input$slider9R, input$slider10L, input$slider10C, input$slider10Cmp) 
      df <- df %>% filter(material %in% input$usermaterials)
  
    df
  })
  
  newimpacts <- reactive({
   n <- newnew() %>% 
      left_join(I1, by = c("material", "disposition")) %>% 
     select(-c(wasteshed, umbrellaDisp, LCstage)) %>% 
     mutate(old_impact = round(Weight*impactFactor),
            new_impact = round(`Your Weight`*impactFactor)
            )
   # print(n)
  })
  
  
  output$table3 <- DT::renderDataTable({
    newimpacts()
  })
  

  output$table2 <- DT::renderDataTable({
 newnew()
  })

  meltedusermass <- reactive({
    test <- newnew() %>% 
      select(-c(wasteshed, umbrellaDisp)) %>% 
      melt(id.vars = c('material', 'disposition'))
    # print(test)
  })
  
  meltedimpacts <- reactive({
    t <- newimpacts() %>% 
      select(-c(Weight, `Your Weight`, impactFactor)) %>% 
      melt(id.vars = c('material', 'disposition', 'impactCategory', 'impactUnits')) %>% 
      filter(!is.na(impactCategory))
    # print(t)
  })
  output$table4 <- DT::renderDataTable({
    meltedimpacts()
  })


# Impacts plot ------------------------------------------------------------
  output$impactplot <- renderPlot({

    pl <- ggplot(meltedimpacts(), aes(y = value, x = material, fill = material, alpha = variable)) +
      geom_bar(position = "dodge", stat = "identity") +
      theme_bw(base_size = 16) +
      facet_wrap(~impactCategory, ncol = 3, scales = "free_y"
                 ) +
      scale_fill_viridis_d(direction = -1) +
      scale_alpha_discrete(range = c(0.5, 1))
    pl + theme(axis.text.x = element_text(angle = 50, hjust = 1
                                          ))+
      geom_hline(mapping = NULL, data = NULL, size = 1, yintercept = 0,
                 na.rm = FALSE, show.legend = NA)
  }, height = 750, width = 1000)

  output$impactplot2 <- renderPlot({
    
    pl <- ggplot(meltedimpacts(), aes(y = value, x = impactCategory, fill = impactCategory, alpha = variable)) +
      geom_bar(position = "dodge", stat = "identity") +
      theme_bw(base_size = 16) +
      facet_wrap(~material, ncol = 3, scales = "free_y"
      ) +
      scale_fill_viridis_d(direction = -1, option = "A") +
      scale_alpha_discrete(range = c(0.5, 1))
    pl + theme(axis.text.x = element_text(angle = 50, hjust = 1
    )) +
      geom_hline(mapping = NULL, data = NULL, size = 1, yintercept = 0,
                 na.rm = FALSE, show.legend = NA)
  }, height = 750, width = 1200)
  
  
  

# Weights plot ------------------------------------------------------------

  
output$weightsplot2 <- renderPlot({
  if (values$starting)
    return(NULL)
  ggplot(meltedusermass(), aes(y = value, x = variable, fill = material, alpha = variable)) +
    geom_bar(stat = "identity") +
    theme_bw(base_size = 16) +
    theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
    facet_wrap(~material, nrow = 2) +
    scale_fill_viridis_d(direction = -1) +
    scale_alpha_discrete(range = c(0.5, 1)) +
    scale_y_continuous(labels = scales::comma)
})

output$weightsplot3 <- renderPlot({
  ggplot(meltedusermass(), aes(y = value, x = disposition, fill = material, alpha = variable)) +
    geom_bar(position = "dodge", stat = "identity") +
    theme_bw(base_size = 16) +
    theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
    facet_wrap(~material, ncol = 3) +
    scale_fill_viridis_d(direction = -1) +
    scale_alpha_discrete(range = c(0.5, 1)) +
    scale_y_continuous(labels = scales::comma)
}, height = 750, width = 1000)

output$weightsplot4 <- renderPlot({
  req(meltedusermass)
  ggplot(meltedusermass(), aes(y = value, x = variable, fill = disposition, alpha = variable)) +
    geom_bar(stat = "identity") +
    theme_bw(base_size = 16) +
    theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
    facet_wrap(~material, nrow = 2) +
    scale_fill_viridis_d(begin = 0.5, direction = 1) +
    scale_alpha_discrete(range = c(0.5, 1)) +
    scale_y_continuous(labels = scales::comma)
})



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
  
  
}

shinyApp(ui = ui, server = server)
