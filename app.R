
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
  mutate(`Umbrella Disposition` = ifelse(disposition %in% "landfilling", "Disposal", "Recovery")) %>% 
  mutate(Material = recode(material, "FoodWaste" = "Food Waste")) %>% 
  mutate(`Life Cycle Stage` = ifelse(LCstage %in% "endOfLifeTransport", "EOL Transport", "EOL")) %>% 
  filter(`Life Cycle Stage` != "EOL Transport") %>%
  mutate(`2015 Weight` = round(tons, digits = -2)) %>% 
  rename(Wasteshed = wasteshed, Disposition = disposition) %>% 
  select(Wasteshed, Material, Disposition, `Life Cycle Stage`, `Umbrella Disposition`, `2015 Weight`)
# I1 <- read_csv("testBuildImpactFactors_small.csv")
I <- read_csv("imfoAppImpactFactors.csv")

I1 <- I %>% 
  mutate(`Umbrella Disposition` = ifelse(disposition %in% "landfilling", "Disposal", "Recovery")) %>% 
  mutate(Material = recode(material, "FoodWaste" = "Food Waste")) %>% 
  mutate(`Life Cycle Stage` = ifelse(LCstage %in% "endOfLifeTransport", "EOL Transport", "EOL")) %>% 
  rename(Disposition = disposition, `Impact Category` = impactCategory,
         `Impact Units` = impactUnits, `Impact Factor` = impactFactor,
         `Implied Miles` = impliedMiles) %>% 
  select(Material, Disposition, `Life Cycle Stage`, `Umbrella Disposition`, `Impact Category`,
         `Impact Units`, `Impact Factor`, `Implied Miles`)

Wastesheds <- sort(unique(mass$Wasteshed))
Materials <- sort(unique(mass$Material))
Dispositions <- sort(unique(mass$Disposition))

options(shiny.reactlog = TRUE)


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  # theme = "bootstrap.css",
  chooseSliderSkin("Modern"),
  
  navbarPage("Material Impact Visualizer",

# Introduction tab -------------------------------------------------------
    tabPanel("Introduction"),

# User Input Tab ---------------------------------------------------------------

    tabPanel("Visualize Impacts",
             sidebarLayout(
               sidebarPanel(    
                 selectInput(inputId = "selectedwasteshed",
                             label = "Select a wasteshed:",
                             choices = Wastesheds),
                 uiOutput("choose_materials"),
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
                  tabPanel("Stacked by disposition",
                           tags$br(),
                           tags$div(class = "header",
                                    tags$p("Existing weights are shown in the light colors. Moving the sliders generates a new scenario, shown in the darker colors.")),
                           plotOutput("weightsplot4")),
                  tabPanel("Impacts", plotOutput("impactplot")),
                  # tabPanel("impact2", plotOutput("impactplot2")),

# Tables tab --------------------------------------------------------------

                                   tabPanel("Tables", 
                           DT::dataTableOutput("table1"),
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

# Glossary tab ------------------------------------------------------------


tabPanel("Glossary",
         navlistPanel(
           widths = c(2, 6),
           tabPanel("Materials"),
           tabPanel("Dispositions"),
           tabPanel("Impacts")
         )),
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
  
  
  # userwasteshed <- reactive({
  #   # req(input$wasteshed)
  #   mass %>%
  #     filter(Wasteshed == input$selectedwasteshed)
  # })
  
  #widget that filters dataframe to display chosen materials
  output$choose_materials <- renderUI({
    um <- mass %>%
      filter(Wasteshed == input$selectedwasteshed)
    
    selectizeInput(inputId = "usermaterials",
                   label = "Select up to six materials:",
                   choices = Materials,
                   options = list(placeholder = "Select materials",
                                  maxItems = 6))
    # checkboxGroupInput(inputId = "usermaterials",
    #                    label = "Choose materials:",
    #                    choices  = unique(userwasteshed()$material),
    #                    selected = unique(userwasteshed()$material),
    #                    inline = TRUE)
  })
  
  userwastemat <- reactive({
    # req(input$wasteshed)
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
  
  # observe({print(userwastemat)
  # })
  
  newnew <- reactive({
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
      # df <- df %>% filter(Material %in% input$usermaterials)
      # 
    df
  })
  
  output$table2 <- DT::renderDataTable({
    newnew()
  })
  
  newimpacts <- reactive({
   n <- newnew() %>% 
     left_join(I1, by = c("Material", "Disposition",
                          "Life Cycle Stage", "Umbrella Disposition")) %>% 
     mutate(old_impact = round(`2015 Weight`*`Impact Factor`),
            new_impact = round(`New Weight`*`Impact Factor`))
     n
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
  
  meltedimpacts <- reactive({
    t <- newimpacts() %>% 
      select(-c(`2015 Weight`, `New Weight`, `Impact Factor`)) %>% 
      melt(id.vars = c('Material', 'Disposition', `Impact Factor`, `Impact Units`)) %>% 
      filter(!is.na(`Impact Factor`))
    t
  })
  output$table4 <- DT::renderDataTable({
    meltedimpacts()
  })


# Impacts plot ------------------------------------------------------------
  output$impactplot <- renderPlot({

    pl <- ggplot(meltedimpacts(),
                 aes(y = value,
                     x = Material,
                     fill = Material,
                     alpha = variable)) +
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

  
output$weightsplot2 <- renderPlot({
  if (values$starting)
    return(NULL)
  ggplot(meltedusermass(),
         aes(y = value,
             x = variable,
             fill = Material,
             alpha = variable)) +
    geom_bar(stat = "identity") +
    theme_bw(base_size = 16) +
    theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
    facet_wrap(~Material, nrow = 2) +
    scale_fill_viridis_d(direction = -1) +
    scale_alpha_discrete(range = c(0.5, 1)) +
    scale_y_continuous(labels = scales::comma)
})

output$weightsplot3 <- renderPlot({
  ggplot(meltedusermass(), aes(y = value, x = Disposition, fill = Material, alpha = variable)) +
    geom_bar(position = "dodge", stat = "identity") +
    theme_bw(base_size = 16) +
    theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
    facet_wrap(~Material, ncol = 3) +
    scale_fill_viridis_d(direction = -1) +
    scale_alpha_discrete(range = c(0.5, 1)) +
    scale_y_continuous(labels = scales::comma)
}, height = 750, width = 1000)

output$weightsplot4 <- renderPlot({
  req(meltedusermass)
  ggplot(meltedusermass(), aes(y = value, x = variable, fill = Disposition, alpha = variable)) +
    geom_bar(stat = "identity") +
    theme_bw(base_size = 16) +
    theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
    facet_wrap(~Material, nrow = 2) +
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
