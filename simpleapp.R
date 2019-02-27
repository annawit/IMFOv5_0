
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
             tabPanel("Visualize!"),
             navbarMenu("More",
                        tabPanel("Glossary"),
                        tabPanel("Resources"),
                        tabPanel("About")
             )
  )
)

server <- function(input, output, session) {
}

shinyApp(ui = ui, server = server)