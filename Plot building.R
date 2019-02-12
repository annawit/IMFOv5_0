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

# newnew <- reactive({
#   df <- mass %>%
#     
#     filter(Wasteshed == input$selectedwasteshed) %>%
#     filter(Material %in% input$usermaterials) %>%
#     mutate(`New Weight` = `2015 Weight`)
#   df$`New Weight` <- c(input$slider1cb, input$slider1cp,
#                        input$slider1L, input$slider1R, 
#                        input$slider2L, input$slider2R, 
#                        input$slider3AD, input$slider3cb,
#                        input$slider3cp, input$slider3L, 
#                        input$slider4L, input$slider4R, input$slider4U,
#                        input$slider5L, 
#                        input$slider6cb, input$slider6L, input$slider6R, 
#                        input$slider7cb, input$slider7L, input$slider7R, 
#                        input$slider8L, input$slider8R, 
#                        input$slider9cb, input$slider9cp,
#                        input$slider9L, input$slider9R, 
#                        input$slider10AD, input$slider10cb,
#                        input$slider10cp, input$slider10L) 
#   df
# })

newnew <- mass %>%
  filter(Wasteshed == "Benton") %>%
  filter(Material %in% "Cardboard/Kraft") %>%
  mutate(`New Weight` = `2015 Weight` * 0.75)


meltedusermass <- newnew %>% 
    select(-c(Wasteshed, `Umbrella Disposition`, `Life Cycle Stage`)) %>% 
    melt(id.vars = c('Material', 'Disposition'))

allalong <- mass %>%
  mutate(`New Weight` = `2015 Weight` * 0.75) %>% 
  select(-c(Wasteshed, `Umbrella Disposition`, `Life Cycle Stage`)) %>% 
  melt(id.vars = c('Material', 'Disposition'))

write.csv(allalong, "all.csv")


# meltedusermass <- reactive({
#   test <- newnew() %>% 
#     select(-c(Wasteshed, `Umbrella Disposition`, `Life Cycle Stage`)) %>% 
#     melt(id.vars = c('Material', 'Disposition'))
#   # print(test)
# })


ggplot(meltedusermass, aes(y = value, x = Disposition, fill = Material, alpha = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal(base_size = 20) +
  labs(y = "Weight, in Tons") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 50, hjust = 1)) +
  facet_wrap(~Material, ncol = 3) +
  scale_fill_viridis_d(direction = 1) +
  scale_alpha_discrete(range = c(0.5, 1)) +
  scale_y_continuous(labels = scales::comma)

  # ggplot(meltedusermass, aes(y = value, x = Disposition, fill = Material, alpha = variable)) +
  #   geom_bar(position = "dodge", stat = "identity") +
  #   theme_minimal(base_size = 20) +
  #   labs(y = "Weight, in Tons") +
  #   theme(
  #     # axis.title = element_text(size = 12),
  #     # axis.text = element_text(size = 10),
  #     panel.grid.minor = element_blank(),
  #     axis.text.x = element_text(angle = 50, hjust = 1)) +
  #   facet_wrap(~Material, ncol = 3) +
  #   scale_fill_viridis_d(direction = 1) +
  #   scale_alpha_discrete(range = c(0.5, 1)) +
  #   scale_y_continuous(labels = scales::comma)


# output$weightsplot3 <- renderPlot({
#   ggplot(meltedusermass(), aes(y = value, x = Disposition, fill = Material, alpha = variable)) +
#     geom_bar(position = "dodge", stat = "identity") +
#     theme_bw(base_size = 16) +
#     theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
#     facet_wrap(~Material, ncol = 3) +
#     scale_fill_viridis_d(direction = -1) +
#     scale_alpha_discrete(range = c(0.5, 1)) +
#     scale_y_continuous(labels = scales::comma)
# }, height = 750, width = 1000)
  
    # ggplot(meltedusermass,
    #        aes(y = value,
    #            x = variable,
    #            fill = Material,
    #            alpha = variable)) +
    #   geom_bar(stat = "identity") +
    #   theme_bw(base_size = 16) +
    #   theme(axis.title = element_text(size = 12),
    #         axis.text = element_text(size = 10),
    #         panel.grid.minor = element_blank(),
    #         axis.text.x = element_text(angle = 50, hjust = 1)) +
    #   facet_wrap(~Material, nrow = 2) +
    #   scale_fill_viridis_d(direction = -1) +
    #   scale_alpha_discrete(range = c(0.5, 1)) +
    #   scale_y_continuous(labels = scales::comma)
    # 
  
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
    

      ggplot(meltedusermass,
             aes(y = value,
                 x = variable,
                 fill = Disposition,
                 alpha = variable)) +
        geom_bar(stat = "identity") +
        theme_minimal(base_size = 18) +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(
                margin = margin(t = 0, r = 20, b = 0, l = 0),
                size = 16,
                vjust = -0.65),
              axis.text = element_text(size = 16),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              axis.text.x = element_text(angle = 50, hjust = 1)) +
        labs(y = "Weight, in Tons",
             alpha = "") +
        facet_wrap(~Material, nrow = 2) +
        scale_fill_viridis_d(direction = 1, end = 0.85) +
        scale_alpha_discrete(range = c(0.3, 1)) +
        scale_y_continuous(labels = scales::comma,
                           limits = c(0, 40000))
    
    # output$weightsplot4 <- renderPlot({
    #   req(meltedusermass)
    #   ggplot(meltedusermass(),
    #          aes(y = value,
    #              x = variable,
    #              fill = Disposition,
    #              alpha = variable)) +
    #     geom_bar(stat = "identity") +
    #     theme_bw(base_size = 16) +
    #     theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
    #     facet_wrap(~Material, nrow = 2) +
    #     scale_fill_viridis_d(begin = 0.3, direction = 1) +
    #     scale_alpha_discrete(range = c(0.4, 1)) +
    #     scale_y_continuous(labels = scales::comma)
    # }, height = 750, width = 1000)