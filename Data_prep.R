# Anna Withington
# A script to clean and rename data for interactive IMFO before bringing it into
# the app environment

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
  mutate(Material = recode(material, "FoodWaste" = "Food",
                           "Cardboard/Kraft" = "Cardboard",
                           "Carpeting-used" = "Carpet",
                           "Glass Containers" = "Glass",
                           "Nonrecyclables" = "Trash",
                           "Paper Fiber" = "Paper",
                           "Rigid Plastic Cont." = "Rigid Plastic",
                           "Scrap Metal - Other" = "Scrap Metal",
                           "Wood Waste" = "Wood")) %>% 
  mutate(disposition = recode(disposition, "anaerobicDigestion" = "anaerobic digestion",
                              "useAsAggregate" = "use as aggregate")) %>% 
  mutate(`Life Cycle Stage` = ifelse(LCstage %in% "endOfLifeTransport", "EOL Transport", "EOL"),
         Disposition = tools::toTitleCase(disposition),
         `2015 Weight` = round(tons, digits = -2)) %>% 
  filter(`Life Cycle Stage` != "EOL Transport") %>%
  rename(Wasteshed = wasteshed) %>% 
  select(Wasteshed, Material, Disposition, `Life Cycle Stage`, `Umbrella Disposition`, `2015 Weight`)
write_csv(mass, "mass.csv")

I <- read_csv("imfoAppImpactFactors.csv")

I1 <- I %>% 
  mutate(Material = recode(material, "FoodWaste" = "Food",
                           "Cardboard/Kraft" = "Cardboard",
                           "Carpeting-used" = "Carpet",
                           "Glass Containers" = "Glass",
                           "Nonrecyclables" = "Trash",
                           "Paper Fiber" = "Paper",
                           "Rigid Plastic Cont." = "Rigid Plastic",
                           "Scrap Metal - Other" = "Scrap Metal",
                           "Wood Waste" = "Wood")) %>% 
  mutate(`Life Cycle Stage` = ifelse(LCstage %in% "endOfLifeTransport", "EOL Transport", 
                                     ifelse(LCstage %in% "endOfLife", "EOL",
                                            ifelse(LCstage %in% "production", "Production",
                                                   "other")))) %>%
  mutate(disposition = recode(disposition, "anaerobicDigestion" = "anaerobic digestion",
                              "useAsAggregate" = "use as aggregate")) %>%
  mutate(Disposition = tools::toTitleCase(disposition)) %>% 
  rename(Category = impactCategory,
         Units = impactUnits, Factor = impactFactor,
         `Implied Miles` = impliedMiles) %>% 
  mutate(Category = recode(Category, Eutrophication = "Excess nutrients")) %>% 
  select(Material, Disposition, `Life Cycle Stage`, Category,
         Units, Factor, `Implied Miles`) %>% 
  filter(`Life Cycle Stage` != "EOL Transport") %>% 
  select(-`Implied Miles`)

write_csv(I1, "I1.csv")
