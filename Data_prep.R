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
  mutate(Material = recode(material, "FoodWaste" = "Food Waste")) %>% 
  mutate(`Life Cycle Stage` = ifelse(LCstage %in% "endOfLifeTransport", "EOL Transport", "EOL")) %>% 
  filter(`Life Cycle Stage` != "EOL Transport") %>%
  mutate(`2015 Weight` = round(tons, digits = -2)) %>% 
  rename(Wasteshed = wasteshed, Disposition = disposition) %>% 
  select(Wasteshed, Material, Disposition, `Life Cycle Stage`, `Umbrella Disposition`, `2015 Weight`)
write_csv(mass, "mass.csv")

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

write_csv(I1, "I1.csv")