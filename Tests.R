
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
wastesheds <- sort(unique(mass$wasteshed))
materials <- sort(unique(mass$material))


mm <- mass %>% 
  filter(LCstage == "endOfLife")

a <- spread(mass, LCstage, Weight)


ab <- spread(mass, disposition, Weight)
ac <- spread(mass, wasteshed, Weight)


# newimpacts <- reactive({
#   n <- newnew() %>% 
#     left_join(I1, by = c("Material", "Disposition")) %>% 
#     select(-c(Wasteshed, `Umbrella Disposition`, LCstage)) %>% 
#     mutate(old_impact = round(Weight*impactFactor),
#            new_impact = round(`Your Weight`*impactFactor)
#     )
# })
#test of join above
n <- mass %>% 
  filter(Wasteshed == "Benton") %>% 
  left_join(I1, by = c("Material", "Disposition", "Life Cycle Stage", "Umbrella Disposition"))

n1 <- n %>% 
  mutate(old_impact = round(Weight*`Impact Factor`))


newnew <- mass %>%
  filter(Wasteshed == "Benton") %>%
  filter(Material %in% "Cardboard/Kraft") %>%
  mutate(`New Weight` = `2015 Weight` * 0.75)

meltedusermass <- newnew %>% 
  select(-c(Wasteshed, `Umbrella Disposition`)) %>% 
  melt(id.vars = c('Material', 'Disposition'))

newimpacts <- newnew %>% 
  left_join(I1, by = c("Material", "Disposition",
                       "Life Cycle Stage", "Umbrella Disposition")) %>% 
  mutate(old_impact = round(`2015 Weight`*`Impact Factor`),
         new_impact = round(`New Weight`*`Impact Factor`))

meltedimpacts <- newimpacts %>% 
  select(Material, Disposition, `Impact Category`, old_impact, new_impact) %>%
  melt(id.vars = c('Material', 'Disposition', `Impact Category`))


meltedimpacts <- newimpacts %>% 
  select(-c(`2015 Weight`, `New Weight`, `Impact Factor`)) %>% 
  gather(id.vars = c('Material', 'Disposition', `Impact Factor`, `Impact Units`)) %>% 
  filter(!is.na(`Impact Factor`))


# https://stackoverflow.com/questions/16449252/tooltip-on-shiny-r
# library(shinyBS) # Additional Bootstrap Controls
# 
# ## From ui.R: Adds a tooltip to element with inputId = "someInput" 
# ## with text, "This is an input.", that appears to the left on hover.
# bsTooltip(id = "someInput", title = "This is an input", 
#           placement = "left", trigger = "hover")
# 
# ## From server.R: Add the same tooltip as above
# addTooltip(session, id = "someInput", title = "This is an input.",
#            placement = "left", trigger = "hover")