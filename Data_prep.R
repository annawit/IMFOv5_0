
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)


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
