
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
####
# See Below for data
####
 
# m1 <- read_csv("imfoAppMassProfiles.csv")
# mass <- m1 %>%
#   mutate(`Umbrella Disposition` = ifelse(disposition %in% "landfilling", "Disposal", "Recovery")) %>% 
#   mutate(Material = recode(material, "FoodWaste" = "Food Waste")) %>% 
#   mutate(`Life Cycle Stage` = ifelse(LCstage %in% "endOfLifeTransport", "EOL Transport", "EOL")) %>% 
#   filter(`Life Cycle Stage` != "EOL Transport") %>%
#   mutate(`2015 Weight` = round(tons, digits = -2)) %>% 
#   rename(Wasteshed = wasteshed, Disposition = disposition) %>% 
#   select(Wasteshed, Material, Disposition, `Life Cycle Stage`, `Umbrella Disposition`, `2015 Weight`)
# # I1 <- read_csv("testBuildImpactFactors_small.csv")
# I <- read_csv("imfoAppImpactFactors.csv")
# 
# I1 <- I %>% 
#   mutate(`Umbrella Disposition` = ifelse(disposition %in% "landfilling", "Disposal", "Recovery")) %>% 
#   mutate(Material = recode(material, "FoodWaste" = "Food Waste")) %>% 
#   mutate(`Life Cycle Stage` = ifelse(LCstage %in% "endOfLifeTransport", "EOL Transport", "EOL")) %>% 
#   rename(Disposition = disposition, `Impact Category` = impactCategory,
#          `Impact Units` = impactUnits, `Impact Factor` = impactFactor,
#          `Implied Miles` = impliedMiles) %>% 
#   select(Material, Disposition, `Life Cycle Stage`, `Umbrella Disposition`, `Impact Category`,
#          `Impact Units`, `Impact Factor`, `Implied Miles`)
wastesheds <- sort(unique(mass$wasteshed))
materials <- sort(unique(mass$material))




mm <- mass %>% 
  filter(LCstage == "endOfLife")

a <- mass %>% 
  select(-`Umbrella Disposition`) %>% 
  spread(`Life Cycle Stage`, `2015 Weight`)
ab <- mass %>% 
  select(-`Umbrella Disposition`) %>% 
  spread(Disposition, `2015 Weight`)

write.csv(ab, "data_example.csv")
ac <- spread(mass, Wasteshed, `2015 Weight`)


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


# Sample case shiny reactive dfs ------------------------------------------

newnew <- mass %>%
  filter(Wasteshed == "Benton") %>%
  filter(Material %in% "Cardboard/Kraft") %>%
  mutate(`New Weight` = `2015 Weight` * 0.75)

meltedusermass <- newnew %>% 
  select(-c(Wasteshed, `Umbrella Disposition`)) %>% 
  melt(id.vars = c('Material', 'Disposition'))

newimpacts <- newnew %>% 
  left_join(I1, by = c("Material", "Disposition",
                       "Life Cycle Stage")) %>% 
  mutate(`2015 Impact` = round(`2015 Weight`*`Impact Factor`),
         `New Impact` = round(`New Weight`*`Impact Factor`))

newimpactstest <- newnew %>% 
  left_join(I1, by = c("Material", "Disposition")) %>% 
  mutate(`2015 Impact` = round(`2015 Weight`*`Impact Factor`),
         `New Impact` = round(`New Weight`*`Impact Factor`))

meltedimpacts <- newimpacts %>% 
  select(Material, Disposition, `Impact Category`, old_impact, new_impact) %>%
  melt(id.vars = c('Material', 'Disposition', `Impact Category`))
meltedimpacts <- newimpacts %>% 
  select(Material, Disposition, `Impact Category`, old_impact, new_impact) %>%
  melt(id.vars = c('Material', 'Disposition'))

meltedimpacts <- newimpacts %>% 
  select(-c(`2015 Weight`, `New Weight`, `Impact Factor`)) %>% 
  gather(id.vars = c('Material', 'Disposition', `Impact Factor`, `Impact Units`)) %>% 
  filter(!is.na(`Impact Factor`))

mi <- newimpacts %>% 
  select(Material, Disposition, `Life Cycle Stage`, `Umbrella Disposition`, `Impact Category`, `Impact Units`, `2015 Impact`, `New Impact`) %>% 
  gather(key = "Scenario", value = "Impact", -c(Material, Disposition, `Life Cycle Stage`, `Umbrella Disposition`, `Impact Category`, `Impact Units`))



pl <- ggplot(mi,
             aes(y = Impact,
                 x = Material,
                 fill = Material,
                 alpha = Scenario)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw(base_size = 16) +
  facet_wrap(~`Impact Category`, ncol = 3, scales = "free_y"
  ) +
  scale_fill_viridis_d(direction = -1) +
  scale_alpha_discrete(range = c(0.5, 1))
pl + theme(axis.text.x = element_text(angle = 50, hjust = 1
)) +
  geom_hline(mapping = NULL, data = NULL, size = 1, yintercept = 0,
             na.rm = FALSE, show.legend = NA)

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


# 2/5/2019 ----------------------------------------------------------------
#Revising data processing

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

newnew <- mass %>%
  filter(Wasteshed == "Benton") %>%
  filter(Material %in% "Cardboard/Kraft") %>%
  mutate(`New Weight` = `2015 Weight` * 0.75)

meltedusermass <- newnew %>% 
  select(-c(Wasteshed, `Umbrella Disposition`)) %>% 
  melt(id.vars = c('Material', 'Disposition'))

newimpacts <- newnew %>% 
  left_join(I1, by = c("Material", "Disposition",
                       "Life Cycle Stage")) %>% 
  mutate(`2015 Impact` = round(`2015 Weight`*`Impact Factor`),
         `New Impact` = round(`New Weight`*`Impact Factor`))

newimpactstest <- newnew %>% 
  left_join(I1, by = c("Material", "Disposition")) %>% 
  mutate(`2015 Impact` = round(`2015 Weight`*`Impact Factor`),
         `New Impact` = round(`New Weight`*`Impact Factor`))

meltedimpacts <- newimpacts %>% 
  select(Material, Disposition, `Impact Category`, old_impact, new_impact) %>%
  melt(id.vars = c('Material', 'Disposition', `Impact Category`))
meltedimpacts <- newimpacts %>% 
  select(Material, Disposition, `Impact Category`, old_impact, new_impact) %>%
  melt(id.vars = c('Material', 'Disposition'))

meltedimpacts <- newimpacts %>% 
  select(-c(`2015 Weight`, `New Weight`, `Impact Factor`)) %>% 
  gather(id.vars = c('Material', 'Disposition', `Impact Factor`, `Impact Units`)) %>% 
  filter(!is.na(`Impact Factor`))


nT <- newnew %>% 
  select(-(`Life Cycle Stage`)) %>% 
  mutate(`Life Cycle Stage` = "EOL Transport")

nP <- newnew %>% 
  select(-`Life Cycle Stage`, Disposition) %>% 
  mutate(`Life Cycle Stage` = "Production",
         Disposition = "production")

nn <- newnew %>% 
  rbind(nT) %>% 
  rbind(nP)

n <- nn %>% 
  left_join(I1, by = c("Material", "Disposition",
                       "Life Cycle Stage")) %>% 
  mutate(`2015 Impact` = round(`2015 Weight`*`Impact Factor`),
         `New Impact` = round(`New Weight`*`Impact Factor`))

# %>% 
#   mutate(`2015 Impact` = round(`2015 Weight`*`Impact Factor`),
#          `New Impact` = round(`New Weight`*`Impact Factor`))
n


# 2/6/2019 ----------------------------------------------------------------

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

newnew_all <- mass %>%
  mutate(`New Weight` = `2015 Weight` * 0.75)

# meltedusermass <- newnew_all %>% 
#   select(-c(Wasteshed, `Umbrella Disposition`)) %>% 
#   melt(id.vars = c('Material', 'Disposition'))

nT <- newnew_all %>% 
  mutate(`Life Cycle Stage` = "EOL Transport")

nP <- newnew_all %>% 
  mutate(`Life Cycle Stage` = "Production",
         Disposition = "production")

nn <- newnew_all %>% 
  rbind(nT) %>% 
  rbind(nP)

n <- nn %>% 
  left_join(I1, by = c("Material", "Disposition",
                       "Life Cycle Stage")) %>% 
  mutate(`2015 Impact` = round(`2015 Weight`*`Impact Factor`),
         `New Impact` = round(`New Weight`*`Impact Factor`))

###########
# newimpacts <- newnew_all %>% 
#   left_join(I1, by = c("Material", "Disposition",
#                        "Life Cycle Stage")) %>% 
#   mutate(`2015 Impact` = round(`2015 Weight`*`Impact Factor`),
#          `New Impact` = round(`New Weight`*`Impact Factor`))

newimpactstest <- newnew %>% 
  left_join(I1, by = c("Material", "Disposition")) %>% 
  mutate(`2015 Impact` = round(`2015 Weight`*`Impact Factor`),
         `New Impact` = round(`New Weight`*`Impact Factor`))

meltedimpacts <- newimpacts %>% 
  select(Material, Disposition, `Impact Category`, old_impact, new_impact) %>%
  melt(id.vars = c('Material', 'Disposition', `Impact Category`))
meltedimpacts <- newimpacts %>% 
  select(Material, Disposition, `Impact Category`, old_impact, new_impact) %>%
  melt(id.vars = c('Material', 'Disposition'))

meltedimpacts <- newimpacts %>% 
  select(-c(`2015 Weight`, `New Weight`, `Impact Factor`)) %>% 
  gather(id.vars = c('Material', 'Disposition', `Impact Factor`, `Impact Units`)) %>% 
  filter(!is.na(`Impact Factor`))


nT <- newnew %>% 
  select(-(`Life Cycle Stage`)) %>% 
  mutate(`Life Cycle Stage` = "EOL Transport")

nP <- newnew %>% 
  select(-`Life Cycle Stage`, Disposition) %>% 
  mutate(`Life Cycle Stage` = "Production",
         Disposition = "production")




nn <- newnew %>% 
  rbind(nT) %>% 
  rbind(nP)

n <- nn %>% 
  left_join(I1, by = c("Material", "Disposition",
                       "Life Cycle Stage")) %>% 
  mutate(`2015 Impact` = round(`2015 Weight`*`Impact Factor`),
         `New Impact` = round(`New Weight`*`Impact Factor`))

# %>% 
#   mutate(`2015 Impact` = round(`2015 Weight`*`Impact Factor`),
#          `New Impact` = round(`New Weight`*`Impact Factor`))
n


t <- n %>% 
  select(Material, Disposition, `Life Cycle Stage`, `Umbrella Disposition`, `Impact Category`, `Impact Units`, `2015 Impact`, `New Impact`) %>% 
  gather(key = "Scenario", value = "Impact", -c(Material, Disposition, `Life Cycle Stage`, `Umbrella Disposition`, `Impact Category`, `Impact Units`))



# Revised data 2/14/2019 --------------------------------------------------

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


newnew_all <- mass %>%
  filter(Wasteshed %in% "Baker") %>% 
  filter(Material %in% "Cardboard/Kraft") %>% 
  mutate(`New Weight` = `2015 Weight` * 0.75)

  nT <- newnew_all %>% 
    mutate(`Life Cycle Stage` = "EOL Transport")
  
  nP <- newnew_all %>% 
    mutate(`Life Cycle Stage` = "Production",
           Disposition = "production")
  
  nn <- newnew_all %>% 
    rbind(nT) %>% 
    rbind(nP) %>% 
    left_join(I1, by = c("Material", "Disposition",
                         "Life Cycle Stage")) %>% 
    mutate(`2015 Impact` = round(`2015 Weight`*`Impact Factor`),
           `New Impact` = round(`New Weight`*`Impact Factor`))

sn <- nn %>%
  filter(`Impact Category` %in% "Eutrophication")
  
simplesubset <- I1 %>%
  filter(Material %in% "Rigid Plastic Cont.") %>% 
  filter(`Life Cycle Stage` != "EOL Transport") %>% 
  filter(`Impact Category` %in% "Global warming")

f <- mass %>%
  filter(Wasteshed %in% "Baker") %>% 
  filter(Material %in% "Rigid Plastic Cont.") %>% 
  mutate(`New Weight` = `2015 Weight` * 0.75)

f[4,] <- c("Baker", "Rigid Plastic Cont.", "production", "Production", "P", 300, 225)

df <- f %>% 
  left_join(simplesubset) %>% 
  select(Disposition, `2015 Weight`, `New Weight`, `Impact Category`, `Impact Factor`) %>% 
  mutate(`Initial Impact` = as.numeric(`2015 Weight`)*`Impact Factor`,
         `Scenario Impact` = as.numeric(`New Weight`)*`Impact Factor`) %>%
  select(Disposition, `Initial Impact`, `Scenario Impact`) %>%
  gather(key = "Scenario", value = "Impact", c(`Initial Impact`, `Scenario Impact`)) %>%
  spread("Disposition", "Impact") %>%
  mutate(Sum = rowSums(.[2:4]))


# for circle packing ------------------------------------------------------
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)


df11 <- df %>% 
  filter(Scenario == "Scenario Impact") %>% 
  select(-Scenario) %>% 
  gather(key = "disposition", value = "value") %>%
  filter(value != 0) %>% 
  mutate(neg = ifelse(value < 0, "neg", (ifelse(disposition == "Sum", "sum", "pos")))) %>% 
  mutate(value = abs(value)) %>% 
  mutate(text = paste("name: ",df11$disposition, "\n", "value:", round(df11$value), "\n", "!"))


packing1 <- circleProgressiveLayout(df11$value, sizetype = 'area')
df11 = cbind(df11, packing1)
dat.gg11 <- circleLayoutVertices(packing1, npoints = 50)

q = ggplot() + 
  geom_polygon_interactive(data = dat.gg11, 
                           aes(x,
                               y, 
                               group = id,
                               fill= df11$neg[id],
                               tooltip = df11$text[id],
                               data_id = id),
                           colour = "black",
                           alpha = 0.5) +
  scale_fill_viridis_d() +
  geom_text(data = df11, aes(x, y, label = gsub("Group_", "", disposition)), size=2, color="black") +
  theme_void() + 
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  coord_equal()
q


