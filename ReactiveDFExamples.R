mass <- read_csv("mass.csv")

I1 <- read_csv("I1.csv")

d <- read_csv("impact_dictionary.csv")

usermass <- mass %>% 
  filter(Wasteshed == "Oregon total") %>% 
  filter(Material == "Cardboard")

tweight <- usermass %>% 
    pull(`2015 Weight`)

tdisp <- usermass %>% 
    pull(Disposition)

userimpact <-   I1 %>% 
    filter(Category == "Global warming") %>% 
    filter(Material == "Cardboard")

# cbmassdf <- reactive({
totalp <- sum(tweight)

Disposition <- c(tdisp[1], tdisp[2], tdisp[3], "Production")

`Initial Weight` <- c(tweight[1], tweight[2], tweight[3], sum(tweight))

`Scenario Weight` <- c(totalp*.1,
                       totalp*.6,
                       totalp*.3,
                       totalp)

cbmassdf <- tibble(Disposition, `Initial Weight`, `Scenario Weight`) %>% 
  left_join(userimpact) %>% 
              mutate(`Initial Impact` = `Initial Weight`*Factor,
                     `New Scenario Impact` = `Scenario Weight`*Factor) %>% 
              select(Disposition, Units, `Initial Weight`, `Scenario Weight`, `Initial Impact`, `New Scenario Impact`)
  # %>% 
  #     gather(key = `Scenario`, value = "Weight", c(`Initial Weight`, `Scenario Weight`))

cardboarddf <- cbmassdf %>% 
  gather(key = "Variable", value = "Value", c(`Initial Weight`, `Scenario Weight`, `Initial Impact`, `New Scenario Impact`)) 
ul# %>%
  # filter(grepl("Weight", Variable)) %>% 
  #   spread("Disposition", "Value")


massplot <- plot_ly(cardboarddf %>% filter(grepl("Weight", Variable)) %>% spread("Disposition", "Value"),
                    # %>% select(-`Initial Impact`, -`New Scenario Impact`)
               # %>% spread("Disposition", "Weight"),
               x = ~Variable,
               y = ~Combustion,
               name = "Combustion weight",
               marker = list(color = ("#A7B753")),
               type = "bar") %>% 
    add_trace(y = ~Landfilling,
              name = "Landfilling weight",
              marker = list(color = ("#492F42"))) %>% 
    add_trace(y = ~Recycling,
              name = "Recycling weight",
              marker = list(color = ("#389476"))) %>% 
    layout(barmode = "relative")
  
massplot %>% 
    layout(yaxis = list(overlaying = "y",
                        title = "Weight in tons"),
           xaxis = list(title = ""),
           legend = list(orientation = 'h')
    )


Animals <- c("giraffes", "orangutans", "monkeys")
SF_Zoo <- c(20, 14, 23)
LA_Zoo <- c(12, 18, 29)
data <- data.frame(Animals, SF_Zoo, LA_Zoo)

p <- plot_ly(data, x = ~Animals, y = ~SF_Zoo, type = 'bar', name = 'SF Zoo') %>%
  add_trace(y = ~LA_Zoo, name = 'LA Zoo') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
p


cardboarddf %>% filter(grepl("Impact", Variable)) %>% spread("Disposition", "Value") %>% mutate(Sum = rowSums(.[2:4]))

