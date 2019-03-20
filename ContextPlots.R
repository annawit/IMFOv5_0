library(dplyr)
library(plotly)
library(tidyr)

tr <- readRDS("impacts_by_LC_stage_data.RData")

Bakertr <- tr %>% 
  filter(wasteshed == "Baker")

p <- plot_ly(data = Bakertr,
       x = ~appMaterial,
       y = ~impact,
       type = "bar") %>% 
  add_trace(y = ~tons, yaxis = "y2") %>% 
  layout(barmode = "group",
         yaxis2 = list(overlaying = "y",
                       side = "right"))
p

Animals <- c("giraffes", "orangutans", "monkeys")
SF_Zoo <- c(20, 14, 23)
LA_Zoo <- c(12, 18, 29)
data <- data.frame(Animals, SF_Zoo, LA_Zoo)

#create stacked bar

Bakertrimpact <- Bakertr %>% 
  filter(umbDisp != "Net") %>% 
  select(wasteshed, appMaterial, umbDisp, impact, tons) %>% 
  spread(key = umbDisp, value = impact)


q <- plot_ly(data = Bakertrimpact,
             x = ~appMaterial,
             y = ~Disposal,
             name = "Disposal",
             type = "bar") %>%
  add_trace(y = ~Production,
            name = "Production") %>% 
  add_trace(y = ~Recovery,
            name = "Recovery") %>% 
  layout(barmode = "relative")
         
q

pp <- plot_ly(data = Bakertrimpact,
             y = ~appMaterial,
             x = ~Disposal,
             name = "Disposal",
             type = "bar") %>%
  add_trace(x = ~Production,
            name = "Production") %>% 
  add_trace(x = ~Recovery,
            name = "Recovery") %>% 
  layout(barmode = "relative")

pp

Bakertrtons <- Bakertr %>% 
  filter(umbDisp != "Net") %>%
  select(wasteshed, appMaterial, umbDisp, impact, tons) %>% 
  filter(umbDisp != "Production") %>% 
  spread(key = umbDisp, value = tons)

tt <- plot_ly(data = Bakertrtons,
              y = ~appMaterial,
              x = ~Disposal,
              name = "Disposal",
              type = "bar") %>%
  add_trace(x = ~Recovery,
            name = "Recovery") %>% 
  layout(barmode = "relative")

tt

sub <- subplot(pp, tt, shareY = TRUE)
sub

# Net impact
Bakertrnet <- Bakertr %>% 
  filter(umbDisp == "Net") %>% 
  select(wasteshed, appMaterial, umbDisp, impact, tons)

bb <- plot_ly(data = Bakertrnet,
              y = ~appMaterial,
              x = ~impact,
              name = "Net Impact",
              type = "bar") %>%
  layout(barmode = "relative")
bb

sub2 <- subplot(tt, bb)
sub2

scatter <- plot_ly(data = Bakertrnet,
        x = ~tons,
        y = ~impact,
        text = ~appMaterial,
        type = "scatter")
scatter
