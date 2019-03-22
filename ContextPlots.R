library(dplyr)
library(plotly)
library(tidyr)

tr <- readRDS("impacts_by_LC_stage_data.RData")

regionalcontextb <- tr %>% 
  filter(wasteshed == "Baker")



p <- plot_ly(data = regionalcontextb %>% filter(umbDisp == "Net"),
       x = ~appMaterial,
       y = ~impact,
       type = "bar") %>%
  add_trace(y = ~tons, yaxis = "y2") %>%
  layout(barmode = "group",
         yaxis2 = list(overlaying = "y",
                       side = "right"))
p

rcn <- regionalcontextb %>%
  filter(umbDisp == "Net")
p <- plot_ly(data = rcn,
             x = ~appMaterial,
             y = ~impact,
             type = "bar") %>%
  add_trace(y = ~tons, yaxis = "y2") %>%
  layout(barmode = "group",
         yaxis2 = list(overlaying = "y",
                       side = "right"))
p




p <- plot_ly(data = regionalcontextb %>% filter(umbDisp == "Net"),
       x = ~appMaterial,
       y = ~impact,
       type = "bar") %>%
  add_trace(y = ~tons) %>%
  layout(barmode = "group")
p



#look at data structure for stacked bar
# Animals <- c("giraffes", "orangutans", "monkeys")
# SF_Zoo <- c(20, 14, 23)
# LA_Zoo <- c(12, 18, 29)
# data <- data.frame(Animals, SF_Zoo, LA_Zoo)

#create stacked bar

regionalcontextbimpact <- regionalcontextb %>% 
  filter(umbDisp != "Net") %>% 
  select(wasteshed, appMaterial, umbDisp, impact, tons) %>% 
  spread(key = umbDisp, value = impact)


q <- plot_ly(data = regionalcontextbimpact,
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

# change x and y axes to create horizontal stacked bar
pp <- plot_ly(data = regionalcontextbimpact,
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

#tons, divided by disposal and recovery
regionalcontextbtons <- regionalcontextb %>% 
  filter(umbDisp != "Net") %>%
  select(wasteshed, appMaterial, umbDisp, impact, tons) %>% 
  filter(umbDisp != "Production") %>% 
  spread(key = umbDisp, value = tons)

tt <- plot_ly(data = regionalcontextbtons,
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
regionalcontextbnet <- regionalcontextb %>% 
  filter(umbDisp == "Net") %>% 
  select(wasteshed, appMaterial, umbDisp, impact, tons)


tons <- plot_ly(data = regionalcontextbtons,
              y = ~appMaterial,
              x = ~tons,
              name = "Waste, in tons",
              type = "bar") %>%
  layout(barmode = "relative")

impact <- plot_ly(data = regionalcontextbnet,
              y = ~appMaterial,
              x = ~impact,
              name = "Net Global Warming Impact",
              type = "bar") %>%
  layout(barmode = "relative")

sub2 <- subplot(tons, impact)
sub2

scatter <- plot_ly(data = regionalcontextbnet,
        x = ~tons,
        y = ~impact,
        text = ~appMaterial,
        type = "scatter")
scatter
