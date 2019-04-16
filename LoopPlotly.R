library(plotly)

mass %>% 
  left_join(impact) %>%
  filter(Material == "Glass") %>% 
  filter(Category == "Energy demand")


df2 <- impact %>% 
  filter(Category == "Energy demand")

dd <- df2 %>% 
  select(-"Life Cycle Stage") %>% 
  spread("Disposition", "Factor", fill = 0)


iplot <- plot_ly(dd,
                x = ~Material,
                type = 'bar') %>%
  add_trace(y = ~Landfilling) %>% 
  add_trace(y = ~Combustion) %>% 
  add_trace(y = ~Recycling) %>% 
  add_trace(y = ~Production) %>% 
  add_trace(y = ~`Anaerobic Digestion`) %>% 
  add_trace(y = ~Composting) %>% 
  add_trace(y = ~`Use as Aggregate`) %>% 
  layout(barmode = 'group')
iplot

unique(impact$Disposition)

#First create the chart
chart <- plot_ly(type = ‘bar’,
                 hoverinfo = ‘x+y’)%>%
  
  #Then define the layout. You can’t do it after the loop
  layout(title = NULL,
         xaxis = list(title = x_axis_title, type = ‘categorical’, nticks = 10, categoryorder = ‘trace’),
         yaxis = list(title = ‘’),
         barmode = ‘stack’,
         paper_bgcolor = white,
         plot_bgcolor = white,
         font = font_regular,
         margin = list(b = 100),
         showlegend = TRUE)

p <- plot_ly()
for (i in 1:nrow(clientlist)){
  client <- clientlist[i,]
  chart <- add_trace(chart,
                     x = ~data$date,
                     y = ~data[[client]],
                     name = paste0(’’,
                                     sum(data[[client]]), ’ (’, round(((100/total)*sum(data[[client]])), 1),’%): , gsub("_", " ", client)), marker = list(color = pal[i]))
}
}




#First create the chart
chart <- plot_ly(type = 'bar') %>%
  
  #Then define the layout. You can’t do it after the loop
  layout(barmode = "group")
         

for (i in 1:ncol(dd)) {
  
  disposition <- dd[,i]
  chart <- add_trace(chart,
                     x = ~dd$Material,
                     y = ~dd[[disposition]])
  
}
chart


df <- mass %>% 
  group_by(Material) %>% 
  distinct(Material, Disposition) %>% 
  mutate(Weight = 0)

write_csv(df, "df.csv")
