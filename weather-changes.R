#Weather file without sd bands

install.packages("plotly")
install.packages("here")

library(tidyverse)
library(plotly)
library(readxl)


data <- read_excel("146 O'Leary Road - NIWA Weather.xlsx")

pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa',
                       '#bfd730', '#a7a9ac', '#666666')

tk.palette <- c('#3b4035', '#9c8755', '#61655c',
                '#d1bc8b', '#40696f', '#2e5150',
                '#5f5131', '#9e513a')


dat <- data %>%
  select( Date, `Temp Max`, `Temp Min`, `Rain (mm)`) %>%
  rename("high" = "Temp Max",
         "low" = "Temp Min",
         "rain" = "Rain (mm)") %>%
  mutate(day = day(Date),
         month = month(Date),
         year = year(Date)) %>%
   mutate(date = Date) %>%
  gather(high:rain, key = var, value = value) %>%
  group_by(var, day) %>%
  mutate(max = max(value, na.rm = T),
         min = min(value, na.rm = T)) %>%
  ungroup() %>%
  select(-day) %>%
  gather(value:min, key = type, value = measurement) %>%
  unite(var, c(var, type)) %>%
  filter(var %in% c('high_max', 'high_value', 'low_min', 'low_value', 'rain_max', 'rain_value')) %>%
  mutate(var = gsub('_max|_min', '_record', var)) %>%
  separate(var, into = c('var', 'type')) %>%
  spread(key = type, value = measurement) %>%
  mutate(text = paste0('<b>', var, ':<b>', value))%>%
  arrange(Date, var)

plot1 <- plot_ly(x = ~date) %>%
#  add_lines(data = dat %>% filter(var == 'high'), y = ~record,
#            line = list(color = 'transparent'),
#            hoverinfo = 'none') %>%
#  add_lines(data = dat %>% filter(var == 'low'), y = ~record,
#            line = list(color = 'transparent'),
#            fill = 'tonexty',
#            fillcolor = scales::alpha(pointblue.palette[2], alpha = 0.2),
#            hoverinfo = 'none', name = 'spline') %>%
  add_trace(data = dat %>% filter(var == 'high'), y = ~value,
            type = 'scatter', mode = 'lines',
            line = list(color = pointblue.palette[3]),
#            marker = list(color = pointblue.palette[3]),
            text = ~text,
            hoverinfo = 'x+text') %>%
  add_trace(data = dat %>% filter(var == 'low'), y = ~value, 
            type = 'scatter', mode = 'lines',
            line = list(color = pointblue.palette[2]),
#            marker = list(color = pointblue.palette[2]),
            text = ~text,
            hoverinfo = 'text') %>%
#  add_lines(data = dat %>% filter(var == 'rain'), y = ~record, 
#            line = list(color = 'transparent'),
#            fill = 'tozeroy',
#            fillcolor = scales::alpha('gray80', alpha = 0.5),
#            hoverinfo = 'none',
#            yaxis = 'y2') %>%
  add_trace(data = dat %>% filter(var == 'rain'), y = ~value,
            type = 'scatter', mode = 'lines',
            line = list(color = pointblue.palette[1]),
#            marker = list(color = pointblue.palette[1]),
            text = ~text,
            hoverinfo = 'text',
            yaxis = 'y2') %>%
  add_annotations(text = 'Precipitation (mm)', 
                  textangle = 90,
                  x = 1,
                  y = 0.5,
                  xshift = 50,
                  xref = 'paper',
                  yref = 'paper',
                  yanchor = 'middle',
                  font = list(family = 'sans-serif',
                              size = 14),
                  showarrow = FALSE,
                  visible = TRUE) %>%
  layout(yaxis = list(range = c(0, 35),
                      title = 'Temperature (C)',
                      gridcolor = 'white',
                      font = list(family = 'sans-serif',
                                  size = 14),
                      automargin = TRUE),
         yaxis2 = list(overlaying = 'y',
                       side = 'right',
                       range = c(0, 100),
                       title = NA,
                       gridcolor = 'white',
                       font = list(family = 'sans-serif',
                                   size = 14)),
         xaxis = list(title = NA, 
                      type = 'date',
                      font = list(family = 'sans-serif',
                                  size = 14),
                      rangeselector = list(buttons = list(list(count = 3,
                                                               label = "3 mo",
                                                               step = "month",
                                                               stepmode = "backward"),
                                                          list(count = 6,
                                                               label = "6 mo",
                                                               step = "month",
                                                               stepmode = "backward"),
                                                          list(count = 12,
                                                               label = "12 mo",
                                                               step = "month",
                                                               stepmode = "backward"),
                                                          list(count = 1,
                                                               label = "YTD",
                                                               step = "year",
                                                               stepmode = "todate"),
                                                          list(step = "all")),
                                           font = list(family = 'sans-serif',
                                                       size = 14,
                                                       color = 'white'),
                                           bgcolor = tk.palette[2],
                                           activecolor = tk.palette[5])),
         showlegend = FALSE,
         hovermode = 'x',
         dragmode = 'pan',
         margin = list(r = 50, b = 10, t = 10)) %>%
  rangeslider(max(as.numeric(dat$date))-365, max(dat$date), thickness = 0.05) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
                                       'zoomIn2d', 'zoomOut2d', 
                                       'pan2d', 'toggleSpikelines'))

plot1

