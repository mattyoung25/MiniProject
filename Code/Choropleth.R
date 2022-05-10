##################
# Choropleth Maps
##################

### Load libraries
library(tidyverse)
library(plotly)
library(choroplethr)
library(choroplethrMaps)

### Read csv
df = read.csv("Data/chloropleth.csv")
count = df$count

## Reduce variables for simple choropleth plotting
# plot1df = df[c(1,3)]
# plot1df$state <- tolower(plot1df$state)
# colnames(plot1df) <- c("region","value")
# plot2df = df[c(1,4)]
# colnames(plot2df) <- c("region","value")
# plot2df$state <- tolower(plot2df$state)
# state_choropleth(plot1df, num_colors = 3, title = "Money Awarded for Hypersonic Missile Development in Each State", legend = "U.S. Dollars")
# state_choropleth(plot2df, num_colors = 3, title = "Number of Contracts Awarded for in Hypersonic Missile Development in Each State", legend = "Number of Contracts")

## Now adjust to  Plotly for more information
df = df %>% rename(money_awarded = value)
df$hover1 = with(df, paste(state, '<br>', "Contracts Awarded", count))
df$hover2 = with(df, paste(state, '<br>', "Money Awarded", money_awarded))


# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options

# g <- list(
#   scope = "usa",
#   projection = list(type = "albers usa"),
#   showland = TRUE,
#   showlakes = TRUE,
#   showcountries = FALSE,
#   lakecolor = toRGB("#edf9fc"),  #theme color
#   landcolor = toRGB("grey90"),
#   subunitcolor = toRGB("black"),
#   countrycolor = toRGB("grey15"),
#   coastlinecolor = toRGB("black"),
#   countrywidth = 0.5,
#   subunitwidth = 0.5,
#   bgcolor = toRGB('#edf9fc') #theme color
# )
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('#edf9fc'),
  bgcolor = toRGB('#edf9fc')
)

fig <- plot_geo(df, locationmode = 'USA-states')
fig <- fig %>% add_trace(
  z = ~money_awarded, text = ~hover1, locations = ~code,
  color = ~money_awarded, colors = 'Reds'
)
fig <- fig %>% colorbar(title = "USD")
fig <- fig %>% layout(
  title = 'Money Awarded for Hypersonic Missile Development in Each State<br>(Hover for breakdown)',
  geo = g,
  plot_bgcolor='#edf9fc',
  paper_bgcolor='#edf9fc'
)

fig

fig2 <- plot_geo(df, locationmode = 'USA-states')
fig2 <- fig2 %>% add_trace(
  z = ~count, text = ~hover2, locations = ~code,
  color = ~count, colors = 'Reds'
)
fig2 <- fig2 %>% colorbar(title = "# of Contracts")
fig2 <- fig2 %>% layout(
  title = 'Number of Contracts Awarded for Hypersonic Missile Development in Each State<br>(Hover for breakdown)',
  geo = g,
  plot_bgcolor='#edf9fc',
  paper_bgcolor='#edf9fc'
)

fig2

#### Saving visuals as a html files
htmlwidgets::saveWidget(as_widget(fig), "Visuals/MoneyMap.html")
htmlwidgets::saveWidget(as_widget(fig2), "Visuals/ContractsMap.html")                
                
                
                
                
                
                