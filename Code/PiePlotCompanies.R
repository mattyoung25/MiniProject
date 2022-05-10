##################
# Pie Charts
##################

### Load libraries
library(tidyverse)
library(plotly)

### Read csv
df = read.csv("Data/businessnames.csv")

df = df %>% filter(Contracts > 9) #Remove Smaller Companies


fig2 <- plot_ly(data = df, labels = ~CompanyName, values = ~Contracts, textinfo = 'none', hoverinfo = "text", text = ~ paste("Company Name:", CompanyName, "\n", "Number of Contracts:", Contracts, "\n", "Region:", Region),
type = 'pie')
fig2 <- fig2 %>% layout(title = "Key Companies in Supply Chain", showlegend = T,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      legend = list(font = list(size = 8), orientation = 'h'),
                      plot_bgcolor='#edf9fc',
                      paper_bgcolor='#edf9fc')

fig2
htmlwidgets::saveWidget(as_widget(fig2), "Visuals/keyregionalcompanies.html")