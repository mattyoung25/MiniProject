##################
# Multiple Bar Chart
##################

### Load libraries
library(tidyverse)
library(plotly)

### Read csv
df = read.csv("Data/businessdescription.csv")

### Additional name cleaning
df$Description = gsub("Time  Material", "Time Material", df$Description)
df$Description = gsub("CableAssy HarnMulti", "Cable Assy Harn Multi", df$Description)

### Pivot data for plotting
df = df %>% pivot_wider(names_from = region, values_from = Freq)
fig <- plot_ly(df, x = ~Description, y = ~West, type = 'bar', name = 'West')
fig <- fig %>% add_trace(y = ~South, name = 'South')
fig <- fig %>% add_trace(y = ~Midwest, name = 'Midwest')
fig <- fig %>% add_trace(y = ~Northeast, name = 'Northeast')
fig <- fig %>% layout(title = "Top Business Descriptions in Contracts over Various Regions",
                      xaxis = list(title = 'Business Description', categoryorder = "total descending"),
                      yaxis = list(title = 'Count'), barmode = 'group',
                      plot_bgcolor='#edf9fc',
                      paper_bgcolor='#edf9fc')

fig

#### Saving visuals as a html files
htmlwidgets::saveWidget(as_widget(fig), "Visuals/BusinessDescriptionBarChart.html")
