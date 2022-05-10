##################
# Network
##################

### Load libraries
library(arules)
library(igraph)
library(networkD3)
## Read the transactions data into a dataframe
df <- read.csv("Data/businesstypestransactions.csv")
label <- df$label
df <- df[-10]
colnames(df) <- NULL
write.csv(df, "Data/updatedtransactions.csv", row.names = F)


#### Association Rule Mining
Trans <- read.transactions("Data/updatedtransactions.csv", sep =",", 
                                format("basket"),  rm.duplicates = TRUE)
Trans_rules = arules::apriori(Trans, 
                                   parameter = list(support=.08, conf=.08, minlen=2))
inspect(Trans_rules)
##  Sort by Conf
SortedRules_conf <- sort(Trans_rules, by="confidence", decreasing=TRUE)
inspect(SortedRules_conf)
## Sort by Sup
SortedRules_sup <- sort(Trans_rules, by="support", decreasing=TRUE)
inspect(SortedRules_sup)
## Sort by Lift
SortedRules_lift <- sort(Trans_rules, by="lift", decreasing=TRUE)
inspect(SortedRules_lift)

## Convert the RULES to a DATAFRAME
Rules_DF<-DATAFRAME(Trans_rules, separate = TRUE)
(head(Rules_DF))
str(Rules_DF)
## Convert to char
Rules_DF$LHS<-as.character(Rules_DF$LHS)
Rules_DF$RHS<-as.character(Rules_DF$RHS)

## Remove all {}
Rules_DF[] <- lapply(Rules_DF, gsub, pattern='[{]', replacement='')
Rules_DF[] <- lapply(Rules_DF, gsub, pattern='[}]', replacement='')

## USING SUP
Rules_S<-Rules_DF[c(1,2,3)]
names(Rules_S) <- c("SourceName", "TargetName", "Weight")
head(Rules_S,30)

Rules_Sup<-Rules_S

###########################################################################
#############       Build a Network edgeList and nodeList    ############
###########################################################################

edgeList<-Rules_Sup
# Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
graph <- igraph::graph.data.frame(edgeList, directed=TRUE)
V(graph)$label.cex = 1


png(file="Visuals/networkgraph.png", width=948, height = 624)
plot(graph, vertex.color = "white", edge.width = E(graph)$weight *20, edge.arrow.size = E(graph)$weight)
dev.off()


