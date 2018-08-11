library(tidygraph)
library(networkD3)

links_1 <- read.csv("/Volumes/mydata/RStudio/sentiment_analysis/Data/edges_d1.csv", stringsAsFactors = FALSE)
links_1 <- links_1 %>%
  select(Source, Target, Weight)

glimpse(nodes_1)

nodes_1 <- read.csv("/Volumes/mydata/RStudio/sentiment_analysis/Data/nodes_d1.csv")
nodes_1 <- nodes_1 %>%
  select(Id, Label, modularity_class, betweenesscentrality)

#deleting 1st chr
nodes_1$Id <- as.character(nodes_1$Id)
nodes_1$Id <- substring(nodes_1$Id, 2)
links_1$Source <- substring(links_1$Source, 2)
links_1$Target <- substring(links_1$Target, 2)

links_1$Source <- as.integer(links_1$Source)
links_1$Target <- as.integer(links_1$Target)
nodes_1$Id <- as.integer(nodes_1$Id)

glimpse(nodes_1)
glimpse(links_1)

links_1$Weight <- as.data.frame(links_1$Weight/100)

forceNetwork(Links = links_1, Nodes = nodes_1, 
             Source = "Source",
             Target = "Target", 
             Value = "Weight",
             NodeID = "Label",
             Nodesize = "betweenesscentrality",
             arrows = TRUE, legend = TRUE,
             Group = "modularity_class", 
             opacity = 1,
             fontSize = 15, 
             zoom = TRUE)