# Visualisasi network dengan networkD3
# Data diambil dari network analisis yang dilakukan di gephi.

library(tidygraph)
library(networkD3)

# edges data ----
links_1 <- read.csv("/Volumes/mydata/RStudio/text-mining-r/Data/edges_d1.csv", 
                    stringsAsFactors = FALSE)
links_1 <- links_1 %>%
  select(Source, Target, Weight)

# nodes data ----
nodes_1 <- read.csv("/Volumes/mydata/RStudio/text-mining-r/Data/nodes_d1.csv")
nodes_1 <- nodes_1 %>%
  select(Id, Label, modularity_class, Degree)

#deleting 1st chr -----
nodes_1$Id <- as.character(nodes_1$Id)
nodes_1$Id <- substring(nodes_1$Id, 2)
links_1$Source <- substring(links_1$Source, 2)
links_1$Target <- substring(links_1$Target, 2)

links_1$Source <- as.integer(links_1$Source)
links_1$Target <- as.integer(links_1$Target)
nodes_1$Id <- as.integer(nodes_1$Id)

glimpse(nodes_1)
glimpse(links_1)

#links_1$Weight <- as.data.frame(links_1$Weight/100)

D3_network_LM <- forceNetwork(Links = links_1, Nodes = nodes_1, 
             Source = "Source",
             Target = "Target", 
             Value = "Weight",
             NodeID = "Label",
             Nodesize = "Degree",
             arrows = FALSE, 
             legend = FALSE,
             Group = "modularity_class", 
             opacity = 1,
             fontSize = 15, 
             zoom = TRUE)

# Save as html file ----
networkD3::saveNetwork(D3_network_LM, "semnet-twit-gojek.html", selfcontained = TRUE)
