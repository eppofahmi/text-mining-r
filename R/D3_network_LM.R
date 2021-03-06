# Data format: dataframe with 3 variables; variables 1 & 2 correspond to interactions; variable 3 is weight of interaction

library(tidyverse)
library(networkD3)

# data ----
edgeList <- data_net[-c(1:3), ]
colnames(edgeList) <- c("SourceName", "TargetName", "Weight")
edgeList <- edgeList %>%
  filter(Weight >= 4)

# create graph ----
gD <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=FALSE))

# Create a node list object (actually a data frame object) that will contain information about nodes requires IDs to start at 0
nodeList <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)),
                       nName = igraph::V(gD)$name) # because networkD3 library 

# Map node names from the edge list to node IDs
getNodeID <- function(x){
  which(x == igraph::V(gD)$name) - 1 # to ensure that IDs start at 0
}

# And add them to the edge list
edgeList <- plyr::ddply(edgeList, .variables = c("SourceName", "TargetName", "Weight"), 
                        function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                                                TargetID = getNodeID(x$TargetName)))

# Node properties ----
# Calculate some node properties and node similarities that will be used to illustrate different plotting abilities and add them to the edge and node lists

# degree ----
nodeList <- cbind(nodeList, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all"))

# betweenness ----
# We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
betAll <- igraph::betweenness(gD, v = igraph::V(gD), directed = FALSE) / (((igraph::vcount(gD) - 1) * (igraph::vcount(gD)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
nodeList <- cbind(nodeList, nodeBetweenness=100*betAll.norm)

rm(betAll, betAll.norm)

# Dice similarities ----
dsAll <- igraph::similarity.dice(gD, vids = igraph::V(gD), mode = "all")

F1 <- function(x) {data.frame(diceSim = dsAll[x$SourceID +1, x$TargetID + 1])}
edgeList <- plyr::ddply(edgeList, .variables=c("SourceName", "TargetName", 
                                               "Weight", "SourceID", "TargetID"), 
                           function(x) data.frame(F1(x)))

rm(dsAll, F1, getNodeID, gD)
############################################################################################
# We will also create a set of colors for each edge, based on their dice similarity values
# We'll interpolate edge colors based on the using the "colorRampPalette" function, that 
# returns a function corresponding to a collor palete of "bias" number of elements (in our case, that
# will be a total number of edges, i.e., number of rows in the edgeList data frame)

F2 <- colorRampPalette(c("#FFFF00", "#FF0123"), bias = nrow(edgeList), space = "rgb", interpolate = "linear")

F2

colCodes <- F2(length(unique(edgeList$diceSim)))

edges_col <- sapply(edgeList$diceSim, function(x) colCodes[which(sort(unique(edgeList$diceSim)) == x)])

rm(colCodes, F2)

############################################################################################
# Let's create a network
D3_network_LM <- networkD3::forceNetwork(Links = edgeList, # data frame that contains info about edges
                        Nodes = nodeList, # data frame that contains info about nodes
                        Source = "SourceID", # ID of source node 
                        Target = "TargetID", # ID of target node
                        Value = "Weight", # value from the edge list (data frame) 
                        NodeID = "nName", # value from the node list (data frame)
                        Nodesize = "nodeBetweenness",  # value from the node list (data frame)
                        Group = "nodeDegree",  # value from the node list (data frame) that
                        height = 700, # Size of the plot (vertical)
                        width = 1200,  # Size of the plot (horizontal)
                        fontSize = 18, # Font size
                        linkDistance = networkD3::JS("function(d) { return 10*d.value; }"), 
                        # Function to determine distance between any two nodes
                        linkWidth = networkD3::JS("function(d) { return d.value/5; }"),
                        # Function to determine link/edge thickness
                        opacity = 5, # opacity
                        zoom = TRUE, # ability to zoom when click on the node
                        opacityNoHover = 1, # opacity of labels when static
                        linkColour = edges_col) # edge colors

# Plot network
D3_network_LM 
         
# Save network as html file
networkD3::saveNetwork(D3_network_LM, "D3_LM.html", selfcontained = TRUE)