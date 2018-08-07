# Directed network
# di dalam Twitter directed network berarti dari akun yang ada di kolom screen name (sender) ke username yang ada di kolom text (user_inv).

library(tidyverse)
library(tidytext)

directed_net <- wrangled %>%
  filter(user_count >=2) %>%
  select(1,2) %>%
  unnest_tokens(user, user_inv, token = "words", to_lower = FALSE)

View(directed_net)

colnames(directed_net) <- c("V1", "V2")

#directed_net$V1 <- paste0("@", directed_net$V1)
directed_net$V2 <- paste0("@", directed_net$V2)

library(igraph)
library(rgexf)

gD <- simplify(graph.data.frame(directed_net, directed=TRUE))
# Create a dataframe nodes: 1st column - node ID, 2nd column -node name
nodes_df <- data.frame(ID = c(1:vcount(gD)), NAME = V(gD)$name)
# Create a dataframe edges: 1st column - source node ID, 2nd column -target node ID
edges_df <- as.data.frame(get.edges(gD, c(1:ecount(gD))))
write.gexf(nodes = nodes_df, edges = edges_df, 
           defaultedgetype = "directed", 
           output = "/Volumes/mydata/RStudio/sentiment_analysis/Data/directed_net.gexf")

