# undirected network ----
data_net <- wrangled %>%
  filter(user_count >=2) %>%
  select(user_all) %>%
  unnest_tokens(user_net, user_all, token = "ngrams", n = 2, to_lower = FALSE) %>%
  separate(user_net, into = c("word1", "word2"), sep = " ")

colnames(data_net) <- c("V1", "V2")

data_net$V1 <- paste0("@", data_net$V1)
data_net$V2 <- paste0("@", data_net$V2)

library(igraph)
library(rgexf)

gD <- simplify(graph.data.frame(data_net, directed=FALSE))
# Create a dataframe nodes: 1st column - node ID, 2nd column -node name
nodes_df <- data.frame(ID = c(1:vcount(gD)), NAME = V(gD)$name)
# Create a dataframe edges: 1st column - source node ID, 2nd column -target node ID
edges_df <- as.data.frame(get.edges(gD, c(1:ecount(gD))))
write.gexf(nodes = nodes_df, edges = edges_df, 
           defaultedgetype = "undirected", 
           output = "/Volumes/mydata/RStudio/sentiment_analysis/Data/graphOutput.gexf")

