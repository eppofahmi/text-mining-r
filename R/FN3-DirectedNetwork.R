# Directed network
# di dalam Twitter directed network berarti dari akun yang ada di kolom screen name (sender) ke username yang ada di kolom text (user_inv).

library(tidyverse)
library(tidytext)

directed_net <- wrangled %>%
  filter(user_count >=2) %>%
  select(1,2) %>%
  unnest_tokens(user, user_inv, token = "words", to_lower = FALSE)

directed_net1 <- directed_net %>%
  unite(net, sep = " ", remove = TRUE) %>%
  group_by(net) %>%
  count(net, sort = TRUE) %>%
  filter(n >= 15)

#View(directed_net)

directed_net1 <- directed_net1 %>%
  separate(net, into = c("V1", "V2"), sep = " ")

colnames(directed_net1) <- c("V1", "V2", "V3")

#directed_net$V1 <- paste0("@", directed_net$V1)
directed_net1$V2 <- paste0("@", directed_net1$V2)

library(igraph)
library(rgexf)

# create igraph objek
d_net <- simplify(graph_from_data_frame(d = directed_net1, directed = TRUE),
                  remove.loops = TRUE, remove.multiple = FALSE, 
                  edge.attr.comb = igraph_opt("edge.attr.comb"))

# save langsung ----
# Menyiampannya secara langsung dan kolom lain selain 2 pertama dianggap sebagai atribut network
write_graph(graph = d_net, file = "/Volumes/mydata/RStudio/sentiment_analysis/Data/tes_net.graphml", format = "graphml")

# Create a dataframe nodes: 1st column - node ID, 2nd column -node name
nodes_df <- data.frame(ID = c(1:vcount(d_net)), NAME = V(d_net)$name)

# Create a dataframe edges: 1st column - source node ID, 2nd column -target node ID
edges_df <- as.data.frame(get.edges(d_net, c(1:ecount(d_net))))

# save
write.gexf(nodes = nodes_df, edges = edges_df, 
           defaultedgetype = "directed", 
           output = "/Volumes/mydata/RStudio/sentiment_analysis/Data/directed_net.gexf")
