# Undirected netwrok
# undirected network berasal dari seluruh akun yang ada dalam twit, yaitu baik akun dari kolom screen name (sender), maupun dalam text (user_inv)

library(tidyverse)
library(tidytext)

# undirected network ----
data_net <- wrangled %>%
  filter(user_count >=2) %>%
  select(user_all) %>%
  unnest_tokens(user_net, user_all, token = "ngrams", n = 2, to_lower = FALSE) %>%
  separate(user_net, into = c("word1", "word2"), sep = " ")

colnames(data_net) <- c("V1", "V2")

# data_net$V1 <- paste0("@", data_net$V1)
# data_net$V2 <- paste0("@", data_net$V2)

library(igraph)
library(rgexf)

u_net <- simplify(graph.data.frame(data_net, directed=FALSE))
# ini bisa langsung disimpan sebagai .graphml namun pada saat memvisualisasikannya di gephi perlu ada beberapa penyesuaian, karena selain dua kolom pertama dianggap sebagai edges atribut

# Create a dataframe nodes: 1st column - node ID, 2nd column -node name
nodes_df <- data.frame(ID = c(1:vcount(u_net)), NAME = V(u_net)$name)

# Create a dataframe edges: 1st column - source node ID, 2nd column -target node ID
edges_df <- as.data.frame(get.edges(u_net, c(1:ecount(u_net))))

# saving file to visualise in gephi
write.gexf(nodes = nodes_df, edges = edges_df, 
           defaultedgetype = "undirected", 
           output = "/Volumes/mydata/RStudio/sentiment_analysis/Data/undirected_net.gexf")