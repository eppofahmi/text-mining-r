# undirected network ----
data_net <- wrangled %>%
  filter(user_count >=2) %>%
  select(user_all) %>%
  unnest_tokens(user_net, user_all, token = "ngrams", n = 2, to_lower = FALSE) %>%
  separate(user_net, into = c("from", "to"), sep = " ")

colnames(data_net) <- c("source", "target")

data_net$source <- paste0("@", data_net$source)
data_net$target <- paste0("@", data_net$target)

library(igraph)
library(rgexf)


write.gexf(nodes = nodes_df, edges = edges_df, nodesAtt = nodes_att, edgesAtt = edges_att, nodesVizAtt = nodes_att_viz, edgesVizAtt = edges_att_viz, defaultedgetype = "undirected", output = "lesmis.gexf")
