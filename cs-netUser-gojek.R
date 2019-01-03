library(readr)
library(data.table)
library(textclean)

netTwit_gojek <- read_csv("Data/netTwit_gojek.csv")
netTwit_gojek$tweets <- replace_non_ascii(netTwit_gojek$tweets)

userTwitGojek <- tweet_wrangler(data = netTwit_gojek, column1 = 2, column2 = 1)
userTwitGojek <- userTwitGojek %>%
  filter(user_count >= 2)

userTwitGojek <- userTwitGojek %>%
  mutate(chr_coun = nchar(user_inv)) %>%
  filter(chr_coun >= 2)

# create netwrok ----
directed_net1 <- userTwitGojek %>%
  filter(user_count >=2) %>%
  select(1,2) %>%
  unnest_tokens(user, user_inv, token = "words", to_lower = FALSE) 

directed_net1$user <- paste0("@", directed_net1$user)

directed_net1 <- directed_net1 %>%
  unite(net, sep = " ", remove = TRUE) %>%
  group_by(net) %>%
  count(net, sort = TRUE) %>%
  filter(n >= 2)

directed_net1 <- directed_net1 %>%
  separate(net, into = c("V1", "V2"), sep = " ")

colnames(directed_net1) <- c("V1", "V2", "V3")


# ========
library(igraph)
library(rgexf)

# create igraph objek
d_net <- simplify(graph_from_data_frame(d = directed_net1, directed = TRUE),
                  remove.loops = TRUE, remove.multiple = FALSE, 
                  edge.attr.comb = igraph_opt("edge.attr.comb"))

# saving directly -----
write_graph(graph = d_net, 
            file = "/Volumes/mydata/RStudio/sentiment_analysis/Data/netAktorGojek.graphml", 
            format = "graphml")


# Create a dataframe nodes: 1st column - node ID, 2nd column -node name
nodes_df <- data.frame(Id = c(1:vcount(d_net)), NAME = V(d_net)$name)

# Create a dataframe edges: 1st column - source node ID, 2nd column -target node ID
edges_df <- as.data.frame(get.edges(d_net, c(1:ecount(d_net))))

# save
write.gexf(nodes = nodes_df, 
           edges = edges_df, 
           defaultedgetype = "directed", 
           output = "/Volumes/mydata/RStudio/sentiment_analysis/Data/directed_userBNFS.gexf")

