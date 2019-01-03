# wrangling twit btr + bnfs

library(readr)
library(data.table)
library(textclean)

# data 1
twit_bnfs <- read_delim("Data/twit-bnfs.csv", 
                        ";", escape_double = FALSE, col_names = FALSE, 
                        col_types = cols(X1 = col_date(format = "%m %b %Y"), 
                                         X5 = col_logical(), X6 = col_integer()), 
                        trim_ws = TRUE)

twit_bnfs <- twit_bnfs %>%
  filter(str_detect(X3, "@"))
twit_bnfs$X3 <- gsub("\\b@\\b", "", twit_bnfs$X3)

# data 2
twit_btr <- read_delim("Data/twit-btr.csv", 
                       ";", escape_double = FALSE, col_names = FALSE, 
                       col_types = cols(X1 = col_date(format = "%m %b %Y"), 
                                        X2 = col_time(format = "%H:%M:%S"), 
                                        X5 = col_logical(), X6 = col_integer(), 
                                        X8 = col_integer()))
twit_btr <- twit_btr %>%
  filter(str_detect(X3, "@"))

twit_btr$X3 <- gsub("\\b@\\b", "", twit_btr$X3)

# wrangling user ----
userBTR <- tweet_wrangler(data = twit_btr, column1 = 4, column2 = 3)
userBTR <- userBTR %>%
  filter(user_count >= 2)

userBNFS <- tweet_wrangler(data = twit_bnfs, column1 = 4, column2 = 3)
userBNFS <- userBNFS %>%
  filter(user_count >= 2)

# create netwrok ----
# 1. BTR ----
directed_net1 <- userBTR %>%
  filter(user_count >=2) %>%
  select(1,2) %>%
  unnest_tokens(user, user_inv, token = "words", to_lower = FALSE)

directed_net1$user <- paste0("@", directed_net1$user)

directed_net1 <- directed_net1 %>%
  unite(net, sep = " ", remove = TRUE) %>%
  group_by(net) %>%
  count(net, sort = TRUE) %>%
  filter(n >= 10)

directed_net1 <- directed_net1 %>%
  separate(net, into = c("V1", "V2"), sep = " ")

colnames(directed_net1) <- c("V1", "V2", "V3")

# bnfs ----
directed_net2 <- userBNFS %>%
  filter(user_count >=2) %>%
  select(1,2) %>%
  unnest_tokens(user, user_inv, token = "words", to_lower = FALSE)

directed_net2$user <- paste0("@", directed_net2$user)

directed_net2 <- directed_net2 %>%
  unite(net, sep = " ", remove = TRUE) %>%
  group_by(net) %>%
  count(net, sort = TRUE) %>%
  filter(n >= 3)

directed_net2 <- directed_net2 %>%
  separate(net, into = c("V1", "V2"), sep = " ")

colnames(directed_net2) <- c("V1", "V2", "V3")

library(igraph)
library(rgexf)

# create igraph objek
d_net <- simplify(graph_from_data_frame(d = directed_net2, directed = TRUE),
                  remove.loops = TRUE, remove.multiple = FALSE, 
                  edge.attr.comb = igraph_opt("edge.attr.comb"))

# Create a dataframe nodes: 1st column - node ID, 2nd column -node name
nodes_df <- data.frame(Id = c(1:vcount(d_net)), NAME = V(d_net)$name)

# Create a dataframe edges: 1st column - source node ID, 2nd column -target node ID
edges_df <- as.data.frame(get.edges(d_net, c(1:ecount(d_net))))

# save
write.gexf(nodes = nodes_df, 
           edges = edges_df, 
           defaultedgetype = "directed", 
           output = "/Volumes/mydata/RStudio/sentiment_analysis/Data/directed_userBNFS.gexf")

write_graph(graph = d_net, 
            file = "/Volumes/mydata/RStudio/sentiment_analysis/Data/directed_userBNFS.graphml", 
            format = "graphml")
