#semantic network gendovaras

library(RCurl)
library(tidyverse)
library(qdap)
library(textclean)
library(tidytext)

gendo <- read.csv("/Volumes/mydata/RStudio/sentiment_analysis/Data/gendovara.csv", header = TRUE, stringsAsFactors = FALSE)

class(gendo)

clean_text1 <- tweet_cleaner(data = gendo, column = 2)
clean_text1$clean_text <- gsub("\\bcc\\b", '', clean_text1$clean_text)

# semantc network gendo -----

library(tidyverse)
library(tidytext)

# undirected network ----
data_net <- clean_text1 %>%
  select(clean_text) %>%
  unnest_tokens(user_net, clean_text, token = "ngrams", n = 2, to_lower = FALSE) %>%
  separate(user_net, into = c("word1", "word2"), sep = " ")

colnames(data_net) <- c("V1", "V2")

# data_net$V1 <- paste0("@", data_net$V1)
# data_net$V2 <- paste0("@", data_net$V2)

library(igraph)
library(rgexf)

u_net <- simplify(graph.data.frame(data_net, directed=FALSE))

# Create a dataframe nodes: 1st column - node ID, 2nd column -node name
nodes_df <- data.frame(ID = c(1:vcount(u_net)), NAME = V(u_net)$name)

# Create a dataframe edges: 1st column - source node ID, 2nd column -target node ID
edges_df <- as.data.frame(get.edges(u_net, c(1:ecount(u_net))))

# saving file to visualise in gephi
write.gexf(nodes = nodes_df, edges = edges_df, 
           defaultedgetype = "undirected", 
           output = "/Volumes/mydata/RStudio/sentiment_analysis/Data/gendo_net.gexf")


a <- as_data_frame(s_net, what = c("edges", "vertices", "both"))