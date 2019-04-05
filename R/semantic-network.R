library(RCurl)
library(tidyverse)
library(qdap)
library(textclean)
library(tidytext)
library(googledrive)
library(quanteda)
library(ggplot2)

rm(list = ls())

# Data ----
id1 <- "11WTedaaE79EtT4vOGNU2vfG7xgcLtJcE" # twit-tagar-balitolakreklamasi.csv
id2 <- "1984jmFur63PmYvoC8Ab6P2iwfUQYVp71" # twit-tagar-balinotforsale.csv

btr_raw <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id1))
bns_raw <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id2))

twitAll <- bind_rows(btr_raw, bns_raw)
rm(btr_raw, bns_raw)

# data 1
twitJrx <- twitAll %>%
  filter(user == "JRX_SID") %>%
  filter(is_duplicate == FALSE) %>%
  filter(word_count >= 2) %>%
  select(date, tweets, clean_text, user)
# data 2
twitGendo <- twitAll %>%
  filter(user == "gendovara") %>%
  filter(is_duplicate == FALSE) %>%
  filter(word_count >= 2) %>%
  select(date, tweets, clean_text, user)
# data 3
twitForbali <- twitAll %>%
  filter(user == "ForBALI13") %>%
  filter(is_duplicate == FALSE) %>%
  filter(word_count >= 2) %>%
  select(date, tweets, clean_text, user)
# data 4
twitLeakBali <- twitAll %>%
  filter(user == "leakBALI_") %>%
  filter(is_duplicate == FALSE) %>%
  filter(word_count >= 2) %>%
  select(date, tweets, clean_text, user)

# data gabungan
twitZ <- bind_rows(twitJrx, twitLeakBali, twitGendo, twitForbali)
twitZ <- as.data.frame(twitZ)

# cleaning ----
twitZ_clean <- tweet_cleaner(data = twitZ, column = 2)

# daftar_kata <- twitZ_clean %>%
#   select(clean_text) %>%
#   unnest_tokens(kata, clean_text, token = "words") %>%
#   count(kata, sort = TRUE)

net_data <- bind_cols(twitZ, twitZ_clean)
net_data <- net_data %>% select(tanggal = date, pengirim = user, clean_text = clean_text1)

# ada kemungkinan masing-masing aktor membuat twit yang sama untuk mensosialiasikan gerakan. Untuk itu hasil akhir preproceesing diberi satu kolom baru untuk mengidentifikasi apakah twitnya duplikat atau bukan. 

net_data <- net_data %>%
  mutate(is_dup = duplicated(clean_text)) %>%
  filter(is_dup == FALSE)

# twitAll %>% 
#   select(user) %>%
#   group_by(user) %>%
#   count(user, sort = TRUE) %>%
#   head(n = 20)

# save_net <- net_data[ , -3]
# save_net <- save_net[ , -5]
# 
# write_tsv(save_net, path = "/Volumes/mydata/RStudio/sentiment_analysis/Data/twit_aktor.tsv")

net_data %>%
  group_by(pengirim, tanggal) %>%
  count(pengirim, sort = TRUE) %>%
  ggplot(aes(tanggal, n, group = 1, col = pengirim)) + 
  geom_line(show.legend = FALSE) + 
  facet_wrap(~pengirim, scales = "free") + 
  scale_x_date(date_labels = "%b %y")

net_data <- net_data[ , -4]

# semantic data ----
data_net1 <- net_data %>%
  filter(str_detect(pengirim, "gendovara")) %>%
  unnest_tokens(user_net, clean_text, token = "ngrams", n = 2, to_lower = FALSE) %>% 
  count(user_net) %>%
  separate(user_net, into = c("word1", "word2"), sep = " ")

data_net1 <- data_net1 %>%
  select(V1 = word1, V2 = word2, V3 = n)

# create network ----
library(igraph)
library(rgexf)

# s_net <- simplify(graph.data.frame(data_net1, directed=FALSE))

test_net <- simplify(graph_from_data_frame(d = data_net1, directed = FALSE),
                     remove.loops = TRUE, remove.multiple = FALSE, 
                     edge.attr.comb = igraph_opt("edge.attr.comb"))

# write_graph(graph = test_net, file = "/Volumes/mydata/RStudio/sentiment_analysis/Data/tes_net.graphml", format = "graphml")

summary(s_net)

# Create a dataframe nodes: 1st column - node ID, 2nd column -node name
data_nodes <- data.frame(ID = c(1:vcount(test_net)), NAME = V(test_net)$name)
# Create a dataframe edges: 1st column - source node ID, 2nd column -target node ID
data_edges <- as.data.frame(get.edges(test_net, c(1:ecount(test_net))))

# save ----
write.gexf(nodes = data_nodes, edges = data_edges, 
           defaultedgetype = "undirected",
           output = "/Volumes/mydata/RStudio/sentiment_analysis/Data/gendovara.gexf")

# rm(twitAll, twitForbali, twitGendo, twitJrx, twitLeakBali, twitZ)
# rm(id1, id2)