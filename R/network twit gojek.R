# Semantic network twit gojek
library(tidyverse)
library(googledrive)

id_tw_gojek <- "1ALZxvnmISCHuRzawaP5K6FjUTN3Wy700"
twit_gojek <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_tw_gojek), 
                       header = TRUE, stringsAsFactors = FALSE, sep = ",")

twit_gojek <- twit_gojek %>%
  filter(word_count >= 2)

glimpse(twit_gojek)
class(twit_gojek)

twit_gojek1 <- twit_gojek %>%
  filter(word_count >= 2) %>%
  select(tweets)

clean_text <- tweet_cleaner(data = twit_gojek1, column = 1)
clean_text$ori <- twit_gojek$tweets
clean_text$param <- twit_gojek$parameter

data_net <- clean_text %>%
  select(clean_text, param) %>%
  unnest_tokens(user_net, clean_text, token = "ngrams", n = 2, to_lower = FALSE) %>%
  group_by(param) %>%
  count(user_net, sort = TRUE) %>%
  filter(n >= 2) %>%
  separate(user_net, into = c("word1", "word2"), sep = " ")

data_net <- data_net %>%
  select(word1, word2, n, param)

colnames(data_net) <- c("V1", "V2", "V3", "V4")

library(igraph)
library(rgexf)

d_net <- simplify(graph_from_data_frame(d = data_net, directed = FALSE),
                  remove.loops = TRUE, remove.multiple = FALSE, 
                  edge.attr.comb = igraph_opt("edge.attr.comb"))

# save langsung ----
# Menyiampannya secara langsung dan kolom lain selain 2 pertama dianggap sebagai atribut network
write_graph(graph = d_net, file = "/Volumes/mydata/RStudio/text-mining-r/Data/net-twit-gojek.graphml", format = "graphml")


