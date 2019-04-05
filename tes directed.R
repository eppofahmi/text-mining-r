# Directed network
# di dalam Twitter directed network berarti dari akun yang ada di kolom screen name (sender) ke username yang ada di kolom text (user_inv).

library(tidyverse)
library(tidytext)

directed_net1 <- wrangled %>%
  filter(user_count >=2) %>%
  select(1,2) %>%
  unnest_tokens(user, user_inv, token = "words", to_lower = FALSE) %>%
  unite(userall, sep = " ", remove = TRUE) %>%
  count(userall, sort = TRUE) %>%
  filter(n >= 20) %>%
  separate(userall, into = c("V1", "V2"), sep = " ", remove = TRUE)

#View(directed_net1)

colnames(directed_net1) <- c("V1", "V2", "V3")

#directed_net1$V1 <- paste0("@", directed_net1$V1)
directed_net1$V2 <- paste0("@", directed_net1$V2)

library(igraph)
library(rgexf)

# create igraph objek
d_net1 <- simplify(graph.data.frame(directed_net1, directed=TRUE))

plot(d_net1)
