# skrip ini digunakan untuk mengenerate perplexity topik model dari sebuah kolom data frame dengan paramter: 
# 1. jumlah topik awal = 4
# 2. token yang dapat disesuaikan
# 3. hasil akhir berupa visualisasi nilai k optimal 

library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(RWeka) # create dtm bigram 

# Fungsi ---- 
# the LDA function using topicmodels package
tm_perplexity <- function(input_text, # should be a columm from a dataframe
                      min_words = 2, # minmum token n = 2 by default
                      max_words = 2, # minmum token n = 2 by default
                      min_k = 5, # minimum topik
                      max_k = 20, # maximum topik
                      multi = 5, # kelipatan
                      number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- VCorpus(VectorSource(input_text))
  
  if (Sys.info()['sysname'] == 'Darwin') {
    libjvm <- paste0(system2('/usr/libexec/java_home',stdout = TRUE)[1],'/jre/lib/server/libjvm.dylib')
    message (paste0('Load libjvm.dylib from: ',libjvm))
    dyn.load(libjvm)
  }
  
  library(RWeka)
  # function for creating bigram in the DTM
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = min_words, max = max_words))
  DTM <- DocumentTermMatrix(Corpus, control=list(tokenize=BigramTokenizer))
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  ap_td <- tidy(DTM)
  ap_dtm <- ap_td %>%  
    anti_join(stop_words, by = c(term = "word")) %>%  
    cast_dtm(document, term, count)
  # preform LDA & get the words/topic in a tidy text format
  ap_lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  perplexity(ap_lda)
  n_topics <- seq(from = min_k, to = max_k, by = multi)
  ap_lda_compare <- n_topics %>%  
    map(LDA, x = ap_dtm, control = list(seed = 1234))
  perplex <- data_frame(k = n_topics,
             perplex = map_dbl(ap_lda_compare, perplexity)) %>%
    ggplot(aes(k, perplex)) +
    geom_point() +
    geom_line() +
    labs(title = "Mengevaluasi topic model LDA",
         subtitle = "Titik tertinggi adalah jumlah topik (k) optimal dari dokumen",
         x = "Jumlah Topik",
         y = "Perplexity")
  perplex
}

# tes perplex ----
# test_perplex <- tm_perplexity(clean_text$clean_text, 
#                          number_of_topics = 4,
#                          min_words = 2,
#                          max_words = 2, 
#                          min_k = 5, 
#                          max_k = 20, 
#                          multi = 5)
# test_perplex
