# skrip ini digunakan untuk melakukan pemodelan topik dan mendapatkan jumlah topik yang optimal berdasarkan perplexity
# Runing RJava
if (Sys.info()['sysname'] == 'Darwin') {
  libjvm <- paste0(system2('/usr/libexec/java_home',stdout = TRUE)[1],'/jre/lib/server/libjvm.dylib')
  message (paste0('Load libjvm.dylib from: ',libjvm))
  dyn.load(libjvm)
}

library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(RWeka) # create dtm bigram 

# Fungsi ---- 
bigram_tm <- function(input_text, # should be a columm from a dataframe
                      plot = T, # return a plot? TRUE by defult
                      min_words = 2, # minmum token n = 2 by default
                      max_words = 2, # minmum token n = 2 by default
                      topic_term = 10, # term per topic
                      number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  library(tidyverse) # general utility & workflow functions
  library(tidytext) # tidy implimentation of NLP methods
  library(topicmodels) # for LDA topic modelling 
  library(tm) # general text mining functions, making document term matrixes
  library(SnowballC) # for stemming
  library(RWeka) # create dtm bigram
  
  Corpus <- VCorpus(VectorSource(input_text))
  
  if (Sys.info()['sysname'] == 'Darwin') {
    libjvm <- paste0(system2('/usr/libexec/java_home',stdout = TRUE)[1],'/jre/lib/server/libjvm.dylib')
    message (paste0('Load libjvm.dylib from: ',libjvm))
    dyn.load(libjvm)
  }

  # function for creating bigram in the DTM
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = min_words, max = max_words))
  DTM <- DocumentTermMatrix(Corpus, control=list(tokenize=BigramTokenizer))
  
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% 
    group_by(topic) %>% 
    top_n(topic_term, beta) %>% 
    ungroup() %>% 
    arrange(topic, -beta)
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    top_terms %>%
      mutate(term = reorder(term, beta)) %>% 
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      labs(x = NULL, y = "Beta") +
      coord_flip()
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}

# tes TM ----
to_kemen_clean <- read_csv("to_kemen_clean.csv", 
                           col_types = cols(X1 = col_date(format = "%d-%m-%Y"), 
                                            X4 = col_logical()), trim_ws = FALSE)
from_kemen_clean <- read_csv("from_kemen_clean.csv", 
                             col_types = cols(X1 = col_date(format = "%d-%m-%Y")), 
                             trim_ws = FALSE)

glimpse(from_kemen_clean)

# topic to -----
topik_to_kemen <- bigram_tm(to_kemen_clean$clean_text,
                         number_of_topics = 10,
                         plot = FALSE,
                         topic_term = 30,
                         min_words = 2,
                         max_words = 2)
topik_to_kemen
topik_to_kemen$beta <- round(topik_to_kemen$beta, 4)

write_csv(topik_to_kemen, path = "topik_to_kemen.csv")

# topic from -----
topik_from_kemen <- bigram_tm(from_kemen_clean$clean_text,
                            number_of_topics = 10,
                            plot = FALSE,
                            topic_term = 30,
                            min_words = 2,
                            max_words = 2)
topik_from_kemen
topik_from_kemen$beta <- round(topik_from_kemen$beta, 4)

write_csv(topik_from_kemen, path = "topik_from_kemen.csv")
