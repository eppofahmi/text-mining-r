# skrip ini digunakan untuk melakukan pemodelan topik dan mendapatkan jumlah topik yang optimal berdasarkan perplexity

library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(RWeka) # create dtm bigram 

# Data ----
id_tw_gojek <- "1ALZxvnmISCHuRzawaP5K6FjUTN3Wy700"
twit_gojek <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_tw_gojek), 
                       header = TRUE, stringsAsFactors = FALSE, sep = ",")

twit_gojek$tweets <- gsub("\\bgo jek\\b", "gojek", twit_gojek$tweets)

twit_gojek <- twit_gojek %>%
  filter(is_duplicate == FALSE) %>%
  filter(word_count >= 2) %>%
  select(date, tweets)

clean_text <- tweet_cleaner(data = twit_gojek, column = 2)
clean_text$ori <- twit_gojek$tweets
clean_text$tanggal <- twit_gojek$date

clean_text$word_count <- sapply(clean_text$clean_text, function(x) 
  length(unlist(strsplit(as.character(x), "\\S+"))))

clean_text <- clean_text %>%
  filter(word_count >= 2)

# Fungsi ---- 
# the LDA function using topicmodels package
bigram_tm <- function(input_text, # should be a columm from a dataframe
                      plot = T, # return a plot? TRUE by defult
                      min_words = 2, # minmum token n = 2 by default
                      max_words = 2, # minmum token n = 2 by default
                      number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
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
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}

# tes TM ----
test_bigram <- bigram_tm(clean_text$clean_text, 
                         number_of_topics = 5, 
                         plot = T, 
                         min_words = 2, 
                         max_words = 2)
test_bigram