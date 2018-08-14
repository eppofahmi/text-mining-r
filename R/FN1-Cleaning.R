# skrip ini digunakan untuk melakukan cleaning text dari twitter dengan menggunakan beberapa package. Tujuannya adalah mendapatkan teks yang sudah bersih dari:
# 1. URL umum dan twitter
# 2. non ascci character
# 3. emoji
# 4. username
# 5. tagar
# 6. teks dalam bentuk lowercase
# 7. punctuation
# 9. term yang disingkat (e.g: laki2 = laki, musik2nya = musik)
# 10. term dengan character yang diulang minimal >= 3 kali (e.g: kuaaat = kuat)
# 11. angka menjadi teks terpisah (e.g: 21 = twentyone, 2013 = 101 = one hundred and one)
# 12. normalisai term yang terdiri dari tiga karakter (297 term)
# 13. penghilangan stopwords khusus untuk twitter (314 term)
# 14. penghilangan stopwords bahasa indonesia (758 term)
# 15. strip white space

library(RCurl)
library(tidyverse)
library(qdap)
library(textclean)
library(tidytext)

# Cleaning function ----
tweet_cleaner <- function(data, # data frame
                          column) # column number
  {
  # taking text column
  data <- as.character(data[ ,column])
  data <- replace_html(data, symbol = FALSE) # r html
  rm_twitter_n_url <- rm_(pattern=pastex("@rm_twitter_url", "@rm_url"))
  data <- rm_twitter_n_url(data, clean = TRUE, trim = TRUE) # r urls
  data <- replace_non_ascii(data) # non ascii
  data <- replace_emoji(data, emoji_dt = lexicon::hash_emojis)
  data <- str_replace_all(data, "(@[[:alnum:]_]*)", "") # replace username
  data <- rm_hash(data, pattern = "@rm_hash", clean = TRUE) # replace hashtag
  data <- tolower(data) # lower case
  data <- gsub("[[:punct:][:blank:]]+", " ", data)   # replace punctuation
  data <- str_replace_all(data, "(^| )[0-9.() -]{5,}( |$)", replacement = " ") # remove phone number
  data <- replace_number(data, num.paste = TRUE) # replace number
  # replace double words, e.g: kata2, laki2, musik2nya
  data <- mgsub_regex(data, "[2]", ' 2')
  data <- gsub("(2[[:alpha:]]*)", "", data) # replace 2aA-zZ
  # reduce repeated (3 times) chr in word
  data <- gsub("([[:alpha:]])\\1{2,}", "\\1", data)
  # normalisation
  kt_normal <- read.csv(text=getURL("https://raw.githubusercontent.com/eppofahmi/sentiment_analysis/master/Data/kata3karakter.csv"), 
                        header=T, sep = ";", stringsAsFactors = FALSE)
  kt_normal$from <- paste0("\\b", kt_normal$from, "\\b") # excact macth
  pattern1 <- as.character(kt_normal$from)
  replacement1 <- as.character(kt_normal$to)
  data <- mgsub_regex(data, pattern = pattern1, replacement = replacement1, fixed = FALSE)
  # stopwords bahasa indonesia
  stopwords_id <- read.delim(text=getURL("https://raw.githubusercontent.com/eppofahmi/ID-Stopwords/master/id.stopwords.02.01.2016.txt"), header=F)
  stopwords_id$to <- ""
  stopwords_id$V1 <- paste0("\\b", stopwords_id$V1, "\\b") # excact macth
  pattern2 <- as.character(stopwords_id$V1)
  replacement2 <- as.character(stopwords_id$to)
  data <- mgsub_regex(data, pattern = pattern2, replacement = replacement2, fixed = FALSE)
  # stopword twitter
  kt_delete <- read.csv(text=getURL("https://raw.githubusercontent.com/eppofahmi/sentiment_analysis/master/Data/katatobedeleted.csv"), header=T, sep = ";", stringsAsFactors = FALSE)
  colnames(kt_delete) <- c("from", "to")
  kt_delete$from <- paste0("\\b", kt_delete$from, "\\b") # excact macth
  pattern3 <- as.character(kt_delete$from)
  replacement3 <- as.character(kt_delete$to)
  data <- mgsub_regex(data, pattern = pattern3, replacement = replacement3, fixed = FALSE)
  data <- gsub("\\bxyz\\b", '', data)
  # stopwords bahasa inggris
  stopwords_en <- stop_words
  stopwords_en <- stopwords_en %>%
    select(V1 = word)
  stopwords_en$to <- ""
  stopwords_en$V1 <- paste0("\\b", stopwords_en$V1, "\\b") # excact macth
  pattern4 <- as.character(stopwords_en$V1)
  replacement4 <- as.character(stopwords_en$to)
  data <- mgsub_regex(data, pattern = pattern4, replacement = replacement4, fixed = FALSE)
  # replace single chr
  data <- gsub("\\W*\\b\\w\\b\\W*", " ", data)
  # replace white space
  data <- replace_white(data)
  data <- gsub("^[[:space:]]+", "", data)
  data <- gsub("[[:space:]]+$", "", data)
  
  print("your data is clean!!!")
  return(data_frame(clean_text = data))
}

# tesss -----
data_tweet <-read.csv(text=getURL("https://raw.githubusercontent.com/eppofahmi/belajaR/master/cdc-workshop/latihan-cdc.csv"), header=T, sep = ",", stringsAsFactors = FALSE)

data_tweet <- data_tweet %>%
  filter(isRetweet == FALSE)

clean_text <- tweet_cleaner(data = data_tweet, column = 2)
clean_text$ori <- data_tweet$text

clean_text$ori <- replace_non_ascii(clean_text$ori)

daftar_kata <- clean_text %>%
  select(clean_text) %>%
  unnest_tokens(daftar, clean_text, token = "words", to_lower = TRUE) %>%
  count(daftar, sort = TRUE)
