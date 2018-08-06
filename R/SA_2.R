# Lexicon bases - bahasa Indonesia


# Mendapatkan lexicon bahasa Indonesia dan data dari repo github
library(RCurl)
library(tidyverse)
library(qdap)
library(textclean)
library(tidytext)


# data tweets ----
data_tweet <-read.csv(text=getURL("https://raw.githubusercontent.com/eppofahmi/belajaR/master/cdc-workshop/latihan-cdc.csv"), header=T, sep = ",", stringsAsFactors = FALSE)


# stopwords ----
stopwords_id <-read.delim(text=getURL("https://raw.githubusercontent.com/eppofahmi/ID-Stopwords/master/id.stopwords.02.01.2016.txt"), header=F)

# lexicon negatif ----
neg_id <-read.delim(text=getURL("https://raw.githubusercontent.com/eppofahmi/ID-OpinionWords/master/negative.txt"), header=F)

# lexicon positif ---- 
pos_id <-read.delim(text=getURL("https://raw.githubusercontent.com/eppofahmi/ID-OpinionWords/master/positive.txt"), header=F)

# menghilangkan twit retwit
data_tweet <- data_tweet %>%
  filter(isRetweet == FALSE) 

#perbandingan jumlah twit
data_tweet %>% 
  group_by(person) %>%
  count(person)
#memilih kolom yg akan dimanfaatkan
data_tweet <- data_tweet %>% 
  select(person, screenName, created, text)

# cleaning tweets ----
data_clean <- data_tweet %>% select(text_ori = text)
data_clean$text <- data_clean$text_ori

# remove urls ---
data_clean$text <- replace_html(data_clean$text, symbol = FALSE)
rm_twitter_n_url <- rm_(pattern=pastex("@rm_twitter_url", "@rm_url"))
data_clean$text <- rm_twitter_n_url(data_clean$text, clean = TRUE, trim = TRUE)

# replace non ascii 
data_clean$text <- replace_non_ascii(data_clean$text)

# replace emoji
data_clean$text <- replace_emoji(data_clean$text, emoji_dt = lexicon::hash_emojis)

# replace username
data_clean$text <- str_replace_all(data_clean$text, "(@[[:alnum:]_]*)", "")

# replace hashtag
data_clean$text <- rm_hash(data_clean$text, pattern = "@rm_hash", clean = TRUE)

# lower case
data_clean$text <- tolower(data_clean$text)

# replace punctuation
data_clean$text <- gsub("[[:punct:][:blank:]]+", " ", data_clean$text)

###########
# replace number 
data_clean$text <- replace_number(data_clean$text, num.paste = TRUE)

# replace double words, e.g: kata2, laki2, musik2nya
data_clean$text <- mgsub_regex(data_clean$text, "[2]", ' 2')

# replace 2aA-zZ
data_clean$text <- gsub("(2[[:alpha:]]*)", "", data_clean$text)

#data_clean$text <- gsub("\\btwo\\b", ' ', data_clean$text)
# reduce repeated (3 times) chr in word
data_clean$text <- gsub("([[:alpha:]])\\1{2,}", "\\1", data_clean$text)

head(data_clean$text, n = 20)

# normalisasi based on library ----
# normalisasi ini dibuat secara manual berdasarkan singkatan yang umum digunakan oleh pengguna twitter
kt_normal <- read_delim("Data/kata3karakter.csv", delim = ";")
kt_normal$from <- paste0("\\b", kt_normal$from, "\\b") # excact macth

pattern1 <- as.character(kt_normal$from)
replacement1 <- as.character(kt_normal$to)

clean <- as.character(data_clean$text)
clean <- mgsub_regex(clean, pattern = pattern1, replacement = replacement1, fixed = FALSE)

# stopwords bahasa ----
stopwords_id$to <- ""
stopwords_id$V1 <- paste0("\\b", stopwords_id$V1, "\\b") # excact macth

pattern3 <- as.character(stopwords_id$V1)
replacement3 <- as.character(stopwords_id$to)

clean <- mgsub_regex(sw_tes, pattern = pattern3, replacement = replacement3, fixed = FALSE)

# stopwords twitter ----
# stopword_twit adalah term yang dianggap sebagai stopword yang lazim digunakan di Twitter yang diseleksi secara manual, bassicly stopwords here replace by xyz

kt_delete <- read_delim("Data/katatobedeleted.csv", delim = ";")
kt_delete$from <- paste0("\\b", kt_delete$from, "\\b") # excact macth

pattern2 <- as.character(kt_delete$from)
replacement2 <- as.character(kt_delete$to)

clean <- data_frame(mgsub_regex(clean, pattern = pattern2, replacement = replacement2, fixed = FALSE))

colnames(clean) <- "text"

data_clean$text <- clean$text
data_clean$text <- gsub("\\bxyz\\b", '', data_clean$text)

# replace single chr ----
data_clean$text <- gsub("\\W*\\b\\w\\b\\W*", " ", data_clean$text)

# replace white space
data_clean$text <- replace_white(data_clean$text)
data_clean$text <- gsub("^[[:space:]]+", "", data_clean$text)
data_clean$text <- gsub("[[:space:]]+$", "", data_clean$text)

head(data_clean$text, n = 20)

# checking result ----
daftar_kata <- data_clean %>% 
  select(text) %>%
  unnest_tokens(df_kata, text, token = "words") %>%
  count(df_kata, sort = TRUE) %>%
  mutate(chr_count = nchar(df_kata, type = "chars")) 


# simpan data 
write_csv(data_tweet, path = "/Volumes/mydata/RStudio/sentiment_analysis/Data/data_tweet.csv")
write_csv(data_clean, path = "/Volumes/mydata/RStudio/sentiment_analysis/Data/tweet_clean.csv")
write_csv(daftar_kata, path = "/Volumes/mydata/RStudio/sentiment_analysis/Data/3karakter.csv")

glimpse(data_clean)
