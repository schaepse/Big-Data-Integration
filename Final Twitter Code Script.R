#Read packages into script#
library(tidyverse)
library(tm)
library(ggwordcloud)
library(tidyr)

#Vincent's Tweets#
vincents <- c("vincents", "pasta")
vincents_search <- paste(vincents, collapse = " AND ")
vincents_tweets <- search_tweets(q = vincents_search, n = 1000, lang = "en", include_rts = FALSE)

tibble(vincents_tweets)
head(vincents_tweets$text)

vin_text <- vincents_tweets$text

corpus_vin <- Corpus(VectorSource(vincents_tweets$text))
head(corpus_vin)
inspect(corpus_vin[1])

vin_emoji_clean <- tm_map(corpus_vin, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)
url_clean_vin <- tm_map(vin_emoji_clean, content_transformer(remove_urls))

lower_vin <- tm_map(url_clean_vin, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_vin <- tm_map(lower_vin, content_transformer(remove_items))

remove_punct_vin <- tm_map(remove_vin, removePunctuation)

remove_num_vin <- tm_map(remove_punct_vin, removeNumbers)

remove_stop_vin <- tm_map(remove_num_vin, removeWords, stopwords("english"))

remove_white_vin <- tm_map(remove_stop_vin, stripWhitespace)

remove_words_vin <- tm_map(remove_white_vin, removeWords, c("vincent", "vincent's", "vincents", "amp", "pasta", "pastas"))

matrix_vin <- TermDocumentMatrix((remove_words_vin))
matrix_vin
matrix_vin_2
sort_vin
matrix_vin_2 <- as.matrix(matrix_vin)
sort_vin <- sort(rowSums(matrix_vin_2), decreasing = TRUE)
data_frame_vin <- data.frame(word = names(sort_vin), freq = sort_vin)

top_10_vin <- data_frame_vin %>%
  arrange(desc(freq)) %>% head(10)
top_10_vin

#Pine Mountain Tweets#
pine_mountain <- c('"pine mountain"', "syrup")
pine_mountain_search <- paste(pine_mountain, collapse = " AND ")
pine_mountain_tweets <- search_tweets(q = pine_mountain_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(pine_mountain_tweets)

head(pine_mountain_tweets$text)

pine_text <- pine_mountain_tweets$text

corpus_pine <- Corpus(VectorSource(pine_mountain_tweets$text))

pine_emoji_clean <- tm_map(corpus_pine, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_pine <- tm_map(pine_emoji_clean, content_transformer(remove_urls))


lower_pine <- tm_map(url_clean_pine, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_pine <- tm_map(lower_pine, content_transformer(remove_items))

remove_punct_pine <- tm_map(remove_pine, removePunctuation)

remove_num_pine <- tm_map(remove_punct_pine, removeNumbers)

remove_stop_pine <- tm_map(remove_num_pine, removeWords, stopwords("english"))

remove_white_pine <- tm_map(remove_stop_pine, stripWhitespace)

remove_words_pine <- tm_map(remove_white_pine, removeWords, c("pine mountain", "pine", "mountain", "pines", "mountains", "amp", "syrup", "syrups"))

matrix_pine <- TermDocumentMatrix((remove_words_pine))
matrix_pine_2 <- as.matrix(matrix_pine)
sort_pine <- sort(rowSums(matrix_pine_2), decreasing = TRUE)
data_frame_pine <- data.frame(word = names(sort_pine), freq = sort_pine)

top_10_pine <- data_frame_pine %>%
  arrange(desc(freq)) %>% head(10)

#Miller Tweets# - check again
miller <- c("miller", "syrup")
miller_search <- paste(miller, collapse = " AND ")
miller_tweets <- search_tweets(q = miller_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(miller_tweets)

head(miller_tweets$text)

miller_text <- miller_tweets$text

corpus_miller <- Corpus(VectorSource(miller_tweets$text))

miller_emoji_clean <- tm_map(corpus_miller, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_miller<- tm_map(miller_emoji_clean, content_transformer(remove_urls))

lower_miller <- tm_map(url_clean_miller, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_miller <- tm_map(lower_miller, content_transformer(remove_items))


remove_punct_miller <- tm_map(remove_miller, removePunctuation)

remove_num_miller <- tm_map(remove_punct_miller, removeNumbers)

remove_stop_miller <- tm_map(remove_num_miller, removeWords, stopwords("english"))

remove_white_miller <- tm_map(remove_stop_miller, stripWhitespace)

remove_words_miller <- tm_map(remove_white_miller, removeWords, c("miller", "millers", "amp", "syrup", "syrups", "kcpqtxfk"))

matrix_miller <- TermDocumentMatrix((remove_words_miller))
matrix_miller_2 <- as.matrix(matrix_miller)
sort_miller <- sort(rowSums(matrix_miller_2), decreasing = TRUE)
data_frame_miller <- data.frame(word = names(sort_miller), freq = sort_miller)

top_10_miller <- data_frame_miller %>%
  arrange(desc(freq)) %>% head(10) 

miller_final <- crossing(top_10_miller, brand = "Miller")
miller_final

#Barilla Tweets#
barilla <- c("barilla", "pasta")
barilla_search <- paste(barilla, collapse = " AND ")
barilla_tweets <- search_tweets(q = barilla_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(barilla_tweets)

head(barilla_tweets$text)

barilla_text <- barilla_tweets$text

corpus_barilla <- Corpus(VectorSource(barilla_tweets$text))

barilla_emoji_clean <- tm_map(corpus_barilla, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_barilla<- tm_map(barilla_emoji_clean, content_transformer(remove_urls))

lower_barilla <- tm_map(url_clean_barilla, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_barilla <- tm_map(lower_barilla, content_transformer(remove_items))

remove_punct_barilla <- tm_map(remove_barilla, removePunctuation)

remove_num_barilla <- tm_map(remove_punct_barilla, removeNumbers)

remove_stop_barilla <- tm_map(remove_num_barilla, removeWords, stopwords("english"))

remove_white_barilla <- tm_map(remove_stop_barilla, stripWhitespace)

remove_words_barilla <- tm_map(remove_white_barilla, removeWords, c("barilla", "barillas", "cbarilla", "amp", "pasta", "pastas"))

matrix_barilla <- TermDocumentMatrix((remove_words_barilla))
matrix_barilla_2 <- as.matrix(matrix_barilla)
sort_barilla <- sort(rowSums(matrix_barilla_2), decreasing = TRUE)
data_frame_barilla <- data.frame(word = names(sort_barilla), freq = sort_barilla)

top_10_barilla <- data_frame_barilla %>%
  arrange(desc(freq)) %>% head(10)

barilla_final <- crossing(top_10_barilla, brand = "Barilla")
barilla_final

#Elena's Tweets#
elenas <- c("elenas", "pasta")
elenas_search <- paste(elenas, collapse = " AND ")
elenas_tweets <- search_tweets(q = elenas_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(elenas_tweets)

head(elenas_tweets$text)

elenas_text <- elenas_tweets$text

corpus_elenas <- Corpus(VectorSource(elenas_tweets$text))

elenas_emoji_clean <- tm_map(corpus_elenas, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_elenas<- tm_map(elenas_emoji_clean, content_transformer(remove_urls))

lower_elenas <- tm_map(url_clean_elenas, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_elenas <- tm_map(lower_elenas, content_transformer(remove_items))

remove_punct_elenas <- tm_map(remove_elenas, removePunctuation)

remove_num_elenas <- tm_map(remove_punct_elenas, removeNumbers)

remove_stop_elenas <- tm_map(remove_num_elenas, removeWords, stopwords("english"))

remove_white_elenas <- tm_map(remove_stop_elenas, stripWhitespace)

remove_words_elenas <- tm_map(remove_white_elenas, removeWords, c("elenas", "elena", "elena's", "elana", "elanas", "elana's", "amp", "pasta", "pastas"))

matrix_elenas <- TermDocumentMatrix((remove_words_elenas))
matrix_elenas_2 <- as.matrix(matrix_elenas)
sort_elenas <- sort(rowSums(matrix_elenas_2), decreasing = TRUE)
data_frame_elenas <- data.frame(word = names(sort_elenas), freq = sort_elenas)

top_10_elenas <- data_frame_elenas %>%
  arrange(desc(freq)) %>% head(10) 

#Boves Tweets#
boves <- c("boves", "pasta")
boves_search <- paste(boves, collapse = " AND ")
boves_tweets <- search_tweets(q = boves_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(boves_tweets)

head(boves_tweets$text)

boves_text <- boves_tweets$text

corpus_boves <- Corpus(VectorSource(boves_tweets$text))

boves_emoji_clean <- tm_map(corpus_boves, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_boves<- tm_map(boves_emoji_clean, content_transformer(remove_urls))

lower_boves <- tm_map(url_clean_boves, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_boves <- tm_map(lower_boves, content_transformer(remove_items))

remove_punct_boves <- tm_map(remove_boves, removePunctuation)

remove_num_boves <- tm_map(remove_punct_boves, removeNumbers)

remove_stop_boves <- tm_map(remove_num_boves, removeWords, stopwords("english"))

remove_white_boves <- tm_map(remove_stop_boves, stripWhitespace)

remove_words_boves <- tm_map(remove_white_boves, removeWords, c("boves", "bove", "amp", "pasta", "pastas"))

matrix_boves <- TermDocumentMatrix((remove_words_boves))
matrix_boves_2 <- as.matrix(matrix_boves)
sort_boves <- sort(rowSums(matrix_boves_2), decreasing = TRUE)
data_frame_boves <- data.frame(word = names(sort_boves), freq = sort_boves)

top_10_boves <- data_frame_boves %>%
  arrange(desc(freq)) %>% head(10)

#Fastshake Tweets#
fastshake <- c("fastshake", "pancake")
fastshake_search <- paste(fastshake, collapse = " AND ")
fastshake_tweets <- search_tweets(q = fastshake_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(fastshake_tweets)

head(fastshake_tweets$text)

fastshake_text <- fastshake_tweets$text

corpus_fastshake <- Corpus(VectorSource(fastshake_tweets$text))

fastshake_emoji_clean <- tm_map(corpus_fastshake, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_fastshake<- tm_map(fastshake_emoji_clean, content_transformer(remove_urls))

lower_fastshake <- tm_map(url_clean_fastshake, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_fastshake <- tm_map(lower_fastshake, content_transformer(remove_items))

remove_punct_fastshake <- tm_map(remove_fastshake, removePunctuation)

remove_num_fastshake <- tm_map(remove_punct_fastshake, removeNumbers)

remove_stop_fastshake <- tm_map(remove_num_fastshake, removeWords, stopwords("english"))

remove_white_fastshake <- tm_map(remove_stop_fastshake, stripWhitespace)

remove_words_fastshake <- tm_map(remove_white_fastshake, removeWords, c("fastshake", "fastshakes", "fast", "fasts", "shake", "shakes", "amp", "pancake", "pancakes"))


matrix_fastshake <- TermDocumentMatrix((remove_words_fastshake))
matrix_fastshake_2 <- as.matrix(matrix_fastshake)

sort_fastshake <- sort(rowSums(matrix_fastshake_2), decreasing = TRUE)
data_frame_fastshake <- data.frame(word = names(sort_fastshake), freq = sort_fastshake)

top_10_fastshake <- data_frame_fastshake %>%
  arrange(desc(freq)) %>% head(10)

#Golden Eagle Tweets#
golden_eagle <- c('"golden eagle"', "syrup")
golden_eagle_search <- paste(golden_eagle, collapse = " AND ")
golden_eagle_tweets <- search_tweets(q = golden_eagle_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(golden_eagle_tweets)

head(golden_eagle_tweets$text)

golden_eagle_text <- golden_eagle_tweets$text

corpus_golden_eagle <- Corpus(VectorSource(golden_eagle_tweets$text))

golden_eagle_emoji_clean <- tm_map(corpus_golden_eagle, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_golden_eagle<- tm_map(golden_eagle_emoji_clean, content_transformer(remove_urls))

lower_golden_eagle <- tm_map(url_clean_golden_eagle, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_golden_eagle <- tm_map(lower_golden_eagle, content_transformer(remove_items))

remove_punct_golden_eagle <- tm_map(remove_golden_eagle, removePunctuation)

remove_num_golden_eagle <- tm_map(remove_punct_golden_eagle, removeNumbers)

remove_stop_golden_eagle <- tm_map(remove_num_golden_eagle, removeWords, stopwords("english"))

remove_white_golden_eagle <- tm_map(remove_stop_golden_eagle, stripWhitespace)

remove_words_golden_eagle <- tm_map(remove_white_golden_eagle, removeWords, c("golden eagle", "golden", "eagle", "goldens", "eagles", "amp", "syrup", "syrups", "coreyatkins", "realjameswoods", "shadowchimera"))

matrix_golden_eagle <- TermDocumentMatrix((remove_words_golden_eagle))
matrix_golden_eagle_2 <- as.matrix(matrix_golden_eagle)
sort_golden_eagle <- sort(rowSums(matrix_golden_eagle_2), decreasing = TRUE)
data_frame_golden_eagle <- data.frame(word = names(sort_golden_eagle), freq = sort_golden_eagle)

top_10_golden_eagle <- data_frame_golden_eagle %>%
  arrange(desc(freq)) %>% head(10)

golden_eagle_final <- crossing(top_10_golden_eagle, brand = "Golden Eagle")
golden_eagle_final

#RR Tweets# -check again
rr <- c("rr", "pasta")
rr_search <- paste(rr, collapse = " AND ")
rr_tweets <- search_tweets(q = rr_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(rr_tweets)

head(rr_tweets$text)

rr_text <- rr_tweets$text

corpus_rr <- Corpus(VectorSource(rr_tweets$text))

rr_emoji_clean <- tm_map(corpus_rr, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_rr<- tm_map(rr_emoji_clean, content_transformer(remove_urls))

lower_rr <- tm_map(url_clean_rr, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_rr <- tm_map(lower_rr, content_transformer(remove_items))

remove_punct_rr <- tm_map(remove_rr, removePunctuation)

remove_num_rr <- tm_map(remove_punct_rr, removeNumbers)

remove_stop_rr <- tm_map(remove_num_rr, removeWords, stopwords("english"))

remove_white_rr <- tm_map(remove_stop_rr, stripWhitespace)

remove_words_rr <- tm_map(remove_white_rr, removeWords, c("rr", "r", "rrs", "amp", "pasta", "pastas"))

matrix_rr <- TermDocumentMatrix((remove_words_rr))
matrix_rr_2 <- as.matrix(matrix_rr)
sort_rr <- sort(rowSums(matrix_rr_2), decreasing = TRUE)
data_frame_rr <- data.frame(word = names(sort_rr), freq = sort_rr)

top_10_rr <- data_frame_rr %>%
  arrange(desc(freq)) %>% head(10) 

rr_final <- crossing(top_10_rr, brand = "RR")
rr_final

#Gooch Tweets#
gooch <- c("gooch", "pasta")
gooch_search <- paste(gooch, collapse = " AND ")
gooch_tweets <- search_tweets(q = gooch_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(gooch_tweets)

head(gooch_tweets$text)

gooch_text <- gooch_tweets$text

corpus_gooch <- Corpus(VectorSource(gooch_tweets$text))

gooch_emoji_clean <- tm_map(corpus_gooch, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_gooch<- tm_map(gooch_emoji_clean, content_transformer(remove_urls))

lower_gooch <- tm_map(url_clean_gooch, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_gooch <- tm_map(lower_gooch, content_transformer(remove_items))

remove_punct_gooch <- tm_map(remove_gooch, removePunctuation)

remove_num_gooch <- tm_map(remove_punct_gooch, removeNumbers)

remove_stop_gooch <- tm_map(remove_num_gooch, removeWords, stopwords("english"))

remove_white_gooch <- tm_map(remove_stop_gooch, stripWhitespace)

remove_words_gooch <- tm_map(remove_white_gooch, removeWords, c("gooch", "goochs", "amp", "pasta", "pastas"))

matrix_gooch <- TermDocumentMatrix((remove_words_gooch))
matrix_gooch_2 <- as.matrix(matrix_gooch)
sort_gooch <- sort(rowSums(matrix_gooch_2), decreasing = TRUE)
data_frame_gooch <- data.frame(word = names(sort_gooch), freq = sort_gooch)

top_10_gooch <- data_frame_gooch %>%
  arrange(desc(freq)) %>% head(10)

#Alaga Tweets#
alaga <- c("alaga", "syrup")
alaga_search <- paste(alaga, collapse = " AND ")
alaga_tweets <- search_tweets(q = alaga_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(alaga_tweets)

head(alaga_tweets$text)

alaga_text <- alaga_tweets$text

corpus_alaga <- Corpus(VectorSource(alaga_tweets$text))

alaga_emoji_clean <- tm_map(corpus_alaga, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_alaga<- tm_map(alaga_emoji_clean, content_transformer(remove_urls))

lower_alaga <- tm_map(url_clean_alaga, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_alaga <- tm_map(lower_alaga, content_transformer(remove_items))

remove_punct_alaga <- tm_map(remove_alaga, removePunctuation)

remove_num_alaga <- tm_map(remove_punct_alaga, removeNumbers)

remove_stop_alaga <- tm_map(remove_num_alaga, removeWords, stopwords("english"))

remove_white_alaga <- tm_map(remove_stop_alaga, stripWhitespace)

remove_words_alaga <- tm_map(remove_white_alaga, removeWords, c("alaga", "alagas", "amp", "syrup", "syrups", "coachdouglas", "swnmnheya"))

matrix_alaga <- TermDocumentMatrix((remove_words_alaga))
matrix_alaga_2 <- as.matrix(matrix_alaga)
sort_alaga <- sort(rowSums(matrix_alaga_2), decreasing = TRUE)
data_frame_alaga <- data.frame(word = names(sort_alaga), freq = sort_alaga)

top_10_alaga <- data_frame_alaga %>%
  arrange(desc(freq)) %>% head(10) 

alaga_final <- crossing(top_10_alaga, brand = "Alaga")
alaga_final

#M W Flapstax Tweets#
m_w_flapstax <- c('"m w flapstax"', "pancake")
m_w_flapstax_search <- paste(m_w_flapstax, collapse = " AND ")
m_w_flapstax_tweets <- search_tweets(q = m_w_flapstax_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(m_w_flapstax_tweets)

head(m_w_flapstax_tweets$text)

m_w_flapstax_text <- m_w_flapstax_tweets$text

corpus_m_w_flapstax <- Corpus(VectorSource(m_w_flapstax_tweets$text))

m_w_flapstax_emoji_clean <- tm_map(corpus_m_w_flapstax, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_m_w_flapstax<- tm_map(m_w_flapstax_emoji_clean, content_transformer(remove_urls))

lower_m_w_flapstax <- tm_map(url_clean_m_w_flapstax, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_m_w_flapstax <- tm_map(lower_m_w_flapstax, content_transformer(remove_items))

remove_punct_m_w_flapstax <- tm_map(remove_m_w_flapstax, removePunctuation)

remove_num_m_w_flapstax <- tm_map(remove_punct_m_w_flapstax, removeNumbers)

remove_stop_m_w_flapstax <- tm_map(remove_num_m_w_flapstax, removeWords, stopwords("english"))

remove_white_m_w_flapstax <- tm_map(remove_stop_m_w_flapstax, stripWhitespace)

remove_words_m_w_flapstax <- tm_map(remove_white_m_w_flapstax, removeWords, c("m w flapstax", "m w flapstaxes", "flapstax", "flapstaxes", "m", "w", "amp", "pancake", "pancakes"))

matrix_m_w_flapstax <- TermDocumentMatrix((remove_words_m_w_flapstax))
matrix_m_w_flapstax_2 <- as.matrix(matrix_m_w_flapstax)
sort_m_w_flapstax <- sort(rowSums(matrix_m_w_flapstax_2), decreasing = TRUE)
data_frame_m_w_flapstax <- data.frame(word = names(sort_m_w_flapstax), freq = sort_m_w_flapstax)

top_10_m_w_flapstax <- data_frame_m_w_flapstax %>%
  arrange(desc(freq)) %>% head(10)

#Grandma Molases Tweets#
grandma_molases <- c('"grandma molases"', "syrup")
grandma_molases_search <- paste(grandma_molases, collapse = " AND ")
grandma_molases_tweets <- search_tweets(q = grandma_molases_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(grandma_molases_tweets)

head(grandma_molases_tweets$text)

grandma_molases_text <- grandma_molases_tweets$text

corpus_grandma_molases <- Corpus(VectorSource(grandma_molases_tweets$text))

grandma_molases_emoji_clean <- tm_map(corpus_grandma_molases, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_grandma_molases<- tm_map(grandma_molases_emoji_clean, content_transformer(remove_urls))

lower_grandma_molases <- tm_map(url_clean_grandma_molases, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_grandma_molases <- tm_map(lower_grandma_molases, content_transformer(remove_items))

remove_punct_grandma_molases <- tm_map(remove_grandma_molases, removePunctuation)

remove_num_grandma_molases <- tm_map(remove_punct_grandma_molases, removeNumbers)

remove_stop_grandma_molases <- tm_map(remove_num_grandma_molases, removeWords, stopwords("english"))

remove_white_grandma_molases <- tm_map(remove_stop_grandma_molases, stripWhitespace)

remove_words_grandma_molases <- tm_map(remove_white_grandma_molases, removeWords, c("grandma molases", "grandma", "molases", "amp", "syrup", "syrups"))

matrix_grandma_molases <- TermDocumentMatrix((remove_words_grandma_molases))
matrix_grandma_molases_2 <- as.matrix(matrix_grandma_molases)
sort_grandma_molases <- sort(rowSums(matrix_grandma_molases_2), decreasing = TRUE)
data_frame_grandma_molases <- data.frame(word = names(sort_grandma_molases), freq = sort_grandma_molases)

top_10_grandma_molases <- data_frame_grandma_molases %>%
  arrange(desc(freq)) %>% head(10)

#Dell Amore Tweets#
dell_amore <- c('"dell amore"', "pasta")
dell_amore_search <- paste(dell_amore, collapse = " AND ")
dell_amore_tweets <- search_tweets(q = dell_amore_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(dell_amore_tweets)

head(dell_amore_tweets$text)

dell_amore_text <- dell_amore_tweets$text

corpus_dell_amore <- Corpus(VectorSource(dell_amore_tweets$text))

dell_amore_emoji_clean <- tm_map(corpus_dell_amore, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_dell_amore<- tm_map(dell_amore_emoji_clean, content_transformer(remove_urls))

lower_dell_amore <- tm_map(url_clean_dell_amore, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_dell_amore <- tm_map(lower_dell_amore, content_transformer(remove_items))

remove_punct_dell_amore <- tm_map(remove_dell_amore, removePunctuation)

remove_num_dell_amore <- tm_map(remove_punct_dell_amore, removeNumbers)

remove_stop_dell_amore <- tm_map(remove_num_dell_amore, removeWords, stopwords("english"))

remove_white_dell_amore <- tm_map(remove_stop_dell_amore, stripWhitespace)

remove_words_dell_amore <- tm_map(remove_white_dell_amore, removeWords, c("dell amore", "dell", "amore", "amp", "pasta", "pastas"))

matrix_dell_amore <- TermDocumentMatrix((remove_words_dell_amore))
matrix_dell_amore_2 <- as.matrix(matrix_dell_amore)
sort_dell_amore <- sort(rowSums(matrix_dell_amore_2), decreasing = TRUE)
data_frame_dell_amore <- data.frame(word = names(sort_dell_amore), freq = sort_dell_amore)

top_10_dell_amore <- data_frame_dell_amore %>%
  arrange(desc(freq)) %>% head(10)

#Creamette Tweets#
creamette <- c("creamette", "pasta")
creamette_search <- paste(creamette, collapse = " AND ")
creamette_tweets <- search_tweets(q = creamette_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(creamette_tweets)

head(creamette_tweets$text)

creamette_text <- creamette_tweets$text

corpus_creamette <- Corpus(VectorSource(creamette_tweets$text))

creamette_emoji_clean <- tm_map(corpus_creamette, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_creamette<- tm_map(creamette_emoji_clean, content_transformer(remove_urls))

lower_creamette <- tm_map(url_clean_creamette, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_creamette <- tm_map(lower_creamette, content_transformer(remove_items))

remove_punct_creamette <- tm_map(remove_creamette, removePunctuation)

remove_num_creamette <- tm_map(remove_punct_creamette, removeNumbers)

remove_stop_creamette <- tm_map(remove_num_creamette, removeWords, stopwords("english"))

remove_white_creamette <- tm_map(remove_stop_creamette, stripWhitespace)

remove_words_creamette <- tm_map(remove_white_creamette, removeWords, c("creamette", "creamettes", "cream", "creams", "amp", "pasta", "pastas", "bdzxgfhn", "isn"))

matrix_creamette <- TermDocumentMatrix((remove_words_creamette))
matrix_creamette_2 <- as.matrix(matrix_creamette)
sort_creamette <- sort(rowSums(matrix_creamette_2), decreasing = TRUE)
data_frame_creamette <- data.frame(word = names(sort_creamette), freq = sort_creamette)

top_10_creamette <- data_frame_creamette %>%
  arrange(desc(freq)) %>% head(10)

creamette_final <- crossing(top_10_creamette, brand = "Creamette")
creamette_final

#Pennsylvania Dutch Tweets#
pennsylvania_dutch <- c('"pennsylvania dutch"', "pasta")
pennsylvania_dutch_search <- paste(pennsylvania_dutch, collapse = " AND ")
pennsylvania_dutch_tweets <- search_tweets(q = pennsylvania_dutch_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(pennsylvania_dutch_tweets)

head(pennsylvania_dutch_tweets$text)

pennsylvania_dutch_text <- pennsylvania_dutch_tweets$text

corpus_pennsylvania_dutch <- Corpus(VectorSource(pennsylvania_dutch_tweets$text))

pennsylvania_dutch_emoji_clean <- tm_map(corpus_pennsylvania_dutch, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_pennsylvania_dutch<- tm_map(pennsylvania_dutch_emoji_clean, content_transformer(remove_urls))

lower_pennsylvania_dutch <- tm_map(url_clean_pennsylvania_dutch, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_pennsylvania_dutch <- tm_map(lower_pennsylvania_dutch, content_transformer(remove_items))

remove_punct_pennsylvania_dutch <- tm_map(remove_pennsylvania_dutch, removePunctuation)

remove_num_pennsylvania_dutch <- tm_map(remove_punct_pennsylvania_dutch, removeNumbers)

remove_stop_pennsylvania_dutch <- tm_map(remove_num_pennsylvania_dutch, removeWords, stopwords("english"))

remove_white_pennsylvania_dutch <- tm_map(remove_stop_pennsylvania_dutch, stripWhitespace)

remove_words_pennsylvania_dutch <- tm_map(remove_white_pennsylvania_dutch, removeWords, c("pennsylvania dutch", "pennsylvania", "dutch", "amp", "pasta", "pastas", "pensylvania dutches", "dutches"))

matrix_pennsylvania_dutch <- TermDocumentMatrix((remove_words_pennsylvania_dutch))
matrix_pennsylvania_dutch_2 <- as.matrix(matrix_pennsylvania_dutch)
sort_pennsylvania_dutch <- sort(rowSums(matrix_pennsylvania_dutch_2), decreasing = TRUE)
data_frame_pennsylvania_dutch <- data.frame(word = names(sort_pennsylvania_dutch), freq = sort_pennsylvania_dutch)

top_10_pennsylvania_dutch <- data_frame_pennsylvania_dutch %>%
  arrange(desc(freq)) %>% head(10)

#Healthy Harvest Tweets#
healthy_harvest <- c('"healthy harvest"', "pasta")
healthy_harvest_search <- paste(healthy_harvest, collapse = " AND ")
healthy_harvest_tweets <- search_tweets(q = healthy_harvest_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(healthy_harvest_tweets)

head(healthy_harvest_tweets$text)

healthy_harvest_text <- healthy_harvest_tweets$text

corpus_healthy_harvest <- Corpus(VectorSource(healthy_harvest_tweets$text))

healthy_harvest_emoji_clean <- tm_map(corpus_healthy_harvest, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_healthy_harvest<- tm_map(healthy_harvest_emoji_clean, content_transformer(remove_urls))

lower_healthy_harvest <- tm_map(url_clean_healthy_harvest, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_healthy_harvest <- tm_map(lower_healthy_harvest, content_transformer(remove_items))

remove_punct_healthy_harvest <- tm_map(remove_healthy_harvest, removePunctuation)

remove_num_healthy_harvest <- tm_map(remove_punct_healthy_harvest, removeNumbers)

remove_stop_healthy_harvest <- tm_map(remove_num_healthy_harvest, removeWords, stopwords("english"))

remove_white_healthy_harvest <- tm_map(remove_stop_healthy_harvest, stripWhitespace)

remove_words_healthy_harvest <- tm_map(remove_white_healthy_harvest, removeWords, c("healthy harvest", "healthy", "harvest", "harvests", "pasta", "pastas", "amp"))

matrix_healthy_harvest <- TermDocumentMatrix((remove_words_healthy_harvest))
matrix_healthy_harvest_2 <- as.matrix(matrix_healthy_harvest)
sort_healthy_harvest <- sort(rowSums(matrix_healthy_harvest_2), decreasing = TRUE)
data_frame_healthy_harvest <- data.frame(word = names(sort_healthy_harvest), freq = sort_healthy_harvest)

top_10_healthy_harvest <- data_frame_healthy_harvest %>%
  arrange(desc(freq)) %>% head(10)

#Bisquick Tweets#
bisquick <- c("bisquick", "pancake")
bisquick_search <- paste(bisquick, collapse = " AND ")
bisquick_tweets <- search_tweets(q = bisquick_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(bisquick_tweets)

head(bisquick_tweets$text)

bisquick_text <- bisquick_tweets$text

corpus_bisquick <- Corpus(VectorSource(bisquick_tweets$text))

bisquick_emoji_clean <- tm_map(corpus_bisquick, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_bisquick<- tm_map(bisquick_emoji_clean, content_transformer(remove_urls))

lower_bisquick <- tm_map(url_clean_bisquick, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_bisquick <- tm_map(lower_bisquick, content_transformer(remove_items))

remove_punct_bisquick <- tm_map(remove_bisquick, removePunctuation)

remove_num_bisquick <- tm_map(remove_punct_bisquick, removeNumbers)

remove_stop_bisquick <- tm_map(remove_num_bisquick, removeWords, stopwords("english"))

remove_white_bisquick <- tm_map(remove_stop_bisquick, stripWhitespace)

remove_words_bisquick <- tm_map(remove_white_bisquick, removeWords, c("bisquick", "quick", "bisquicks", "amp", "pancake", "pancakes"))

matrix_bisquick <- TermDocumentMatrix((remove_words_bisquick))
matrix_bisquick_2 <- as.matrix(matrix_bisquick)
sort_bisquick <- sort(rowSums(matrix_bisquick_2), decreasing = TRUE)
data_frame_bisquick <- data.frame(word = names(sort_bisquick), freq = sort_bisquick)

top_10_bisquick <- data_frame_bisquick %>%
  arrange(desc(freq)) %>% head(10) 
bisquick_final <- crossing(top_10_bisquick, brand = "Bisquick")
bisquick_final

#Bruce's Tweets#
bruces <- c("bruces", "pancake")
bruces_search <- paste(bruces, collapse = " AND ")
bruces_tweets <- search_tweets(q = bruces_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(bruces_tweets)

head(bruces_tweets$text)

bruces_text <- bruces_tweets$text

corpus_bruces <- Corpus(VectorSource(bruces_tweets$text))

bruces_emoji_clean <- tm_map(corpus_bruces, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_bruces<- tm_map(bruces_emoji_clean, content_transformer(remove_urls))

lower_bruces <- tm_map(url_clean_bruces, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_bruces <- tm_map(lower_bruces, content_transformer(remove_items))

remove_punct_bruces <- tm_map(remove_bruces, removePunctuation)

remove_num_bruces <- tm_map(remove_punct_bruces, removeNumbers)

remove_stop_bruces <- tm_map(remove_num_bruces, removeWords, stopwords("english"))

remove_white_bruces <- tm_map(remove_stop_bruces, stripWhitespace)

remove_words_bruces <- tm_map(remove_white_bruces, removeWords, c("bruce's", "bruce", "bruces", "amp", "pancake", "pancakes"))

matrix_bruces <- TermDocumentMatrix((remove_words_bruces))
matrix_bruces_2 <- as.matrix(matrix_bruces)
sort_bruces <- sort(rowSums(matrix_bruces_2), decreasing = TRUE)
data_frame_bruces <- data.frame(word = names(sort_bruces), freq = sort_bruces)

top_10_bruces <- data_frame_bruces %>%
  arrange(desc(freq)) %>% head(10)

#Hungry Jack Tweets#
hungry_jack <- c("pancake", "syrup")
hungry_jack_combine <- paste(hungry_jack, collapse = " OR ")
hungry_jack_2 <- c('"hungry jack"', hungry_jack_combine)
hungry_jack_search <- paste(hungry_jack_2, collapse = " AND ")
hungry_jack_tweets <- search_tweets(q = hungry_jack_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(hungry_jack_tweets)

head(hungry_jack_tweets$text)

hungry_jack_text <- hungry_jack_tweets$text

corpus_hungry_jack <- Corpus(VectorSource(hungry_jack_tweets$text))

hungry_jack_emoji_clean <- tm_map(corpus_hungry_jack, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_hungry_jack<- tm_map(hungry_jack_emoji_clean, content_transformer(remove_urls))

lower_hungry_jack <- tm_map(url_clean_hungry_jack, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_hungry_jack <- tm_map(lower_hungry_jack, content_transformer(remove_items))

remove_punct_hungry_jack <- tm_map(remove_hungry_jack, removePunctuation)

remove_num_hungry_jack <- tm_map(remove_punct_hungry_jack, removeNumbers)

remove_stop_hungry_jack <- tm_map(remove_num_hungry_jack, removeWords, stopwords("english"))

remove_white_hungry_jack <- tm_map(remove_stop_hungry_jack, stripWhitespace)

remove_words_hungry_jack <- tm_map(remove_white_hungry_jack, removeWords, c("hungry jack", "hungry", "jack", "pancake", "pancakes", "syrup", "syrups", "amp", "drcullenphd", "pancakeday", "let", "put"))

matrix_hungry_jack <- TermDocumentMatrix((remove_words_hungry_jack))
matrix_hungry_jack_2 <- as.matrix(matrix_hungry_jack)
sort_hungry_jack <- sort(rowSums(matrix_hungry_jack_2), decreasing = TRUE)
data_frame_hungry_jack <- data.frame(word = names(sort_hungry_jack), freq = sort_hungry_jack)

top_10_hungry_jack <- data_frame_hungry_jack %>%
  arrange(desc(freq)) %>% head(10) 

hungry_jack_final <- crossing(top_10_hungry_jack, brand = "Hungry Jack")
hungry_jack_final

#Newman's Tweets#
newmans <- c("newmans", "pasta")
newmans_search <- paste(newmans, collapse = " AND ")
newmans_tweets <- search_tweets(q = newmans_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(newmans_tweets)

head(newmans_tweets$text)

newmans_text <- newmans_tweets$text

corpus_newmans <- Corpus(VectorSource(newmans_tweets$text))

newmans_emoji_clean <- tm_map(corpus_newmans, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_newmans<- tm_map(newmans_emoji_clean, content_transformer(remove_urls))

lower_newmans <- tm_map(url_clean_newmans, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_newmans <- tm_map(lower_newmans, content_transformer(remove_items))

remove_punct_newmans <- tm_map(remove_newmans, removePunctuation)

remove_num_newmans <- tm_map(remove_punct_newmans, removeNumbers)

remove_stop_newmans <- tm_map(remove_num_newmans, removeWords, stopwords("english"))

remove_white_newmans <- tm_map(remove_stop_newmans, stripWhitespace)

remove_words_newmans <- tm_map(remove_white_newmans, removeWords, c("newman's", "newman", "newmans", "pasta", "pastas", "amp"))

matrix_newmans <- TermDocumentMatrix((remove_words_newmans))
matrix_newmans_2 <- as.matrix(matrix_newmans)
sort_newmans <- sort(rowSums(matrix_newmans_2), decreasing = TRUE)
data_frame_newmans <- data.frame(word = names(sort_newmans), freq = sort_newmans)

top_10_newmans <- data_frame_newmans %>%
  arrange(desc(freq)) %>% head(10)

#Kraft Tweets#
kraft <- c("kraft", "pasta")
kraft_search <- paste(kraft, collapse = " AND ")
kraft_tweets <- search_tweets(q = kraft_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(kraft_tweets)

head(kraft_tweets$text)

kraft_text <- kraft_tweets$text

corpus_kraft <- Corpus(VectorSource(kraft_tweets$text))

kraft_emoji_clean <- tm_map(corpus_kraft, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_kraft<- tm_map(kraft_emoji_clean, content_transformer(remove_urls))

lower_kraft <- tm_map(url_clean_kraft, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_kraft <- tm_map(lower_kraft, content_transformer(remove_items))

remove_punct_kraft <- tm_map(remove_kraft, removePunctuation)

remove_num_kraft <- tm_map(remove_punct_kraft, removeNumbers)

remove_stop_kraft <- tm_map(remove_num_kraft, removeWords, stopwords("english"))

remove_white_kraft <- tm_map(remove_stop_kraft, stripWhitespace)

remove_words_kraft <- tm_map(remove_white_kraft, removeWords, c("kraft", "krafts", "pasta", "pastas", "amp"))

matrix_kraft <- TermDocumentMatrix((remove_words_kraft))
matrix_kraft_2 <- as.matrix(matrix_kraft)
sort_kraft <- sort(rowSums(matrix_kraft_2), decreasing = TRUE)
data_frame_kraft <- data.frame(word = names(sort_kraft), freq = sort_kraft)

top_10_kraft <- data_frame_kraft %>%
  arrange(desc(freq)) %>% head(10) 

kraft_final <- crossing(top_10_kraft, brand = "Kraft")
kraft_final

#Brier Rabbit Tweets#
brier_rabbit <- c('"brier rabbit"', "syrup")
brier_rabbit_search <- paste(brier_rabbit, collapse = " AND ")
brier_rabbit_tweets <- search_tweets(q = brier_rabbit_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(brier_rabbit_tweets)

head(brier_rabbit_tweets$text)

brier_rabbit_text <- brier_rabbit_tweets$text

corpus_brier_rabbit <- Corpus(VectorSource(brier_rabbit_tweets$text))

brier_rabbit_emoji_clean <- tm_map(corpus_brier_rabbit, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_brier_rabbit<- tm_map(brier_rabbit_emoji_clean, content_transformer(remove_urls))

lower_brier_rabbit <- tm_map(url_clean_brier_rabbit, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

remove_brier_rabbit <- tm_map(lower_brier_rabbit, content_transformer(remove_items))

remove_punct_brier_rabbit <- tm_map(remove_brier_rabbit, removePunctuation)

remove_num_brier_rabbit <- tm_map(remove_punct_brier_rabbit, removeNumbers)

remove_stop_brier_rabbit <- tm_map(remove_num_brier_rabbit, removeWords, stopwords("english"))

remove_white_brier_rabbit <- tm_map(remove_stop_brier_rabbit, stripWhitespace)

remove_words_brier_rabbit <- tm_map(remove_white_brier_rabbit, removeWords, c("brier rabbit", "brier", "rabbit", "rabbits", "brier rabbits", "syrup", "syrups", "amp"))

matrix_brier_rabbit <- TermDocumentMatrix((remove_words_brier_rabbit))
matrix_brier_rabbit_2 <- as.matrix(matrix_brier_rabbit)
sort_brier_rabbit <- sort(rowSums(matrix_brier_rabbit_2), decreasing = TRUE)
data_frame_brier_rabbit <- data.frame(word = names(sort_brier_rabbit), freq = sort_brier_rabbit)

top_10_brier_rabbit <- data_frame_brier_rabbit %>%
  arrange(desc(freq)) %>% head(10)

#Sobrab Bkstrp Tweets#
sobrab_bkstrp <- c('"sobrab bkstrp"', "syrup")
sobrab_bkstrp_search <- paste(sobrab_bkstrp, collapse = " AND ")
sobrab_bkstrp_tweets <- search_tweets(q = sobrab_bkstrp_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(sobrab_bkstrp_tweets)

head(sobrab_bkstrp_tweets$text)

sobrab_bkstrp_text <- sobrab_bkstrp_tweets$text

corpus_sobrab_bkstrp <- Corpus(VectorSource(sobrab_bkstrp_tweets$text))

sobrab_bkstrp_emoji_clean <- tm_map(corpus_sobrab_bkstrp, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_sobrab_bkstrp<- tm_map(sobrab_bkstrp_emoji_clean, content_transformer(remove_urls))

lower_sobrab_bkstrp <- tm_map(url_clean_sobrab_bkstrp, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_sobrab_bkstrp <- tm_map(lower_sobrab_bkstrp, content_transformer(remove_items))

remove_punct_sobrab_bkstrp <- tm_map(remove_sobrab_bkstrp, removePunctuation)

remove_num_sobrab_bkstrp <- tm_map(remove_punct_sobrab_bkstrp, removeNumbers)

remove_stop_sobrab_bkstrp <- tm_map(remove_num_sobrab_bkstrp, removeWords, stopwords("english"))

remove_white_sobrab_bkstrp <- tm_map(remove_stop_sobrab_bkstrp, stripWhitespace)

remove_words_sobrab_bkstrp <- tm_map(remove_white_sobrab_bkstrp, removeWords, c("sobrab bkstrp", "sobrab", "bkstrp", "sobrabs", "bkstrps", "syrup", "syrups", "amp"))

matrix_sobrab_bkstrp <- TermDocumentMatrix((remove_words_sobrab_bkstrp))
matrix_sobrab_bkstrp_2 <- as.matrix(matrix_sobrab_bkstrp)
sort_sobrab_bkstrp <- sort(rowSums(matrix_sobrab_bkstrp_2), decreasing = TRUE)
data_frame_sobrab_bkstrp <- data.frame(word = names(sort_sobrab_bkstrp), freq = sort_sobrab_bkstrp)

top_10_sobrab_bkstrp <- data_frame_sobrab_bkstrp %>%
  arrange(desc(freq)) %>% head(10)

#Dececco Tweets#- check again
dececco <- c("dececco", "pasta")
dececco_search <- paste(dececco, collapse = " AND ")
dececco_tweets <- search_tweets(q = dececco_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(dececco_tweets)

head(dececco_tweets$text)

dececco_text <- dececco_tweets$text

corpus_dececco <- Corpus(VectorSource(dececco_tweets$text))

dececco_emoji_clean <- tm_map(corpus_dececco, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_dececco<- tm_map(dececco_emoji_clean, content_transformer(remove_urls))

lower_dececco <- tm_map(url_clean_dececco, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_dececco <- tm_map(lower_dececco, content_transformer(remove_items))

remove_punct_dececco <- tm_map(remove_dececco, removePunctuation)

remove_num_dececco <- tm_map(remove_punct_dececco, removeNumbers)

remove_stop_dececco <- tm_map(remove_num_dececco, removeWords, stopwords("english"))

remove_white_dececco <- tm_map(remove_stop_dececco, stripWhitespace)

remove_words_dececco <- tm_map(remove_white_dececco, removeWords, c("dececco", "dececcos", "pasta", "pastas", "amp", "dececcopasta", "andreapecchia", "aronoele", "avvermetico", "biondabipolare", "calapaura", "gfdario", "itscloe", "mrestinzione", "pierpirla", "santasofficial", "simonetavelin", "snailale", "socialmentediv", "volpereal"))

matrix_dececco <- TermDocumentMatrix((remove_words_dececco))
matrix_dececco_2 <- as.matrix(matrix_dececco)
sort_dececco <- sort(rowSums(matrix_dececco_2), decreasing = TRUE)
data_frame_dececco <- data.frame(word = names(sort_dececco), freq = sort_dececco)

top_10_dececco <- data_frame_dececco %>%
  arrange(desc(freq)) %>% head(10) 

dececco_final <- crossing(top_10_dececco, brand = "Dececco")
dececco_final

#Eden Tweets#
eden <- c("eden", "pasta")
eden_search <- paste(eden, collapse = " AND ")
eden_tweets <- search_tweets(q = eden_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(eden_tweets)

head(eden_tweets$text)

eden_text <- eden_tweets$text

corpus_eden <- Corpus(VectorSource(eden_tweets$text))

eden_emoji_clean <- tm_map(corpus_eden, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_eden<- tm_map(eden_emoji_clean, content_transformer(remove_urls))

lower_eden <- tm_map(url_clean_eden, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_eden <- tm_map(lower_eden, content_transformer(remove_items))

remove_punct_eden <- tm_map(remove_eden, removePunctuation)

remove_num_eden <- tm_map(remove_punct_eden, removeNumbers)

remove_stop_eden <- tm_map(remove_num_eden, removeWords, stopwords("english"))

remove_white_eden <- tm_map(remove_stop_eden, stripWhitespace)

remove_words_eden <- tm_map(remove_white_eden, removeWords, c("eden", "edens", "pasta", "pastas", "amp"))

matrix_eden <- TermDocumentMatrix((remove_words_eden))
matrix_eden_2 <- as.matrix(matrix_eden)
sort_eden <- sort(rowSums(matrix_eden_2), decreasing = TRUE)
data_frame_eden <- data.frame(word = names(sort_eden), freq = sort_eden)

top_10_eden <- data_frame_eden %>%
  arrange(desc(freq)) %>% head(10) 

eden_final <- crossing(top_10_eden, brand = "Eden")
eden_final

#Pomi Tweets#
pomi <- c("pomi", "pasta")
pomi_search <- paste(pomi, collapse = " AND ")
pomi_tweets <- search_tweets(q = pomi_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(pomi_tweets)

head(pomi_tweets$text)

pomi_text <- pomi_tweets$text

corpus_pomi <- Corpus(VectorSource(pomi_tweets$text))

pomi_emoji_clean <- tm_map(corpus_pomi, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_pomi<- tm_map(pomi_emoji_clean, content_transformer(remove_urls))

lower_pomi <- tm_map(url_clean_pomi, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_pomi <- tm_map(lower_pomi, content_transformer(remove_items))

remove_punct_pomi <- tm_map(remove_pomi, removePunctuation)

remove_num_pomi <- tm_map(remove_punct_pomi, removeNumbers)

remove_stop_pomi <- tm_map(remove_num_pomi, removeWords, stopwords("english"))

remove_white_pomi <- tm_map(remove_stop_pomi, stripWhitespace)

remove_words_pomi <- tm_map(remove_white_pomi, removeWords, c("pomi", "pomis", "pasta", "pastas", "amp"))

matrix_pomi <- TermDocumentMatrix((remove_words_pomi))
matrix_pomi_2 <- as.matrix(matrix_pomi)
sort_pomi <- sort(rowSums(matrix_pomi_2), decreasing = TRUE)
data_frame_pomi <- data.frame(word = names(sort_pomi), freq = sort_pomi)

top_10_pomi <- data_frame_pomi %>%
  arrange(desc(freq)) %>% head(10)

pomi_final <- crossing(top_10_pomi, brand = "Pomi")
pomi_final

#Annarino Tweets#
annarino <- c("annarino", "pasta")
annarino_search <- paste(annarino, collapse = " AND ")
annarino_tweets <- search_tweets(q = annarino_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(annarino_tweets)

head(annarino_tweets$text)

annarino_text <- annarino_tweets$text

corpus_annarino <- Corpus(VectorSource(annarino_tweets$text))

annarino_emoji_clean <- tm_map(corpus_annarino, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_annarino<- tm_map(annarino_emoji_clean, content_transformer(remove_urls))

lower_annarino <- tm_map(url_clean_annarino, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_annarino <- tm_map(lower_annarino, content_transformer(remove_items))

remove_punct_annarino <- tm_map(remove_annarino, removePunctuation)

remove_num_annarino <- tm_map(remove_punct_annarino, removeNumbers)

remove_stop_annarino <- tm_map(remove_num_annarino, removeWords, stopwords("english"))

remove_white_annarino <- tm_map(remove_stop_annarino, stripWhitespace)

remove_words_annarino <- tm_map(remove_white_annarino, removeWords, c("anna", "annarino", "annarinos", "rino", "rinos", "pasta", "pastas", "amp"))

matrix_annarino <- TermDocumentMatrix((remove_words_annarino))
matrix_annarino_2 <- as.matrix(matrix_annarino)
sort_annarino <- sort(rowSums(matrix_annarino_2), decreasing = TRUE)
data_frame_annarino <- data.frame(word = names(sort_annarino), freq = sort_annarino)

top_10_annarino <- data_frame_annarino %>%
  arrange(desc(freq)) %>% head(10)

#La Russa Tweets#
la_russa <- c("la russa", "pasta")
la_russa_search <- paste(la_russa, collapse = " AND ")
la_russa_tweets <- search_tweets(q = la_russa_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(la_russa_tweets)

head(la_russa_tweets$text)

la_russa_text <- la_russa_tweets$text

corpus_la_russa <- Corpus(VectorSource(la_russa_tweets$text))

la_russa_emoji_clean <- tm_map(corpus_la_russa, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_la_russa<- tm_map(la_russa_emoji_clean, content_transformer(remove_urls))

lower_la_russa <- tm_map(url_clean_la_russa, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_la_russa <- tm_map(lower_la_russa, content_transformer(remove_items))

remove_punct_la_russa <- tm_map(remove_la_russa, removePunctuation)

remove_num_la_russa <- tm_map(remove_punct_la_russa, removeNumbers)

remove_stop_la_russa <- tm_map(remove_num_la_russa, removeWords, stopwords("english"))

remove_white_la_russa <- tm_map(remove_stop_la_russa, stripWhitespace)

remove_words_la_russa <- tm_map(remove_white_la_russa, removeWords, c("la russa", "la", "russa", "la russas", "pasta", "pastas", "amp"))

matrix_la_russa <- TermDocumentMatrix((remove_words_la_russa))
matrix_la_russa_2 <- as.matrix(matrix_la_russa)
sort_la_russa <- sort(rowSums(matrix_la_russa_2), decreasing = TRUE)
data_frame_la_russa <- data.frame(word = names(sort_la_russa), freq = sort_la_russa)

top_10_la_russa <- data_frame_la_russa %>%
  arrange(desc(freq)) %>% head(10)

#Hunt's Tweets# - check again
hunts <- c("hunts", "pasta")
hunts_search <- paste(hunts, collapse = " AND ")
hunts_tweets <- search_tweets(q = hunts_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(hunts_tweets)

head(hunts_tweets$text)

hunts_text <- hunts_tweets$text

corpus_hunts <- Corpus(VectorSource(hunts_tweets$text))

hunts_emoji_clean <- tm_map(corpus_hunts, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_hunts<- tm_map(hunts_emoji_clean, content_transformer(remove_urls))

lower_hunts <- tm_map(url_clean_hunts, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_hunts <- tm_map(lower_hunts, content_transformer(remove_items))

remove_punct_hunts <- tm_map(remove_hunts, removePunctuation)

remove_num_hunts <- tm_map(remove_punct_hunts, removeNumbers)

remove_stop_hunts <- tm_map(remove_num_hunts, removeWords, stopwords("english"))

remove_white_hunts <- tm_map(remove_stop_hunts, stripWhitespace)

remove_words_hunts <- tm_map(remove_white_hunts, removeWords, c("hunt's", "hunts", "hunt", "pasta", "pastas", "amp", "musfyafucu"))

matrix_hunts <- TermDocumentMatrix((remove_words_hunts))
matrix_hunts_2 <- as.matrix(matrix_hunts)
sort_hunts <- sort(rowSums(matrix_hunts_2), decreasing = TRUE)
data_frame_hunts <- data.frame(word = names(sort_hunts), freq = sort_hunts)

top_10_hunts <- data_frame_hunts %>%
  arrange(desc(freq)) %>% head(10) 

hunts_final <- crossing(top_10_hunts, brand = "Hunt's")
hunts_final

#Vita Tweets# -check again
vita <- c("vita", "pasta")
vita_search <- paste(vita, collapse = " AND ")
vita_tweets <- search_tweets(q = vita_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(vita_tweets)

head(vita_tweets$text)

vita_text <- vita_tweets$text

corpus_vita <- Corpus(VectorSource(vita_tweets$text))

vita_emoji_clean <- tm_map(corpus_vita, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_vita<- tm_map(vita_emoji_clean, content_transformer(remove_urls))

lower_vita <- tm_map(url_clean_vita, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_vita <- tm_map(lower_vita, content_transformer(remove_items))

remove_punct_vita <- tm_map(remove_vita, removePunctuation)

remove_num_vita <- tm_map(remove_punct_vita, removeNumbers)

remove_stop_vita <- tm_map(remove_num_vita, removeWords, stopwords("english"))

remove_white_vita <- tm_map(remove_stop_vita, stripWhitespace)

remove_words_vita <- tm_map(remove_white_vita, removeWords, c("vita", "vitas", "pasta", "pastas", "amp", "cookingcarafes", "filippoberio", "sharaibravo", "signorellildn"))

matrix_vita <- TermDocumentMatrix((remove_words_vita))
matrix_vita_2 <- as.matrix(matrix_vita)
sort_vita <- sort(rowSums(matrix_vita_2), decreasing = TRUE)
data_frame_vita <- data.frame(word = names(sort_vita), freq = sort_vita)

top_10_vita <- data_frame_vita %>%
  arrange(desc(freq)) %>% head(10) 

vita_final <- crossing(top_10_vita, brand = "Vita")
vita_final

#Maggi Spaetzle Tweets#
maggi_spaetzle <- c('"maggi spaetzle"', "pasta")
maggi_spaetzle_search <- paste(maggi_spaetzle, collapse = " AND ")
maggi_spaetzle_tweets <- search_tweets(q = maggi_spaetzle_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(maggi_spaetzle_tweets)

head(maggi_spaetzle_tweets$text)

maggi_spaetzle_text <- maggi_spaetzle_tweets$text

corpus_maggi_spaetzle <- Corpus(VectorSource(maggi_spaetzle_tweets$text))

maggi_spaetzle_emoji_clean <- tm_map(corpus_maggi_spaetzle, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_maggi_spaetzle<- tm_map(maggi_spaetzle_emoji_clean, content_transformer(remove_urls))

lower_maggi_spaetzle <- tm_map(url_clean_maggi_spaetzle, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_maggi_spaetzle <- tm_map(lower_maggi_spaetzle, content_transformer(remove_items))

remove_punct_maggi_spaetzle <- tm_map(remove_maggi_spaetzle, removePunctuation)

remove_num_maggi_spaetzle <- tm_map(remove_punct_maggi_spaetzle, removeNumbers)

remove_stop_maggi_spaetzle <- tm_map(remove_num_maggi_spaetzle, removeWords, stopwords("english"))

remove_white_maggi_spaetzle <- tm_map(remove_stop_maggi_spaetzle, stripWhitespace)

remove_words_maggi_spaetzle <- tm_map(remove_white_maggi_spaetzle, removeWords, c("maggi spaetzle", "maggi", "spaetzle", "maggi spaetzles", "pasta", "pastas", "amp"))

matrix_maggi_spaetzle <- TermDocumentMatrix((remove_words_maggi_spaetzle))
matrix_maggi_spaetzle_2 <- as.matrix(matrix_maggi_spaetzle)
sort_maggi_spaetzle <- sort(rowSums(matrix_maggi_spaetzle_2), decreasing = TRUE)
data_frame_maggi_spaetzle <- data.frame(word = names(sort_maggi_spaetzle), freq = sort_maggi_spaetzle)

top_10_maggi_spaetzle <- data_frame_maggi_spaetzle %>%
  arrange(desc(freq)) %>% head(10)

#Mother's Tweets# -check again
mothers <- c("mother's", "pasta")
mothers_search <- paste(mothers, collapse = " AND ")
mothers_tweets <- search_tweets(q = mothers_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(mothers_tweets)

head(mothers_tweets$text)

mothers_text <- mothers_tweets$text
mothers_text

corpus_mothers <- Corpus(VectorSource(mothers_tweets$text))

mothers_emoji_clean <- tm_map(corpus_mothers, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_mothers<- tm_map(mothers_emoji_clean, content_transformer(remove_urls))

lower_mothers <- tm_map(url_clean_mothers, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_mothers <- tm_map(lower_mothers, content_transformer(remove_items))

remove_punct_mothers <- tm_map(remove_mothers, removePunctuation)

remove_num_mothers <- tm_map(remove_punct_mothers, removeNumbers)

remove_stop_mothers <- tm_map(remove_num_mothers, removeWords, stopwords("english"))

remove_white_mothers <- tm_map(remove_stop_mothers, stripWhitespace)

remove_words_mothers <- tm_map(remove_white_mothers, removeWords, c("mother's", "mothers", "mother", "mom", "moms", "pasta", "pastas", "amp"))

matrix_mothers <- TermDocumentMatrix((remove_words_mothers))
matrix_mothers_2 <- as.matrix(matrix_mothers)
sort_mothers <- sort(rowSums(matrix_mothers_2), decreasing = TRUE)
data_frame_mothers <- data.frame(word = names(sort_mothers), freq = sort_mothers)

top_10_mothers <- data_frame_mothers %>%
  arrange(desc(freq)) %>% head(10) 

mothers_final <- crossing(top_10_mothers, brand = "Mother's")
mothers_final

#Mueller Tweets#
mueller <- c("mueller", "pasta")
mueller_search <- paste(mueller, collapse = " AND ")
mueller_tweets <- search_tweets(q = mueller_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(mueller_tweets)

head(mueller_tweets$text)

mueller_text <- mueller_tweets$text

corpus_mueller <- Corpus(VectorSource(mueller_tweets$text))

mueller_emoji_clean <- tm_map(corpus_mueller, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_mueller<- tm_map(mueller_emoji_clean, content_transformer(remove_urls))

lower_mueller <- tm_map(url_clean_mueller, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_mueller <- tm_map(lower_mueller, content_transformer(remove_items))

remove_punct_mueller <- tm_map(remove_mueller, removePunctuation)

remove_num_mueller <- tm_map(remove_punct_mueller, removeNumbers)

remove_stop_mueller <- tm_map(remove_num_mueller, removeWords, stopwords("english"))

remove_white_mueller <- tm_map(remove_stop_mueller, stripWhitespace)

remove_words_mueller <- tm_map(remove_white_mueller, removeWords, c("mueller", "muellers", "pasta", "pastas", "amp", "etc"))

matrix_mueller <- TermDocumentMatrix((remove_words_mueller))
matrix_mueller_2 <- as.matrix(matrix_mueller)
sort_mueller <- sort(rowSums(matrix_mueller_2), decreasing = TRUE)
data_frame_mueller <- data.frame(word = names(sort_mueller), freq = sort_mueller)

top_10_mueller <- data_frame_mueller %>%
  arrange(desc(freq)) %>% head(10)

mueller_final <- crossing(top_10_mueller, brand = "Mueller")
mueller_final

#La Moderna Tweets#
la_moderna <- c('"la moderna"', "pasta")
la_moderna_search <- paste(la_moderna, collapse = " AND ")
la_moderna_tweets <- search_tweets(q = la_moderna_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(la_moderna_tweets)

head(la_moderna_tweets$text)

la_moderna_text <- la_moderna_tweets$text

corpus_la_moderna <- Corpus(VectorSource(la_moderna_tweets$text))

la_moderna_emoji_clean <- tm_map(corpus_la_moderna, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_la_moderna<- tm_map(la_moderna_emoji_clean, content_transformer(remove_urls))

lower_la_moderna <- tm_map(url_clean_la_moderna, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_la_moderna <- tm_map(lower_la_moderna, content_transformer(remove_items))

remove_punct_la_moderna <- tm_map(remove_la_moderna, removePunctuation)

remove_num_la_moderna <- tm_map(remove_punct_la_moderna, removeNumbers)

remove_stop_la_moderna <- tm_map(remove_num_la_moderna, removeWords, stopwords("english"))

remove_white_la_moderna <- tm_map(remove_stop_la_moderna, stripWhitespace)

remove_words_la_moderna <- tm_map(remove_white_la_moderna, removeWords, c("la moderna", "la", "moderna", "la modernas", "modern", "pasta", "pastas", "amp", "also", "become", "krqjggnq"))

matrix_la_moderna <- TermDocumentMatrix((remove_words_la_moderna))
matrix_la_moderna_2 <- as.matrix(matrix_la_moderna)
sort_la_moderna <- sort(rowSums(matrix_la_moderna_2), decreasing = TRUE)
data_frame_la_moderna <- data.frame(word = names(sort_la_moderna), freq = sort_la_moderna)

top_10_la_moderna <- data_frame_la_moderna %>%
  arrange(desc(freq)) %>% head(10)

la_moderna_final <- crossing(top_10_la_moderna, brand = "La Moderna")
la_moderna_final

#Aunt Jemima Tweets#
aunt_jemima <- c("pancake", "syrup")
aunt_jemima_combine <- paste(aunt_jemima, collapse = " OR ")
aunt_jemima_2 <- c('"aunt jemima"', aunt_jemima_combine)
aunt_jemima_search <- paste(aunt_jemima_2, collapse = " AND ")
aunt_jemima_tweets <- search_tweets(q = aunt_jemima_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(aunt_jemima_tweets)

head(aunt_jemima_tweets$text)

aunt_jemima_text <- aunt_jemima_tweets$text

corpus_aunt_jemima <- Corpus(VectorSource(aunt_jemima_tweets$text))

aunt_jemima_emoji_clean <- tm_map(corpus_aunt_jemima, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_aunt_jemima<- tm_map(aunt_jemima_emoji_clean, content_transformer(remove_urls))

lower_aunt_jemima <- tm_map(url_clean_aunt_jemima, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_aunt_jemima <- tm_map(lower_aunt_jemima, content_transformer(remove_items))

remove_punct_aunt_jemima <- tm_map(remove_aunt_jemima, removePunctuation)

remove_num_aunt_jemima <- tm_map(remove_punct_aunt_jemima, removeNumbers)

remove_stop_aunt_jemima <- tm_map(remove_num_aunt_jemima, removeWords, stopwords("english"))

remove_white_aunt_jemima <- tm_map(remove_stop_aunt_jemima, stripWhitespace)

remove_words_aunt_jemima <- tm_map(remove_white_aunt_jemima, removeWords, c("aunt jemima", "aunt", "jemima", "aunts", "aunt jemimas", "pancake", "pancakes", "syrup", "syrups", "amp"))

matrix_aunt_jemima <- TermDocumentMatrix((remove_words_aunt_jemima))
matrix_aunt_jemima_2 <- as.matrix(matrix_aunt_jemima)
sort_aunt_jemima <- sort(rowSums(matrix_aunt_jemima_2), decreasing = TRUE)
data_frame_aunt_jemima <- data.frame(word = names(sort_aunt_jemima), freq = sort_aunt_jemima)

top_10_aunt_jemima <- data_frame_aunt_jemima %>%
  arrange(desc(freq)) %>% head(10)

aunt_jemima_final <- crossing(top_10_aunt_jemima, brand = "Aunt Jemima")
aunt_jemima_final

#Tree of Life Tweets#
tree_of_life <- c('"tree of life"', "syrup")
tree_of_life_search <- paste(tree_of_life, collapse = " AND ")
tree_of_life_tweets <- search_tweets(q = tree_of_life_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(tree_of_life_tweets)

head(tree_of_life_tweets$text)

tree_of_life_text <- tree_of_life_tweets$text

corpus_tree_of_life <- Corpus(VectorSource(tree_of_life_tweets$text))

tree_of_life_emoji_clean <- tm_map(corpus_tree_of_life, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_tree_of_life<- tm_map(tree_of_life_emoji_clean, content_transformer(remove_urls))

lower_tree_of_life <- tm_map(url_clean_tree_of_life, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_tree_of_life <- tm_map(lower_tree_of_life, content_transformer(remove_items))

remove_punct_tree_of_life <- tm_map(remove_tree_of_life, removePunctuation)

remove_num_tree_of_life <- tm_map(remove_punct_tree_of_life, removeNumbers)

remove_stop_tree_of_life <- tm_map(remove_num_tree_of_life, removeWords, stopwords("english"))

remove_white_tree_of_life <- tm_map(remove_stop_tree_of_life, stripWhitespace)

remove_words_tree_of_life <- tm_map(remove_white_tree_of_life, removeWords, c("tree of life", "tree", "of", "life", "trees", "lives", "syrup", "syrups", "amp", "let"))

matrix_tree_of_life <- TermDocumentMatrix((remove_words_tree_of_life))
matrix_tree_of_life_2 <- as.matrix(matrix_tree_of_life)
sort_tree_of_life <- sort(rowSums(matrix_tree_of_life_2), decreasing = TRUE)
data_frame_tree_of_life <- data.frame(word = names(sort_tree_of_life), freq = sort_tree_of_life)

top_10_tree_of_life <- data_frame_tree_of_life %>%
  arrange(desc(freq)) %>% head(10)

tree_of_life_final <- crossing(top_10_tree_of_life, brand = "Tree of Life")
tree_of_life_final

#Fifty 50 Tweets#
fifty_fifty <- c('"fifty 50"', "syrup")
fifty_fifty_search <- paste(fifty_fifty, collapse = " AND ")
fifty_fifty_tweets <- search_tweets(q = fifty_fifty_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(fifty_fifty_tweets)

head(fifty_fifty_tweets$text)

fifty_fifty_text <- fifty_fifty_tweets$text

corpus_fifty_fifty <- Corpus(VectorSource(fifty_fifty_tweets$text))

fifty_fifty_emoji_clean <- tm_map(corpus_fifty_fifty, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_fifty_fifty<- tm_map(fifty_fifty_emoji_clean, content_transformer(remove_urls))

lower_fifty_fifty <- tm_map(url_clean_fifty_fifty, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_fifty_fifty <- tm_map(lower_fifty_fifty, content_transformer(remove_items))

remove_punct_fifty_fifty <- tm_map(remove_fifty_fifty, removePunctuation)

remove_num_fifty_fifty <- tm_map(remove_punct_fifty_fifty, removeNumbers)

remove_stop_fifty_fifty <- tm_map(remove_num_fifty_fifty, removeWords, stopwords("english"))

remove_white_fifty_fifty <- tm_map(remove_stop_fifty_fifty, stripWhitespace)

remove_words_fifty_fifty <- tm_map(remove_white_fifty_fifty, removeWords, c("fifty 50", "fifty", "fifties", "50", "syrup", "syrups", "amp"))
matrix_fifty_fifty <- TermDocumentMatrix((remove_words_fifty_fifty))
matrix_fifty_fifty_2 <- as.matrix(matrix_fifty_fifty)
sort_fifty_fifty <- sort(rowSums(matrix_fifty_fifty_2), decreasing = TRUE)
data_frame_fifty_fifty <- data.frame(word = names(sort_fifty_fifty), freq = sort_fifty_fifty)

top_10_fifty_fifty <- data_frame_fifty_fifty %>%
  arrange(desc(freq)) %>% head(10)

#White Lily Tweets#
white_lily <- c('"white lily"', "pancake")
white_lily_search <- paste(white_lily, collapse = " AND ")
white_lily_tweets <- search_tweets(q = white_lily_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(white_lily_tweets)

head(white_lily_tweets$text)

white_lily_text <- white_lily_tweets$text

corpus_white_lily <- Corpus(VectorSource(white_lily_tweets$text))

white_lily_emoji_clean <- tm_map(corpus_white_lily, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_white_lily<- tm_map(white_lily_emoji_clean, content_transformer(remove_urls))

lower_white_lily <- tm_map(url_clean_white_lily, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_white_lily <- tm_map(lower_white_lily, content_transformer(remove_items))

remove_punct_white_lily <- tm_map(remove_white_lily, removePunctuation)

remove_num_white_lily <- tm_map(remove_punct_white_lily, removeNumbers)

remove_stop_white_lily <- tm_map(remove_num_white_lily, removeWords, stopwords("english"))

remove_white_white_lily <- tm_map(remove_stop_white_lily, stripWhitespace)

remove_words_white_lily <- tm_map(remove_white_white_lily, removeWords, c("white lily", "white", "whites", "lily", "lilies", "pancake", "pancakes", "amp"))

matrix_white_lily <- TermDocumentMatrix((remove_words_white_lily))
matrix_white_lily_2 <- as.matrix(matrix_white_lily)
sort_white_lily <- sort(rowSums(matrix_white_lily_2), decreasing = TRUE)
data_frame_white_lily <- data.frame(word = names(sort_white_lily), freq = sort_white_lily)

top_10_white_lily <- data_frame_white_lily %>%
  arrange(desc(freq)) %>% head(10)

#Sugar Buster Tweets#
sugar_buster <- c('"sugar buster"', "pasta")
sugar_buster_search <- paste(sugar_buster, collapse = " AND ")
sugar_buster_tweets <- search_tweets(q = sugar_buster_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(sugar_buster_tweets)

head(sugar_buster_tweets$text)

sugar_buster_text <- sugar_buster_tweets$text

corpus_sugar_buster <- Corpus(VectorSource(sugar_buster_tweets$text))

sugar_buster_emoji_clean <- tm_map(corpus_sugar_buster, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_sugar_buster<- tm_map(sugar_buster_emoji_clean, content_transformer(remove_urls))

lower_sugar_buster <- tm_map(url_clean_sugar_buster, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_sugar_buster <- tm_map(lower_sugar_buster, content_transformer(remove_items))

remove_punct_sugar_buster <- tm_map(remove_sugar_buster, removePunctuation)

remove_num_sugar_buster <- tm_map(remove_punct_sugar_buster, removeNumbers)

remove_stop_sugar_buster <- tm_map(remove_num_sugar_buster, removeWords, stopwords("english"))

remove_white_sugar_buster <- tm_map(remove_stop_sugar_buster, stripWhitespace)

remove_words_sugar_buster <- tm_map(remove_white_sugar_buster, removeWords, c("sugar buster", "sugar", "sugars", "buster", "busters", "pasta", "pastas", "amp"))

matrix_sugar_buster <- TermDocumentMatrix((remove_words_sugar_buster))
matrix_sugar_buster_2 <- as.matrix(matrix_sugar_buster)
sort_sugar_buster <- sort(rowSums(matrix_sugar_buster_2), decreasing = TRUE)
data_frame_sugar_buster <- data.frame(word = names(sort_sugar_buster), freq = sort_sugar_buster)

top_10_sugar_buster <- data_frame_sugar_buster %>%
  arrange(desc(freq)) %>% head(10)

#Ronzoni Tweets#
ronzoni <- c("ronzoni", "pasta")
ronzoni_search <- paste(ronzoni, collapse = " AND ")
ronzoni_tweets <- search_tweets(q = ronzoni_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(ronzoni_tweets)

head(ronzoni_tweets$text)

ronzoni_text <- ronzoni_tweets$text

corpus_ronzoni <- Corpus(VectorSource(ronzoni_tweets$text))

ronzoni_emoji_clean <- tm_map(corpus_ronzoni, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_ronzoni<- tm_map(ronzoni_emoji_clean, content_transformer(remove_urls))

lower_ronzoni <- tm_map(url_clean_ronzoni, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_ronzoni <- tm_map(lower_ronzoni, content_transformer(remove_items))

remove_punct_ronzoni <- tm_map(remove_ronzoni, removePunctuation)

remove_num_ronzoni <- tm_map(remove_punct_ronzoni, removeNumbers)

remove_stop_ronzoni <- tm_map(remove_num_ronzoni, removeWords, stopwords("english"))

remove_white_ronzoni <- tm_map(remove_stop_ronzoni, stripWhitespace)

remove_words_ronzoni <- tm_map(remove_white_ronzoni, removeWords, c("ronzoni", "ronzonis", "ron", "pasta", "pastas", "amp", "jxxffeayoz"))

matrix_ronzoni <- TermDocumentMatrix((remove_words_ronzoni))
matrix_ronzoni_2 <- as.matrix(matrix_ronzoni)
sort_ronzoni <- sort(rowSums(matrix_ronzoni_2), decreasing = TRUE)
data_frame_ronzoni <- data.frame(word = names(sort_ronzoni), freq = sort_ronzoni)

top_10_ronzoni <- data_frame_ronzoni %>%
  arrange(desc(freq)) %>% head(10)

ronzoni_final <- crossing(top_10_ronzoni, brand = "Ronzoni")
ronzoni_final

#San Giorgio Tweets#
san_giorgio <- c('"san giorgio"', "pasta")
san_giorgio_search <- paste(san_giorgio, collapse = " AND ")
san_giorgio_tweets <- search_tweets(q = san_giorgio_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(san_giorgio_tweets)

head(san_giorgio_tweets$text)

san_giorgio_text <- san_giorgio_tweets$text

corpus_san_giorgio <- Corpus(VectorSource(san_giorgio_tweets$text))

san_giorgio_emoji_clean <- tm_map(corpus_san_giorgio, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_san_giorgio<- tm_map(san_giorgio_emoji_clean, content_transformer(remove_urls))

lower_san_giorgio <- tm_map(url_clean_san_giorgio, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_san_giorgio <- tm_map(lower_san_giorgio, content_transformer(remove_items))

remove_punct_san_giorgio <- tm_map(remove_san_giorgio, removePunctuation)

remove_num_san_giorgio <- tm_map(remove_punct_san_giorgio, removeNumbers)

remove_stop_san_giorgio <- tm_map(remove_num_san_giorgio, removeWords, stopwords("english"))

remove_white_san_giorgio <- tm_map(remove_stop_san_giorgio, stripWhitespace)

remove_words_san_giorgio <- tm_map(remove_white_san_giorgio, removeWords, c("san giorgio", "san", "giorgio", "giorgios", "pasta", "pastas", "amp", "grazie"))

matrix_san_giorgio <- TermDocumentMatrix((remove_words_san_giorgio))
matrix_san_giorgio_2 <- as.matrix(matrix_san_giorgio)
sort_san_giorgio <- sort(rowSums(matrix_san_giorgio_2), decreasing = TRUE)
data_frame_san_giorgio <- data.frame(word = names(sort_san_giorgio), freq = sort_san_giorgio)

top_10_san_giorgio <- data_frame_san_giorgio %>%
  arrange(desc(freq)) %>% head(10)

san_giorgio_final <- crossing(top_10_san_giorgio, brand = "San Giorgio")
san_giorgio_final

#China Mandarin Tweets#
china_mandarin <- c('"china mandarin"', "pasta")
china_mandarin_search <- paste(china_mandarin, collapse = " AND ")
china_mandarin_tweets <- search_tweets(q = china_mandarin_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(china_mandarin_tweets)

head(china_mandarin_tweets$text)

china_mandarin_text <- china_mandarin_tweets$text

corpus_china_mandarin <- Corpus(VectorSource(china_mandarin_tweets$text))

china_mandarin_emoji_clean <- tm_map(corpus_china_mandarin, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_china_mandarin<- tm_map(china_mandarin_emoji_clean, content_transformer(remove_urls))

lower_china_mandarin <- tm_map(url_clean_china_mandarin, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_china_mandarin <- tm_map(lower_china_mandarin, content_transformer(remove_items))

remove_punct_china_mandarin <- tm_map(remove_china_mandarin, removePunctuation)

remove_num_china_mandarin <- tm_map(remove_punct_china_mandarin, removeNumbers)

remove_stop_china_mandarin <- tm_map(remove_num_china_mandarin, removeWords, stopwords("english"))

remove_white_china_mandarin <- tm_map(remove_stop_china_mandarin, stripWhitespace)

remove_words_china_mandarin <- tm_map(remove_white_china_mandarin, removeWords, c("china mandarin", "china", "mandarin", "chinas", "mandarins", "pasta", "pastas", "amp"))

matrix_china_mandarin <- TermDocumentMatrix((remove_words_china_mandarin))
matrix_china_mandarin_2 <- as.matrix(matrix_china_mandarin)
sort_china_mandarin <- sort(rowSums(matrix_china_mandarin_2), decreasing = TRUE)
data_frame_china_mandarin <- data.frame(word = names(sort_china_mandarin), freq = sort_china_mandarin)

top_10_china_mandarin <- data_frame_china_mandarin %>%
  arrange(desc(freq)) %>% head(10)

#Ragu Tweets#
ragu <- c("ragu", "pasta")
ragu_search <- paste(ragu, collapse = " AND ")
ragu_tweets <- search_tweets(q = ragu_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(ragu_tweets)

head(ragu_tweets$text)

ragu_text <- ragu_tweets$text

corpus_ragu <- Corpus(VectorSource(ragu_tweets$text))

ragu_emoji_clean <- tm_map(corpus_ragu, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_ragu<- tm_map(ragu_emoji_clean, content_transformer(remove_urls))

lower_ragu <- tm_map(url_clean_ragu, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_ragu <- tm_map(lower_ragu, content_transformer(remove_items))

remove_punct_ragu <- tm_map(remove_ragu, removePunctuation)

remove_num_ragu <- tm_map(remove_punct_ragu, removeNumbers)

remove_stop_ragu <- tm_map(remove_num_ragu, removeWords, stopwords("english"))

remove_white_ragu <- tm_map(remove_stop_ragu, stripWhitespace)

remove_words_ragu <- tm_map(remove_white_ragu, removeWords, c("ragu", "ragus", "rag", "pasta", "pastas", "amp", "pappardelle", "ragù", "tagliatelle"))

matrix_ragu <- TermDocumentMatrix((remove_words_ragu))
matrix_ragu_2 <- as.matrix(matrix_ragu)
sort_ragu <- sort(rowSums(matrix_ragu_2), decreasing = TRUE)
data_frame_ragu <- data.frame(word = names(sort_ragu), freq = sort_ragu)

top_10_ragu <- data_frame_ragu %>%
  arrange(desc(freq)) %>% head(10)

ragu_final <- crossing(top_10_ragu, brand = "Ragu")
ragu_final

#Bertolli Tweets#
bertolli <- c("bertolli", "pasta")
bertolli_search <- paste(bertolli, collapse = " AND ")
bertolli_tweets <- search_tweets(q = bertolli_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(bertolli_tweets)

head(bertolli_tweets$text)

bertolli_text <- bertolli_tweets$text

corpus_bertolli <- Corpus(VectorSource(bertolli_tweets$text))

bertolli_emoji_clean <- tm_map(corpus_bertolli, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_bertolli<- tm_map(bertolli_emoji_clean, content_transformer(remove_urls))

lower_bertolli <- tm_map(url_clean_bertolli, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_bertolli <- tm_map(lower_bertolli, content_transformer(remove_items))

remove_punct_bertolli <- tm_map(remove_bertolli, removePunctuation)

remove_num_bertolli <- tm_map(remove_punct_bertolli, removeNumbers)

remove_stop_bertolli <- tm_map(remove_num_bertolli, removeWords, stopwords("english"))

remove_white_bertolli <- tm_map(remove_stop_bertolli, stripWhitespace)

remove_words_bertolli <- tm_map(remove_white_bertolli, removeWords, c("bertolli", "bertollis", "pasta", "pastas", "amp"))

matrix_bertolli <- TermDocumentMatrix((remove_words_bertolli))
matrix_bertolli_2 <- as.matrix(matrix_bertolli)
sort_bertolli <- sort(rowSums(matrix_bertolli_2), decreasing = TRUE)
data_frame_bertolli <- data.frame(word = names(sort_bertolli), freq = sort_bertolli)

top_10_bertolli <- data_frame_bertolli %>%
  arrange(desc(freq)) %>% head(10)

bertolli_final <- crossing(top_10_bertolli, brand = "Bertolli")
bertolli_final

#Farm Style Tweets#
farm_style <- c('"farm style"', "pasta")
farm_style_search <- paste(farm_style, collapse = " AND ")
farm_style_tweets <- search_tweets(q = farm_style_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(farm_style_tweets)

head(farm_style_tweets$text)

farm_style_text <- farm_style_tweets$text

corpus_farm_style <- Corpus(VectorSource(farm_style_tweets$text))

farm_style_emoji_clean <- tm_map(corpus_farm_style, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_farm_style<- tm_map(farm_style_emoji_clean, content_transformer(remove_urls))

lower_farm_style <- tm_map(url_clean_farm_style, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_farm_style <- tm_map(lower_farm_style, content_transformer(remove_items))

remove_punct_farm_style <- tm_map(remove_farm_style, removePunctuation)

remove_num_farm_style <- tm_map(remove_punct_farm_style, removeNumbers)

remove_stop_farm_style <- tm_map(remove_num_farm_style, removeWords, stopwords("english"))

remove_white_farm_style <- tm_map(remove_stop_farm_style, stripWhitespace)

remove_words_farm_style <- tm_map(remove_white_farm_style, removeWords, c("farm style", "farm", "farms", "style", "styles", "pasta", "pastas", "amp"))

matrix_farm_style <- TermDocumentMatrix((remove_words_farm_style))
matrix_farm_style_2 <- as.matrix(matrix_farm_style)
sort_farm_style <- sort(rowSums(matrix_farm_style_2), decreasing = TRUE)
data_frame_farm_style <- data.frame(word = names(sort_farm_style), freq = sort_farm_style)

top_10_farm_style <- data_frame_farm_style %>%
  arrange(desc(freq)) %>% head(10)

#DaVinci Tweets#
davinci <- c("pasta", "syrup")
davinci_combine <- paste(davinci, collapse = " OR ")
davinci_2 <- c("davinci", davinci_combine)
davinci_search <- paste(davinci_2, collapse = " AND ")
davinci_tweets <- search_tweets(q = davinci_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(davinci_tweets)

head(davinci_tweets$text)

davinci_text <- davinci_tweets$text

corpus_davinci <- Corpus(VectorSource(davinci_tweets$text))

davinci_emoji_clean <- tm_map(corpus_davinci, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_davinci<- tm_map(davinci_emoji_clean, content_transformer(remove_urls))

lower_davinci <- tm_map(url_clean_davinci, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_davinci <- tm_map(lower_davinci, content_transformer(remove_items))

remove_punct_davinci <- tm_map(remove_davinci, removePunctuation)

remove_num_davinci <- tm_map(remove_punct_davinci, removeNumbers)

remove_stop_davinci <- tm_map(remove_num_davinci, removeWords, stopwords("english"))

remove_white_davinci <- tm_map(remove_stop_davinci, stripWhitespace)

remove_words_davinci <- tm_map(remove_white_davinci, removeWords, c("davinci", "davincis", "pasta", "pastas", "syrup", "syrups", "amp"))

matrix_davinci <- TermDocumentMatrix((remove_words_davinci))
matrix_davinci_2 <- as.matrix(matrix_davinci)
sort_davinci <- sort(rowSums(matrix_davinci_2), decreasing = TRUE)
data_frame_davinci <- data.frame(word = names(sort_davinci), freq = sort_davinci)

top_10_davinci <- data_frame_davinci %>%
  arrange(desc(freq)) %>% head(10)

davinci_final <- crossing(top_10_davinci, brand = "DaVinci")
davinci_final

#Kellogg Tweets#
kellogg <- c("kellogg", "syrup")
kellogg_search <- paste(kellogg, collapse = " AND ")
kellogg_tweets <- search_tweets(q = kellogg_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(kellogg_tweets)

head(kellogg_tweets$text)

kellogg_text <- kellogg_tweets$text

corpus_kellogg <- Corpus(VectorSource(kellogg_tweets$text))

kellogg_emoji_clean <- tm_map(corpus_kellogg, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_kellogg<- tm_map(kellogg_emoji_clean, content_transformer(remove_urls))

lower_kellogg <- tm_map(url_clean_kellogg, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_kellogg <- tm_map(lower_kellogg, content_transformer(remove_items))

remove_punct_kellogg <- tm_map(remove_kellogg, removePunctuation)

remove_num_kellogg <- tm_map(remove_punct_kellogg, removeNumbers)

remove_stop_kellogg <- tm_map(remove_num_kellogg, removeWords, stopwords("english"))

remove_white_kellogg <- tm_map(remove_stop_kellogg, stripWhitespace)

remove_words_kellogg <- tm_map(remove_white_kellogg, removeWords, c("kellogg", "kelloggs", "syrup", "syrups", "amp", "puts", "since"))

matrix_kellogg <- TermDocumentMatrix((remove_words_kellogg))
matrix_kellogg_2 <- as.matrix(matrix_kellogg)
sort_kellogg <- sort(rowSums(matrix_kellogg_2), decreasing = TRUE)
data_frame_kellogg <- data.frame(word = names(sort_kellogg), freq = sort_kellogg)

top_10_kellogg <- data_frame_kellogg %>%
  arrange(desc(freq)) %>% head(10)

kellogg_final <- crossing(top_10_kellogg, brand = "Kellogg")
kellogg_final

#Spring Tree Tweets#
spring_tree <- c('"spring tree"', "syrup")
spring_tree_search <- paste(spring_tree, collapse = " AND ")
spring_tree_tweets <- search_tweets(q = spring_tree_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(spring_tree_tweets)

head(spring_tree_tweets$text)

spring_tree_text <- spring_tree_tweets$text

corpus_spring_tree <- Corpus(VectorSource(spring_tree_tweets$text))

spring_tree_emoji_clean <- tm_map(corpus_spring_tree, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_spring_tree<- tm_map(spring_tree_emoji_clean, content_transformer(remove_urls))

lower_spring_tree <- tm_map(url_clean_spring_tree, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_spring_tree <- tm_map(lower_spring_tree, content_transformer(remove_items))

remove_punct_spring_tree <- tm_map(remove_spring_tree, removePunctuation)

remove_num_spring_tree <- tm_map(remove_punct_spring_tree, removeNumbers)

remove_stop_spring_tree <- tm_map(remove_num_spring_tree, removeWords, stopwords("english"))

remove_white_spring_tree <- tm_map(remove_stop_spring_tree, stripWhitespace)

remove_words_spring_tree <- tm_map(remove_white_spring_tree, removeWords, c("spring tree", "spring", "tree", "springs", "trees", "syrup", "syrups", "amp"))

matrix_spring_tree <- TermDocumentMatrix((remove_words_spring_tree))
matrix_spring_tree_2 <- as.matrix(matrix_spring_tree)
sort_spring_tree <- sort(rowSums(matrix_spring_tree_2), decreasing = TRUE)
data_frame_spring_tree <- data.frame(word = names(sort_spring_tree), freq = sort_spring_tree)

top_10_spring_tree <- data_frame_spring_tree %>%
  arrange(desc(freq)) %>% head(10)

#Colavita Tweets#
colavita <- c("colavita", "pasta")
colavita_search <- paste(colavita, collapse = " AND ")
colavita_tweets <- search_tweets(q = colavita_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(colavita_tweets)

head(colavita_tweets$text)

colavita_text <- colavita_tweets$text

corpus_colavita <- Corpus(VectorSource(colavita_tweets$text))

colavita_emoji_clean <- tm_map(corpus_colavita, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_colavita<- tm_map(colavita_emoji_clean, content_transformer(remove_urls))

lower_colavita <- tm_map(url_clean_colavita, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_colavita <- tm_map(lower_colavita, content_transformer(remove_items))

remove_punct_colavita <- tm_map(remove_colavita, removePunctuation)

remove_num_colavita <- tm_map(remove_punct_colavita, removeNumbers)

remove_stop_colavita <- tm_map(remove_num_colavita, removeWords, stopwords("english"))

remove_white_colavita <- tm_map(remove_stop_colavita, stripWhitespace)

remove_words_colavita <- tm_map(remove_white_colavita, removeWords, c("colavita", "vita", "cola", "colavitas", "pasta", "pastas", "amp", "apijuawk", "colavitaindia", "let"))

matrix_colavita <- TermDocumentMatrix((remove_words_colavita))
matrix_colavita_2 <- as.matrix(matrix_colavita)
sort_colavita <- sort(rowSums(matrix_colavita_2), decreasing = TRUE)
data_frame_colavita <- data.frame(word = names(sort_colavita), freq = sort_colavita)

top_10_colavita <- data_frame_colavita %>%
  arrange(desc(freq)) %>% head(10)

colavita_final <- crossing(top_10_colavita, brand = "Colavita")
colavita_final

#Pasta Shoppe Tweets#
pasta_shoppe <- c('"pasta shoppe"', "pasta")
pasta_shoppe_search <- paste(pasta_shoppe, collapse = " AND ")
pasta_shoppe_tweets <- search_tweets(q = pasta_shoppe_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(pasta_shoppe_tweets)

head(pasta_shoppe_tweets$text)

pasta_shoppe_text <- pasta_shoppe_tweets$text

corpus_pasta_shoppe <- Corpus(VectorSource(pasta_shoppe_tweets$text))

pasta_shoppe_emoji_clean <- tm_map(corpus_pasta_shoppe, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_pasta_shoppe<- tm_map(pasta_shoppe_emoji_clean, content_transformer(remove_urls))

lower_pasta_shoppe <- tm_map(url_clean_pasta_shoppe, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_pasta_shoppe <- tm_map(lower_pasta_shoppe, content_transformer(remove_items))

remove_punct_pasta_shoppe <- tm_map(remove_pasta_shoppe, removePunctuation)

remove_num_pasta_shoppe <- tm_map(remove_punct_pasta_shoppe, removeNumbers)

remove_stop_pasta_shoppe <- tm_map(remove_num_pasta_shoppe, removeWords, stopwords("english"))

remove_white_pasta_shoppe <- tm_map(remove_stop_pasta_shoppe, stripWhitespace)

remove_words_pasta_shoppe <- tm_map(remove_white_pasta_shoppe, removeWords, c("pasta shoppe", "pasta", "shoppe", "shop", "shops", "pastas", "pasta shoppes", "amp"))

matrix_pasta_shoppe <- TermDocumentMatrix((remove_words_pasta_shoppe))
matrix_pasta_shoppe_2 <- as.matrix(matrix_pasta_shoppe)
sort_pasta_shoppe <- sort(rowSums(matrix_pasta_shoppe_2), decreasing = TRUE)
data_frame_pasta_shoppe <- data.frame(word = names(sort_pasta_shoppe), freq = sort_pasta_shoppe)

top_10_pasta_shoppe <- data_frame_pasta_shoppe %>%
  arrange(desc(freq)) %>% head(10)

#San Marzano Tweets#
san_marzano <- c('"san marzano"', "pasta")
san_marzano_search <- paste(san_marzano, collapse = " AND ")
san_marzano_tweets <- search_tweets(q = san_marzano_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(san_marzano_tweets)

head(san_marzano_tweets$text)

san_marzano_text <- san_marzano_tweets$text

corpus_san_marzano <- Corpus(VectorSource(san_marzano_tweets$text))

san_marzano_emoji_clean <- tm_map(corpus_san_marzano, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_san_marzano<- tm_map(san_marzano_emoji_clean, content_transformer(remove_urls))

lower_san_marzano <- tm_map(url_clean_san_marzano, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_san_marzano <- tm_map(lower_san_marzano, content_transformer(remove_items))

remove_punct_san_marzano <- tm_map(remove_san_marzano, removePunctuation)

remove_num_san_marzano <- tm_map(remove_punct_san_marzano, removeNumbers)

remove_stop_san_marzano <- tm_map(remove_num_san_marzano, removeWords, stopwords("english"))

remove_white_san_marzano <- tm_map(remove_stop_san_marzano, stripWhitespace)

remove_words_san_marzano <- tm_map(remove_white_san_marzano, removeWords, c("san marzano", "san", "marzano", "san marzanos", "pasta", "pastas", "amp"))

matrix_san_marzano <- TermDocumentMatrix((remove_words_san_marzano))
matrix_san_marzano_2 <- as.matrix(matrix_san_marzano)
sort_san_marzano <- sort(rowSums(matrix_san_marzano_2), decreasing = TRUE)
data_frame_san_marzano <- data.frame(word = names(sort_san_marzano), freq = sort_san_marzano)

top_10_san_marzano <- data_frame_san_marzano %>%
  arrange(desc(freq)) %>% head(10)

san_marzano_final <- crossing(top_10_san_marzano, brand = "San Marzano")
san_marzano_final

#Classico Tweets#
classico <- c("classico", "pasta")
classico_search <- paste(classico, collapse = " AND ")
classico_tweets <- search_tweets(q = classico_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(classico_tweets)

head(classico_tweets$text)

classico_text <- classico_tweets$text

corpus_classico <- Corpus(VectorSource(classico_tweets$text))

classico_emoji_clean <- tm_map(corpus_classico, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_classico<- tm_map(classico_emoji_clean, content_transformer(remove_urls))

lower_classico <- tm_map(url_clean_classico, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_classico <- tm_map(lower_classico, content_transformer(remove_items))

remove_punct_classico <- tm_map(remove_classico, removePunctuation)

remove_num_classico <- tm_map(remove_punct_classico, removeNumbers)

remove_stop_classico <- tm_map(remove_num_classico, removeWords, stopwords("english"))

remove_white_classico <- tm_map(remove_stop_classico, stripWhitespace)

remove_words_classico <- tm_map(remove_white_classico, removeWords, c("classico", "classicos", "classic", "pasta", "pastas", "amp", "sauce"))

matrix_classico <- TermDocumentMatrix((remove_words_classico))
matrix_classico_2 <- as.matrix(matrix_classico)
sort_classico <- sort(rowSums(matrix_classico_2), decreasing = TRUE)
data_frame_classico <- data.frame(word = names(sort_classico), freq = sort_classico)

top_10_classico <- data_frame_classico %>%
  arrange(desc(freq)) %>% head(10)

classico_final <- crossing(top_10_classico, brand = "Classico")
classico_final

#B F Tweets#
b_f <- c('"b f"', "pasta")
b_f_search <- paste(b_f, collapse = " AND ")
b_f_tweets <- search_tweets(q = b_f_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(b_f_tweets)

head(b_f_tweets$text)

b_f_text <- b_f_tweets$text

corpus_b_f <- Corpus(VectorSource(b_f_tweets$text))

b_f_emoji_clean <- tm_map(corpus_b_f, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_b_f<- tm_map(b_f_emoji_clean, content_transformer(remove_urls))

lower_b_f <- tm_map(url_clean_b_f, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_b_f <- tm_map(lower_b_f, content_transformer(remove_items))

remove_punct_b_f <- tm_map(remove_b_f, removePunctuation)

remove_num_b_f <- tm_map(remove_punct_b_f, removeNumbers)

remove_stop_b_f <- tm_map(remove_num_b_f, removeWords, stopwords("english"))

remove_white_b_f <- tm_map(remove_stop_b_f, stripWhitespace)

remove_words_b_f <- tm_map(remove_white_b_f, removeWords, c("b f", "b", "f", "bs", "fs", "pasta", "pastas", "amp"))

matrix_b_f <- TermDocumentMatrix((remove_words_b_f))
matrix_b_f_2 <- as.matrix(matrix_b_f)
sort_b_f <- sort(rowSums(matrix_b_f_2), decreasing = TRUE)
data_frame_b_f <- data.frame(word = names(sort_b_f), freq = sort_b_f)

top_10_b_f <- data_frame_b_f %>%
  arrange(desc(freq)) %>% head(10)

#Krusteaz Tweets#
krusteaz <- c("krusteaz", "pancake")
krusteaz_search <- paste(krusteaz, collapse = " AND ")
krusteaz_tweets <- search_tweets(q = krusteaz_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(krusteaz_tweets)

head(krusteaz_tweets$text)

krusteaz_text <- krusteaz_tweets$text

corpus_krusteaz <- Corpus(VectorSource(krusteaz_tweets$text))

krusteaz_emoji_clean <- tm_map(corpus_krusteaz, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_krusteaz<- tm_map(krusteaz_emoji_clean, content_transformer(remove_urls))

lower_krusteaz <- tm_map(url_clean_krusteaz, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_krusteaz <- tm_map(lower_krusteaz, content_transformer(remove_items))

remove_punct_krusteaz <- tm_map(remove_krusteaz, removePunctuation)

remove_num_krusteaz <- tm_map(remove_punct_krusteaz, removeNumbers)

remove_stop_krusteaz <- tm_map(remove_num_krusteaz, removeWords, stopwords("english"))

remove_white_krusteaz <- tm_map(remove_stop_krusteaz, stripWhitespace)

remove_words_krusteaz <- tm_map(remove_white_krusteaz, removeWords, c("krusteaz", "krusteazs", "krust", "krusts", "pancake", "pancakes", "amp", "mix", "just", "per"))

matrix_krusteaz <- TermDocumentMatrix((remove_words_krusteaz))
matrix_krusteaz_2 <- as.matrix(matrix_krusteaz)
sort_krusteaz <- sort(rowSums(matrix_krusteaz_2), decreasing = TRUE)
data_frame_krusteaz <- data.frame(word = names(sort_krusteaz), freq = sort_krusteaz)

top_10_krusteaz <- data_frame_krusteaz %>%
  arrange(desc(freq)) %>% head(10)

krusteaz_final <- crossing(top_10_krusteaz, brand = "Krusteaz")
krusteaz_final

#Pioneer Tweets#
pioneer <- c("pioneer", "pancake")
pioneer_search <- paste(pioneer, collapse = " AND ")
pioneer_tweets <- search_tweets(q = pioneer_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(pioneer_tweets)

head(pioneer_tweets$text)

pioneer_text <- pioneer_tweets$text

corpus_pioneer <- Corpus(VectorSource(pioneer_tweets$text))

pioneer_emoji_clean <- tm_map(corpus_pioneer, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_pioneer<- tm_map(pioneer_emoji_clean, content_transformer(remove_urls))

lower_pioneer <- tm_map(url_clean_pioneer, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_pioneer <- tm_map(lower_pioneer, content_transformer(remove_items))

remove_punct_pioneer <- tm_map(remove_pioneer, removePunctuation)

remove_num_pioneer <- tm_map(remove_punct_pioneer, removeNumbers)

remove_stop_pioneer <- tm_map(remove_num_pioneer, removeWords, stopwords("english"))

remove_white_pioneer <- tm_map(remove_stop_pioneer, stripWhitespace)

remove_words_pioneer <- tm_map(remove_white_pioneer, removeWords, c("pioneer", "pioneers", "pancake", "pancakes", "amp", "got"))

matrix_pioneer <- TermDocumentMatrix((remove_words_pioneer))
matrix_pioneer_2 <- as.matrix(matrix_pioneer)
sort_pioneer <- sort(rowSums(matrix_pioneer_2), decreasing = TRUE)
data_frame_pioneer <- data.frame(word = names(sort_pioneer), freq = sort_pioneer)

top_10_pioneer <- data_frame_pioneer %>%
  arrange(desc(freq)) %>% head(10)

pioneer_final <- crossing(top_10_pioneer, brand = "Pioneer")
pioneer_final

#Pennant Tweets#
pennant <- c("pennant", "syrup")
pennant_search <- paste(pennant, collapse = " AND ")
pennant_tweets <- search_tweets(q = pennant_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(pennant_tweets)

head(pennant_tweets$text)

pennant_text <- pennant_tweets$text

corpus_pennant <- Corpus(VectorSource(pennant_tweets$text))

pennant_emoji_clean <- tm_map(corpus_pennant, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_pennant<- tm_map(pennant_emoji_clean, content_transformer(remove_urls))

lower_pennant <- tm_map(url_clean_pennant, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_pennant <- tm_map(lower_pennant, content_transformer(remove_items))

remove_punct_pennant <- tm_map(remove_pennant, removePunctuation)

remove_num_pennant <- tm_map(remove_punct_pennant, removeNumbers)

remove_stop_pennant <- tm_map(remove_num_pennant, removeWords, stopwords("english"))

remove_white_pennant <- tm_map(remove_stop_pennant, stripWhitespace)

remove_words_pennant <- tm_map(remove_white_pennant, removeWords, c("pennant", "pennants", "pen", "pens", "syrup", "syrups", "amp"))

matrix_pennant <- TermDocumentMatrix((remove_words_pennant))
matrix_pennant_2 <- as.matrix(matrix_pennant)
sort_pennant <- sort(rowSums(matrix_pennant_2), decreasing = TRUE)
data_frame_pennant <- data.frame(word = names(sort_pennant), freq = sort_pennant)

top_10_pennant <- data_frame_pennant %>%
  arrange(desc(freq)) %>% head(10)

#Lund Swede Tweets#
lund_swede <- c('"lund swede"', "pancake")
lund_swede_search <- paste(lund_swede, collapse = " AND ")
lund_swede_tweets <- search_tweets(q = lund_swede_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(lund_swede_tweets)

head(lund_swede_tweets$text)

lund_swede_text <- lund_swede_tweets$text

corpus_lund_swede <- Corpus(VectorSource(lund_swede_tweets$text))

lund_swede_emoji_clean <- tm_map(corpus_lund_swede, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_lund_swede<- tm_map(lund_swede_emoji_clean, content_transformer(remove_urls))

lower_lund_swede <- tm_map(url_clean_lund_swede, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_lund_swede <- tm_map(lower_lund_swede, content_transformer(remove_items))

remove_punct_lund_swede <- tm_map(remove_lund_swede, removePunctuation)

remove_num_lund_swede <- tm_map(remove_punct_lund_swede, removeNumbers)

remove_stop_lund_swede <- tm_map(remove_num_lund_swede, removeWords, stopwords("english"))

remove_white_lund_swede <- tm_map(remove_stop_lund_swede, stripWhitespace)

remove_words_lund_swede <- tm_map(remove_white_lund_swede, removeWords, c("lund swede", "lund", "swede", "lunds", "swedes", "pancake", "pancakes", "amp"))

matrix_lund_swede <- TermDocumentMatrix((remove_words_lund_swede))
matrix_lund_swede_2 <- as.matrix(matrix_lund_swede)
sort_lund_swede <- sort(rowSums(matrix_lund_swede_2), decreasing = TRUE)
data_frame_lund_swede <- data.frame(word = names(sort_lund_swede), freq = sort_lund_swede)

top_10_lund_swede <- data_frame_lund_swede %>%
  arrange(desc(freq)) %>% head(10)

#Braswell Tweets#
braswell <- c("braswell", "syrup")
braswell_search <- paste(braswell, collapse = " AND ")
braswell_tweets <- search_tweets(q = braswell_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(braswell_tweets)

head(braswell_tweets$text)

braswell_text <- braswell_tweets$text

corpus_braswell <- Corpus(VectorSource(braswell_tweets$text))

braswell_emoji_clean <- tm_map(corpus_braswell, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_braswell<- tm_map(braswell_emoji_clean, content_transformer(remove_urls))

lower_braswell <- tm_map(url_clean_braswell, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_braswell <- tm_map(lower_braswell, content_transformer(remove_items))

remove_punct_braswell <- tm_map(remove_braswell, removePunctuation)

remove_num_braswell <- tm_map(remove_punct_braswell, removeNumbers)

remove_stop_braswell <- tm_map(remove_num_braswell, removeWords, stopwords("english"))

remove_white_braswell <- tm_map(remove_stop_braswell, stripWhitespace)

remove_words_braswell <- tm_map(remove_white_braswell, removeWords, c("braswell", "braswells", "bra", "bras", "well", "wells", "syrup", "syrups", "amp"))

matrix_braswell <- TermDocumentMatrix((remove_words_braswell))
matrix_braswell_2 <- as.matrix(matrix_braswell)
sort_braswell <- sort(rowSums(matrix_braswell_2), decreasing = TRUE)
data_frame_braswell <- data.frame(word = names(sort_braswell), freq = sort_braswell)

top_10_braswell <- data_frame_braswell %>%
  arrange(desc(freq)) %>% head(10)

#Log Cabin Tweets#
log_cabin <- c('"log cabin"', "syrup")
log_cabin_search <- paste(log_cabin, collapse = " AND ")
log_cabin_tweets <- search_tweets(q = log_cabin_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(log_cabin_tweets)

head(log_cabin_tweets$text)

log_cabin_text <- log_cabin_tweets$text

corpus_log_cabin <- Corpus(VectorSource(log_cabin_tweets$text))

log_cabin_emoji_clean <- tm_map(corpus_log_cabin, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_log_cabin<- tm_map(log_cabin_emoji_clean, content_transformer(remove_urls))

lower_log_cabin <- tm_map(url_clean_log_cabin, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_log_cabin <- tm_map(lower_log_cabin, content_transformer(remove_items))

remove_punct_log_cabin <- tm_map(remove_log_cabin, removePunctuation)

remove_num_log_cabin <- tm_map(remove_punct_log_cabin, removeNumbers)

remove_stop_log_cabin <- tm_map(remove_num_log_cabin, removeWords, stopwords("english"))

remove_white_log_cabin <- tm_map(remove_stop_log_cabin, stripWhitespace)

remove_words_log_cabin <- tm_map(remove_white_log_cabin, removeWords, c("log cabin", "log", "cabin", "logs", "cabins", "syrup", "syrups", "amp"))

matrix_log_cabin <- TermDocumentMatrix((remove_words_log_cabin))
matrix_log_cabin_2 <- as.matrix(matrix_log_cabin)
sort_log_cabin <- sort(rowSums(matrix_log_cabin_2), decreasing = TRUE)
data_frame_log_cabin <- data.frame(word = names(sort_log_cabin), freq = sort_log_cabin)

top_10_log_cabin <- data_frame_log_cabin %>%
  arrange(desc(freq)) %>% head(10)

log_cabin_final <- crossing(top_10_log_cabin, brand = "Log Cabin")
log_cabin_final

#Buitoni Tweets#
buitoni <- c("buitoni", "pasta")
buitoni_search <- paste(buitoni, collapse = " AND ")
buitoni_tweets <- search_tweets(q = buitoni_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(buitoni_tweets)

head(buitoni_tweets$text)

buitoni_text <- buitoni_tweets$text

corpus_buitoni <- Corpus(VectorSource(buitoni_tweets$text))

buitoni_emoji_clean <- tm_map(corpus_buitoni, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_buitoni<- tm_map(buitoni_emoji_clean, content_transformer(remove_urls))

lower_buitoni <- tm_map(url_clean_buitoni, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_buitoni <- tm_map(lower_buitoni, content_transformer(remove_items))

remove_punct_buitoni <- tm_map(remove_buitoni, removePunctuation)

remove_num_buitoni <- tm_map(remove_punct_buitoni, removeNumbers)

remove_stop_buitoni <- tm_map(remove_num_buitoni, removeWords, stopwords("english"))

remove_white_buitoni <- tm_map(remove_stop_buitoni, stripWhitespace)

remove_words_buitoni <- tm_map(remove_white_buitoni, removeWords, c("buitoni", "buitonis", "toni", "tonis", "pasta", "pastas", "amp", "also", "ever", "horusxero", "lemme"))

matrix_buitoni <- TermDocumentMatrix((remove_words_buitoni))
matrix_buitoni_2 <- as.matrix(matrix_buitoni)
sort_buitoni <- sort(rowSums(matrix_buitoni_2), decreasing = TRUE)
data_frame_buitoni <- data.frame(word = names(sort_buitoni), freq = sort_buitoni)

top_10_buitoni <- data_frame_buitoni %>%
  arrange(desc(freq)) %>% head(10)

buitoni_final <- crossing(top_10_buitoni, brand = "Buitoni")
buitoni_final

#Mrs Butterworth Tweets#
mrs_butterworth <- c("pancake", "syrup")
mrs_butterworth_combine <- paste(mrs_butterworth, collapse = " OR ")
mrs_butterworth_2 <- c('"mrs butterworth"', mrs_butterworth_combine)
mrs_butterworth_search <- paste(mrs_butterworth_2, collapse = " AND")
mrs_butterworth_tweets <- search_tweets(q = mrs_butterworth_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(mrs_butterworth_tweets)

head(mrs_butterworth_tweets$text)

mrs_butterworth_text <- mrs_butterworth_tweets$text

corpus_mrs_butterworth <- Corpus(VectorSource(mrs_butterworth_tweets$text))

mrs_butterworth_emoji_clean <- tm_map(corpus_mrs_butterworth, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_mrs_butterworth<- tm_map(mrs_butterworth_emoji_clean, content_transformer(remove_urls))

lower_mrs_butterworth <- tm_map(url_clean_mrs_butterworth, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_mrs_butterworth <- tm_map(lower_mrs_butterworth, content_transformer(remove_items))

remove_punct_mrs_butterworth <- tm_map(remove_mrs_butterworth, removePunctuation)

remove_num_mrs_butterworth <- tm_map(remove_punct_mrs_butterworth, removeNumbers)

remove_stop_mrs_butterworth <- tm_map(remove_num_mrs_butterworth, removeWords, stopwords("english"))

remove_white_mrs_butterworth <- tm_map(remove_stop_mrs_butterworth, stripWhitespace)

remove_words_mrs_butterworth <- tm_map(remove_white_mrs_butterworth, removeWords, c("mrs butterworth", "mrs", "butterworth", "butterworths", "butter", "butters", "worth", "worths", "syrup", "syrups", "amp", "don", "just", "know", "really", "til"))

matrix_mrs_butterworth <- TermDocumentMatrix((remove_words_mrs_butterworth))
matrix_mrs_butterworth_2 <- as.matrix(matrix_mrs_butterworth)
sort_mrs_butterworth <- sort(rowSums(matrix_mrs_butterworth_2), decreasing = TRUE)
data_frame_mrs_butterworth <- data.frame(word = names(sort_mrs_butterworth), freq = sort_mrs_butterworth)

top_10_mrs_butterworth <- data_frame_mrs_butterworth %>%
  arrange(desc(freq)) %>% head(10)

mrs_butterworth_final <- crossing(top_10_mrs_butterworth, brand = "Mrs Butterworth")
mrs_butterworth_final

#Raos Tweets#
raos <- c("raos", "pasta")
raos_search <- paste(raos, collapse = " AND ")
raos_tweets <- search_tweets(q = raos_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(raos_tweets)

head(raos_tweets$text)

raos_text <- raos_tweets$text

corpus_raos <- Corpus(VectorSource(raos_tweets$text))

raos_emoji_clean <- tm_map(corpus_raos, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_raos<- tm_map(raos_emoji_clean, content_transformer(remove_urls))

lower_raos <- tm_map(url_clean_raos, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_raos <- tm_map(lower_raos, content_transformer(remove_items))

remove_punct_raos <- tm_map(remove_raos, removePunctuation)

remove_num_raos <- tm_map(remove_punct_raos, removeNumbers)

remove_stop_raos <- tm_map(remove_num_raos, removeWords, stopwords("english"))

remove_white_raos <- tm_map(remove_stop_raos, stripWhitespace)

remove_words_raos <- tm_map(remove_white_raos, removeWords, c("raos", "rao", "pasta", "pastas", "amp", "deidrescaramuc", "etc", "sauce"))

matrix_raos <- TermDocumentMatrix((remove_words_raos))
matrix_raos_2 <- as.matrix(matrix_raos)
sort_raos <- sort(rowSums(matrix_raos_2), decreasing = TRUE)
data_frame_raos <- data.frame(word = names(sort_raos), freq = sort_raos)

top_10_raos <- data_frame_raos %>%
  arrange(desc(freq)) %>% head(10)

raos_final <- crossing(top_10_raos, brand = "Raos")
raos_final

#Castelna Tweets#
castelna <- c("castelna", "pasta")
castelna_search <- paste(castelna, collapse = " AND ")
castelna_tweets <- search_tweets(q = castelna_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(castelna_tweets)

head(castelna_tweets$text)

castelna_text <- castelna_tweets$text

corpus_castelna <- Corpus(VectorSource(castelna_tweets$text))

castelna_emoji_clean <- tm_map(corpus_castelna, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_castelna<- tm_map(castelna_emoji_clean, content_transformer(remove_urls))

lower_castelna <- tm_map(url_clean_castelna, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_castelna <- tm_map(lower_castelna, content_transformer(remove_items))

remove_punct_castelna <- tm_map(remove_castelna, removePunctuation)

remove_num_castelna <- tm_map(remove_punct_castelna, removeNumbers)

remove_stop_castelna <- tm_map(remove_num_castelna, removeWords, stopwords("english"))

remove_white_castelna <- tm_map(remove_stop_castelna, stripWhitespace)

remove_words_castelna <- tm_map(remove_white_castelna, removeWords, c("castelna", "castelnas", "cast", "casts", "pasta", "pastas", "amp"))

matrix_castelna <- TermDocumentMatrix((remove_words_castelna))
matrix_castelna_2 <- as.matrix(matrix_castelna)
sort_castelna <- sort(rowSums(matrix_castelna_2), decreasing = TRUE)
data_frame_castelna <- data.frame(word = names(sort_castelna), freq = sort_castelna)

top_10_castelna <- data_frame_castelna %>%
  arrange(desc(freq)) %>% head(10)

#Karo Tweets#
karo <- c("karo", "syrup")
karo_search <- paste(karo, collapse = " AND ")
karo_tweets <- search_tweets(q = karo_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(karo_tweets)

head(karo_tweets$text)

karo_text <- karo_tweets$text

corpus_karo <- Corpus(VectorSource(karo_tweets$text))

karo_emoji_clean <- tm_map(corpus_karo, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_karo<- tm_map(karo_emoji_clean, content_transformer(remove_urls))

lower_karo <- tm_map(url_clean_karo, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_karo <- tm_map(lower_karo, content_transformer(remove_items))

remove_punct_karo <- tm_map(remove_karo, removePunctuation)

remove_num_karo <- tm_map(remove_punct_karo, removeNumbers)

remove_stop_karo <- tm_map(remove_num_karo, removeWords, stopwords("english"))

remove_white_karo <- tm_map(remove_stop_karo, stripWhitespace)

remove_words_karo <- tm_map(remove_white_karo, removeWords, c("karo", "karos", "syrup", "syrups", "amp", "just", "use"))

matrix_karo <- TermDocumentMatrix((remove_words_karo))
matrix_karo_2 <- as.matrix(matrix_karo)
sort_karo <- sort(rowSums(matrix_karo_2), decreasing = TRUE)
data_frame_karo <- data.frame(word = names(sort_karo), freq = sort_karo)

top_10_karo <- data_frame_karo %>%
  arrange(desc(freq)) %>% head(10)

karo_final <- crossing(top_10_karo, brand = "Karo")
karo_final

#Cara Nona Tweets#
cara_nona <- c('"cara nona"', "pasta")
cara_nona_search <- paste(cara_nona, collapse = " AND ")
cara_nona_tweets <- search_tweets(q = cara_nona_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(cara_nona_tweets)

head(cara_nona_tweets$text)

cara_nona_text <- cara_nona_tweets$text

corpus_cara_nona <- Corpus(VectorSource(cara_nona_tweets$text))

cara_nona_emoji_clean <- tm_map(corpus_cara_nona, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_cara_nona<- tm_map(cara_nona_emoji_clean, content_transformer(remove_urls))

lower_cara_nona <- tm_map(url_clean_cara_nona, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_cara_nona <- tm_map(lower_cara_nona, content_transformer(remove_items))

remove_punct_cara_nona <- tm_map(remove_cara_nona, removePunctuation)

remove_num_cara_nona <- tm_map(remove_punct_cara_nona, removeNumbers)

remove_stop_cara_nona <- tm_map(remove_num_cara_nona, removeWords, stopwords("english"))

remove_white_cara_nona <- tm_map(remove_stop_cara_nona, stripWhitespace)

remove_words_cara_nona <- tm_map(remove_white_cara_nona, removeWords, c("cara nona", "cara", "caras", "nona", "nonas", "pasta", "pastas", "amp"))

matrix_cara_nona <- TermDocumentMatrix((remove_words_cara_nona))
matrix_cara_nona_2 <- as.matrix(matrix_cara_nona)
sort_cara_nona <- sort(rowSums(matrix_cara_nona_2), decreasing = TRUE)
data_frame_cara_nona <- data.frame(word = names(sort_cara_nona), freq = sort_cara_nona)

top_10_cara_nona <- data_frame_cara_nona %>%
  arrange(desc(freq)) %>% head(10)

#Walnut Acres Tweets#
walnut_acres <- c('"walnut acres"', "pasta")
walnut_acres_search <- paste(walnut_acres, collapse = " AND ")
walnut_acres_tweets <- search_tweets(q = walnut_acres_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(walnut_acres_tweets)

head(walnut_acres_tweets$text)

walnut_acres_text <- walnut_acres_tweets$text

corpus_walnut_acres <- Corpus(VectorSource(walnut_acres_tweets$text))

walnut_acres_emoji_clean <- tm_map(corpus_walnut_acres, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_walnut_acres<- tm_map(walnut_acres_emoji_clean, content_transformer(remove_urls))

lower_walnut_acres <- tm_map(url_clean_walnut_acres, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_walnut_acres <- tm_map(lower_walnut_acres, content_transformer(remove_items))

remove_punct_walnut_acres <- tm_map(remove_walnut_acres, removePunctuation)

remove_num_walnut_acres <- tm_map(remove_punct_walnut_acres, removeNumbers)

remove_stop_walnut_acres <- tm_map(remove_num_walnut_acres, removeWords, stopwords("english"))

remove_white_walnut_acres <- tm_map(remove_stop_walnut_acres, stripWhitespace)

remove_words_walnut_acres <- tm_map(remove_white_walnut_acres, removeWords, c("walnut acres", "walnut", "walnuts", "nut", "nuts", "acre", "acres", "pasta", "pastas", "amp"))

matrix_walnut_acres <- TermDocumentMatrix((remove_words_walnut_acres))
matrix_walnut_acres_2 <- as.matrix(matrix_walnut_acres)
sort_walnut_acres <- sort(rowSums(matrix_walnut_acres_2), decreasing = TRUE)
data_frame_walnut_acres <- data.frame(word = names(sort_walnut_acres), freq = sort_walnut_acres)

top_10_walnut_acres <- data_frame_walnut_acres %>%
  arrange(desc(freq)) %>% head(10)

#Lucini Tweets#
lucini <- c("lucini", "pasta")
lucini_search <- paste(lucini, collapse = " AND ")
lucini_tweets <- search_tweets(q = lucini_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(lucini_tweets)

head(lucini_tweets$text)

lucini_text <- lucini_tweets$text

corpus_lucini <- Corpus(VectorSource(lucini_tweets$text))

lucini_emoji_clean <- tm_map(corpus_lucini, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_lucini<- tm_map(lucini_emoji_clean, content_transformer(remove_urls))

lower_lucini <- tm_map(url_clean_lucini, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_lucini <- tm_map(lower_lucini, content_transformer(remove_items))

remove_punct_lucini <- tm_map(remove_lucini, removePunctuation)

remove_num_lucini <- tm_map(remove_punct_lucini, removeNumbers)

remove_stop_lucini <- tm_map(remove_num_lucini, removeWords, stopwords("english"))

remove_white_lucini <- tm_map(remove_stop_lucini, stripWhitespace)

remove_words_lucini <- tm_map(remove_white_lucini, removeWords, c("lucini", "lucinis", "amp", "pasta", "pastas"))

matrix_lucini <- TermDocumentMatrix((remove_words_lucini))
matrix_lucini_2 <- as.matrix(matrix_lucini)
sort_lucini <- sort(rowSums(matrix_lucini_2), decreasing = TRUE)
data_frame_lucini <- data.frame(word = names(sort_lucini), freq = sort_lucini)

top_10_lucini <- data_frame_lucini %>%
  arrange(desc(freq)) %>% head(10)

#Prego Tweets#
prego <- c("prego", "pasta")
prego_search <- paste(prego, collapse = " AND ")
prego_tweets <- search_tweets(q = prego_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(prego_tweets)

head(prego_tweets$text)

prego_text <- prego_tweets$text

corpus_prego <- Corpus(VectorSource(prego_tweets$text))

prego_emoji_clean <- tm_map(corpus_prego, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_prego<- tm_map(prego_emoji_clean, content_transformer(remove_urls))

lower_prego <- tm_map(url_clean_prego, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_prego <- tm_map(lower_prego, content_transformer(remove_items))

remove_punct_prego <- tm_map(remove_prego, removePunctuation)

remove_num_prego <- tm_map(remove_punct_prego, removeNumbers)

remove_stop_prego <- tm_map(remove_num_prego, removeWords, stopwords("english"))

remove_white_prego <- tm_map(remove_stop_prego, stripWhitespace)

remove_words_prego <- tm_map(remove_white_prego, removeWords, c("prego", "pregos", "pasta", "pastas", "amp", "sauce", "even", "just", "preggo", "sauce"))

matrix_prego <- TermDocumentMatrix((remove_words_prego))
matrix_prego_2 <- as.matrix(matrix_prego)
sort_prego <- sort(rowSums(matrix_prego_2), decreasing = TRUE)
data_frame_prego <- data.frame(word = names(sort_prego), freq = sort_prego)

top_10_prego <- data_frame_prego %>%
  arrange(desc(freq)) %>% head(10)

prego_final <- crossing(top_10_prego, brand = "Prego")
prego_final

#Joey's Tweets#
joeys <- c("joey's", "pasta")
joeys_search <- paste(joeys, collapse = " AND ")
joeys_tweets <- search_tweets(q = joeys_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(joeys_tweets)

head(joeys_tweets$text)

joeys_text <- joeys_tweets$text

corpus_joeys <- Corpus(VectorSource(joeys_tweets$text))

joeys_emoji_clean <- tm_map(corpus_joeys, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_joeys<- tm_map(joeys_emoji_clean, content_transformer(remove_urls))

lower_joeys <- tm_map(url_clean_joeys, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_joeys <- tm_map(lower_joeys, content_transformer(remove_items))

remove_punct_joeys <- tm_map(remove_joeys, removePunctuation)

remove_num_joeys <- tm_map(remove_punct_joeys, removeNumbers)

remove_stop_joeys <- tm_map(remove_num_joeys, removeWords, stopwords("english"))

remove_white_joeys <- tm_map(remove_stop_joeys, stripWhitespace)

remove_words_joeys <- tm_map(remove_white_joeys, removeWords, c("joey's", "joeys", "joey", "joe", "joes", "pasta", "pastas", "amp", "shelbylanza"))

matrix_joeys <- TermDocumentMatrix((remove_words_joeys))
matrix_joeys_2 <- as.matrix(matrix_joeys)
sort_joeys <- sort(rowSums(matrix_joeys_2), decreasing = TRUE)
data_frame_joeys <- data.frame(word = names(sort_joeys), freq = sort_joeys)

top_10_joeys <- data_frame_joeys %>%
  arrange(desc(freq)) %>% head(10)

joeys_final <- crossing(top_10_joeys, brand = "Joey's")
joeys_final

#Smuckers Tweets#
smuckers <- c("smuckers", "syrup")
smuckers_search <- paste(smuckers, collapse = " AND ")
smuckers_tweets <- search_tweets(q = smuckers_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(smuckers_tweets)

head(smuckers_tweets$text)

smuckers_text <- smuckers_tweets$text

corpus_smuckers <- Corpus(VectorSource(smuckers_tweets$text))

smuckers_emoji_clean <- tm_map(corpus_smuckers, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_smuckers<- tm_map(smuckers_emoji_clean, content_transformer(remove_urls))

lower_smuckers <- tm_map(url_clean_smuckers, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_smuckers <- tm_map(lower_smuckers, content_transformer(remove_items))

remove_punct_smuckers <- tm_map(remove_smuckers, removePunctuation)

remove_num_smuckers <- tm_map(remove_punct_smuckers, removeNumbers)

remove_stop_smuckers <- tm_map(remove_num_smuckers, removeWords, stopwords("english"))

remove_white_smuckers <- tm_map(remove_stop_smuckers, stripWhitespace)

remove_words_smuckers <- tm_map(remove_white_smuckers, removeWords, c("smuckers", "smucker", "syrup", "syrups", "amp", "andersonsmaple"))

matrix_smuckers <- TermDocumentMatrix((remove_words_smuckers))
matrix_smuckers_2 <- as.matrix(matrix_smuckers)
sort_smuckers <- sort(rowSums(matrix_smuckers_2), decreasing = TRUE)
data_frame_smuckers <- data.frame(word = names(sort_smuckers), freq = sort_smuckers)

top_10_smuckers <- data_frame_smuckers %>%
  arrange(desc(freq)) %>% head(10)

smuckers_final <- crossing(top_10_smuckers, brand = "Smuckers")
smuckers_final

#Dave's Tweets#
daves <- c("dave's", "pasta")
daves_search <- paste(daves, collapse = " AND ")
daves_tweets <- search_tweets(q = daves_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(daves_tweets)

head(daves_tweets$text)

daves_text <- daves_tweets$text

corpus_daves <- Corpus(VectorSource(daves_tweets$text))

daves_emoji_clean <- tm_map(corpus_daves, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_daves<- tm_map(daves_emoji_clean, content_transformer(remove_urls))

lower_daves <- tm_map(url_clean_daves, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_daves <- tm_map(lower_daves, content_transformer(remove_items))

remove_punct_daves <- tm_map(remove_daves, removePunctuation)

remove_num_daves <- tm_map(remove_punct_daves, removeNumbers)

remove_stop_daves <- tm_map(remove_num_daves, removeWords, stopwords("english"))

remove_white_daves <- tm_map(remove_stop_daves, stripWhitespace)

remove_words_daves <- tm_map(remove_white_daves, removeWords, c("dave's", "daves", "dave", "pasta", "pastas", "amp", "just", "mrscglambert", "universalhub", "know"))

matrix_daves <- TermDocumentMatrix((remove_words_daves))
matrix_daves_2 <- as.matrix(matrix_daves)
sort_daves <- sort(rowSums(matrix_daves_2), decreasing = TRUE)
data_frame_daves <- data.frame(word = names(sort_daves), freq = sort_daves)

top_10_daves <- data_frame_daves %>%
  arrange(desc(freq)) %>% head(10)

daves_final <- crossing(top_10_daves, brand = "Dave's")
daves_final

#Cary's Tweets#
carys <- c("cary's", "syrup")
carys_search <- paste(carys, collapse = " AND ")
carys_tweets <- search_tweets(q = carys_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(carys_tweets)

head(carys_tweets$text)

carys_text <- carys_tweets$text

corpus_carys <- Corpus(VectorSource(carys_tweets$text))

carys_emoji_clean <- tm_map(corpus_carys, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_carys<- tm_map(carys_emoji_clean, content_transformer(remove_urls))

lower_carys <- tm_map(url_clean_carys, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_carys <- tm_map(lower_carys, content_transformer(remove_items))

remove_punct_carys <- tm_map(remove_carys, removePunctuation)

remove_num_carys <- tm_map(remove_punct_carys, removeNumbers)

remove_stop_carys <- tm_map(remove_num_carys, removeWords, stopwords("english"))

remove_white_carys <- tm_map(remove_stop_carys, stripWhitespace)

remove_words_carys <- tm_map(remove_white_carys, removeWords, c("cary's", "cary", "carys", "car", "cars", "syrup", "syrups", "amp"))

matrix_carys <- TermDocumentMatrix((remove_words_carys))
matrix_carys_2 <- as.matrix(matrix_carys)
sort_carys <- sort(rowSums(matrix_carys_2), decreasing = TRUE)
data_frame_carys <- data.frame(word = names(sort_carys), freq = sort_carys)

top_10_carys <- data_frame_carys %>%
  arrange(desc(freq)) %>% head(10)

#Brother's Tweets#
brothers <- c("brother's", "pasta")
brothers_search <- paste(brothers, collapse = " AND ")
brothers_tweets <- search_tweets(q = brothers_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(brothers_tweets)

head(brothers_tweets$text)

brothers_text <- brothers_tweets$text

corpus_brothers <- Corpus(VectorSource(brothers_tweets$text))

brothers_emoji_clean <- tm_map(corpus_brothers, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_brothers<- tm_map(brothers_emoji_clean, content_transformer(remove_urls))

lower_brothers <- tm_map(url_clean_brothers, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_brothers <- tm_map(lower_brothers, content_transformer(remove_items))

remove_punct_brothers <- tm_map(remove_brothers, removePunctuation)

remove_num_brothers <- tm_map(remove_punct_brothers, removeNumbers)

remove_stop_brothers <- tm_map(remove_num_brothers, removeWords, stopwords("english"))

remove_white_brothers <- tm_map(remove_stop_brothers, stripWhitespace)

remove_words_brothers <- tm_map(remove_white_brothers, removeWords, c("brother's", "brothers", "brother", "pasta", "pastas", "amp", "doesn", "told"))

matrix_brothers <- TermDocumentMatrix((remove_words_brothers))
matrix_brothers_2 <- as.matrix(matrix_brothers)
sort_brothers <- sort(rowSums(matrix_brothers_2), decreasing = TRUE)
data_frame_brothers <- data.frame(word = names(sort_brothers), freq = sort_brothers)

top_10_brothers <- data_frame_brothers %>%
  arrange(desc(freq)) %>% head(10)

brothers_final <- crossing(top_10_brothers, brand = "Brother's")
brothers_final

#Vermont Gold Tweets#
vermont_gold <- c('"vermont gold"', "syrup")
vermont_gold_search <- paste(vermont_gold, collapse = " AND ")
vermont_gold_tweets <- search_tweets(q = vermont_gold_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(vermont_gold_tweets)

head(vermont_gold_tweets$text)

vermont_gold_text <- vermont_gold_tweets$text

corpus_vermont_gold <- Corpus(VectorSource(vermont_gold_tweets$text))

vermont_gold_emoji_clean <- tm_map(corpus_vermont_gold, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_vermont_gold<- tm_map(vermont_gold_emoji_clean, content_transformer(remove_urls))

lower_vermont_gold <- tm_map(url_clean_vermont_gold, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_vermont_gold <- tm_map(lower_vermont_gold, content_transformer(remove_items))

remove_punct_vermont_gold <- tm_map(remove_vermont_gold, removePunctuation)

remove_num_vermont_gold <- tm_map(remove_punct_vermont_gold, removeNumbers)

remove_stop_vermont_gold <- tm_map(remove_num_vermont_gold, removeWords, stopwords("english"))

remove_white_vermont_gold <- tm_map(remove_stop_vermont_gold, stripWhitespace)

remove_words_vermont_gold <- tm_map(remove_white_vermont_gold, removeWords, c("vermont gold", "vermont", "vermont golds", "gold", "golds", "syrup", "syrups", "amp"))

matrix_vermont_gold <- TermDocumentMatrix((remove_words_vermont_gold))
matrix_vermont_gold_2 <- as.matrix(matrix_vermont_gold)
sort_vermont_gold <- sort(rowSums(matrix_vermont_gold_2), decreasing = TRUE)
data_frame_vermont_gold <- data.frame(word = names(sort_vermont_gold), freq = sort_vermont_gold)

top_10_vermont_gold <- data_frame_vermont_gold %>%
  arrange(desc(freq)) %>% head(10)

#Chef Pizza Tweets#
chef_pizza <- c('"chef pizza"', "pasta")
chef_pizza_search <- paste(chef_pizza, collapse = " AND ")
chef_pizza_tweets <- search_tweets(q = chef_pizza_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(chef_pizza_tweets)

head(chef_pizza_tweets$text)

chef_pizza_text <- chef_pizza_tweets$text

corpus_chef_pizza <- Corpus(VectorSource(chef_pizza_tweets$text))

chef_pizza_emoji_clean <- tm_map(corpus_chef_pizza, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_chef_pizza<- tm_map(chef_pizza_emoji_clean, content_transformer(remove_urls))

lower_chef_pizza <- tm_map(url_clean_chef_pizza, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_chef_pizza <- tm_map(lower_chef_pizza, content_transformer(remove_items))

remove_punct_chef_pizza <- tm_map(remove_chef_pizza, removePunctuation)

remove_num_chef_pizza <- tm_map(remove_punct_chef_pizza, removeNumbers)

remove_stop_chef_pizza <- tm_map(remove_num_chef_pizza, removeWords, stopwords("english"))

remove_white_chef_pizza <- tm_map(remove_stop_chef_pizza, stripWhitespace)

remove_words_chef_pizza <- tm_map(remove_white_chef_pizza, removeWords, c("chef pizza", "chef pizzas", "chef", "chefs", "pizza", "pizzas", "pasta", "pastas", "amp"))

matrix_chef_pizza <- TermDocumentMatrix((remove_words_chef_pizza))
matrix_chef_pizza_2 <- as.matrix(matrix_chef_pizza)
sort_chef_pizza <- sort(rowSums(matrix_chef_pizza_2), decreasing = TRUE)
data_frame_chef_pizza <- data.frame(word = names(sort_chef_pizza), freq = sort_chef_pizza)

top_10_chef_pizza <- data_frame_chef_pizza %>%
  arrange(desc(freq)) %>% head(10)

#Annie Chns Tweets#
annie_chns <- c('"annie chns"', "pasta")
annie_chns_search <- paste(annie_chns, collapse = " AND ")
annie_chns_tweets <- search_tweets(q = annie_chns_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(annie_chns_tweets)

head(annie_chns_tweets$text)

annie_chns_text <- annie_chns_tweets$text

corpus_annie_chns <- Corpus(VectorSource(annie_chns_tweets$text))

annie_chns_emoji_clean <- tm_map(corpus_annie_chns, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_annie_chns<- tm_map(annie_chns_emoji_clean, content_transformer(remove_urls))

lower_annie_chns <- tm_map(url_clean_annie_chns, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_annie_chns <- tm_map(lower_annie_chns, content_transformer(remove_items))

remove_punct_annie_chns <- tm_map(remove_annie_chns, removePunctuation)

remove_num_annie_chns <- tm_map(remove_punct_annie_chns, removeNumbers)

remove_stop_annie_chns <- tm_map(remove_num_annie_chns, removeWords, stopwords("english"))

remove_white_annie_chns <- tm_map(remove_stop_annie_chns, stripWhitespace)

remove_words_annie_chns <- tm_map(remove_white_annie_chns, removeWords, c("annie chns", "annie chn", "annie", "annies", "chn", "chns", "pasta", "pastas", "amp"))

matrix_annie_chns <- TermDocumentMatrix((remove_words_annie_chns))
matrix_annie_chns_2 <- as.matrix(matrix_annie_chns)
sort_annie_chns <- sort(rowSums(matrix_annie_chns_2), decreasing = TRUE)
data_frame_annie_chns <- data.frame(word = names(sort_annie_chns), freq = sort_annie_chns)

top_10_annie_chns <- data_frame_annie_chns %>%
  arrange(desc(freq)) %>% head(10)

#Dreamfield Tweets#
dreamfield <- c("dreamfield", "pasta")
dreamfield_search <- paste(dreamfield, collapse = " AND ")
dreamfield_tweets <- search_tweets(q = dreamfield_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(dreamfield_tweets)

head(dreamfield_tweets$text)

dreamfield_text <- dreamfield_tweets$text

corpus_dreamfield <- Corpus(VectorSource(dreamfield_tweets$text))

dreamfield_emoji_clean <- tm_map(corpus_dreamfield, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_dreamfield<- tm_map(dreamfield_emoji_clean, content_transformer(remove_urls))

lower_dreamfield <- tm_map(url_clean_dreamfield, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_dreamfield <- tm_map(lower_dreamfield, content_transformer(remove_items))

remove_punct_dreamfield <- tm_map(remove_dreamfield, removePunctuation)

remove_num_dreamfield <- tm_map(remove_punct_dreamfield, removeNumbers)

remove_stop_dreamfield <- tm_map(remove_num_dreamfield, removeWords, stopwords("english"))

remove_white_dreamfield <- tm_map(remove_stop_dreamfield, stripWhitespace)

remove_words_dreamfield <- tm_map(remove_white_dreamfield, removeWords, c("dreamfield", "dream", "dreams", "field", "fields", "dreamfields", "pasta", "pastas", "amp"))

matrix_dreamfield <- TermDocumentMatrix((remove_words_dreamfield))
matrix_dreamfield_2 <- as.matrix(matrix_dreamfield)
sort_dreamfield <- sort(rowSums(matrix_dreamfield_2), decreasing = TRUE)
data_frame_dreamfield <- data.frame(word = names(sort_dreamfield), freq = sort_dreamfield)

top_10_dreamfield <- data_frame_dreamfield %>%
  arrange(desc(freq)) %>% head(10)

#Orzo Tweets#
orzo <- c("orzo", "pasta")
orzo_search <- paste(orzo, collapse = " AND ")
orzo_tweets <- search_tweets(q = orzo_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(orzo_tweets)

head(orzo_tweets$text)

orzo_text <- orzo_tweets$text

corpus_orzo <- Corpus(VectorSource(orzo_tweets$text))

orzo_emoji_clean <- tm_map(corpus_orzo, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_orzo<- tm_map(orzo_emoji_clean, content_transformer(remove_urls))

lower_orzo <- tm_map(url_clean_orzo, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_orzo <- tm_map(lower_orzo, content_transformer(remove_items))

remove_punct_orzo <- tm_map(remove_orzo, removePunctuation)

remove_num_orzo <- tm_map(remove_punct_orzo, removeNumbers)

remove_stop_orzo <- tm_map(remove_num_orzo, removeWords, stopwords("english"))

remove_white_orzo <- tm_map(remove_stop_orzo, stripWhitespace)

remove_words_orzo <- tm_map(remove_white_orzo, removeWords, c("orzo", "orzos", "pasta", "pastas", "amp"))

matrix_orzo <- TermDocumentMatrix((remove_words_orzo))
matrix_orzo_2 <- as.matrix(matrix_orzo)
sort_orzo <- sort(rowSums(matrix_orzo_2), decreasing = TRUE)
data_frame_orzo <- data.frame(word = names(sort_orzo), freq = sort_orzo)

top_10_orzo <- data_frame_orzo %>%
  arrange(desc(freq)) %>% head(10)

orzo_final <- crossing(top_10_orzo, brand = "Orzo")
orzo_final

#Patsy's Tweets#
patsys <- c("patsy's", "pasta")
patsys_search <- paste(patsys, collapse = " AND ")
patsys_tweets <- search_tweets(q = patsys_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(patsys_tweets)

head(patsys_tweets$text)

patsys_text <- patsys_tweets$text

corpus_patsys <- Corpus(VectorSource(patsys_tweets$text))

patsys_emoji_clean <- tm_map(corpus_patsys, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_patsys<- tm_map(patsys_emoji_clean, content_transformer(remove_urls))

lower_patsys <- tm_map(url_clean_patsys, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_patsys <- tm_map(lower_patsys, content_transformer(remove_items))

remove_punct_patsys <- tm_map(remove_patsys, removePunctuation)

remove_num_patsys <- tm_map(remove_punct_patsys, removeNumbers)

remove_stop_patsys <- tm_map(remove_num_patsys, removeWords, stopwords("english"))

remove_white_patsys <- tm_map(remove_stop_patsys, stripWhitespace)

remove_words_patsys <- tm_map(remove_white_patsys, removeWords, c("patsy's", "patsy", "patsys", "pat", "pats", "pasta", "pastas", "amp"))

matrix_patsys <- TermDocumentMatrix((remove_words_patsys))
matrix_patsys_2 <- as.matrix(matrix_patsys)
sort_patsys <- sort(rowSums(matrix_patsys_2), decreasing = TRUE)
data_frame_patsys <- data.frame(word = names(sort_patsys), freq = sort_patsys)

top_10_patsys <- data_frame_patsys %>%
  arrange(desc(freq)) %>% head(10)

#Pastariso Tweets#
pastariso <- c("pastariso", "pasta")
pastariso_search <- paste(pastariso, collapse = " AND ")
pastariso_tweets <- search_tweets(q = pastariso_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(pastariso_tweets)

head(pastariso_tweets$text)

pastariso_text <- pastariso_tweets$text

corpus_pastariso <- Corpus(VectorSource(pastariso_tweets$text))

pastariso_emoji_clean <- tm_map(corpus_pastariso, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_pastariso<- tm_map(pastariso_emoji_clean, content_transformer(remove_urls))

lower_pastariso <- tm_map(url_clean_pastariso, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_pastariso <- tm_map(lower_pastariso, content_transformer(remove_items))

remove_punct_pastariso <- tm_map(remove_pastariso, removePunctuation)

remove_num_pastariso <- tm_map(remove_punct_pastariso, removeNumbers)

remove_stop_pastariso <- tm_map(remove_num_pastariso, removeWords, stopwords("english"))

remove_white_pastariso <- tm_map(remove_stop_pastariso, stripWhitespace)

remove_words_pastariso <- tm_map(remove_white_pastariso, removeWords, c("pastariso", "pasta", "pastarisos", "pastas", "amp"))

matrix_pastariso <- TermDocumentMatrix((remove_words_pastariso))
matrix_pastariso_2 <- as.matrix(matrix_pastariso)
sort_pastariso <- sort(rowSums(matrix_pastariso_2), decreasing = TRUE)
data_frame_pastariso <- data.frame(word = names(sort_pastariso), freq = sort_pastariso)

top_10_pastariso <- data_frame_pastariso %>%
  arrange(desc(freq)) %>% head(10)

#Howard's Tweets#
howards <- c("howard's", "syrup")
howards_search <- paste(howards, collapse = " AND ")
howards_tweets <- search_tweets(q = howards_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(howards_tweets)

head(howards_tweets$text)

howards_text <- howards_tweets$text

corpus_howards <- Corpus(VectorSource(howards_tweets$text))

howards_emoji_clean <- tm_map(corpus_howards, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_howards<- tm_map(howards_emoji_clean, content_transformer(remove_urls))

lower_howards <- tm_map(url_clean_howards, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_howards <- tm_map(lower_howards, content_transformer(remove_items))

remove_punct_howards <- tm_map(remove_howards, removePunctuation)

remove_num_howards <- tm_map(remove_punct_howards, removeNumbers)

remove_stop_howards <- tm_map(remove_num_howards, removeWords, stopwords("english"))

remove_white_howards <- tm_map(remove_stop_howards, stripWhitespace)

remove_words_howards <- tm_map(remove_white_howards, removeWords, c("howard's", "howards", "howard", "syrup", "syrups", "amp"))

matrix_howards <- TermDocumentMatrix((remove_words_howards))
matrix_howards_2 <- as.matrix(matrix_howards)
sort_howards <- sort(rowSums(matrix_howards_2), decreasing = TRUE)
data_frame_howards <- data.frame(word = names(sort_howards), freq = sort_howards)

top_10_howards <- data_frame_howards %>%
  arrange(desc(freq)) %>% head(10)

howards_final <- crossing(top_10_howards, brand = "Howard's")
howards_final

#Arlow Tweets#
arlow <- c("arlow", "pasta")
arlow_search <- paste(arlow, collapse = " AND ")
arlow_tweets <- search_tweets(q = arlow_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(arlow_tweets)

head(arlow_tweets$text)

arlow_text <- arlow_tweets$text

corpus_arlow <- Corpus(VectorSource(arlow_tweets$text))

arlow_emoji_clean <- tm_map(corpus_arlow, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_arlow<- tm_map(arlow_emoji_clean, content_transformer(remove_urls))

lower_arlow <- tm_map(url_clean_arlow, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_arlow <- tm_map(lower_arlow, content_transformer(remove_items))

remove_punct_arlow <- tm_map(remove_arlow, removePunctuation)

remove_num_arlow <- tm_map(remove_punct_arlow, removeNumbers)

remove_stop_arlow <- tm_map(remove_num_arlow, removeWords, stopwords("english"))

remove_white_arlow <- tm_map(remove_stop_arlow, stripWhitespace)

remove_words_arlow <- tm_map(remove_white_arlow, removeWords, c("arlow", "arlows", "low", "lows", "pasta", "pastas", "amp"))

matrix_arlow <- TermDocumentMatrix((remove_words_arlow))
matrix_arlow_2 <- as.matrix(matrix_arlow)
sort_arlow <- sort(rowSums(matrix_arlow_2), decreasing = TRUE)
data_frame_arlow <- data.frame(word = names(sort_arlow), freq = sort_arlow)

top_10_arlow <- data_frame_arlow %>%
  arrange(desc(freq)) %>% head(10)

#Notta Tweets#
notta <- c("notta", "pasta")
notta_search <- paste(notta, collapse = " AND ")
notta_tweets <- search_tweets(q = notta_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(notta_tweets)

head(notta_tweets$text)

notta_text <- notta_tweets$text

corpus_notta <- Corpus(VectorSource(notta_tweets$text))

notta_emoji_clean <- tm_map(corpus_notta, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_notta<- tm_map(notta_emoji_clean, content_transformer(remove_urls))

lower_notta <- tm_map(url_clean_notta, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_notta <- tm_map(lower_notta, content_transformer(remove_items))

remove_punct_notta <- tm_map(remove_notta, removePunctuation)

remove_num_notta <- tm_map(remove_punct_notta, removeNumbers)

remove_stop_notta <- tm_map(remove_num_notta, removeWords, stopwords("english"))

remove_white_notta <- tm_map(remove_stop_notta, stripWhitespace)

remove_words_notta <- tm_map(remove_white_notta, removeWords, c("notta", "nottas", "nots", "not", "pasta", "pastas", "amp"))

matrix_notta <- TermDocumentMatrix((remove_words_notta))
matrix_notta_2 <- as.matrix(matrix_notta)
sort_notta <- sort(rowSums(matrix_notta_2), decreasing = TRUE)
data_frame_notta <- data.frame(word = names(sort_notta), freq = sort_notta)

top_10_notta <- data_frame_notta %>%
  arrange(desc(freq)) %>% head(10)

#Antoine's Tweets#
antoines <- c("antoine's", "pasta")
antoines_search <- paste(antoines, collapse = " AND ")
antoines_tweets <- search_tweets(q = antoines_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(antoines_tweets)

head(antoines_tweets$text)

antoines_text <- antoines_tweets$text

corpus_antoines <- Corpus(VectorSource(antoines_tweets$text))

antoines_emoji_clean <- tm_map(corpus_antoines, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_antoines<- tm_map(antoines_emoji_clean, content_transformer(remove_urls))

lower_antoines <- tm_map(url_clean_antoines, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_antoines <- tm_map(lower_antoines, content_transformer(remove_items))

remove_punct_antoines <- tm_map(remove_antoines, removePunctuation)

remove_num_antoines <- tm_map(remove_punct_antoines, removeNumbers)

remove_stop_antoines <- tm_map(remove_num_antoines, removeWords, stopwords("english"))

remove_white_antoines <- tm_map(remove_stop_antoines, stripWhitespace)

remove_words_antoines <- tm_map(remove_white_antoines, removeWords, c("antoine's", "antoine", "antoines", "ant", "ants", "pasta", "pastas", "amp"))

matrix_antoines <- TermDocumentMatrix((remove_words_antoines))
matrix_antoines_2 <- as.matrix(matrix_antoines)
sort_antoines <- sort(rowSums(matrix_antoines_2), decreasing = TRUE)
data_frame_antoines <- data.frame(word = names(sort_antoines), freq = sort_antoines)

top_10_antoines <- data_frame_antoines %>%
  arrange(desc(freq)) %>% head(10)

#Bellino Tweets#
bellino <- c("bellino", "pasta")
bellino_search <- paste(bellino, collapse = " AND ")
bellino_tweets <- search_tweets(q = bellino_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(bellino_tweets)

head(bellino_tweets$text)

bellino_text <- bellino_tweets$text

corpus_bellino <- Corpus(VectorSource(bellino_tweets$text))

bellino_emoji_clean <- tm_map(corpus_bellino, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_bellino<- tm_map(bellino_emoji_clean, content_transformer(remove_urls))

lower_bellino <- tm_map(url_clean_bellino, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_bellino <- tm_map(lower_bellino, content_transformer(remove_items))

remove_punct_bellino <- tm_map(remove_bellino, removePunctuation)

remove_num_bellino <- tm_map(remove_punct_bellino, removeNumbers)

remove_stop_bellino <- tm_map(remove_num_bellino, removeWords, stopwords("english"))

remove_white_bellino <- tm_map(remove_stop_bellino, stripWhitespace)

remove_words_bellino <- tm_map(remove_white_bellino, removeWords, c("bellino", "bellinos", "bell", "bells", "pasta", "pastas", "amp"))

matrix_bellino <- TermDocumentMatrix((remove_words_bellino))
matrix_bellino_2 <- as.matrix(matrix_bellino)
sort_bellino <- sort(rowSums(matrix_bellino_2), decreasing = TRUE)
data_frame_bellino <- data.frame(word = names(sort_bellino), freq = sort_bellino)

top_10_bellino <- data_frame_bellino %>%
  arrange(desc(freq)) %>% head(10)

#Cento Tweets#
cento <- c("cento", "pasta")
cento_search <- paste(cento, collapse = " AND ")
cento_tweets <- search_tweets(q = cento_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(cento_tweets)

head(cento_tweets$text)

cento_text <- cento_tweets$text

corpus_cento <- Corpus(VectorSource(cento_tweets$text))

cento_emoji_clean <- tm_map(corpus_cento, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_cento<- tm_map(cento_emoji_clean, content_transformer(remove_urls))

lower_cento <- tm_map(url_clean_cento, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_cento <- tm_map(lower_cento, content_transformer(remove_items))

remove_punct_cento <- tm_map(remove_cento, removePunctuation)

remove_num_cento <- tm_map(remove_punct_cento, removeNumbers)

remove_stop_cento <- tm_map(remove_num_cento, removeWords, stopwords("english"))

remove_white_cento <- tm_map(remove_stop_cento, stripWhitespace)

remove_words_cento <- tm_map(remove_white_cento, removeWords, c("cento", "centos", "cent", "cents"< "pasta", "pastas", "amp"))

matrix_cento <- TermDocumentMatrix((remove_words_cento))
matrix_cento_2 <- as.matrix(matrix_cento)
sort_cento <- sort(rowSums(matrix_cento_2), decreasing = TRUE)
data_frame_cento <- data.frame(word = names(sort_cento), freq = sort_cento)

top_10_cento <- data_frame_cento %>%
  arrange(desc(freq)) %>% head(10)

#Knott's Tweets#
knotts <- c("knott's", "syrup")
knotts_search <- paste(knotts, collapse = " AND ")
knotts_tweets <- search_tweets(q = knotts_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(knotts_tweets)

head(knotts_tweets$text)

knotts_text <- knotts_tweets$text

corpus_knotts <- Corpus(VectorSource(knotts_tweets$text))

knotts_emoji_clean <- tm_map(corpus_knotts, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_knotts<- tm_map(knotts_emoji_clean, content_transformer(remove_urls))

lower_knotts <- tm_map(url_clean_knotts, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_knotts <- tm_map(lower_knotts, content_transformer(remove_items))

remove_punct_knotts <- tm_map(remove_knotts, removePunctuation)

remove_num_knotts <- tm_map(remove_punct_knotts, removeNumbers)

remove_stop_knotts <- tm_map(remove_num_knotts, removeWords, stopwords("english"))

remove_white_knotts <- tm_map(remove_stop_knotts, stripWhitespace)

remove_words_knotts <- tm_map(remove_white_knotts, removeWords, c("knott's", "knotts", "knott", "not", "nots", "syrup", "syrups", "amp", "debonairedamon", "grrs"))

matrix_knotts <- TermDocumentMatrix((remove_words_knotts))
matrix_knotts_2 <- as.matrix(matrix_knotts)
sort_knotts <- sort(rowSums(matrix_knotts_2), decreasing = TRUE)
data_frame_knotts <- data.frame(word = names(sort_knotts), freq = sort_knotts)

top_10_knotts <- data_frame_knotts %>%
  arrange(desc(freq)) %>% head(10)

knotts_final <- crossing(top_10_knotts, brand = "Knott's")
knotts_final

#Alessi Tweets#
alessi <- c("alessi", "pasta")
alessi_search <- paste(alessi, collapse = "AND")
alessi_tweets <- search_tweets(q = alessi_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(alessi_tweets)

head(alessi_tweets$text)

alessi_text <- alessi_tweets$text

corpus_alessi <- Corpus(VectorSource(alessi_tweets$text))

alessi_emoji_clean <- tm_map(corpus_alessi, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_alessi<- tm_map(alessi_emoji_clean, content_transformer(remove_urls))

lower_alessi <- tm_map(url_clean_alessi, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_alessi <- tm_map(lower_alessi, content_transformer(remove_items))

remove_punct_alessi <- tm_map(remove_alessi, removePunctuation)

remove_num_alessi <- tm_map(remove_punct_alessi, removeNumbers)

remove_stop_alessi <- tm_map(remove_num_alessi, removeWords, stopwords("english"))

remove_white_alessi <- tm_map(remove_stop_alessi, stripWhitespace)

remove_words_alessi <- tm_map(remove_white_alessi, removeWords, c("alessi", "alessis", "pasta", "pastas", "amp"))

matrix_alessi <- TermDocumentMatrix((remove_words_alessi))
matrix_alessi_2 <- as.matrix(matrix_alessi)
sort_alessi <- sort(rowSums(matrix_alessi_2), decreasing = TRUE)
data_frame_alessi <- data.frame(word = names(sort_alessi), freq = sort_alessi)

top_10_alessi <- data_frame_alessi %>%
  arrange(desc(freq)) %>% head(10)

#Ferrara Tweets#
ferrara <- c("ferrara", "pasta")
ferrara_search <- paste(ferrara, collapse = " AND ")
ferrara_tweets <- search_tweets(q = ferrara_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(ferrara_tweets)

head(ferrara_tweets$text)

ferrara_text <- ferrara_tweets$text

corpus_ferrara <- Corpus(VectorSource(ferrara_tweets$text))

ferrara_emoji_clean <- tm_map(corpus_ferrara, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_ferrara<- tm_map(ferrara_emoji_clean, content_transformer(remove_urls))

lower_ferrara <- tm_map(url_clean_ferrara, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_ferrara <- tm_map(lower_ferrara, content_transformer(remove_items))

remove_punct_ferrara <- tm_map(remove_ferrara, removePunctuation)

remove_num_ferrara <- tm_map(remove_punct_ferrara, removeNumbers)

remove_stop_ferrara <- tm_map(remove_num_ferrara, removeWords, stopwords("english"))

remove_white_ferrara <- tm_map(remove_stop_ferrara, stripWhitespace)

remove_words_ferrara <- tm_map(remove_white_ferrara, removeWords, c("ferrara", "ferraras", "pasta", "pastas", "amp"))

matrix_ferrara <- TermDocumentMatrix((remove_words_ferrara))
matrix_ferrara_2 <- as.matrix(matrix_ferrara)
sort_ferrara <- sort(rowSums(matrix_ferrara_2), decreasing = TRUE)
data_frame_ferrara <- data.frame(word = names(sort_ferrara), freq = sort_ferrara)

top_10_ferrara <- data_frame_ferrara %>%
  arrange(desc(freq)) %>% head(10)

#Hodgson Mills Tweets#
hodgson_mills <- c("pancake", "pasta")
hodgson_mills_combine <- paste(hodgson_mills, collapse = " OR ")
hodgson_mills_2 <- c('"hodgson mills"', hodgson_mills_combine)
hodgson_mills_search <- paste(hodgson_mills_2, collapse = " AND ")
hodgson_mills_tweets <- search_tweets(q = hodgson_mills_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(hodgson_mills_tweets)

head(hodgson_mills_tweets$text)

hodgson_mills_text <- hodgson_mills_tweets$text

corpus_hodgson_mills <- Corpus(VectorSource(hodgson_mills_tweets$text))

hodgson_mills_emoji_clean <- tm_map(corpus_hodgson_mills, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_hodgson_mills<- tm_map(hodgson_mills_emoji_clean, content_transformer(remove_urls))

lower_hodgson_mills <- tm_map(url_clean_hodgson_mills, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_hodgson_mills <- tm_map(lower_hodgson_mills, content_transformer(remove_items))

remove_punct_hodgson_mills <- tm_map(remove_hodgson_mills, removePunctuation)

remove_num_hodgson_mills <- tm_map(remove_punct_hodgson_mills, removeNumbers)

remove_stop_hodgson_mills <- tm_map(remove_num_hodgson_mills, removeWords, stopwords("english"))

remove_white_hodgson_mills <- tm_map(remove_stop_hodgson_mills, stripWhitespace)

remove_words_hodgson_mills <- tm_map(remove_white_hodgson_mills, removeWords, c("hodgson mills", "hodgson", "mills", "mill", "pancake", "pancakes", "pasta", "pastas", "amp", "also", "chejfoojgg", "dslchmekk", "mix", "nhquepicr"))

matrix_hodgson_mills <- TermDocumentMatrix((remove_words_hodgson_mills))
matrix_hodgson_mills_2 <- as.matrix(matrix_hodgson_mills)
sort_hodgson_mills <- sort(rowSums(matrix_hodgson_mills_2), decreasing = TRUE)
data_frame_hodgson_mills <- data.frame(word = names(sort_hodgson_mills), freq = sort_hodgson_mills)

top_10_hodgson_mills <- data_frame_hodgson_mills %>%
  arrange(desc(freq)) %>% head(10)

hodgson_mills_final <- crossing(top_10_hodgson_mills, brand = "Hodgson Mills")
hodgson_mills_final

#R&F Tweets#
rf <- c("r&f", "pasta")
rf_search <- paste(rf, collapse = " AND ")
rf_tweets <- search_tweets(q = rf_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(rf_tweets)

head(rf_tweets$text)

rf_text <- rf_tweets$text

corpus_rf <- Corpus(VectorSource(rf_tweets$text))

rf_emoji_clean <- tm_map(corpus_rf, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_rf<- tm_map(rf_emoji_clean, content_transformer(remove_urls))

lower_rf <- tm_map(url_clean_rf, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_rf <- tm_map(lower_rf, content_transformer(remove_items))

remove_punct_rf <- tm_map(remove_rf, removePunctuation)

remove_num_rf <- tm_map(remove_punct_rf, removeNumbers)

remove_stop_rf <- tm_map(remove_num_rf, removeWords, stopwords("english"))

remove_white_rf <- tm_map(remove_stop_rf, stripWhitespace)

remove_words_rf <- tm_map(remove_white_rf, removeWords, c("r&f", "pasta", "pastas", "amp", "aftertheboop", "andrewfairbairn", "angrymoocow", "annotatedmst", "battlefleetb", "bhodii", "bobbilljim", "catfacejoe", "detroitqspider", "djcozmik", "dtjqfixwh", "etc", "fartcrab", "formatamerica", "garbagedotnet", "glennsmithpr", "henderburn", "ilovepets", "iluvcomputer", "jerrybindle", "jesuswep", "jfarmdogg", "jimprov", "joedonbaker", "joenihl", "jonny", "kennethlogins", "modusoperandi"))

matrix_rf <- TermDocumentMatrix((remove_words_rf))
matrix_rf_2 <- as.matrix(matrix_rf)
sort_rf <- sort(rowSums(matrix_rf_2), decreasing = TRUE)
data_frame_rf <- data.frame(word = names(sort_rf), freq = sort_rf)

top_10_rf <- data_frame_rf %>%
  arrange(desc(freq)) %>% head(10)

rf_final <- crossing(top_10_rf, brand = "R&F")
rf_final

#Wacky Mac Tweets#
wacky_mac <- c('"wacky mac"', "pasta")
wacky_mac_search <- paste(wacky_mac, collapse = " AND ")
wacky_mac_tweets <- search_tweets(q = wacky_mac_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(wacky_mac_tweets)

head(wacky_mac_tweets$text)

wacky_mac_text <- wacky_mac_tweets$text

corpus_wacky_mac <- Corpus(VectorSource(wacky_mac_tweets$text))

wacky_mac_emoji_clean <- tm_map(corpus_wacky_mac, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_wacky_mac<- tm_map(wacky_mac_emoji_clean, content_transformer(remove_urls))

lower_wacky_mac <- tm_map(url_clean_wacky_mac, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_wacky_mac <- tm_map(lower_wacky_mac, content_transformer(remove_items))

remove_punct_wacky_mac <- tm_map(remove_wacky_mac, removePunctuation)

remove_num_wacky_mac <- tm_map(remove_punct_wacky_mac, removeNumbers)

remove_stop_wacky_mac <- tm_map(remove_num_wacky_mac, removeWords, stopwords("english"))

remove_white_wacky_mac <- tm_map(remove_stop_wacky_mac, stripWhitespace)

remove_words_wacky_mac <- tm_map(remove_white_wacky_mac, removeWords, c("wacky mac", "wacky", "mac", "wack", "pasta", "pastas", "amp"))

matrix_wacky_mac <- TermDocumentMatrix((remove_words_wacky_mac))
matrix_wacky_mac_2 <- as.matrix(matrix_wacky_mac)
sort_wacky_mac <- sort(rowSums(matrix_wacky_mac_2), decreasing = TRUE)
data_frame_wacky_mac <- data.frame(word = names(sort_wacky_mac), freq = sort_wacky_mac)

top_10_wacky_mac <- data_frame_wacky_mac %>%
  arrange(desc(freq)) %>% head(10)

#No Yolks Tweets#
no_yolks <- c('"no yolks"', "pasta")
no_yolks_search <- paste(no_yolks, collapse = " AND ")
no_yolks_tweets <- search_tweets(q = no_yolks_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(no_yolks_tweets)

head(no_yolks_tweets$text)

no_yolks_text <- no_yolks_tweets$text

corpus_no_yolks <- Corpus(VectorSource(no_yolks_tweets$text))

no_yolks_emoji_clean <- tm_map(corpus_no_yolks, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_no_yolks<- tm_map(no_yolks_emoji_clean, content_transformer(remove_urls))

lower_no_yolks <- tm_map(url_clean_no_yolks, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_no_yolks <- tm_map(lower_no_yolks, content_transformer(remove_items))

remove_punct_no_yolks <- tm_map(remove_no_yolks, removePunctuation)

remove_num_no_yolks <- tm_map(remove_punct_no_yolks, removeNumbers)

remove_stop_no_yolks <- tm_map(remove_num_no_yolks, removeWords, stopwords("english"))

remove_white_no_yolks <- tm_map(remove_stop_no_yolks, stripWhitespace)

remove_words_no_yolks <- tm_map(remove_white_no_yolks, removeWords, c("no yolks", "no", "yolks", "yolk", "pasta", "pastas", "amp", "dcryzmpcf", "essentia"))

matrix_no_yolks <- TermDocumentMatrix((remove_words_no_yolks))
matrix_no_yolks_2 <- as.matrix(matrix_no_yolks)
sort_no_yolks <- sort(rowSums(matrix_no_yolks_2), decreasing = TRUE)
data_frame_no_yolks <- data.frame(word = names(sort_no_yolks), freq = sort_no_yolks)

top_10_no_yolks <- data_frame_no_yolks %>%
  arrange(desc(freq)) %>% head(10)

no_yolks_final <- crossing(top_10_no_yolks, brand = "No Yolks")
no_yolks_final

#Mrs Weiss Tweets#
mrs_weiss <- c('"mrs weiss"', "pasta")
mrs_weiss_search <- paste(mrs_weiss, collapse = " AND ")
mrs_weiss_tweets <- search_tweets(q = mrs_weiss_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(mrs_weiss_tweets)

head(mrs_weiss_tweets$text)

mrs_weiss_text <- mrs_weiss_tweets$text

corpus_mrs_weiss <- Corpus(VectorSource(mrs_weiss_tweets$text))

mrs_weiss_emoji_clean <- tm_map(corpus_mrs_weiss, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_mrs_weiss<- tm_map(mrs_weiss_emoji_clean, content_transformer(remove_urls))

lower_mrs_weiss <- tm_map(url_clean_mrs_weiss, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_mrs_weiss <- tm_map(lower_mrs_weiss, content_transformer(remove_items))

remove_punct_mrs_weiss <- tm_map(remove_mrs_weiss, removePunctuation)

remove_num_mrs_weiss <- tm_map(remove_punct_mrs_weiss, removeNumbers)

remove_stop_mrs_weiss <- tm_map(remove_num_mrs_weiss, removeWords, stopwords("english"))

remove_white_mrs_weiss <- tm_map(remove_stop_mrs_weiss, stripWhitespace)

remove_words_mrs_weiss <- tm_map(remove_white_mrs_weiss, removeWords, c("mrs weiss", "mrs", "weiss", "mr", "pasta", "pastas", "amp"))

matrix_mrs_weiss <- TermDocumentMatrix((remove_words_mrs_weiss))
matrix_mrs_weiss_2 <- as.matrix(matrix_mrs_weiss)
sort_mrs_weiss <- sort(rowSums(matrix_mrs_weiss_2), decreasing = TRUE)
data_frame_mrs_weiss <- data.frame(word = names(sort_mrs_weiss), freq = sort_mrs_weiss)

top_10_mrs_weiss <- data_frame_mrs_weiss %>%
  arrange(desc(freq)) %>% head(10)

#Amore Tweets#
amore <- c("amore", "pasta")
amore_search <- paste(amore, collapse = " AND ")
amore_tweets <- search_tweets(q = amore_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(amore_tweets)

head(amore_tweets$text)

amore_text <- amore_tweets$text

corpus_amore <- Corpus(VectorSource(amore_tweets$text))

amore_emoji_clean <- tm_map(corpus_amore, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_amore<- tm_map(amore_emoji_clean, content_transformer(remove_urls))

lower_amore <- tm_map(url_clean_amore, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_amore <- tm_map(lower_amore, content_transformer(remove_items))

remove_punct_amore <- tm_map(remove_amore, removePunctuation)

remove_num_amore <- tm_map(remove_punct_amore, removeNumbers)

remove_stop_amore <- tm_map(remove_num_amore, removeWords, stopwords("english"))

remove_white_amore <- tm_map(remove_stop_amore, stripWhitespace)

remove_words_amore <- tm_map(remove_white_amore, removeWords, c("amore", "amores", "more", "mores", "pasta", "pastas", "amp", "also", "fazool", "just"))

matrix_amore <- TermDocumentMatrix((remove_words_amore))
matrix_amore_2 <- as.matrix(matrix_amore)
sort_amore <- sort(rowSums(matrix_amore_2), decreasing = TRUE)
data_frame_amore <- data.frame(word = names(sort_amore), freq = sort_amore)

top_10_amore <- data_frame_amore %>%
  arrange(desc(freq)) %>% head(10)

amore_final <- crossing(top_10_amore, brand = "Amore")
amore_final

#HSE Tweets#
hse <- c("hse", "syrup")
hse_search <- paste(hse, collapse = " AND ")
hse_tweets <- search_tweets(q = hse_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(hse_tweets)

head(hse_tweets$text)

hse_text <- hse_tweets$text

corpus_hse <- Corpus(VectorSource(hse_tweets$text))

hse_emoji_clean <- tm_map(corpus_hse, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_hse<- tm_map(hse_emoji_clean, content_transformer(remove_urls))

lower_hse <- tm_map(url_clean_hse, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

remove_hse <- tm_map(lower_hse, content_transformer(remove_items))

remove_punct_hse <- tm_map(remove_hse, removePunctuation)

remove_num_hse <- tm_map(remove_punct_hse, removeNumbers)

remove_stop_hse <- tm_map(remove_num_hse, removeWords, stopwords("english"))

remove_white_hse <- tm_map(remove_stop_hse, stripWhitespace)

remove_words_hse <- tm_map(remove_white_hse, removeWords, c("hse", "syrup", "syrups", "amp"))

matrix_hse <- TermDocumentMatrix((remove_words_hse))
matrix_hse_2 <- as.matrix(matrix_hse)
sort_hse <- sort(rowSums(matrix_hse_2), decreasing = TRUE)
data_frame_hse <- data.frame(word = names(sort_hse), freq = sort_hse)

top_10_hse <- data_frame_hse %>%
  arrange(desc(freq)) %>% head(10)

hse_final <- crossing(top_10_hse, brand = "HSE")
hse_final

#Enrico Tweets#
enrico <- c("enrico", "pasta")
enrico_search <- paste(enrico, collapse = " AND ")
enrico_tweets <- search_tweets(q = enrico_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(enrico_tweets)

head(enrico_tweets$text)

enrico_text <- enrico_tweets$text

corpus_enrico <- Corpus(VectorSource(enrico_tweets$text))

enrico_emoji_clean <- tm_map(corpus_enrico, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_enrico<- tm_map(enrico_emoji_clean, content_transformer(remove_urls))

lower_enrico <- tm_map(url_clean_enrico, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_enrico <- tm_map(lower_enrico, content_transformer(remove_items))

remove_punct_enrico <- tm_map(remove_enrico, removePunctuation)

remove_num_enrico <- tm_map(remove_punct_enrico, removeNumbers)

remove_stop_enrico <- tm_map(remove_num_enrico, removeWords, stopwords("english"))

remove_white_enrico <- tm_map(remove_stop_enrico, stripWhitespace)

remove_words_enrico <- tm_map(remove_white_enrico, removeWords, c("enrico", "enricos", "rico", "ricos", "pasta", "pastas", "amp"))

matrix_enrico <- TermDocumentMatrix((remove_words_enrico))
matrix_enrico_2 <- as.matrix(matrix_enrico)
sort_enrico <- sort(rowSums(matrix_enrico_2), decreasing = TRUE)
data_frame_enrico <- data.frame(word = names(sort_enrico), freq = sort_enrico)

top_10_enrico <- data_frame_enrico %>%
  arrange(desc(freq)) %>% head(10)

#Kedem Tweets#
kedem <- c("kedem", "pasta")
kedem_search <- paste(kedem, collapse = " AND ")
kedem_tweets <- search_tweets(q = kedem_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(kedem_tweets)

head(kedem_tweets$text)

kedem_text <- kedem_tweets$text

corpus_kedem <- Corpus(VectorSource(kedem_tweets$text))

kedem_emoji_clean <- tm_map(corpus_kedem, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_kedem<- tm_map(kedem_emoji_clean, content_transformer(remove_urls))

lower_kedem <- tm_map(url_clean_kedem, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_kedem <- tm_map(lower_kedem, content_transformer(remove_items))

remove_punct_kedem <- tm_map(remove_kedem, removePunctuation)

remove_num_kedem <- tm_map(remove_punct_kedem, removeNumbers)

remove_stop_kedem <- tm_map(remove_num_kedem, removeWords, stopwords("english"))

remove_white_kedem <- tm_map(remove_stop_kedem, stripWhitespace)

remove_words_kedem <- tm_map(remove_white_kedem, removeWords, c("kedem", "kedems", "pasta", "pastas", "amp"))

matrix_kedem <- TermDocumentMatrix((remove_words_kedem))
matrix_kedem_2 <- as.matrix(matrix_kedem)
sort_kedem <- sort(rowSums(matrix_kedem_2), decreasing = TRUE)
data_frame_kedem <- data.frame(word = names(sort_kedem), freq = sort_kedem)

top_10_kedem <- data_frame_kedem %>%
  arrange(desc(freq)) %>% head(10)

#Maple Grove Tweets#
maple_grove <- c("pancake", "syrup")
maple_grove_combine <- paste(maple_grove, collapse = " OR ")
maple_grove_2 <- c('"maple grove"', maple_grove_combine)
maple_grove_search <- paste(maple_grove_2, collapse = " AND ")
maple_grove_tweets <- search_tweets(q = maple_grove_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(maple_grove_tweets)

head(maple_grove_tweets$text)

maple_grove_text <- maple_grove_tweets$text

corpus_maple_grove <- Corpus(VectorSource(maple_grove_tweets$text))

maple_grove_emoji_clean <- tm_map(corpus_maple_grove, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_maple_grove<- tm_map(maple_grove_emoji_clean, content_transformer(remove_urls))

lower_maple_grove <- tm_map(url_clean_maple_grove, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_maple_grove <- tm_map(lower_maple_grove, content_transformer(remove_items))

remove_punct_maple_grove <- tm_map(remove_maple_grove, removePunctuation)

remove_num_maple_grove <- tm_map(remove_punct_maple_grove, removeNumbers)

remove_stop_maple_grove <- tm_map(remove_num_maple_grove, removeWords, stopwords("english"))

remove_white_maple_grove <- tm_map(remove_stop_maple_grove, stripWhitespace)

remove_words_maple_grove <- tm_map(remove_white_maple_grove, removeWords, c("maple grove", "maple", "maples", "grove", "groves", "pancake", "pancakes", "syrup", "syrups", "amp", "also", "mix", "nhquepicr"))

matrix_maple_grove <- TermDocumentMatrix((remove_words_maple_grove))
matrix_maple_grove_2 <- as.matrix(matrix_maple_grove)
sort_maple_grove <- sort(rowSums(matrix_maple_grove_2), decreasing = TRUE)
data_frame_maple_grove <- data.frame(word = names(sort_maple_grove), freq = sort_maple_grove)

top_10_maple_grove <- data_frame_maple_grove %>%
  arrange(desc(freq)) %>% head(10)

maple_grove_final <- crossing(top_10_maple_grove, brand = "Maple Grove")
maple_grove_final

#Cozy Cottage Tweets#
cozy_cottage <- c('"cozy cottage"', "syrup")
cozy_cottage_search <- paste(cozy_cottage, collapse = " AND ")
cozy_cottage_tweets <- search_tweets(q = cozy_cottage_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(cozy_cottage_tweets)

head(cozy_cottage_tweets$text)

cozy_cottage_text <- cozy_cottage_tweets$text

corpus_cozy_cottage <- Corpus(VectorSource(cozy_cottage_tweets$text))

cozy_cottage_emoji_clean <- tm_map(corpus_cozy_cottage, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_cozy_cottage<- tm_map(cozy_cottage_emoji_clean, content_transformer(remove_urls))

lower_cozy_cottage <- tm_map(url_clean_cozy_cottage, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_cozy_cottage <- tm_map(lower_cozy_cottage, content_transformer(remove_items))

remove_punct_cozy_cottage <- tm_map(remove_cozy_cottage, removePunctuation)

remove_num_cozy_cottage <- tm_map(remove_punct_cozy_cottage, removeNumbers)

remove_stop_cozy_cottage <- tm_map(remove_num_cozy_cottage, removeWords, stopwords("english"))

remove_white_cozy_cottage <- tm_map(remove_stop_cozy_cottage, stripWhitespace)

remove_words_cozy_cottage <- tm_map(remove_white_cozy_cottage, removeWords, c("cozy cottage", "cozy cottages", "cozy", "cottage", "cottages", "syrup", "syrups", "amp"))

matrix_cozy_cottage <- TermDocumentMatrix((remove_words_cozy_cottage))
matrix_cozy_cottage_2 <- as.matrix(matrix_cozy_cottage)
sort_cozy_cottage <- sort(rowSums(matrix_cozy_cottage_2), decreasing = TRUE)
data_frame_cozy_cottage <- data.frame(word = names(sort_cozy_cottage), freq = sort_cozy_cottage)

top_10_cozy_cottage <- data_frame_cozy_cottage %>%
  arrange(desc(freq)) %>% head(10)

#Emeril's Tweets#
emerils <- c("emeril's", "pasta")
emerils_search <- paste(emerils, collapse = " AND ")
emerils_tweets <- search_tweets(q = emerils_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(emerils_tweets)

head(emerils_tweets$text)

emerils_text <- emerils_tweets$text

corpus_emerils <- Corpus(VectorSource(emerils_tweets$text))

emerils_emoji_clean <- tm_map(corpus_emerils, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_emerils<- tm_map(emerils_emoji_clean, content_transformer(remove_urls))

lower_emerils <- tm_map(url_clean_emerils, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_emerils <- tm_map(lower_emerils, content_transformer(remove_items))

remove_punct_emerils <- tm_map(remove_emerils, removePunctuation)

remove_num_emerils <- tm_map(remove_punct_emerils, removeNumbers)

remove_stop_emerils <- tm_map(remove_num_emerils, removeWords, stopwords("english"))

remove_white_emerils <- tm_map(remove_stop_emerils, stripWhitespace)

remove_words_emerils <- tm_map(remove_white_emerils, removeWords, c("emeril's", "emeril", "emerils", "pasta", "pastas", "amp"))

matrix_emerils <- TermDocumentMatrix((remove_words_emerils))
matrix_emerils_2 <- as.matrix(matrix_emerils)
sort_emerils <- sort(rowSums(matrix_emerils_2), decreasing = TRUE)
data_frame_emerils <- data.frame(word = names(sort_emerils), freq = sort_emerils)

top_10_emerils <- data_frame_emerils %>%
  arrange(desc(freq)) %>% head(10)

#Mayacmas Tweets#
mayacmas <- c("mayacmas", "pasta")
mayacmas_search <- paste(mayacmas, collapse = " AND ")
mayacmas_tweets <- search_tweets(q = mayacmas_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(mayacmas_tweets)

head(mayacmas_tweets$text)

mayacmas_text <- mayacmas_tweets$text

corpus_mayacmas <- Corpus(VectorSource(mayacmas_tweets$text))

mayacmas_emoji_clean <- tm_map(corpus_mayacmas, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_mayacmas<- tm_map(mayacmas_emoji_clean, content_transformer(remove_urls))

lower_mayacmas <- tm_map(url_clean_mayacmas, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_mayacmas <- tm_map(lower_mayacmas, content_transformer(remove_items))

remove_punct_mayacmas <- tm_map(remove_mayacmas, removePunctuation)

remove_num_mayacmas <- tm_map(remove_punct_mayacmas, removeNumbers)

remove_stop_mayacmas <- tm_map(remove_num_mayacmas, removeWords, stopwords("english"))

remove_white_mayacmas <- tm_map(remove_stop_mayacmas, stripWhitespace)

remove_words_mayacmas <- tm_map(remove_white_mayacmas, removeWords, c("mayacmas", "mayacma", "maya", "mayas", "pasta", "pastas", "amp"))

matrix_mayacmas <- TermDocumentMatrix((remove_words_mayacmas))
matrix_mayacmas_2 <- as.matrix(matrix_mayacmas)
sort_mayacmas <- sort(rowSums(matrix_mayacmas_2), decreasing = TRUE)
data_frame_mayacmas <- data.frame(word = names(sort_mayacmas), freq = sort_mayacmas)

top_10_mayacmas <- data_frame_mayacmas %>%
  arrange(desc(freq)) %>% head(10)

#Lyles Tweets#
lyles <- c("lyles", "syrup")
lyles_search <- paste(lyles, collapse = " AND ")
lyles_tweets <- search_tweets(q = lyles_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(lyles_tweets)

head(lyles_tweets$text)

lyles_text <- lyles_tweets$text

corpus_lyles <- Corpus(VectorSource(lyles_tweets$text))

lyles_emoji_clean <- tm_map(corpus_lyles, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_lyles<- tm_map(lyles_emoji_clean, content_transformer(remove_urls))

lower_lyles <- tm_map(url_clean_lyles, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_lyles <- tm_map(lower_lyles, content_transformer(remove_items))

remove_punct_lyles <- tm_map(remove_lyles, removePunctuation)

remove_num_lyles <- tm_map(remove_punct_lyles, removeNumbers)

remove_stop_lyles <- tm_map(remove_num_lyles, removeWords, stopwords("english"))

remove_white_lyles <- tm_map(remove_stop_lyles, stripWhitespace)

remove_words_lyles <- tm_map(remove_white_lyles, removeWords, c("lyles", "lyle", "syrup", "syrups", "amp"))

matrix_lyles <- TermDocumentMatrix((remove_words_lyles))
matrix_lyles_2 <- as.matrix(matrix_lyles)
sort_lyles <- sort(rowSums(matrix_lyles_2), decreasing = TRUE)
data_frame_lyles <- data.frame(word = names(sort_lyles), freq = sort_lyles)

top_10_lyles <- data_frame_lyles %>%
  arrange(desc(freq)) %>% head(10)

lyles_final <- crossing(top_10_lyles, brand = "Lyles")
lyles_final

#Eddie Tweets#
eddie <- c("eddie", "pasta")
eddie_search <- paste(eddie, collapse = " AND ")
eddie_tweets <- search_tweets(q = eddie_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(eddie_tweets)

head(eddie_tweets$text)

eddie_text <- eddie_tweets$text

corpus_eddie <- Corpus(VectorSource(eddie_tweets$text))

eddie_emoji_clean <- tm_map(corpus_eddie, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_eddie<- tm_map(eddie_emoji_clean, content_transformer(remove_urls))

lower_eddie <- tm_map(url_clean_eddie, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_eddie <- tm_map(lower_eddie, content_transformer(remove_items))

remove_punct_eddie <- tm_map(remove_eddie, removePunctuation)

remove_num_eddie <- tm_map(remove_punct_eddie, removeNumbers)

remove_stop_eddie <- tm_map(remove_num_eddie, removeWords, stopwords("english"))

remove_white_eddie <- tm_map(remove_stop_eddie, stripWhitespace)

remove_words_eddie <- tm_map(remove_white_eddie, removeWords, c("eddie", "eddies", "ed", "eds", "pasta", "pastas", "amp", "just", "surgital"))

matrix_eddie <- TermDocumentMatrix((remove_words_eddie))
matrix_eddie_2 <- as.matrix(matrix_eddie)
sort_eddie <- sort(rowSums(matrix_eddie_2), decreasing = TRUE)
data_frame_eddie <- data.frame(word = names(sort_eddie), freq = sort_eddie)

top_10_eddie <- data_frame_eddie %>%
  arrange(desc(freq)) %>% head(10)

eddie_final <- crossing(top_10_eddie, brand = "Eddie")
eddie_final

#Northwoods Tweets#
northwoods <- c("northwoods", "syrup")
northwoods_search <- paste(northwoods, collapse = " AND ")
northwoods_tweets <- search_tweets(q = northwoods_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(northwoods_tweets)

head(northwoods_tweets$text)

northwoods_text <- northwoods_tweets$text

corpus_northwoods <- Corpus(VectorSource(northwoods_tweets$text))

northwoods_emoji_clean <- tm_map(corpus_northwoods, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_northwoods<- tm_map(northwoods_emoji_clean, content_transformer(remove_urls))

lower_northwoods <- tm_map(url_clean_northwoods, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_northwoods <- tm_map(lower_northwoods, content_transformer(remove_items))

remove_punct_northwoods <- tm_map(remove_northwoods, removePunctuation)

remove_num_northwoods <- tm_map(remove_punct_northwoods, removeNumbers)

remove_stop_northwoods <- tm_map(remove_num_northwoods, removeWords, stopwords("english"))

remove_white_northwoods <- tm_map(remove_stop_northwoods, stripWhitespace)

remove_words_northwoods <- tm_map(remove_white_northwoods, removeWords, c("northwoods", "northwood", "north", "norths", "wood", "woods", "syrup", "syrups", "amp", "aren"))

matrix_northwoods <- TermDocumentMatrix((remove_words_northwoods))
matrix_northwoods_2 <- as.matrix(matrix_northwoods)
sort_northwoods <- sort(rowSums(matrix_northwoods_2), decreasing = TRUE)
data_frame_northwoods <- data.frame(word = names(sort_northwoods), freq = sort_northwoods)

top_10_northwoods <- data_frame_northwoods %>%
  arrange(desc(freq)) %>% head(10)

northwoods_final <- crossing(top_10_northwoods, brand = "Northwoods")
northwoods_final

#Dell Alpe Tweets#
dell_alpe <- c('"dell alpe"', "pasta")
dell_alpe_search <- paste(dell_alpe, collapse = " AND ")
dell_alpe_tweets <- search_tweets(q = dell_alpe_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(dell_alpe_tweets)

head(dell_alpe_tweets$text)

dell_alpe_text <- dell_alpe_tweets$text

corpus_dell_alpe <- Corpus(VectorSource(dell_alpe_tweets$text))

dell_alpe_emoji_clean <- tm_map(corpus_dell_alpe, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_dell_alpe<- tm_map(dell_alpe_emoji_clean, content_transformer(remove_urls))

lower_dell_alpe <- tm_map(url_clean_dell_alpe, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_dell_alpe <- tm_map(lower_dell_alpe, content_transformer(remove_items))

remove_punct_dell_alpe <- tm_map(remove_dell_alpe, removePunctuation)

remove_num_dell_alpe <- tm_map(remove_punct_dell_alpe, removeNumbers)

remove_stop_dell_alpe <- tm_map(remove_num_dell_alpe, removeWords, stopwords("english"))

remove_white_dell_alpe <- tm_map(remove_stop_dell_alpe, stripWhitespace)

remove_words_dell_alpe <- tm_map(remove_white_dell_alpe, removeWords, c("dell alpe", "dell alpes", "dell", "dells", "alpe", "alpes", "pasta", "pastas", "amp"))

matrix_dell_alpe <- TermDocumentMatrix((remove_words_dell_alpe))
matrix_dell_alpe_2 <- as.matrix(matrix_dell_alpe)
sort_dell_alpe <- sort(rowSums(matrix_dell_alpe_2), decreasing = TRUE)
data_frame_dell_alpe <- data.frame(word = names(sort_dell_alpe), freq = sort_dell_alpe)

top_10_dell_alpe <- data_frame_dell_alpe %>%
  arrange(desc(freq)) %>% head(10)

#Classique Tweets#
classique <- c("classique", "pancake")
classique_search <- paste(classique, collapse = " AND ")
classique_tweets <- search_tweets(q = classique_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(classique_tweets)

head(classique_tweets$text)

classique_text <- classique_tweets$text

corpus_classique <- Corpus(VectorSource(classique_tweets$text))

classique_emoji_clean <- tm_map(corpus_classique, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_classique<- tm_map(classique_emoji_clean, content_transformer(remove_urls))

lower_classique <- tm_map(url_clean_classique, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_classique <- tm_map(lower_classique, content_transformer(remove_items))

remove_punct_classique <- tm_map(remove_classique, removePunctuation)

remove_num_classique <- tm_map(remove_punct_classique, removeNumbers)

remove_stop_classique <- tm_map(remove_num_classique, removeWords, stopwords("english"))

remove_white_classique <- tm_map(remove_stop_classique, stripWhitespace)

remove_words_classique <- tm_map(remove_white_classique, removeWords, c("classique", "classiques", "pancake", "pancakes", "amp"))

matrix_classique <- TermDocumentMatrix((remove_words_classique))
matrix_classique_2 <- as.matrix(matrix_classique)
sort_classique <- sort(rowSums(matrix_classique_2), decreasing = TRUE)
data_frame_classique <- data.frame(word = names(sort_classique), freq = sort_classique)

top_10_classique <- data_frame_classique %>%
  arrange(desc(freq)) %>% head(10)

#Osem Bissli Tweets#
osem_bissli <- c('"osem bissli"', "pancake")
osem_bissli_search <- paste(osem_bissli, collapse = " AND ")
osem_bissli_tweets <- search_tweets(q = osem_bissli_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(osem_bissli_tweets)

head(osem_bissli_tweets$text)

osem_bissli_text <- osem_bissli_tweets$text

corpus_osem_bissli <- Corpus(VectorSource(osem_bissli_tweets$text))

osem_bissli_emoji_clean <- tm_map(corpus_osem_bissli, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_osem_bissli<- tm_map(osem_bissli_emoji_clean, content_transformer(remove_urls))

lower_osem_bissli <- tm_map(url_clean_osem_bissli, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_osem_bissli <- tm_map(lower_osem_bissli, content_transformer(remove_items))

remove_punct_osem_bissli <- tm_map(remove_osem_bissli, removePunctuation)

remove_num_osem_bissli <- tm_map(remove_punct_osem_bissli, removeNumbers)

remove_stop_osem_bissli <- tm_map(remove_num_osem_bissli, removeWords, stopwords("english"))

remove_white_osem_bissli <- tm_map(remove_stop_osem_bissli, stripWhitespace)

remove_words_osem_bissli <- tm_map(remove_white_osem_bissli, removeWords, c("osem bissli", "osem bisslis", "osem", "osems", "bissli", "bisslis", "pancake", "pancakes", "amp"))

matrix_osem_bissli <- TermDocumentMatrix((remove_words_osem_bissli))
matrix_osem_bissli_2 <- as.matrix(matrix_osem_bissli)
sort_osem_bissli <- sort(rowSums(matrix_osem_bissli_2), decreasing = TRUE)
data_frame_osem_bissli <- data.frame(word = names(sort_osem_bissli), freq = sort_osem_bissli)

top_10_osem_bissli <- data_frame_osem_bissli %>%
  arrange(desc(freq)) %>% head(10)

#Darielle Tweets#
darielle <- c("darielle", "pasta")
darielle_search <- paste(darielle, collapse = " AND ")
darielle_tweets <- search_tweets(q = darielle_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(darielle_tweets)

head(darielle_tweets$text)

darielle_text <- darielle_tweets$text

corpus_darielle <- Corpus(VectorSource(darielle_tweets$text))

darielle_emoji_clean <- tm_map(corpus_darielle, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_darielle<- tm_map(darielle_emoji_clean, content_transformer(remove_urls))

lower_darielle <- tm_map(url_clean_darielle, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_darielle <- tm_map(lower_darielle, content_transformer(remove_items))

remove_punct_darielle <- tm_map(remove_darielle, removePunctuation)

remove_num_darielle <- tm_map(remove_punct_darielle, removeNumbers)

remove_stop_darielle <- tm_map(remove_num_darielle, removeWords, stopwords("english"))

remove_white_darielle <- tm_map(remove_stop_darielle, stripWhitespace)

remove_words_darielle <- tm_map(remove_white_darielle, removeWords, c("darielle", "darielles", "pasta", "pastas", "amp"))

matrix_darielle <- TermDocumentMatrix((remove_words_darielle))
matrix_darielle_2 <- as.matrix(matrix_darielle)
sort_darielle <- sort(rowSums(matrix_darielle_2), decreasing = TRUE)
data_frame_darielle <- data.frame(word = names(sort_darielle), freq = sort_darielle)

top_10_darielle <- data_frame_darielle %>%
  arrange(desc(freq)) %>% head(10)

#Sand Mountain Tweets#
sand_mountain <- c('"sand mountain"', "syrup")
sand_mountain_search <- paste(sand_mountain, collapse = " AND ")
sand_mountain_tweets <- search_tweets(q = sand_mountain_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(sand_mountain_tweets)

head(sand_mountain_tweets$text)

sand_mountain_text <- sand_mountain_tweets$text

corpus_sand_mountain <- Corpus(VectorSource(sand_mountain_tweets$text))

sand_mountain_emoji_clean <- tm_map(corpus_sand_mountain, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_sand_mountain<- tm_map(sand_mountain_emoji_clean, content_transformer(remove_urls))

lower_sand_mountain <- tm_map(url_clean_sand_mountain, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_sand_mountain <- tm_map(lower_sand_mountain, content_transformer(remove_items))

remove_punct_sand_mountain <- tm_map(remove_sand_mountain, removePunctuation)

remove_num_sand_mountain <- tm_map(remove_punct_sand_mountain, removeNumbers)

remove_stop_sand_mountain <- tm_map(remove_num_sand_mountain, removeWords, stopwords("english"))

remove_white_sand_mountain <- tm_map(remove_stop_sand_mountain, stripWhitespace)

remove_words_sand_mountain <- tm_map(remove_white_sand_mountain, removeWords, c("sand mountain", "sand", "sands", "mountain", "mountains", "sand mountains", "syrup", "syrups", "amp"))

matrix_sand_mountain <- TermDocumentMatrix((remove_words_sand_mountain))
matrix_sand_mountain_2 <- as.matrix(matrix_sand_mountain)
sort_sand_mountain <- sort(rowSums(matrix_sand_mountain_2), decreasing = TRUE)
data_frame_sand_mountain <- data.frame(word = names(sort_sand_mountain), freq = sort_sand_mountain)

top_10_sand_mountain <- data_frame_sand_mountain %>%
  arrange(desc(freq)) %>% head(10)

#Defino Tweets#
defino <- c("defino", "pasta")
defino_search <- paste(defino, collapse = " AND ")
defino_tweets <- search_tweets(q = defino_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(defino_tweets)

head(defino_tweets$text)

defino_text <- defino_tweets$text

corpus_defino <- Corpus(VectorSource(defino_tweets$text))

defino_emoji_clean <- tm_map(corpus_defino, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_defino<- tm_map(defino_emoji_clean, content_transformer(remove_urls))

lower_defino <- tm_map(url_clean_defino, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_defino <- tm_map(lower_defino, content_transformer(remove_items))

remove_punct_defino <- tm_map(remove_defino, removePunctuation)

remove_num_defino <- tm_map(remove_punct_defino, removeNumbers)

remove_stop_defino <- tm_map(remove_num_defino, removeWords, stopwords("english"))

remove_white_defino <- tm_map(remove_stop_defino, stripWhitespace)

remove_words_defino <- tm_map(remove_white_defino, removeWords, c("defino", "definos", "pasta", "pastas", "amp"))

matrix_defino <- TermDocumentMatrix((remove_words_defino))
matrix_defino_2 <- as.matrix(matrix_defino)
sort_defino <- sort(rowSums(matrix_defino_2), decreasing = TRUE)
data_frame_defino <- data.frame(word = names(sort_defino), freq = sort_defino)

top_10_defino <- data_frame_defino %>%
  arrange(desc(freq)) %>% head(10)

#Roselli Tweets#
roselli <- c("roselli", "pasta")
roselli_search <- paste(roselli, collapse = " AND ")
roselli_tweets <- search_tweets(q = roselli_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(roselli_tweets)

head(roselli_tweets$text)

roselli_text <- roselli_tweets$text

corpus_roselli <- Corpus(VectorSource(roselli_tweets$text))

roselli_emoji_clean <- tm_map(corpus_roselli, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_roselli<- tm_map(roselli_emoji_clean, content_transformer(remove_urls))

lower_roselli <- tm_map(url_clean_roselli, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_roselli <- tm_map(lower_roselli, content_transformer(remove_items))

remove_punct_roselli <- tm_map(remove_roselli, removePunctuation)

remove_num_roselli <- tm_map(remove_punct_roselli, removeNumbers)

remove_stop_roselli <- tm_map(remove_num_roselli, removeWords, stopwords("english"))

remove_white_roselli <- tm_map(remove_stop_roselli, stripWhitespace)

remove_words_roselli <- tm_map(remove_white_roselli, removeWords, c("roselli", "rosellis", "rose", "roses", "pasta", "pastas", "amp"))

matrix_roselli <- TermDocumentMatrix((remove_words_roselli))
matrix_roselli_2 <- as.matrix(matrix_roselli)
sort_roselli <- sort(rowSums(matrix_roselli_2), decreasing = TRUE)
data_frame_roselli <- data.frame(word = names(sort_roselli), freq = sort_roselli)

top_10_roselli <- data_frame_roselli %>%
  arrange(desc(freq)) %>% head(10)

#Al Dente Tweets#
al_dente <- c('"al dente"', "pasta")
al_dente_search <- paste(al_dente, collapse = " AND ")
al_dente_tweets <- search_tweets(q = al_dente_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(al_dente_tweets)

head(al_dente_tweets$text)

al_dente_text <- al_dente_tweets$text

corpus_al_dente <- Corpus(VectorSource(al_dente_tweets$text))

al_dente_emoji_clean <- tm_map(corpus_al_dente, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_al_dente<- tm_map(al_dente_emoji_clean, content_transformer(remove_urls))

lower_al_dente <- tm_map(url_clean_al_dente, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_al_dente <- tm_map(lower_al_dente, content_transformer(remove_items))

remove_punct_al_dente <- tm_map(remove_al_dente, removePunctuation)

remove_num_al_dente <- tm_map(remove_punct_al_dente, removeNumbers)

remove_stop_al_dente <- tm_map(remove_num_al_dente, removeWords, stopwords("english"))

remove_white_al_dente <- tm_map(remove_stop_al_dente, stripWhitespace)

remove_words_al_dente <- tm_map(remove_white_al_dente, removeWords, c("al dente", "al dentes", "al", "als", "dente", "dentes", "pasta", "pastas", "amp", "don"))

matrix_al_dente <- TermDocumentMatrix((remove_words_al_dente))
matrix_al_dente_2 <- as.matrix(matrix_al_dente)
sort_al_dente <- sort(rowSums(matrix_al_dente_2), decreasing = TRUE)
data_frame_al_dente <- data.frame(word = names(sort_al_dente), freq = sort_al_dente)

top_10_al_dente <- data_frame_al_dente %>%
  arrange(desc(freq)) %>% head(10)

al_dente_final <- crossing(top_10_al_dente, brand = "Al Dente")
al_dente_final

#Moonlite Tweets#
moonlite <- c("moonlite", "syrup")
moonlite_search <- paste(moonlite, collapse = " AND ")
moonlite_tweets <- search_tweets(q = moonlite_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(moonlite_tweets)

head(moonlite_tweets$text)

moonlite_text <- moonlite_tweets$text

corpus_moonlite <- Corpus(VectorSource(moonlite_tweets$text))

moonlite_emoji_clean <- tm_map(corpus_moonlite, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_moonlite<- tm_map(moonlite_emoji_clean, content_transformer(remove_urls))

lower_moonlite <- tm_map(url_clean_moonlite, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_moonlite <- tm_map(lower_moonlite, content_transformer(remove_items))

remove_punct_moonlite <- tm_map(remove_moonlite, removePunctuation)

remove_num_moonlite <- tm_map(remove_punct_moonlite, removeNumbers)

remove_stop_moonlite <- tm_map(remove_num_moonlite, removeWords, stopwords("english"))

remove_white_moonlite <- tm_map(remove_stop_moonlite, stripWhitespace)

remove_words_moonlite <- tm_map(remove_white_moonlite, removeWords, c("moonlite", "moonlites", "moon", "moons", "lite", "lites", "syrup", "syrups", "amp"))

matrix_moonlite <- TermDocumentMatrix((remove_words_moonlite))
matrix_moonlite_2 <- as.matrix(matrix_moonlite)
sort_moonlite <- sort(rowSums(matrix_moonlite_2), decreasing = TRUE)
data_frame_moonlite <- data.frame(word = names(sort_moonlite), freq = sort_moonlite)

top_10_moonlite <- data_frame_moonlite %>%
  arrange(desc(freq)) %>% head(10)

#Candoni Tweets#
candoni <- c("candoni", "pasta")
candoni_search <- paste(candoni, collapse = " AND ")
candoni_tweets <- search_tweets(q = candoni_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(candoni_tweets)

head(candoni_tweets$text)

candoni_text <- candoni_tweets$text

corpus_candoni <- Corpus(VectorSource(candoni_tweets$text))

candoni_emoji_clean <- tm_map(corpus_candoni, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_candoni<- tm_map(candoni_emoji_clean, content_transformer(remove_urls))

lower_candoni <- tm_map(url_clean_candoni, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_candoni <- tm_map(lower_candoni, content_transformer(remove_items))

remove_punct_candoni <- tm_map(remove_candoni, removePunctuation)

remove_num_candoni <- tm_map(remove_punct_candoni, removeNumbers)

remove_stop_candoni <- tm_map(remove_num_candoni, removeWords, stopwords("english"))

remove_white_candoni <- tm_map(remove_stop_candoni, stripWhitespace)

remove_words_candoni <- tm_map(remove_white_candoni, removeWords, c("candoni", "candonis", "can", "cans", "pasta", "pastas", "amp"))

matrix_candoni <- TermDocumentMatrix((remove_words_candoni))
matrix_candoni_2 <- as.matrix(matrix_candoni)
sort_candoni <- sort(rowSums(matrix_candoni_2), decreasing = TRUE)
data_frame_candoni <- data.frame(word = names(sort_candoni), freq = sort_candoni)

top_10_candoni <- data_frame_candoni %>%
  arrange(desc(freq)) %>% head(10)

#Mlinotst Tweets#
mlinotst <- c("mlinotst", "pasta")
mlinotst_search <- paste(mlinotst, collapse = " AND ")
mlinotst_tweets <- search_tweets(q = mlinotst_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(mlinotst_tweets)

head(mlinotst_tweets$text)

mlinotst_text <- mlinotst_tweets$text

corpus_mlinotst <- Corpus(VectorSource(mlinotst_tweets$text))

mlinotst_emoji_clean <- tm_map(corpus_mlinotst, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_mlinotst<- tm_map(mlinotst_emoji_clean, content_transformer(remove_urls))

lower_mlinotst <- tm_map(url_clean_mlinotst, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_mlinotst <- tm_map(lower_mlinotst, content_transformer(remove_items))

remove_punct_mlinotst <- tm_map(remove_mlinotst, removePunctuation)

remove_num_mlinotst <- tm_map(remove_punct_mlinotst, removeNumbers)

remove_stop_mlinotst <- tm_map(remove_num_mlinotst, removeWords, stopwords("english"))

remove_white_mlinotst <- tm_map(remove_stop_mlinotst, stripWhitespace)

remove_words_mlinotst <- tm_map(remove_white_mlinotst, removeWords, c("mlinotst", "mlinotsts", "pasta", "pastas", "amp"))

matrix_mlinotst <- TermDocumentMatrix((remove_words_mlinotst))
matrix_mlinotst_2 <- as.matrix(matrix_mlinotst)
sort_mlinotst <- sort(rowSums(matrix_mlinotst_2), decreasing = TRUE)
data_frame_mlinotst <- data.frame(word = names(sort_mlinotst), freq = sort_mlinotst)

top_10_mlinotst <- data_frame_mlinotst %>%
  arrange(desc(freq)) %>% head(10)

#Silver Palate Tweets#
silver_palate <- c('"silver palate"', "pasta")
silver_palate_search <- paste(silver_palate, collapse = " AND ")
silver_palate_tweets <- search_tweets(q = silver_palate_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(silver_palate_tweets)

head(silver_palate_tweets$text)

silver_palate_text <- silver_palate_tweets$text

corpus_silver_palate <- Corpus(VectorSource(silver_palate_tweets$text))

silver_palate_emoji_clean <- tm_map(corpus_silver_palate, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_silver_palate<- tm_map(silver_palate_emoji_clean, content_transformer(remove_urls))

lower_silver_palate <- tm_map(url_clean_silver_palate, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_silver_palate <- tm_map(lower_silver_palate, content_transformer(remove_items))

remove_punct_silver_palate <- tm_map(remove_silver_palate, removePunctuation)

remove_num_silver_palate <- tm_map(remove_punct_silver_palate, removeNumbers)

remove_stop_silver_palate <- tm_map(remove_num_silver_palate, removeWords, stopwords("english"))

remove_white_silver_palate <- tm_map(remove_stop_silver_palate, stripWhitespace)

remove_words_silver_palate <- tm_map(remove_white_silver_palate, removeWords, c("silver palate", "silver palates", "silver", "silvers", "palate", "palates", "pasta", "pastas", "amp", "sdncfcoct", "yqsaqxfvbp", "sauces"))

matrix_silver_palate <- TermDocumentMatrix((remove_words_silver_palate))
matrix_silver_palate_2 <- as.matrix(matrix_silver_palate)
sort_silver_palate <- sort(rowSums(matrix_silver_palate_2), decreasing = TRUE)
data_frame_silver_palate <- data.frame(word = names(sort_silver_palate), freq = sort_silver_palate)

top_10_silver_palate <- data_frame_silver_palate %>%
  arrange(desc(freq)) %>% head(10)

silver_palate_final <- crossing(top_10_silver_palate, brand = "Silver Palate")
silver_palate_final

#Edd Og Tweets#
edd_og <- c('"edd og"', "pasta")
edd_og_search <- paste(edd_og, collapse = " AND ")
edd_og_tweets <- search_tweets(q = edd_og_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(edd_og_tweets)

head(edd_og_tweets$text)

edd_og_text <- edd_og_tweets$text

corpus_edd_og <- Corpus(VectorSource(edd_og_tweets$text))

edd_og_emoji_clean <- tm_map(corpus_edd_og, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_edd_og<- tm_map(edd_og_emoji_clean, content_transformer(remove_urls))

lower_edd_og <- tm_map(url_clean_edd_og, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_edd_og <- tm_map(lower_edd_og, content_transformer(remove_items))

remove_punct_edd_og <- tm_map(remove_edd_og, removePunctuation)

remove_num_edd_og <- tm_map(remove_punct_edd_og, removeNumbers)

remove_stop_edd_og <- tm_map(remove_num_edd_og, removeWords, stopwords("english"))

remove_white_edd_og <- tm_map(remove_stop_edd_og, stripWhitespace)

remove_words_edd_og <- tm_map(remove_white_edd_og, removeWords, c("edd og", "edd ogs", "edd", "edds", "og", "ogs", "pasta", "pastas", "amp"))

matrix_edd_og <- TermDocumentMatrix((remove_words_edd_og))
matrix_edd_og_2 <- as.matrix(matrix_edd_og)
sort_edd_og <- sort(rowSums(matrix_edd_og_2), decreasing = TRUE)
data_frame_edd_og <- data.frame(word = names(sort_edd_og), freq = sort_edd_og)

top_10_edd_og <- data_frame_edd_og %>%
  arrange(desc(freq)) %>% head(10)

#Michele's Tweets#
micheles <- c("michele's", "syrup")
micheles_search <- paste(micheles, collapse = " AND ")
micheles_tweets <- search_tweets(q = micheles_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(micheles_tweets)

head(micheles_tweets$text)

micheles_text <- micheles_tweets$text

corpus_micheles <- Corpus(VectorSource(micheles_tweets$text))

micheles_emoji_clean <- tm_map(corpus_micheles, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_micheles<- tm_map(micheles_emoji_clean, content_transformer(remove_urls))

lower_micheles <- tm_map(url_clean_micheles, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_micheles <- tm_map(lower_micheles, content_transformer(remove_items))

remove_punct_micheles <- tm_map(remove_micheles, removePunctuation)

remove_num_micheles <- tm_map(remove_punct_micheles, removeNumbers)

remove_stop_micheles <- tm_map(remove_num_micheles, removeWords, stopwords("english"))

remove_white_micheles <- tm_map(remove_stop_micheles, stripWhitespace)

remove_words_micheles <- tm_map(remove_white_micheles, removeWords, c("michele's", "micheles", "michele", "syrup", "syrups", "amp"))

matrix_micheles <- TermDocumentMatrix((remove_words_micheles))
matrix_micheles_2 <- as.matrix(matrix_micheles)
sort_micheles <- sort(rowSums(matrix_micheles_2), decreasing = TRUE)
data_frame_micheles <- data.frame(word = names(sort_micheles), freq = sort_micheles)

top_10_micheles <- data_frame_micheles %>%
  arrange(desc(freq)) %>% head(10)

#Cucina Tweets#
cucina <- c("cucina", "pasta")
cucina_search <- paste(cucina, collapse = " AND ")
cucina_tweets <- search_tweets(q = cucina_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(cucina_tweets)

head(cucina_tweets$text)

cucina_text <- cucina_tweets$text

corpus_cucina <- Corpus(VectorSource(cucina_tweets$text))

cucina_emoji_clean <- tm_map(corpus_cucina, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_cucina<- tm_map(cucina_emoji_clean, content_transformer(remove_urls))

lower_cucina <- tm_map(url_clean_cucina, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_cucina <- tm_map(lower_cucina, content_transformer(remove_items))

remove_punct_cucina <- tm_map(remove_cucina, removePunctuation)

remove_num_cucina <- tm_map(remove_punct_cucina, removeNumbers)

remove_stop_cucina <- tm_map(remove_num_cucina, removeWords, stopwords("english"))

remove_white_cucina <- tm_map(remove_stop_cucina, stripWhitespace)

remove_words_cucina <- tm_map(remove_white_cucina, removeWords, c("cucina", "cucinas", "pasta", "pastas", "amp", "also"))

matrix_cucina <- TermDocumentMatrix((remove_words_cucina))
matrix_cucina_2 <- as.matrix(matrix_cucina)
sort_cucina <- sort(rowSums(matrix_cucina_2), decreasing = TRUE)
data_frame_cucina <- data.frame(word = names(sort_cucina), freq = sort_cucina)

top_10_cucina <- data_frame_cucina %>%
  arrange(desc(freq)) %>% head(10)

cucina_final <- crossing(top_10_cucina, brand = "Cucina")
cucina_final

#Quinoa Tweets#
quinoa <- c("quinoa", "pasta")
quinoa_search <- paste(quinoa, collapse = " AND ")
quinoa_tweets <- search_tweets(q = quinoa_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(quinoa_tweets)

head(quinoa_tweets$text)

quinoa_text <- quinoa_tweets$text

corpus_quinoa <- Corpus(VectorSource(quinoa_tweets$text))

quinoa_emoji_clean <- tm_map(corpus_quinoa, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_quinoa<- tm_map(quinoa_emoji_clean, content_transformer(remove_urls))

lower_quinoa <- tm_map(url_clean_quinoa, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_quinoa <- tm_map(lower_quinoa, content_transformer(remove_items))

remove_punct_quinoa <- tm_map(remove_quinoa, removePunctuation)

remove_num_quinoa <- tm_map(remove_punct_quinoa, removeNumbers)

remove_stop_quinoa <- tm_map(remove_num_quinoa, removeWords, stopwords("english"))

remove_white_quinoa <- tm_map(remove_stop_quinoa, stripWhitespace)

remove_words_quinoa <- tm_map(remove_white_quinoa, removeWords, c("quinoa", "quinoas", "amp", "pasta"))

matrix_quinoa <- TermDocumentMatrix((remove_words_quinoa))
matrix_quinoa_2 <- as.matrix(matrix_quinoa)
sort_quinoa <- sort(rowSums(matrix_quinoa_2), decreasing = TRUE)
data_frame_quinoa <- data.frame(word = names(sort_quinoa), freq = sort_quinoa)

top_10_quinoa <- data_frame_quinoa %>%
  arrange(desc(freq)) %>% head(10)

quinoa_final <- crossing(top_10_quinoa, brand = "Quinoa")
quinoa_final

#M C Tweets#
m_c <- c('"m c"', "pasta")
m_c_search <- paste(m_c, collapse = " AND ")
m_c_tweets <- search_tweets(q = m_c_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(m_c_tweets)

head(m_c_tweets$text)

m_c_text <- m_c_tweets$text

corpus_m_c <- Corpus(VectorSource(m_c_tweets$text))

m_c_emoji_clean <- tm_map(corpus_m_c, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_m_c<- tm_map(m_c_emoji_clean, content_transformer(remove_urls))

lower_m_c <- tm_map(url_clean_m_c, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_m_c <- tm_map(lower_m_c, content_transformer(remove_items))

remove_punct_m_c <- tm_map(remove_m_c, removePunctuation)

remove_num_m_c <- tm_map(remove_punct_m_c, removeNumbers)

remove_stop_m_c <- tm_map(remove_num_m_c, removeWords, stopwords("english"))

remove_white_m_c <- tm_map(remove_stop_m_c, stripWhitespace)

remove_words_m_c <- tm_map(remove_white_m_c, removeWords, c("m c", "m", "c", "pasta", "pastas", "amp"))

matrix_m_c <- TermDocumentMatrix((remove_words_m_c))
matrix_m_c_2 <- as.matrix(matrix_m_c)
sort_m_c <- sort(rowSums(matrix_m_c_2), decreasing = TRUE)
data_frame_m_c <- data.frame(word = names(sort_m_c), freq = sort_m_c)

top_10_m_c <- data_frame_m_c %>%
  arrange(desc(freq)) %>% head(10)

m_c_final <- crossing(top_10_m_c, brand = "M C")
m_c_final

#RAC Tweets#
rac <- c("rac", "pasta")
rac_search <- paste(rac, collapse = " AND ")
rac_tweets <- search_tweets(q = rac_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(rac_tweets)

head(rac_tweets$text)

rac_text <- rac_tweets$text

corpus_rac <- Corpus(VectorSource(rac_tweets$text))

rac_emoji_clean <- tm_map(corpus_rac, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_rac<- tm_map(rac_emoji_clean, content_transformer(remove_urls))

lower_rac <- tm_map(url_clean_rac, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_rac <- tm_map(lower_rac, content_transformer(remove_items))

remove_punct_rac <- tm_map(remove_rac, removePunctuation)

remove_num_rac <- tm_map(remove_punct_rac, removeNumbers)

remove_stop_rac <- tm_map(remove_num_rac, removeWords, stopwords("english"))

remove_white_rac <- tm_map(remove_stop_rac, stripWhitespace)

remove_words_rac <- tm_map(remove_white_rac, removeWords, c("rac", "racs", "pasta", "pastas", "amp", "just", "please"))

matrix_rac <- TermDocumentMatrix((remove_words_rac))
matrix_rac_2 <- as.matrix(matrix_rac)
sort_rac <- sort(rowSums(matrix_rac_2), decreasing = TRUE)
data_frame_rac <- data.frame(word = names(sort_rac), freq = sort_rac)

top_10_rac <- data_frame_rac %>%
  arrange(desc(freq)) %>% head(10)

rac_final <- crossing(top_10_rac, brand = "RAC")
rac_final

#Barilla Plus Tweets#
barilla_plus <- c('"barilla plus"', "pasta")
barilla_plus_search <- paste(barilla_plus, collapse = " AND ")
barilla_plus_tweets <- search_tweets(q = barilla_plus_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(barilla_plus_tweets)

head(barilla_plus_tweets$text)

barilla_plus_text <- barilla_plus_tweets$text

corpus_barilla_plus <- Corpus(VectorSource(barilla_plus_tweets$text))

barilla_plus_emoji_clean <- tm_map(corpus_barilla_plus, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_barilla_plus<- tm_map(barilla_plus_emoji_clean, content_transformer(remove_urls))

lower_barilla_plus <- tm_map(url_clean_barilla_plus, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_barilla_plus <- tm_map(lower_barilla_plus, content_transformer(remove_items))

remove_punct_barilla_plus <- tm_map(remove_barilla_plus, removePunctuation)

remove_num_barilla_plus <- tm_map(remove_punct_barilla_plus, removeNumbers)

remove_stop_barilla_plus <- tm_map(remove_num_barilla_plus, removeWords, stopwords("english"))

remove_white_barilla_plus <- tm_map(remove_stop_barilla_plus, stripWhitespace)

remove_words_barilla_plus <- tm_map(remove_white_barilla_plus, removeWords, c("barilla plus", "barilla", "barillas", "plus", "pluses", "pasta", "pastas", "amp"))

matrix_barilla_plus <- TermDocumentMatrix((remove_words_barilla_plus))
matrix_barilla_plus_2 <- as.matrix(matrix_barilla_plus)
sort_barilla_plus <- sort(rowSums(matrix_barilla_plus_2), decreasing = TRUE)
data_frame_barilla_plus <- data.frame(word = names(sort_barilla_plus), freq = sort_barilla_plus)

top_10_barilla_plus <- data_frame_barilla_plus %>%
  arrange(desc(freq)) %>% head(10)

#Sinatra's Tweets#
sinatras <- c("sinatra's", "pasta")
sinatras_search <- paste(sinatras, collapse = " AND ")
sinatras_tweets <- search_tweets(q = sinatras_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(sinatras_tweets)

head(sinatras_tweets$text)

sinatras_text <- sinatras_tweets$text

corpus_sinatras <- Corpus(VectorSource(sinatras_tweets$text))

sinatras_emoji_clean <- tm_map(corpus_sinatras, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_sinatras<- tm_map(sinatras_emoji_clean, content_transformer(remove_urls))

lower_sinatras <- tm_map(url_clean_sinatras, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_sinatras <- tm_map(lower_sinatras, content_transformer(remove_items))

remove_punct_sinatras <- tm_map(remove_sinatras, removePunctuation)

remove_num_sinatras <- tm_map(remove_punct_sinatras, removeNumbers)

remove_stop_sinatras <- tm_map(remove_num_sinatras, removeWords, stopwords("english"))

remove_white_sinatras <- tm_map(remove_stop_sinatras, stripWhitespace)

remove_words_sinatras <- tm_map(remove_white_sinatras, removeWords, c("sinatra's", "sinatras", "sinatra", "pasta", "pastas", "amp", "got", "sinatrasays"))

matrix_sinatras <- TermDocumentMatrix((remove_words_sinatras))
matrix_sinatras_2 <- as.matrix(matrix_sinatras)
sort_sinatras <- sort(rowSums(matrix_sinatras_2), decreasing = TRUE)
data_frame_sinatras <- data.frame(word = names(sort_sinatras), freq = sort_sinatras)

top_10_sinatras <- data_frame_sinatras %>%
  arrange(desc(freq)) %>% head(10)

sinatras_final <- crossing(top_10_sinatras, brand = "Sinatra's")
sinatras_final

#Mom's Tweets#
moms <- c("mom's", "pasta")
moms_search <- paste(moms, collapse = " AND ")
moms_tweets <- search_tweets(q = moms_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(moms_tweets)

head(moms_tweets$text)

moms_text <- moms_tweets$text

corpus_moms <- Corpus(VectorSource(moms_tweets$text))

moms_emoji_clean <- tm_map(corpus_moms, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_moms<- tm_map(moms_emoji_clean, content_transformer(remove_urls))

lower_moms <- tm_map(url_clean_moms, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_moms <- tm_map(lower_moms, content_transformer(remove_items))

remove_punct_moms <- tm_map(remove_moms, removePunctuation)

remove_num_moms <- tm_map(remove_punct_moms, removeNumbers)

remove_stop_moms <- tm_map(remove_num_moms, removeWords, stopwords("english"))

remove_white_moms <- tm_map(remove_stop_moms, stripWhitespace)

remove_words_moms <- tm_map(remove_white_moms, removeWords, c("mom's", "moms", "mom", "pasta", "pastas", "amp", "made", "sauce", "making"))

matrix_moms <- TermDocumentMatrix((remove_words_moms))
matrix_moms_2 <- as.matrix(matrix_moms)
sort_moms <- sort(rowSums(matrix_moms_2), decreasing = TRUE)
data_frame_moms <- data.frame(word = names(sort_moms), freq = sort_moms)

top_10_moms <- data_frame_moms %>%
  arrange(desc(freq)) %>% head(10)

moms_final <- crossing(top_10_moms, brand = "Mom's")
moms_final

#Bionature Tweets#
bionature <- c("bionature", "pasta")
bionature_search <- paste(bionature, collapse = " AND ")
bionature_tweets <- search_tweets(q = bionature_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(bionature_tweets)

head(bionature_tweets$text)

bionature_text <- bionature_tweets$text

corpus_bionature <- Corpus(VectorSource(bionature_tweets$text))

bionature_emoji_clean <- tm_map(corpus_bionature, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_bionature<- tm_map(bionature_emoji_clean, content_transformer(remove_urls))

lower_bionature <- tm_map(url_clean_bionature, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_bionature <- tm_map(lower_bionature, content_transformer(remove_items))

remove_punct_bionature <- tm_map(remove_bionature, removePunctuation)

remove_num_bionature <- tm_map(remove_punct_bionature, removeNumbers)

remove_stop_bionature <- tm_map(remove_num_bionature, removeWords, stopwords("english"))

remove_white_bionature <- tm_map(remove_stop_bionature, stripWhitespace)

remove_words_bionature <- tm_map(remove_white_bionature, removeWords, c("bionature", "bionatures", "bio", "bios", "nature", "natures", "pasta", "pastas", "amp"))

matrix_bionature <- TermDocumentMatrix((remove_words_bionature))
matrix_bionature_2 <- as.matrix(matrix_bionature)
sort_bionature <- sort(rowSums(matrix_bionature_2), decreasing = TRUE)
data_frame_bionature <- data.frame(word = names(sort_bionature), freq = sort_bionature)

top_10_bionature <- data_frame_bionature %>%
  arrange(desc(freq)) %>% head(10)

#Private Label Premium Tweets#
private_label_premium <- c("pancake", "pasta", "syrup")
private_label_premium_combine <- paste(private_label_premium, collapse = " OR ")
private_label_premium_2 <- c('"private label premium"', private_label_premium_combine)
private_label_premium_search <- paste(private_label_premium_2, collapse = " AND ")
private_label_premium_tweets <- search_tweets(q = private_label_premium_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(private_label_premium_tweets)

head(private_label_premium_tweets$text)

private_label_premium_text <- private_label_premium_tweets$text

corpus_private_label_premium <- Corpus(VectorSource(private_label_premium_tweets$text))

private_label_premium_emoji_clean <- tm_map(corpus_private_label_premium, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_private_label_premium<- tm_map(private_label_premium_emoji_clean, content_transformer(remove_urls))

lower_private_label_premium <- tm_map(url_clean_private_label_premium, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_private_label_premium <- tm_map(lower_private_label_premium, content_transformer(remove_items))

remove_punct_private_label_premium <- tm_map(remove_private_label_premium, removePunctuation)

remove_num_private_label_premium <- tm_map(remove_punct_private_label_premium, removeNumbers)

remove_stop_private_label_premium <- tm_map(remove_num_private_label_premium, removeWords, stopwords("english"))

remove_white_private_label_premium <- tm_map(remove_stop_private_label_premium, stripWhitespace)

remove_words_private_label_premium <- tm_map(remove_white_private_label_premium, removeWords, c("private label premium", "private", "privates", "label", "labels", "premium", "premiums", "pancake", "pancakes", "pasta", "pastas", "syrup", "syrups", "amp"))

matrix_private_label_premium <- TermDocumentMatrix((remove_words_private_label_premium))
matrix_private_label_premium_2 <- as.matrix(matrix_private_label_premium)
sort_private_label_premium <- sort(rowSums(matrix_private_label_premium_2), decreasing = TRUE)
data_frame_private_label_premium <- data.frame(word = names(sort_private_label_premium), freq = sort_private_label_premium)

top_10_private_label_premium <- data_frame_private_label_premium %>%
  arrange(desc(freq)) %>% head(10)

#Private Label Tweets#
private_label <- c("pancake", "pasta", "syrup")
private_label_combine <- paste(private_label, collapse = " OR ")
private_label_2 <- c('"private label"', private_label_combine)
private_label_search <- paste(private_label_2, collapse = " AND ")
private_label_tweets <- search_tweets(q = private_label_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(private_label_tweets)

head(private_label_tweets$text)

private_label_text <- private_label_tweets$text

corpus_private_label <- Corpus(VectorSource(private_label_tweets$text))

private_label_emoji_clean <- tm_map(corpus_private_label, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_private_label<- tm_map(private_label_emoji_clean, content_transformer(remove_urls))

lower_private_label <- tm_map(url_clean_private_label, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_private_label <- tm_map(lower_private_label, content_transformer(remove_items))

remove_punct_private_label <- tm_map(remove_private_label, removePunctuation)

remove_num_private_label <- tm_map(remove_punct_private_label, removeNumbers)

remove_stop_private_label <- tm_map(remove_num_private_label, removeWords, stopwords("english"))

remove_white_private_label <- tm_map(remove_stop_private_label, stripWhitespace)

remove_words_private_label <- tm_map(remove_white_private_label, removeWords, c("private label", "private", "privates", "label", "labels", "pancake", "pancakes", "pasta", "pastas", "syrup", "syrups", "amp"))

matrix_private_label <- TermDocumentMatrix((remove_words_private_label))
matrix_private_label_2 <- as.matrix(matrix_private_label)
sort_private_label <- sort(rowSums(matrix_private_label_2), decreasing = TRUE)
data_frame_private_label <- data.frame(word = names(sort_private_label), freq = sort_private_label)

top_10_private_label <- data_frame_private_label %>%
  arrange(desc(freq)) %>% head(10)

#Private Label Value Tweets#
private_label_value <- c("pasta", "syrup")
private_label_value_combine <- paste(private_label_value, collapse = " OR ")
private_label_value_2 <- c('"private label value"', private_label_value_combine)
private_label_value_search <- paste(private_label_value_2, collapse = " AND ")
private_label_value_tweets <- search_tweets(q = private_label_value_search, n = 1000, lang = "en", include_rts = FALSE)
tibble(private_label_value_tweets)

head(private_label_value_tweets$text)

private_label_value_text <- private_label_value_tweets$text

corpus_private_label_value <- Corpus(VectorSource(private_label_value_tweets$text))

private_label_value_emoji_clean <- tm_map(corpus_private_label_value, content_transformer(gsub), pattern = "\\W", replace = " ")

remove_urls <- function(x) gsub("http[^[:space:]]*", "", x)

url_clean_private_label_value<- tm_map(private_label_value_emoji_clean, content_transformer(remove_urls))

lower_private_label_value <- tm_map(url_clean_private_label_value, content_transformer(tolower))

remove_items <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
remove_private_label_value <- tm_map(lower_private_label_value, content_transformer(remove_items))

remove_punct_private_label_value <- tm_map(remove_private_label_value, removePunctuation)

remove_num_private_label_value <- tm_map(remove_punct_private_label_value, removeNumbers)

remove_stop_private_label_value <- tm_map(remove_num_private_label_value, removeWords, stopwords("english"))

remove_white_private_label_value <- tm_map(remove_stop_private_label_value, stripWhitespace)

remove_words_private_label_value <- tm_map(remove_white_private_label_value, removeWords, c("private label value", "private", "privates", "label", "labels", "value", "values", "pasta", "pastas", "syrup", "syrups", "amp"))

matrix_private_label_value <- TermDocumentMatrix((remove_words_private_label_value))
matrix_private_label_value_2 <- as.matrix(matrix_private_label_value)
sort_private_label_value <- sort(rowSums(matrix_private_label_value_2), decreasing = TRUE)
data_frame_private_label_value <- data.frame(word = names(sort_private_label_value), freq = sort_private_label_value)

top_10_private_label_value <- data_frame_private_label_value %>%
  arrange(desc(freq)) %>% head(10)

#Combine twitter datasets#
twitter_data <- rbind(miller_final, barilla_final, golden_eagle_final, rr_final, alaga_final, creamette_final, bisquick_final, hungry_jack_final, kraft_final, dececco_final, eden_final, pomi_final, hunts_final, vita_final, mothers_final, mueller_final, la_moderna_final, aunt_jemima_final, tree_of_life_final, ronzoni_final, san_giorgio_final, ragu_final, bertolli_final, davinci_final, kellogg_final, colavita_final, san_marzano_final, classico_final, krusteaz_final, pioneer_final, buitoni_final, mrs_butterworth_final, raos_final, karo_final, prego_final, joeys_final, smuckers_final, daves_final, brothers_final, orzo_final, howards_final, knotts_final, hodgson_mills_final, amore_final, no_yolks_final, rf_final, hse_final, maple_grove_final, lyles_final, eddie_final, northwoods_final, al_dente_final, cucina_final, silver_palate_final, quinoa_final, m_c_final, rac_final, moms_final, sinatras_final)
twitter_data
