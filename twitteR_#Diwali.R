load_pacakges <- c("RColorBrewer", "wordcloud", "tm",
                   "stringr", "twitteR", "httr", "devtools", "base64enc")

lapply(load_pacakges, require, character.only = TRUE)

WorkDir <- "~/DA/Projects/twitteR_scrape/"
setwd(dir = WorkDir)
search_string <- "Diwali" # Input search string as required

consumer_key <- ""
consumer_secret <- ""
access_token <- NULL #if no access token available, set to NULL
access_secret <- NULL #the same rules apply as access token

setup_twitter_oauth(consumer_key = consumer_key,
                    consumer_secret = consumer_secret,
                    access_token = access_token,
                    access_secret = access_secret)

search_string_text <- 
  searchTwitter(paste0("#",search_string), n=5000, lang = "en") %>% 
  sapply(function(x) x$getText()) %>% 
  gsub(pattern = "(f|ht)tp(s?) ://(.*)[.][a-z]+", replacement = "") %>% 
  gsub(pattern = "(f|ht)tps(s?)://(.*)[.][a-z]+", replacement = "") %>% 
  gsub(pattern = "https", replacement = "") %>% 
  gsub(pattern = "via", replacement = "") %>%
  gsub(pattern = "amp", replacement = "") %>%
  gsub(pattern = "may", replacement = "") %>%
  gsub(pattern = "see", replacement = "") %>% 
  gsub(pattern = "can", replacement = "") %>%
  gsub(pattern = "now", replacement = "") %>%
  str_replace_all(pattern = "[^a-zA-Z\\s]", replacement = " ")

search_string_corpus <- Corpus(VectorSource(search_string_text)) %>% 
                        tm_map(PlainTextDocument)

tdm <- TermDocumentMatrix(search_string_corpus, 
                          control = list(stopwords = c(search_string,stopwords("english"))))

m<-as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
word_freqs<-word_freqs
dm <- data.frame(word = names(word_freqs), freq = word_freqs)

png(paste0(WorkDir,"plots/#", search_string,".png"))
wordcloud(dm$word, dm$freq, scale=c(6,0.5), random.order = FALSE,
          colors = brewer.pal(n = 5, name = "Set1"), max.words = 500, min.freq = 5)
dev.off()
