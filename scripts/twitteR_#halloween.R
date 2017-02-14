load_pacakges <- c("RColorBrewer", "wordcloud", "tm",
                   "stringr", "twitteR", "httr", "devtools", "base64enc")

lapply(load_pacakges, require, character.only = TRUE)

WorkDir <- "~/DA/Projects/twitteR_scrape/"
setwd(dir = WorkDir)
search_string <- "halloween" # Input search string as required

consumer_key <- "Insert your consumer key here"
consumer_secret <- "Insert your consumer secret here"
access_token <- "Insert your access token here" #if no access token available, set to NULL
access_secret <- "Insert your access secret here" #the same rules apply as access token

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
  gsub(pattern = "can", replacement = "") %>%
  str_replace_all(pattern = "[^a-zA-Z\\s]", replacement = " ")

search_string_corpus <- Corpus(VectorSource(search_string_text)) %>% 
                        tm_map(PlainTextDocument)

tdm <- TermDocumentMatrix(search_string_corpus, 
                          control = list(stopwords = c(search_string,stopwords("english"))))

m<-as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
word_freqs<-word_freqs
dm <- data.frame(word = names(word_freqs), freq = word_freqs)

png(paste0(WorkDir,"word-clouds/", search_string,".png"), height = 2000, width = 2000)
wordcloud(dm$word, dm$freq, scale=c(12,2), random.order = FALSE,
          colors= brewer.pal(5, "Set1"), max.words = 800, min.freq = 5,
          rot.per=.15)
dev.off()
