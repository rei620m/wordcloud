%r
library(dplyr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(lubridate)

file_path <- "my_file_path"
survey_data <- read.csv(my_file_path, stringsAsFactors = FALSE)

# Update filters here
filtered_data <- survey_data %>%
  filter(Gender == "Women") %>%
  filter(PreferredLanguage %in% c("ja", "zh", "en")) %>%
  filter(!is.na(Answer_OpenEnded))

custom_stopwords <- c("like", "see", "can", "also", "will", "maybe", "'s", "make", "love", "hope", "nice", "many", "much", "good", "quite", "please", "want", "etc")

group_phrases <- function(text) {
  text <- gsub("hong kong", "hongkong", text, ignore.case = TRUE)
  text <- gsub("\\bjapan(e)?\\b", "japan", text, ignore.case = TRUE)
  text <- gsub("\\bevents?\\b", "event", text, ignore.case = TRUE)
  text <- gsub("\\b(clothes|clothing)\\b", "clothing", text, ignore.case = TRUE)
  return(text)
}

text_corpus <- Corpus(VectorSource(filtered_data$Answer_OpenEnded))
text_corpus <- tm_map(text_corpus, content_transformer(group_phrases))
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removePunctuation)

all_stopwords <- c(stopwords("en"), custom_stopwords)
text_corpus <- tm_map(text_corpus, removeWords, all_stopwords)
text_corpus <- tm_map(text_corpus, stripWhitespace)

tdm <- TermDocumentMatrix(text_corpus)
matrix <- as.matrix(tdm)

word_freqs <- sort(rowSums(matrix), decreasing=TRUE)
word_data <- data.frame(word=names(word_freqs), freq=word_freqs)
word_data <- word_data %>%
  filter(!is.na(freq) & freq > 0)

previous_month <- format(Sys.Date() - months(1), "%Y%m")

if (nrow(word_data) > 0) {
  set.seed(1234)  
  wordcloud(words = word_data$word, freq = word_data$freq, min.freq = 1,
            max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"),
            main = paste("survey_responses_", previous_month))
}  
