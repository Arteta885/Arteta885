# 패키지
library(tm)
library(tidyverse)
library(topicmodels)
library(stm)
library(tidytext)
library(reshape2)
library(igraph)
library(ggraph)
library(pdftools)
library(stringr)
library(SnowballC)
library(quanteda)
library(widyr)
library(tidygraph)

# 경로(folder_path)
folder_path <- "C:/Users/dlsdn/Desktop/논문작성2/인권문서모음_핵미사일_평화"

# PDF파일목록(pdf_files)
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

# PDF에서 TEXT 추출(이름지정:all_text)
all_text <- list()


##### Custom stemming list (0806 update) #####
stem_rules <- list(
  "leader" = "kim",
  "leadership" = "kim"
)

# 어간추출함수정의(ChatGPT활용)
custom_stem <- function(word) {
  if (word %in% names(stem_rules)) {
    return(stem_rules[[word]])
  } else {
    return(word)
  }
}

# 어간추출적용함수정의(ChatGPT활용)
apply_custom_stemming <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  stemmed_words <- sapply(words, custom_stem)
  return(paste(stemmed_words, collapse = " "))
}

# PDF 파일에서 TEXT 추출 및 어간 추출 적용
for (pdf_file in pdf_files) {
  cat(sprintf("Extracting text from file: %s\n", pdf_file))
  
  # 텍스트 추출
  text <- pdftools::pdf_text(pdf_file)
  
  # 소문자 변환
  text <- tolower(text)
  
  # 어간 추출 적용
  text <- apply_custom_stemming(text)
  
  # 전처리 수행 (마침표, 쉼표 제외)
  text <- gsub("[^a-zA-Z\\s\\.,]", " ", text)  # 특수문자 제거 (마침표와 쉼표 제외)
  text <- gsub("\\b\\d+\\b", " ", text)     # 숫자 제거
  text <- removeWords(text, stopwords("en"))  # 불용어 제거
  
  # 특정 단어 제거 (24 07 13 update)
  remove_words <- c("human", "rights", "special", "rapporteur", "international", 
                    "situation", "un", "united", "nations", "dprk", "democratic", 
                    "people", "s", "republic", "korea", "korean", "also", "commission",
                    "country", "council", "north", "ohchr", "hrc", "will", "report", 
                    "para", "state", "statement", "states", "resolution", "commissioner", 
                    "committee","session", "mr")
  text <- removeWords(text, remove_words)
  
  # 월 제거
  months <- c("january", "february", "march", "april", "may", "june", 
              "july", "august", "september", "october", "november", "december")
  text <- removeWords(text, months)
  
  # 소문자로 변환된 텍스트를 문장 단위로 분리
  sentences <- strsplit(text, "[.,]")[[1]]
  
  # 각 문장을 단어로 분리하여 단어의 개수를 세기
  word_counts <- sapply(sentences, function(sentence) length(strsplit(sentence, "\\s+")[[1]]))
  
  # 단어의 개수가 5개 이상인 문장만 선택
  selected_sentences <- sentences[word_counts >= 5]
  
  # 선택된 문장을 리스트에 추가
  all_text <- c(all_text, selected_sentences)  
}

###########################전 처 리############################

# 정제된 텍스트를 data_frame(tibble)로 변환
text_df <- tibble(text = unlist(all_text))

# 감성 분석 수행
sentiments <- get_sentiments("bing")
text_df <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(sentiments, by = "word")

# 감성 점수 계산
sentiment_scores <- text_df %>%
  count(index = row_number() %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# 감성 점수 시각화
ggplot(sentiment_scores, aes(x = index, y = sentiment)) +
  geom_line() +
  labs(title = "Sentiment Analysis Over Text Segments",
       x = "Text Segment",
       y = "Sentiment Score")

