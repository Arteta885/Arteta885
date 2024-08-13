# 필요한 패키지 로드
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
library(wordcloud)
library(syuzhet)
library(udpipe)

# 경로(folder_path)
folder_path <- "C:/Users/dlsdn/Desktop/논문작성2/인권문서모음_핵미사일"

# PDF 파일 목록(pdf_files)
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

# PDF에서 TEXT 추출(이름 지정: all_text)
all_text <- list()

# Custom stemming list
stem_rules <- list("leader" = "kim", "leadership" = "kim")

# 어간 추출 함수 정의
custom_stem <- function(word) {
  if (word %in% names(stem_rules)) {
    return(stem_rules[[word]])
  } else {
    return(word)
  }
}

# 어간 추출 적용 함수 정의
apply_custom_stemming <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  stemmed_words <- sapply(words, custom_stem)
  return(paste(stemmed_words, collapse = " "))
}

# PDF 파일에서 TEXT 추출 및 어간 추출 적용
for (pdf_file in pdf_files) {
  cat(sprintf("Extracting text from file: %s\n", pdf_file))
  
  text <- pdftools::pdf_text(pdf_file)
  text <- tolower(text)
  text <- apply_custom_stemming(text)
  text <- gsub("[^a-zA-Z\\s\\.,]", " ", text)  # 특수문자 제거 (마침표와 쉼표 제외)
  text <- gsub("\\b\\d+\\b", " ", text)     # 숫자 제거
  text <- removeWords(text, stopwords("en"))  # 불용어 제거
  
  remove_words <- c("human", "rights", "special", "rapporteur", "international", 
                    "situation", "un", "united", "nations", "dprk", "democratic", 
                    "people", "s", "republic", "korea", "korean", "also", "commission",
                    "country", "council", "north", "ohchr", "hrc", "will", "report", 
                    "para", "state", "statement", "states", "resolution", "commissioner", 
                    "committee","session", "mr")
  text <- removeWords(text, remove_words)
  
  months <- c("january", "february", "march", "april", "may", "june", 
              "july", "august", "september", "october", "november", "december")
  text <- removeWords(text, months)
  
  sentences <- strsplit(text, "[.,]")[[1]]
  word_counts <- sapply(sentences, function(sentence) length(strsplit(sentence, "\\s+")[[1]]))
  selected_sentences <- sentences[word_counts >= 5]
  
  all_text <- c(all_text, selected_sentences)  
}

# 텍스트 데이터를 데이터프레임으로 변환
text_df <- tibble(text = unlist(all_text))

# udpipe 모델 다운로드 및 로드
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

# 품사 태깅
text_df$doc_id <- 1:nrow(text_df)
annotations <- udpipe_annotate(ud_model, x = text_df$text)
annotations_df <- as.data.frame(annotations)

# 동사와 형용사만 필터링
verbs_adjectives <- annotations_df %>%
  filter(upos %in% c("VERB", "ADJ"))

# 감성 점수 계산
sentiment_scores <- text_df %>%
  rowwise() %>%
  mutate(
    sentiment_score = {
      words <- str_split(text, "\\s+")[[1]]
      sentiment <- sum(sapply(words, function(word) {
        if (word %in% get_sentiments("bing")$word) {
          if (get_sentiments("bing")$sentiment[get_sentiments("bing")$word == word] == "positive") {
            return(1)
          } else if (get_sentiments("bing")$sentiment[get_sentiments("bing")$word == word] == "negative") {
            return(-1)
          } else {
            return(0)
          }
        } else {
          return(0)
        }
      }))
      if (sentiment > 0) 1 else if (sentiment < 0) -1 else 0
    }
  ) %>%
  select(doc_id, sentiment_score)

# 각 문장의 감성 점수 출력
print(sentiment_scores)

# 전체 감성 점수 합산
total_sentiment_score <- sum(sentiment_scores$sentiment_score)
cat("전체 감성 점수 합산:", total_sentiment_score, "\n")

