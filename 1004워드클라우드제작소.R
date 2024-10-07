##### 패키지 로드 #####
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
library(dplyr)
library(wordcloud)

##### 경로 설정 #####
folder_path <- "C:/Users/dlsdn/Desktop/paper/nuclear"

##### PDF 파일 목록 #####
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

##### PDF에서 텍스트 추출 및 전처리 #####
all_text <- list()

##### Custom stemming list #####
stem_rules <- list(
  "leader" = "kim",
  "leadership" = "kim"
)

##### 정규화 함수 정의 #####
custom_stem <- function(word) {
  if (word %in% names(stem_rules)) {
    return(stem_rules[[word]])
  } else {
    return(word)
  }
}

##### 정규화 적용 함수 정의 #####
apply_custom_stemming <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  stemmed_words <- sapply(words, custom_stem)
  return(paste(stemmed_words, collapse = " "))
}

##### PDF 파일에서 텍스트 추출 및 전처리 수행 #####
for (pdf_file in pdf_files) {
  cat(sprintf("Extracting text from file: %s\n", pdf_file))
  
  # 텍스트 추출
  text <- pdf_text(pdf_file)
  
  # 텍스트를 하나의 문자열로 결합
  text <- paste(text, collapse = " ")
  
  # 소문자 변환
  text <- tolower(text)
  
  # 정규화 적용
  text <- apply_custom_stemming(text)
  
  # 전처리 수행 (마침표, 쉼표 제외)
  text <- gsub("[^a-zA-Z\\s]", " ", text)  # 특수문자 제거 (마침표와 쉼표 제외)
  text <- gsub("\\b\\d+\\b", " ", text)    # 숫자 제거
  text <- removeWords(text, stopwords("en"))  # 불용어 제거
  
  # 특정 단어 제거(24.08.08 확인)
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
  
  # 전처리된 텍스트를 리스트에 추가
  all_text[[basename(pdf_file)]] <- text
}

##### 추출된 텍스트를 하나의 벡터로 결합 #####
all_text_combined <- paste(unlist(all_text), collapse = " ")

##### 단어 리스트 생성 #####
words <- unlist(str_split(all_text_combined, "\\s+"))

##### 어간추출 수행 #####
stemmed_words <- wordStem(words, language = "en")

##### 단어 빈도 계산 #####
word_freq <- table(stemmed_words)
sorted_word_freq <- sort(word_freq, decreasing = TRUE)

##### 상위 30개 단어 추출 #####
top_words <- head(sorted_word_freq, 30)

##### 워드 클라우드 생성 #####
set.seed(970324) 
wordcloud(names(top_words), freq = top_words, scale = c(3.0, 0.5), colors = brewer.pal(8, "Dark2"))

print(top_words)
