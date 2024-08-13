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
library(dplyr)

# 경로 설정
folder_path <- "C:/Users/dlsdn/Desktop/논문작성2/인권문서모음_핵미사일_평화"

# PDF 파일 목록
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

# PDF에서 텍스트 추출 및 전처리
all_text <- list()

# custom stemming list
stem_rules <- list(
  "leader" = "kim"
)

# 정규화 함수 정의
custom_stem <- function(word) {
  if (word %in% names(stem_rules)) {
    return(stem_rules[[word]])
  } else {
    return(word)
  }
}

# 정규화 적용 함수 정의
apply_custom_stemming <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  stemmed_words <- sapply(words, custom_stem)
  return(paste(stemmed_words, collapse = " "))
}

# PDF 파일에서 텍스트 추출 및 전처리
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
  
  # 전처리된 텍스트를 리스트에 추가
  all_text[[basename(pdf_file)]] <- text
}

# 추출된 텍스트를 데이터 프레임으로 변환
pdf_texts_df <- data.frame(
  file_name = names(all_text),
  text = unlist(all_text),
  stringsAsFactors = FALSE
)

#########################################
# metadata 설정
# 각 문서에 대해 다양한 metadata 값 할당
set.seed(970324)  # 재현성을 위해 시드 설정
metadata <- c("kim", "security", "nuclear", "missile")
pdf_texts_df$metadata <- sample(metadata, nrow(pdf_texts_df), replace = TRUE)

# 메타데이터 열을 요인형으로 변환
pdf_texts_df$metadata <- as.factor(pdf_texts_df$metadata)

# 코퍼스 생성
corpus <- VCorpus(VectorSource(pdf_texts_df$text))

# Document-Term Matrix 생성
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(1, Inf)))

# DTM을 STM 형식으로 변환
dtm_matrix <- as.matrix(dtm)
vocab <- colnames(dtm_matrix)
documents <- apply(dtm_matrix, 1, function(x) {
  rbind(as.integer(which(x > 0)), as.integer(x[x > 0]))
})

# STM 모델 생성
K <- 6  # 주제 수 설정
stm_model <- stm(documents, vocab, K = K, data = pdf_texts_df$metadata)

# STM 모델 요약
summary(stm_model)

# 메타데이터가 주제 분포에 미치는 영향을 평가
effect <- estimateEffect(1:K ~ metadata, stm_model, metadata = pdf_texts_df)
summary(effect)

# STM 모델 시각화
plot.STM(stm_model, type = "summary", main = "STM Topic Model Summary")