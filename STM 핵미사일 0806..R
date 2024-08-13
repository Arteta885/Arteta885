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
library(ggplot2)


##### 경로 설정 #####
folder_path <- "C:/Users/dlsdn/Desktop/논문작성2/인권문서모음_핵미사일"

##### PDF 파일 목록 #####
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

##### Custom stemming list (0806 update) #####
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
  stemmed_words <- sapply(words, custom_stem, USE.NAMES = FALSE)
  return(paste(stemmed_words, collapse = " "))
}

##### 불용어 및 제거할 단어 목록 #####
remove_words <- c(stopwords("en"), 
                  "human", "rights", "special", "rapporteur", "international", 
                  "situation", "un", "united", "nations", "dprk", "democratic", 
                  "people", "s", "republic", "korea", "korean", "also", "commission",
                  "country", "council", "north", "ohchr", "hrc", "will", "report", 
                  "para", "state", "statement", "states", "resolution", "commissioner", 
                  "committee","session", "mr",
                  "january", "february", "march", "april", "may", "june", 
                  "july", "august", "september", "october", "november", "december")

##### PDF 파일에서 텍스트 추출 및 전처리 수행 #####
all_text <- list()

for (pdf_file in pdf_files) {
  cat(sprintf("Extracting text from file: %s\n", pdf_file))
  
  # 텍스트 추출
  text <- tryCatch({
    pdf_text(pdf_file)
  }, error = function(e) {
    cat(sprintf("Failed to extract text from file: %s\n", pdf_file))
    return(NULL)
  })
  
  if (!is.null(text)) {
    # 텍스트를 하나의 문자열로 결합
    text <- paste(text, collapse = " ")
    
    # 소문자 변환 및 정규화 적용
    text <- tolower(text) %>%
      apply_custom_stemming() %>%
      str_replace_all("[^a-zA-Z\\s]", " ") %>%  # 특수문자 제거
      str_replace_all("\\b\\d+\\b", " ") %>%    # 숫자 제거
      removeWords(remove_words)
    
    # 전처리된 텍스트를 리스트에 추가
    all_text[[basename(pdf_file)]] <- text
  }
}

##### 추출된 텍스트를 데이터 프레임으로 변환 #####
pdf_texts_df <- data.frame(
  file_name = names(all_text),
  text = unlist(all_text),
  stringsAsFactors = FALSE
)

##### metadata 설정: "kim" 포함 여부 #####
pdf_texts_df$contains_kim <- factor(ifelse(grepl("\\bkim\\b", pdf_texts_df$text), "yes", "no"))

##### 코퍼스 생성 #####
corpus <- VCorpus(VectorSource(pdf_texts_df$text))

##### Document-Term Matrix 생성 #####
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(1, Inf)))

##### DTM을 STM 형식으로 변환 #####
dtm_matrix <- as.matrix(dtm)
vocab <- colnames(dtm_matrix)
documents <- apply(dtm_matrix, 1, function(x) {
  rbind(as.integer(which(x > 0)), as.integer(x[x > 0]))
})

##### STM 모델 생성 #####
K <- 5  # 주제 수 설정
stm_model <- stm(documents, vocab, K = K, data = pdf_texts_df)

##### STM 모델 요약 #####
print(summary(stm_model))


# STM 모델 요약 플롯
plot(stm_model, type = "summary", main = "STM Topic Model Summary")

# 주제별 주요 단어 플롯
plot(stm_model, type = "labels", n = 10, main = "Top 10 Words for Each Topic")

# 주제 분포 히스토그램 플롯
plot(stm_model, type = "hist", main = "Topic Distribution Histogram")

################

##### "kim" 포함 여부가 주제 분포에 미치는 영향 평가 #####
effect <- estimateEffect(1:K ~ contains_kim, stm_model, metadata = pdf_texts_df)
print(summary(effect))


#################################


# 필요한 패키지 설치 및 로드
install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)

# 분석 결과를 데이터프레임으로 준비 (Intercept 제외)
results <- data.frame(
  Topic = factor(rep(1:5, each=1), levels=1:5),
  Variable = rep("contains_kimyes", times=5),
  Estimate = c(-0.20588, 0.04034, 0.02920, 0.01226, 0.12289),
  Std.Error = c(0.05454, 0.02569, 0.03763, 0.05711, 0.04741),
  p.value = c(0.000208, 0.118, 0.438575, 0.83, 0.0102)
)

# p-value 범위에 따라 색상 설정
results <- results %>%
  mutate(Significance = case_when(
    p.value <= 0.001 ~ "Very Significant (***))",
    p.value <= 0.01 ~ "Significant (**) ",
    p.value <= 0.05 ~ "Marginally Significant (*)",
    p.value <= 0.1  ~ "Borderline Significant (.)",
    TRUE ~ "Not Significant"
  ))

# 막대그래프 생성
ggplot(results, aes(x=Topic, y=Estimate, fill=Significance)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=Estimate - Std.Error, ymax=Estimate + Std.Error), 
                position=position_dodge(width=0.9), width=0.25) +
  geom_text(aes(label=sprintf("%.3f", Estimate)), vjust=-0.5, position=position_dodge(width=0.9)) +
  geom_text(aes(label=sprintf("p=%.3f", p.value)), vjust=1.5, position=position_dodge(width=0.9)) +
  labs(title="Effect of 'kim' on Each Topic",
       x="Topic",
       y="Estimate") +
  scale_fill_manual(values=c(
    "Very Significant (***))" = "darkred",
    "Significant (**) " = "red",
    "Marginally Significant (*)" = "lightcoral",
    "Borderline Significant (.)" = "lightpink",
    "Not Significant" = "lightgray"
  )) +
  theme_minimal() +
  theme(legend.position="bottom") # 범례 추가


