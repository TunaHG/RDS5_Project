setwd("C:/FastCampus/ML Project/")
library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(topicmodels)
library(LDAvis)
library(servr)
library(tm)
library(slam)
library(NLP4kec)
library(randomForest)

# Data Crawling -----------------------------------------------------------

# Crawling Place's ID
word <- '제주도'
source(file = '../Crawling/R/pcntEncodingFuns.R')
num <- seq(from = 0, to = 288, by = 18)
home.result <- data.frame()
for(i in num){
  res <- GET(url = 'https://www.airbnb.co.kr/api/v2/explore_tabs',
             query = list(`_format` = 'for_explore_search_web',
                          items_per_grid = '18',
                          selected_tab_id = 'home_tab',
                          items_offset = i,
                          query = word %>% pcntEncoding2Utf8(),
                          key = 'd306zoyjsyarp7ifhu67rjxn52tv0t20'))
  
  home <- res %>% content(as = 'text', encoding = 'UTF-8') %>% fromJSON()
  id <- home$explore_tabs$sections %>% `[[`(1) %>% `[`(, 'listings') %>% `[[`(1) %>% `$`(listing) %>% `$`(id)
  star <- home$explore_tabs$sections %>% `[[`(1) %>% `[`(, 'listings') %>% `[[`(1) %>% `$`(listing) %>% `$`(star_rating)
  name <- home$explore_tabs$sections %>% `[[`(1) %>% `[`(, 'listings') %>% `[[`(1) %>% `$`(listing) %>% `$`(name)
  
  agg <- data.frame(id, star, name)
  home.result <- rbind(home.result, agg)
}

# Crawling Number of Review
count.result <- c()
for(i in home.result$id){
  res2 <- GET(url = paste0('https://www.airbnb.co.kr/rooms/', i))
  
  review <- res2 %>% 
    read_html() %>% 
    html_node(css = 'div._vy3ibx h1') %>% 
    html_text() %>% 
    str_extract(pattern = '\\d+')
  count.result <- c(count.result, review)
}
count.result <- ifelse(is.na(count.result), 0, count.result)

# Crawling Review
review.result <- data.frame()
for(i in 1:306){
  num <- seq(from = 0, to = count.result[i], by = 7)
  for(j in num){
    res3 <- GET(url = 'https://www.airbnb.co.kr/api/v2/reviews',
                query = list(key = 'd306zoyjsyarp7ifhu67rjxn52tv0t20',
                             currency = 'KRW',
                             locale = 'ko',
                             listing_id = home.result$id[i],
                             role = 'guest',
                             `_format` = 'for_p3',
                             `_limit` = '7',
                             `_offset` = j,
                             `_order` = 'combined_ranker_v1'))
    
    json <- res3 %>% content(as = 'text', encoding = 'UTF-8') %>% fromJSON()
    comments <- json$reviews$comments
    mer <- data.frame(Home = rep(home.result$name[i], time = length(comments)), Comments = comments)
    review.result <- rbind(review.result, mer)
  }
  cat(i, '번째 실행중\n')
}


load(file = 'review.Rdata')
# Data Preprocessing ------------------------------------------------------
# 한글이 없는 후기 삭제
for(i in 1:nrow(review.result)){
  review.result[i, 2] <- ifelse(str_detect(review.result[i, 2], pattern = '[가-힣]'),
                                review.result[i, 2], NA)
}

# 영어가 주로 된 후기 삭제
for(i in 1:nrow(review.result)){
  review.result[i, 2] <- ifelse(str_detect(review.result[i, 2], pattern = '[A-z]'), 
                                ifelse(str_extract_all(review.result[i, 2], pattern = '[가-힣]+(?= )') %>% 
                                         `[[`(1) %>% 
                                         length() <= 5,
                                       NA, 
                                       review.result[i, 2]), 
                                review.result[i, 2])
}
review.result <- review.result[complete.cases(review.result), ]

# 한글이 아닌 모든 문자 삭제
review.result[ , 2] <- str_remove_all(review.result[ , 2], pattern = '[^가-힣]')
review.result[ , 2] <- str_remove_all(review.result[ , 2], pattern = '[ㄱ-ㅣ]')
review.result[ , 2] <- str_remove_all(review.result[ , 2], pattern = '\\W')

# 후기 글자수 제한
len <- nchar(review.result[, 2])
len <- ifelse(len <= 30, FALSE,
              ifelse(len > 800, FALSE, TRUE))
review.result <- review.result[len, ]
rownames(review.result) <- 1:nrow(review.result)

# Text Mining ------------------------------------------------------------------

# 사전을 사용하여 파싱
parsed_data <- r_parser_r(review.result$Comments, language = 'ko', useEn = F,
                          korDicPath = '27/dic.txt')

# 불용어 사전 불러오기
stop1 <- read.table(file = '27/stop.txt', sep = '\n', header = FALSE, stringsAsFactors = FALSE)
stop2 <- read.table(file = '27/stop2.txt', sep = '\n', header = FALSE, stringsAsFactors = FALSE)
stop3 <- read.table(file = '27/stop3.txt', sep = '\n', header = FALSE, stringsAsFactors = FALSE)
stop4 <- read.table(file = '27/stop4.txt', sep = '\n', header = FALSE, stringsAsFactors = FALSE)
stop <- c(stop1[, 1], stop2[, 1], stop3[, 1])
rm(stop1, stop2, stop3, stop4)


# 말뭉치 생성
corp <- tm::VCorpus(VectorSource(parsed_data))

#특문, 숫자, 불용어 삭제 
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stop)


#텍스트문서 형식으로 변환
corp <- tm_map(corp, PlainTextDocument)


# DTM 으로 만들기 (TF 기준)
dtm <- DocumentTermMatrix(corp, 
                         control=list(wordLengths=c(2,Inf)))

# 한글자 단어 삭제
colnames(dtm) <- trimws(colnames(dtm))
dtm <- dtm[,nchar(colnames(dtm)) > 1]

# Sparse Terms 삭제
dtm <- removeSparseTerms(dtm, as.numeric(0.997))

# Tf-Idf 값 계산하기( DTM만들때 함수로 넣으면 객체가 TF-IDF객채로 변하는데 그럼 LDA 안돌아감. 
# 그래서 객채는 TF로 냅두고 계산만해서 실제값은 TF-IDF로 바꿔주는거인듯?)
term_tfidf = tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))


# Tf-Idf 값 기준으로 dtm 크기 줄여서 new_dtm 만들기(숫자는 바꿔가면서 조절)
new_dtm = dtm[,term_tfidf >= 0.2]
new_dtm = new_dtm[row_sums(new_dtm) > 0,]


# 데이터 나누기
set.seed(seed = 123)
index <- sample(1:2,
                size = nrow(dtm),
                prob = c(0.7, 0.3),
                replace = TRUE)
train <- new_dtm[index == 1, ]
test <- new_dtm[index == 2, ]


# LDA 파라미터값 세팅
control_LDA_Gibbs = list(alpha = 1, estimate.beta = TRUE, verbose = 0, prefix = tempfile(),
                         save = 0, keep = 0, seed = 1, nstart = 1,
                         best = TRUE, delta = 0.1, iter = 5000)

# LDA 모델링
lda_tm = LDA(x = train, 
             k = 3, 
             method = "Gibbs", 
             control = control_LDA_Gibbs)

#토픽별 핵심단어 확인해보기 (50개)
term_topic = terms(lda_tm, 50)
term_topic

# # LDA결과 시각화 하기 
# # phi는 각 단어별 토픽에 포함될 확률값 입니다.
# phi = posterior(lda_tm)$terms %>% as.matrix
# 
# # theta는 각 문서별 토픽에 포함될 확률값 입니다.
# theta = posterior(lda_tm)$topics %>% as.matrix
# 
# # vocab는 전체 단어 리스트 입니다.
# vocab = colnames(phi)
# 
# #문서별 토픽 번호 저장하기
# doc_topic = topics(lda_tm, 1)
# 
# # 각 문서별 문서 길이를 구합니다.
# doc_length = vector()
# doc_topic_df=as.data.frame(doc_topic)
# 
# for( i in as.numeric(row.names(doc_topic_df))){
#   temp = corp[[i]]$content
#   doc_length = c(doc_length, nchar(temp[1]))
# }
# 
# # 각 단어별 빈도수를 구합니다.
# new_dtm_m = as.matrix(new_dtm)
# freq_matrix = data.frame(ST = colnames(new_dtm_m),
#                          Freq = colSums(new_dtm_m))
# 
# # 위에서 구한 값들을 파라메터 값으로 넘겨서 시각화를 하기 위한 데이터를 만들어 줍니다.
# source("27/createJsonForChart_v2.R")
# json_lda = createJson(phi = phi
#                       , theta = theta,
#                       vocab = vocab,
#                       doc.length = doc_length,
#                       term.frequency = freq_matrix$Freq,
#                       mds.method = jsPCA #canberraPCA가 작동 안할 때 사용
#                       # mds.method = canberraPCA
# )
# 
# name = "hi"
# k = 3 #클러스터 개수 세팅
# 
# # 톰캣으로 보내기
# serVis(json_lda, out.dir = paste("C:/apache-tomcat-8.5.38/webapps/",name,"_",k,sep=""), open.browser = T)
# serVis(json_lda, open.browser = T) # MAC인 경우
# 
# # 예시 URL
# #localhost:8080/petition_LDA_15



# RandomForest ------------------------------------------------------------

#문서별 토픽 번호 저장하기
doc_topic <- topics(lda_tm, 1)
doc_topic_f <- as.factor(doc_topic)
doc_topic_df <- as.data.frame(doc_topic_f)
train_m <- as.matrix(train)
train_df <- as.data.frame(train_m)
test_m <- as.matrix(test)
test_df <- as.data.frame(test_m)
train_topic_df <- cbind(train_df, doc_topic_df)

# # 랜덤포레스트 예측모델
# fitRFC <- randomForest(x = train_topic_df[, -812],
#                        y = train_topic_df[, 812],
#                        ntree = 100,
#                        mtry = 20,
#                        importance = TRUE,
#                        do.trace = 10,
#                        keep.forest = TRUE)
# 
# test_tmp <- test_df[1:20, ]
# predict(object = fitRFC, newdata = test_tmp)

# 랜덤포레스트 하이퍼 파라미터 그리드 탐색 ----
# 하이퍼 파라미터 정의
ntree = c(20, 100, 500, 1000)
mtry = c(25, 30, 35, 40, 45)
nodesize = c(2, 5, 8, 10)
maxnodes = c(2, 5, 8, 10, 20, 50, 100)


# 결과값 넣을 메트릭스
tree_result <- matrix(0, length(ntree)*length(mtry)*length(nodesize)*length(maxnodes),24)
iter_cnt = 1

# 위 파라미터들 다 넣는 포문
for(i in 1:length(ntree)){
  for(j in 1:length(mtry)){
    for(k in 1:length(nodesize)){
      for(l in 1:length(maxnodes)){
        cat('ntree : ', ntree[i],
            ', mtry : ', mtry[j],
            ', nodesize : ', nodesize[k],
            ', maxnodes : ',  maxnodes[l], '\n')
        
        
        ## 위 파라미터로 RF 모형 적합
        
        tmp_rf <- randomForest(x = train_topic_df[, -824],
                               y = train_topic_df[, 824],
                               ntree = ntree[i],
                               mtry = mtry[j],
                               nodesize = nodesize[k],
                               maxnodes = maxnodes[l])
        
        
        ## 위 적합으로 검증데이터 예측 수행
        tmp_rf_val_pred <- predict(tmp_rf, newdata = test_tmp, type = 'class')
        
        
        ### 조건들 저장
        tree_result[iter_cnt, 1] = ntree[i]
        tree_result[iter_cnt, 2] = mtry[j]
        tree_result[iter_cnt, 3] = nodesize[k]
        tree_result[iter_cnt, 4] = maxnodes[l]
        for(m in 1:20){
          tree_result[iter_cnt, 4+m] <- tmp_rf_val_pred[m]
        }
        
        
        iter_cnt = iter_cnt +1
      }
    }
  }
}


colnames(tree_result) <-  c('ntree', 'mtry', 'nodesize', 'maxnodes', 
                            'Review1', 'Review2', 'Review3', 'Review4',
                            'Review5', 'Review6', 'Review7', 'Review8',
                            'Review9', 'Review10', 'Review11', 'Review12',
                            'Review13', 'Review14', 'Review15', 'Review16',
                            'Review17', 'Review18', 'Review19', 'Review20')
writexl::write_xlsx(tree_result, path = '27/result.xlsx')
View(tree_result)

# 최적의 파라미터
set.seed(seed = 123)
fitRFC <- randomForest(x = train_topic_df[, -824],
                       y = train_topic_df[, 824],
                       ntree = 20,
                       mtry = 35,
                       nodesize = 10,
                       maxnodes = 100)

# 예측값
test_topic <- predict(fitRFC, newdata = test_df, type = 'class')
test_df$topic <- test_topic
save(test_df, file = 'test_df.Rdata')
