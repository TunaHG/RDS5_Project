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
word <- 'Á¦ÁÖµµ'
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
  cat(i, '¹øÂ° ½ÇÇàÁß\n')
}


load(file = 'review.Rdata')
# Data Preprocessing ------------------------------------------------------
# ÇÑ±ÛÀÌ ¾ø´Â ÈÄ±â »èÁ¦
for(i in 1:nrow(review.result)){
  review.result[i, 2] <- ifelse(str_detect(review.result[i, 2], pattern = '[°¡-ÆR]'),
                                review.result[i, 2], NA)
}

# ¿µ¾î°¡ ÁÖ·Î µÈ ÈÄ±â »èÁ¦
for(i in 1:nrow(review.result)){
  review.result[i, 2] <- ifelse(str_detect(review.result[i, 2], pattern = '[A-z]'), 
                                ifelse(str_extract_all(review.result[i, 2], pattern = '[°¡-ÆR]+(?= )') %>% 
                                         `[[`(1) %>% 
                                         length() <= 5,
                                       NA, 
                                       review.result[i, 2]), 
                                review.result[i, 2])
}
review.result <- review.result[complete.cases(review.result), ]

# ÇÑ±ÛÀÌ ¾Æ´Ñ ¸ðµç ¹®ÀÚ »èÁ¦
review.result[ , 2] <- str_remove_all(review.result[ , 2], pattern = '[^°¡-ÆR]')
review.result[ , 2] <- str_remove_all(review.result[ , 2], pattern = '[¤¡-¤Ó]')
review.result[ , 2] <- str_remove_all(review.result[ , 2], pattern = '\\W')

# ÈÄ±â ±ÛÀÚ¼ö Á¦ÇÑ
len <- nchar(review.result[, 2])
len <- ifelse(len <= 30, FALSE,
              ifelse(len > 800, FALSE, TRUE))
review.result <- review.result[len, ]
rownames(review.result) <- 1:nrow(review.result)

# Text Mining ------------------------------------------------------------------

# »çÀüÀ» »ç¿ëÇÏ¿© ÆÄ½Ì
parsed_data <- r_parser_r(review.result$Comments, language = 'ko', useEn = F,
                          korDicPath = '27/dic.txt')

# ºÒ¿ë¾î »çÀü ºÒ·¯¿À±â
stop1 <- read.table(file = '27/stop.txt', sep = '\n', header = FALSE, stringsAsFactors = FALSE)
stop2 <- read.table(file = '27/stop2.txt', sep = '\n', header = FALSE, stringsAsFactors = FALSE)
stop3 <- read.table(file = '27/stop3.txt', sep = '\n', header = FALSE, stringsAsFactors = FALSE)
stop4 <- read.table(file = '27/stop4.txt', sep = '\n', header = FALSE, stringsAsFactors = FALSE)
stop <- c(stop1[, 1], stop2[, 1], stop3[, 1])
rm(stop1, stop2, stop3, stop4)


# ¸»¹¶Ä¡ »ý¼º
corp <- tm::VCorpus(VectorSource(parsed_data))

#Æ¯¹®, ¼ýÀÚ, ºÒ¿ë¾î »èÁ¦ 
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stop)


#ÅØ½ºÆ®¹®¼­ Çü½ÄÀ¸·Î º¯È¯
corp <- tm_map(corp, PlainTextDocument)


# DTM À¸·Î ¸¸µé±â (TF ±âÁØ)
dtm <- DocumentTermMatrix(corp, 
                         control=list(wordLengths=c(2,Inf)))

# ÇÑ±ÛÀÚ ´Ü¾î »èÁ¦
colnames(dtm) <- trimws(colnames(dtm))
dtm <- dtm[,nchar(colnames(dtm)) > 1]

# Sparse Terms »èÁ¦
dtm <- removeSparseTerms(dtm, as.numeric(0.997))

# Tf-Idf °ª °è»êÇÏ±â( DTM¸¸µé¶§ ÇÔ¼ö·Î ³ÖÀ¸¸é °´Ã¼°¡ TF-IDF°´Ã¤·Î º¯ÇÏ´Âµ¥ ±×·³ LDA ¾Èµ¹¾Æ°¨. 
# ±×·¡¼­ °´Ã¤´Â TF·Î ³ÀµÎ°í °è»ê¸¸ÇØ¼­ ½ÇÁ¦°ªÀº TF-IDF·Î ¹Ù²ãÁÖ´Â°ÅÀÎµí?)
term_tfidf = tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))


# Tf-Idf °ª ±âÁØÀ¸·Î dtm Å©±â ÁÙ¿©¼­ new_dtm ¸¸µé±â(¼ýÀÚ´Â ¹Ù²ã°¡¸é¼­ Á¶Àý)
new_dtm = dtm[,term_tfidf >= 0.2]
new_dtm = new_dtm[row_sums(new_dtm) > 0,]


# µ¥ÀÌÅÍ ³ª´©±â
set.seed(seed = 123)
index <- sample(1:2,
                size = nrow(dtm),
                prob = c(0.7, 0.3),
                replace = TRUE)
train <- new_dtm[index == 1, ]
test <- new_dtm[index == 2, ]


# LDA ÆÄ¶ó¹ÌÅÍ°ª ¼¼ÆÃ
control_LDA_Gibbs = list(alpha = 1, estimate.beta = TRUE, verbose = 0, prefix = tempfile(),
                         save = 0, keep = 0, seed = 1, nstart = 1,
                         best = TRUE, delta = 0.1, iter = 5000)

# LDA ¸ðµ¨¸µ
lda_tm = LDA(x = train, 
             k = 3, 
             method = "Gibbs", 
             control = control_LDA_Gibbs)

#ÅäÇÈº° ÇÙ½É´Ü¾î È®ÀÎÇØº¸±â (50°³)
term_topic = terms(lda_tm, 50)
term_topic

# # LDA°á°ú ½Ã°¢È­ ÇÏ±â 
# # phi´Â °¢ ´Ü¾îº° ÅäÇÈ¿¡ Æ÷ÇÔµÉ È®·ü°ª ÀÔ´Ï´Ù.
# phi = posterior(lda_tm)$terms %>% as.matrix
# 
# # theta´Â °¢ ¹®¼­º° ÅäÇÈ¿¡ Æ÷ÇÔµÉ È®·ü°ª ÀÔ´Ï´Ù.
# theta = posterior(lda_tm)$topics %>% as.matrix
# 
# # vocab´Â ÀüÃ¼ ´Ü¾î ¸®½ºÆ® ÀÔ´Ï´Ù.
# vocab = colnames(phi)
# 
# #¹®¼­º° ÅäÇÈ ¹øÈ£ ÀúÀåÇÏ±â
# doc_topic = topics(lda_tm, 1)
# 
# # °¢ ¹®¼­º° ¹®¼­ ±æÀÌ¸¦ ±¸ÇÕ´Ï´Ù.
# doc_length = vector()
# doc_topic_df=as.data.frame(doc_topic)
# 
# for( i in as.numeric(row.names(doc_topic_df))){
#   temp = corp[[i]]$content
#   doc_length = c(doc_length, nchar(temp[1]))
# }
# 
# # °¢ ´Ü¾îº° ºóµµ¼ö¸¦ ±¸ÇÕ´Ï´Ù.
# new_dtm_m = as.matrix(new_dtm)
# freq_matrix = data.frame(ST = colnames(new_dtm_m),
#                          Freq = colSums(new_dtm_m))
# 
# # À§¿¡¼­ ±¸ÇÑ °ªµéÀ» ÆÄ¶ó¸ÞÅÍ °ªÀ¸·Î ³Ñ°Ü¼­ ½Ã°¢È­¸¦ ÇÏ±â À§ÇÑ µ¥ÀÌÅÍ¸¦ ¸¸µé¾î ÁÝ´Ï´Ù.
# source("27/createJsonForChart_v2.R")
# json_lda = createJson(phi = phi
#                       , theta = theta,
#                       vocab = vocab,
#                       doc.length = doc_length,
#                       term.frequency = freq_matrix$Freq,
#                       mds.method = jsPCA #canberraPCA°¡ ÀÛµ¿ ¾ÈÇÒ ¶§ »ç¿ë
#                       # mds.method = canberraPCA
# )
# 
# name = "hi"
# k = 3 #Å¬·¯½ºÅÍ °³¼ö ¼¼ÆÃ
# 
# # ÅèÄ¹À¸·Î º¸³»±â
# serVis(json_lda, out.dir = paste("C:/apache-tomcat-8.5.38/webapps/",name,"_",k,sep=""), open.browser = T)
# serVis(json_lda, open.browser = T) # MACÀÎ °æ¿ì
# 
# # ¿¹½Ã URL
# #localhost:8080/petition_LDA_15



# RandomForest ------------------------------------------------------------

#¹®¼­º° ÅäÇÈ ¹øÈ£ ÀúÀåÇÏ±â
doc_topic <- topics(lda_tm, 1)
doc_topic_f <- as.factor(doc_topic)
doc_topic_df <- as.data.frame(doc_topic_f)
train_m <- as.matrix(train)
train_df <- as.data.frame(train_m)
test_m <- as.matrix(test)
test_df <- as.data.frame(test_m)
train_topic_df <- cbind(train_df, doc_topic_df)

# # ·£´ýÆ÷·¹½ºÆ® ¿¹Ãø¸ðµ¨
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

# ·£´ýÆ÷·¹½ºÆ® ÇÏÀÌÆÛ ÆÄ¶ó¹ÌÅÍ ±×¸®µå Å½»ö ----
# ÇÏÀÌÆÛ ÆÄ¶ó¹ÌÅÍ Á¤ÀÇ
ntree = c(20, 100, 500, 1000)
mtry = c(25, 30, 35, 40, 45)
nodesize = c(2, 5, 8, 10)
maxnodes = c(2, 5, 8, 10, 20, 50, 100)


# °á°ú°ª ³ÖÀ» ¸ÞÆ®¸¯½º
tree_result <- matrix(0, length(ntree)*length(mtry)*length(nodesize)*length(maxnodes),24)
iter_cnt = 1

# À§ ÆÄ¶ó¹ÌÅÍµé ´Ù ³Ö´Â Æ÷¹®
for(i in 1:length(ntree)){
  for(j in 1:length(mtry)){
    for(k in 1:length(nodesize)){
      for(l in 1:length(maxnodes)){
        cat('ntree : ', ntree[i],
            ', mtry : ', mtry[j],
            ', nodesize : ', nodesize[k],
            ', maxnodes : ',  maxnodes[l], '\n')
        
        
        ## À§ ÆÄ¶ó¹ÌÅÍ·Î RF ¸ðÇü ÀûÇÕ
        
        tmp_rf <- randomForest(x = train_topic_df[, -824],
                               y = train_topic_df[, 824],
                               ntree = ntree[i],
                               mtry = mtry[j],
                               nodesize = nodesize[k],
                               maxnodes = maxnodes[l])
        
        
        ## À§ ÀûÇÕÀ¸·Î °ËÁõµ¥ÀÌÅÍ ¿¹Ãø ¼öÇà
        tmp_rf_val_pred <- predict(tmp_rf, newdata = test_tmp, type = 'class')
        
        
        ### Á¶°Çµé ÀúÀå
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

# ÃÖÀûÀÇ ÆÄ¶ó¹ÌÅÍ
set.seed(seed = 123)
fitRFC <- randomForest(x = train_topic_df[, -824],
                       y = train_topic_df[, 824],
                       ntree = 20,
                       mtry = 35,
                       nodesize = 10,
                       maxnodes = 100)

# ¿¹Ãø°ª
test_topic <- predict(fitRFC, newdata = test_df, type = 'class')
test_df$topic <- test_topic
save(test_df, file = 'test_df.Rdata')
