library(tidyverse)
library(RColorBrewer)


writexl::write_xlsx(PM, path = 'data/PM.xlsx', col_names = TRUE)
setwd("C:/FastCampus/Regression Project/")

# 데이터합치기 ----
# 크게 합쳐둔 데이터만 합치는거 보여주기.
# PM - 에어코리아 http://www.airkorea.or.kr/web/last_amb_hour_data?pMENU_NO=123
# 기후 - 기상청 기상자료개방포털 https://data.kma.go.kr/data/grnd/selectAsosRltmList.do?pgmNo=36
# kospi - 네이버 크롤링 https://finance.naver.com/sise/sise_index.nhn?code=KOSPI
# 경유 - 서울 소비량 (서울 열린 데이터 광장)
# 석유 - 제품별 소비량 (서울 열린 데이터 광장)

# PM 가져오기. 시간별데이터
load(file = "data/PM.RData")
# SO2(아황산가스), CO(일산화탄소), O3(오존), NO2(이산화질소)들은 전부 대기오염물질로 단위는 ppm(농도)
# ppm(parts per million) : 100만분율, 어떤 양이 전체의 100만분의 몇을 차지하는가의 표현
# PM10 은 1000분의 10mm보다 작은 먼지 (마이크로그램/세제곱 미터) - 농도
# 1마이크로미터는 100만분의 1미터, 1마이크로그램은 100만분의 1그램
# 에어코리아에서 이상데이터를 -999로 표시함.
# 이상데이터 탐색후 NA값으로 변환
PM %>% 
  dplyr::filter(SO2 < 0 & SO2 > -999)
PM %>% 
  dplyr::filter(CO < 0 & CO > -999)
PM %>% 
  dplyr::filter(O3 < 0 & O3 > -999)
PM %>% 
  dplyr::filter(NO2 < 0 & NO2 > -999)
PM %>% 
  dplyr::filter(PM10 < 0 & PM10 > -999)
PM$SO2 <- ifelse(PM$SO2 < 0, NA, PM$SO2)
PM$CO <- ifelse(PM$CO < 0, NA, PM$CO)
PM$O3 <- ifelse(PM$O3 < 0, NA, PM$O3)
PM$NO2 <- ifelse(PM$NO2 < 0, NA, PM$NO2)
PM$PM10 <- ifelse(PM$PM10 < 0, NA, PM$PM10)

# 시간별 데이터를 일별데이터로 변환하는 과정.
loc <- levels(PM$location)
location <- c()
year <- c()
month <- c()
day <- c()
SO2 <- c()
CO <- c()
O3 <- c()
NO2 <- c()
PM10 <- c()
for(a in loc){
  for(b in 2010:2017){
    for(c in 1:12){
      if(b %in% c(2012, 2016)){
        if(c %in% c(1, 3, 5, 7, 8, 10, 12)){
          day.date <- 31
        } else if(c == 2){
          day.date <- 29
        } else {
          day.date <- 30
        }
      } else {
        if(c %in% c(1, 3, 5, 7, 8, 10, 12)){
          day.date <- 31
        } else if(c == 2){
          day.date <- 28
        } else {
          day.date <- 30
        }
      }
      for(d in 1:day.date){
        data <- PM %>% 
          dplyr::filter((location == a) & (year == b) & (month == c) & (day == d))
        location <- c(location, levels(data$location)[1])
        year <- c(year, data$year[1])
        month <- c(month, data$month[1])
        day <- c(day, data$day[1])
        SO2 <- c(SO2, round(mean(data$SO2, na.rm = TRUE), digits = 3))
        CO <- c(CO, round(mean(data$CO, na.rm = TRUE), digits = 1))
        O3 <- c(O3, round(mean(data$O3, na.rm = TRUE), digits = 3))
        NO2 <- c(NO2, round(mean(data$NO2, na.rm = TRUE), digits = 3))
        PM10 <- c(PM10, round(mean(data$PM10, na.rm = TRUE), digits = 0))
      }
      cat(a, '지역의 ', b, '년도 ', c, '월의 작업을 진행중입니다.\n')
    }
  }
}
location2 <- rep(loc, each = 2922)
dailyPM <- data.frame(location2, year, month, day, SO2, CO, O3, NO2, PM10)
save(dailyPM, file = 'data/dailyPM.Rdata')
dailyPM <- PM.all[,1:9]

# 기후
weather.data <- readxl::read_excel(path = "data/기후/기후통합.xlsx", sheet = 1, col_names = TRUE)
weatherPM <- dplyr::left_join(dailyPM, weather.data, by = c('year', 'month', 'day'))

# kospi
result <- data.frame()
for(i in 1:373){
  res <- GET(url = 'https://finance.naver.com/sise/sise_index_day.nhn',
             query = list(code = 'KOSPI',
                          page = i))
  items <- res %>% 
    read_html() %>% 
    html_nodes(css = 'table.type_1') %>% 
    html_table(fill = TRUE) %>% 
    as.data.frame() %>% 
    na.omit() %>% 
    dplyr::select(1:2)
  result <- rbind(result, items)
}
writexl::write_xlsx(result, path = "result.xlsx")

# kospi 데이터에 빠진 날짜가 있기 때문에 빠진날짜에 NA값을 넣기위해서 merge
kospi.data <- readxl::read_excel(path = "data/kospi.xlsx", sheet = 1, col_names = TRUE)
comp <- readxl::read_excel(path = "data/comparison.xlsx", sheet = 1, col_names = TRUE)
kospi.result <- dplyr::left_join(comp, kospi.data, by = c("year", "month", "day"))
kospi <- rep(kospi.result$kospi, time = 25)
weatherPM$kospi <- kospi$price %>% gsub(pattern = ',', replacement = '') %>% as.numeric()


# 경유 소비량
oil <- readxl::read_excel(path = "data/경유/oil.xlsx", sheet = 1, col_names = TRUE)
# 자세한 oil 넣는 과정은 따로있음. Project.R 파일 참고 넣어야하나?
oil <- PM.all[, 10]
save(oil, file = 'data/oil.Rdata')

# 석유 소비량
report <- readxl::read_excel(path = "data/경유/report.xlsx", sheet = 1, col_names = TRUE)
report <- PM.all[, 18:21]
save(report, file = 'data/report.Rdata')

# 데이터 합치기

load(file = 'data/PMall.Rdata')
PM <- read.csv(file = 'data/PMall.csv', header = TRUE)

# EDA ----
# 질적 자료 cbwd, (location은 다 똑같으니 제외)
# 양적 자료 기술통계량result표시, 그래프는 우리가 중요하다 생각하는 변수 선택
# 데이터 NA값 처리 (EDA와 함께 진행)
# 싹다 평균값, cbwd는 최빈수(mode)
# kospi는 회귀분석을 이용해서 NA값 채우고 그래도 안채워지는건 평균값
str(PM)
summary(PM)

# 풍향이기 때문에 질적자료화
PM$cbwd <- as.factor(PM$cbwd)

# 질적과 양적으로 분리
PM.factor <- PM %>% 
  purrr::keep(is.factor)
PM.numeric <- PM %>% 
  purrr::keep(is.numeric)

# 질적자료 분석
# 빈도표
sort(table(PM.factor$location), decreasing = TRUE)
sort(table(PM.factor$cbwd), decreasing = TRUE)
sort(round(prop.table(table(PM.factor$location)) * 100, digits = 1), decreasing = TRUE)
sort(round(prop.table(table(PM.factor$cbwd)) * 100, digits = 1), decreasing = TRUE)
# 그래프
color.palette <- sort(RColorBrewer::brewer.pal(n = 5, name = "Blues"), decreasing = FALSE)
barplot(sort(table(PM.factor$cbwd), decreasing = TRUE), col = color.palette, 
        main = "cbwd", ylim = c(0, 20000))

# 양적자료 분석
Variable <- c()
table.range <- c()
table.value <- c()
prop.range <- c()
prop.value <- c()
pdf(file = "UV_output1.pdf")
par(mfrow = c(3, 2))
for(i in 1:(ncol(PM.numeric))){
  Range <- max(PM.numeric[, i], na.rm = TRUE) - min(PM.numeric[, i], na.rm = TRUE)
  count <- as.integer(1 + 3.3 * log10(length(PM.numeric[, i])))
  width <- Range / count
  seq <- seq(from = min(PM.numeric[, i], na.rm = TRUE), to = max(PM.numeric[, i], na.rm = TRUE), by = width)
  PM.numeric$tmp <- cut(PM.numeric[, i], breaks = seq, right = FALSE)
  table.result <- sort(table(PM.numeric$nm), decreasing = TRUE)
  prop.result <- sort(round(prop.table(table(PM.numeric$nm)) * 100, digits = 1), decreasing = TRUE)
  PM.numeric$tmp <- NULL
  Variable <- c(Variable, rep(colnames(PM.numeric)[i], times = length(table.result)))
  table.range <- c(table.range, names(table.result))
  table.value <- c(table.value, table.result)
  prop.range <- c(prop.range, names(prop.result))
  prop.value <- c(prop.value, prop.result)
  hist(PM.numeric[, i], main = colnames(PM.numeric)[i])
  boxplot(PM.numeric[, i])
}
dev.off()
result <- data.frame(Variable, table.range, table.value, prop.range, prop.value)
writexl::write_xlsx(result, path = "UV_output1.xlsx")
par(mfrow = c(1, 1))

# 기술통계량
summary(PM.numeric)

Mean <- c()
trim.Mean <- c()
Median <- c()
Mode <- c()
Range <- c()
IQR.data <- c()
var.data <- c()
sd.data <- c()
mad.data <- c()
for(i in 1:ncol(PM.numeric)){
  Mean <- c(Mean, mean(PM.numeric[, i], na.rm = TRUE))
  trim.Mean <- c(trim.Mean, mean(PM.numeric[, i], trim = 0.05, na.rm = TRUE))
  Median <- c(Median, median(PM.numeric[, i], na.rm = TRUE))
  Mode <- c(Mode, prettyR::Mode(PM.numeric[, i], na.rm = TRUE))
  Range <- c(Range, diff(range(PM.numeric[, i], na.rm = TRUE)))
  IQR.data <- c(IQR.data, IQR(PM.numeric[, i], na.rm = TRUE))
  var.data <- c(var.data, var(PM.numeric[, i], na.rm = TRUE))
  sd.data <- c(sd.data, sd(PM.numeric[, i], na.rm = TRUE))
  mad.data <- c(mad.data, mad(PM.numeric[, i], na.rm = TRUE))
}
result <- data.frame(colnames(PM.numeric), Mean, trim.Mean, Median, Mode, Range, IQR.data, var.data, sd.data, mad.data)

writexl::write_xlsx(result, path = "UV_output1_ds.xlsx")

# 강수량이 NA인것은 무강수이다. 0인것은 강수가 있으나 0.1mm미만인것.
PM$prec <- ifelse(PM$prec == 0, 0.1, PM$prec)
PM[is.na(PM$prec), 'prec'] <- 0

# boxplot이 평균으로 넣으면 전부 이상치가 됨. -> EDA를 진행한 후 중위수로 NA대체
# 질적자료인 cbwd의 NA값은 최빈수를 넣음.
PM[is.na(PM$SO2), 'SO2'] <- median(PM$SO2, na.rm = TRUE)
PM[is.na(PM$CO), 'CO'] <- median(PM$CO, na.rm = TRUE)
PM[is.na(PM$O3), 'O3'] <- median(PM$O3, na.rm = TRUE)
PM[is.na(PM$NO2), 'NO2'] <- median(PM$NO2, na.rm = TRUE)
PM[is.na(PM$ws), 'ws'] <- median(PM$ws, na.rm = TRUE)
PM[is.na(PM$pres), 'pres'] <- mean(PM$pres, na.rm = TRUE)
PM[is.na(PM$cbwd), 'cbwd'] <- prettyR::Mode(PM$cbwd, na.rm = TRUE)
PM$cbwd <- as.factor(PM$cbwd)

# 타겟변수에도 NA를 채워준다.
PM[is.na(PM$PM10), 'PM10'] <- median(PM$PM10, na.rm = TRUE)

# kospi는 NA값이 너무 많기 때문에 평균으로 채워주면 값이 너무 평균으로 치우칠거라 생각
# kospi값 타겟변수로 회귀분석을 실시하여 kospi값을 예측하여 NA값 대체

# step사용해서 예측값이 좋은모델 사용해볼까? kospi예측을?
PM.kospi <- PM[complete.cases(PM$kospi), ]
PM.kospi.model <- lm(kospi ~ ., data = PM.kospi)
summary(PM.kospi.model)

PM.kospi.na <- PM[!complete.cases(PM$kospi), ]
PM[is.na(PM$kospi), 'kospi'] <- predict(PM.kospi.model, PM.kospi.na)

# month, day는 순환형 데이터이기 때문에 변환
PM$month.x <- sin((360 / 12) * PM$month)
PM$month.y <- cos((360 / 12) * PM$month)
day.x <- c()
day.y <- c()
for(i in 1:73050){
  if(PM$year[i] %in% c(2012, 2016)){
    if(PM$month[i] %in% c(1, 3, 5, 7, 8, 10, 12)){
      day.date <- 31
    } else if(PM$month[i] == 2){
      day.date <- 29
    } else {
      day.date <- 30
    }
  } else {
    if(PM$month[i] %in% c(1, 3, 5, 7, 8, 10, 12)){
      day.date <- 31
    } else if(PM$month[i] == 2){
      day.date <- 28
    } else {
      day.date <- 30
    }
  }
  day.x <- c(day.x, sin((360 / day.date) * PM$day[i]))
  day.y <- c(day.y, cos((360 / day.date) * PM$day[i]))
}
PM$day.x <- day.x
PM$day.y <- day.y

# 이외에도 미세먼지는 지금까지의 미세먼지에 영향을 받는다고 생각.
# 6일전까지의 미세먼지를 독립변수로 추가

ago1 <- c(0, PM$PM10[1:73049])
ago2 <- c(0, ago1[1:73049])
ago3 <- c(0, ago2[1:73049])
ago4 <- c(0, ago3[1:73049])
ago5 <- c(0, ago4[1:73049])
ago6 <- c(0, ago5[1:73049])

PM$PM10ago1 <- ago1
PM$PM10ago2 <- ago2
PM$PM10ago3 <- ago3
PM$PM10ago4 <- ago4
PM$PM10ago5 <- ago5
PM$PM10ago6 <- ago6

save(PM, file = 'data/PM_190130.Rdata')

load(file = 'data/PM_190130.Rdata')
PM$month <- NULL
PM$day <- NULL
# train, test데이터 나누기
set.seed(seed = 1234)

index <- sample(x = 1:3, 
                size = nrow(x = PM), 
                replace = TRUE, 
                prob = c(0.5, 0.3, 0.2))

# index 벡터의 빈도수를 확인합니다. 
index %>% table()

# prop.table() 함수를 추가하여 각각의 비중을 확인합니다. 
index %>% table() %>% prop.table()

# round() 함수를 추가하여 소수점 넷째자리에서 반올림합니다. 
index %>% table(Freq = .) %>% prop.table() %>% round(digits = 4)

# index를 이용하여 데이터셋을 3개로 나눕니다. 
trainSet <- PM[index == 1, ]
validSet <- PM[index == 2, ]
testSet <- PM[index == 3, ]

# 회귀분석 ----
# 모델 만들고 summary()
# stepwise, backward, forward중에 예측률좋은거 선택
# 예측값 뽑고 에러 뽑고 RMSE출력
PM.model <- lm(PM10 ~ ., data = trainSet)
summary(PM.model) # AIC = 213252.7

PM.forward.model <- step(PM.model, direction = 'forward')
summary(PM.forward.model) # AIC = 213252.7

PM.backward.model <- step(PM.model, direction = 'backward')
summary(PM.backward.model) # AIC = 213243.9

PM.stepwise.model <- step(PM.model, direction = 'both')
summary(PM.stepwise.model) # AIC = 213243.9

predict.data <- predict(PM.model, validSet)
validSet$predict <- predict.data
validSet$Error <- (validSet$PM10 - validSet$predict) ** 2
sqrt(mean(validSet$Error))
mean(validSet$PM10)
# RMSE = 18.923 / R-Square = 0.5651

predict.data <- predict(PM.stepwise.model, validSet)
validSet$predict <- predict.data
validSet$Error <- (validSet$PM10 - validSet$predict) ** 2
sqrt(mean(validSet$Error))
mean(validSet$PM10)
# RMSE = 18.921

# 연도별로 나눈 train, test
PM.train <- PM %>% 
  dplyr::filter(year <= 2015)
PM.test <- PM %>% 
  dplyr::filter(year > 2015)

PM.model <- lm(PM10 ~ ., data = PM.train)
summary(PM.model)

PM.forward.model <- step(PM.model, direction = 'forward')
summary(PM.forward.model) # AIC = 326398.4

PM.backward.model <- step(PM.model, direction = 'backward')
summary(PM.backward.model) # AIC = 326396.7

PM.stepwise.model <- step(PM.model, direction = 'both')
summary(PM.stepwise.model) # AIC = 326396.7

predict.data <- predict(PM.model, PM.test)
PM.test$predict <- predict.data
PM.test$Error <- (PM.test$PM10 - PM.test$predict) ** 2
sqrt(mean(PM.test$Error))
mean(PM.test$PM10)
# RMSE = 16.333 / R-Square = 0.559

predict.data <- predict(PM.stepwise.model, PM.test)
PM.test$predict <- predict.data
PM.test$Error <- (PM.test$PM10 - PM.test$predict) ** 2
sqrt(mean(PM.test$Error))
mean(PM.test$PM10)
# RMSE = 16.339 / R-Square = 0.559

# 정규화 이후 회귀분석
PM.normal <- PM
PM.normal$year <- scale(PM.normal$year, center = TRUE, scale = TRUE)
PM.normal$SO2 <- scale(PM.normal$SO2, center = TRUE, scale = TRUE)
PM.normal$CO <- scale(PM.normal$CO, center = TRUE, scale = TRUE)
PM.normal$O3 <- scale(PM.normal$O3, center = TRUE, scale = TRUE)
PM.normal$NO2 <- scale(PM.normal$NO2, center = TRUE, scale = TRUE)
PM.normal$oil <- scale(PM.normal$oil, center = TRUE, scale = TRUE)
PM.normal$temp <- scale(PM.normal$temp, center = TRUE, scale = TRUE)
PM.normal$prec <- scale(PM.normal$prec, center = TRUE, scale = TRUE)
PM.normal$ws <- scale(PM.normal$ws, center = TRUE, scale = TRUE)
PM.normal$humi <- scale(PM.normal$humi, center = TRUE, scale = TRUE)
PM.normal$pres <- scale(PM.normal$pres, center = TRUE, scale = TRUE)
PM.normal$kospi <- scale(PM.normal$kospi, center = TRUE, scale = TRUE)
PM.normal$gasoline <- scale(PM.normal$gasoline, center = TRUE, scale = TRUE)
PM.normal$kerosene <- scale(PM.normal$kerosene, center = TRUE, scale = TRUE)
PM.normal$bunker <- scale(PM.normal$bunker, center = TRUE, scale = TRUE)
PM.normal$LPG <- scale(PM.normal$LPG, center = TRUE, scale = TRUE)
PM.normal$PM10ago1 <- scale(PM.normal$PM10ago1, center = TRUE, scale = TRUE)
PM.normal$PM10ago2 <- scale(PM.normal$PM10ago2, center = TRUE, scale = TRUE)
PM.normal$PM10ago3 <- scale(PM.normal$PM10ago3, center = TRUE, scale = TRUE)
PM.normal$PM10ago4 <- scale(PM.normal$PM10ago4, center = TRUE, scale = TRUE)
PM.normal$PM10ago5 <- scale(PM.normal$PM10ago5, center = TRUE, scale = TRUE)
PM.normal$PM10ago6 <- scale(PM.normal$PM10ago6, center = TRUE, scale = TRUE)
for(i in 1:ncol(PM.normal)){
  if(is.numeric(PM.normal[i])){ 
    PM.normal[, i] <- scale(PM.normal[, i])
  }
}

set.seed(seed = 1234)

index <- sample(x = 1:3, 
                size = nrow(PM.normal), 
                replace = TRUE, 
                prob = c(0.5, 0.3, 0.2))

# index 벡터의 빈도수를 확인합니다. 
index %>% table()

# index를 이용하여 데이터셋을 3개로 나눕니다. 
trainSet <- PM.normal[index == 1, ]
validSet <- PM.normal[index == 2, ]
testSet <- PM.normal[index == 3, ]

# 회귀분석 ----
# 모델 만들고 summary()
# stepwise, backward, forward중에 예측률좋은거 선택
# 예측값 뽑고 에러 뽑고 RMSE출력
PM.model <- lm(PM10 ~ ., data = trainSet)
summary(PM.model) # AIC = 213252.7

PM.forward.model <- step(PM.model, direction = 'forward')
summary(PM.forward.model) # AIC = 213252.7

PM.backward.model <- step(PM.model, direction = 'backward')
summary(PM.backward.model) # AIC = 213243.9

PM.stepwise.model <- step(PM.model, direction = 'both')
summary(PM.stepwise.model) # AIC = 213243.9

predict.data <- predict(PM.model, validSet)
validSet$predict <- predict.data
validSet$Error <- (validSet$PM10 - validSet$predict) ** 2
sqrt(mean(validSet$Error))
mean(validSet$PM10)
# RMSE = 18.923 / R-Square = 0.5651

predict.data <- predict(PM.stepwise.model, validSet)
validSet$predict <- predict.data
validSet$Error <- (validSet$PM10 - validSet$predict) ** 2
sqrt(mean(validSet$Error))
mean(validSet$PM10)
# RMSE = 18.921 / R-Square = 0.5651

# ----
PM.na <- PM %>% 
  dplyr::select(-PM10)

save(PM.na, file = 'data/PMna.Rdata')

load(file = 'data/PMna.Rdata')

rmse <- c()
RMSE <- data.frame()
for(a in 1:(ncol(PM.na)-1)){
  for(b in (a+1):ncol(PM.na)){
    data <- PM.na[, -c(a, b)]
    data$PM10 <- PM$PM10
    
    set.seed(seed = 1234)
    
    index <- sample(x = 1:3, 
                    size = nrow(data), 
                    replace = TRUE, 
                    prob = c(0.5, 0.3, 0.2))
    train.data <- data[index == 1, ]
    valid.data <- data[index == 2, ]
    test.data <- data[index == 3, ]
    
    data.model <- lm(PM10 ~ ., data = train.data)
    
    predict.data <- predict(data.model, newdata = valid.data)
    valid.data$predict <- predict.data
    valid.data$Error <- (valid.data$PM10 - valid.data$predict) ** 2
    mse <- mean(valid.data$Error)
    
    rmse <- c(a, b, RMSE = sqrt(mse))
    RMSE <- rbind(RMSE, rmse)
    rmse <- c()
    valid.data$predict <- NULL
    valid.data$Error <- NULL
  }
}
writexl::write_xlsx(RMSE, path = 'data/RMSE28.xlsx', col_names = TRUE)

rmse <- c()
RMSE <- data.frame()
for(a in 1:(ncol(PM.na)-2)){
  for(b in (a+1):(ncol(PM.na)-1)){
    for(d in (b+1):ncol(PM.na)){
      data <- PM.na[, -c(a, b, d)]
      data$PM10 <- PM$PM10
      
      set.seed(seed = 1234)
      
      index <- sample(x = 1:3, 
                      size = nrow(data), 
                      replace = TRUE, 
                      prob = c(0.5, 0.3, 0.2))
      train.data <- data[index == 1, ]
      valid.data <- data[index == 2, ]
      test.data <- data[index == 3, ]
      
      data.model <- lm(PM10 ~ ., data = train.data)
      
      predict.data <- predict(data.model, newdata = valid.data)
      valid.data$predict <- predict.data
      valid.data$Error <- (valid.data$PM10 - valid.data$predict) ** 2
      mse <- mean(valid.data$Error)
      
      rmse <- c(a, b, RMSE = sqrt(mse))
      RMSE <- rbind(RMSE, rmse)
      rmse <- c()
      valid.data$predict <- NULL
      valid.data$Error <- NULL
    }
    cat('a는', a, 'b는', b, ' 실행중')
  }
}
writexl::write_xlsx(RMSE, path = 'data/RMSE27.xlsx', col_names = TRUE)


# 변수 변환하여 모델링
PM.numeric <- PM %>% 
  purrr::keep(is.numeric) %>% 
  dplyr::select(-PM10, -SO2, -O3, -prec, -month.x, -month.y, -day.x, -day.y, -temp,
                -PM10ago1, -PM10ago2, -PM10ago3, -PM10ago4, -PM10ago5, -PM10ago6)

rmse <- c()
RMSE <- data.frame()
for(b in 1:ncol(PM.numeric)){
  PM.copy <- PM.numeric
  PM.copy[, b] <- PM.copy[, b] %>% log10()
  PM.copy$PM10 <- PM$PM10
  PM.copy$SO2 <- PM$SO2
  PM.copy$O3 <- PM$O3
  PM.copy$prec <- PM$prec
  PM.copy$month.x <- PM$month.x
  PM.copy$month.y <- PM$month.y
  PM.copy$day.x <- PM$day.x
  PM.copy$day.y <- PM$day.y
  PM.copy$temp <- PM$temp
  PM.copy$PM10ago1 <- PM$PM10ago1
  PM.copy$PM10ago2 <- PM$PM10ago2
  PM.copy$PM10ago3 <- PM$PM10ago3
  PM.copy$PM10ago4 <- PM$PM10ago4
  PM.copy$PM10ago5 <- PM$PM10ago5
  PM.copy$PM10ago6 <- PM$PM10ago6
  
  set.seed(seed = 1234)
  
  index <- sample(x = 1:3, 
                  size = nrow(PM.copy), 
                  replace = TRUE, 
                  prob = c(0.5, 0.3, 0.2))
  train.data <- PM.copy[index == 1, ]
  valid.data <- PM.copy[index == 2, ]
  test.data <- PM.copy[index == 3, ]
  data.model <- lm(PM10 ~ ., data = train.data)
  predict.data <- predict(data.model, newdata = valid.data)
  valid.data$predict <- predict.data
  valid.data$Error <- (valid.data$PM10 - valid.data$predict) ** 2
  mse <- mean(valid.data$Error)
  rmse <- c(b, RMSE = sqrt(mse))
  RMSE <- rbind(RMSE, rmse)
  rmse <- c()
  valid.data$predict <- NULL
  valid.data$Error <- NULL
}
writexl::write_xlsx(RMSE, path = 'data/one102sqrt.xlsx', col_names = TRUE)

trans <- c(log10, log2, sqrt)
rmse <- c()
RMSE <- data.frame()
for(a in trans){
  for(b in 1:(ncol(PM.numeric)-1)){
    for(d in (b+1):ncol(PM.numeric)){
      PM.copy <- PM.numeric
      PM.copy[, c(b, d)] <- PM.copy[, c(b, d)] %>% log10()
      PM.copy$PM10 <- PM$PM10
      PM.copy$SO2 <- PM$SO2
      PM.copy$O3 <- PM$O3
      PM.copy$prec <- PM$prec
      PM.copy$month.x <- PM$month.x
      PM.copy$month.y <- PM$month.y
      PM.copy$day.x <- PM$day.x
      PM.copy$day.y <- PM$day.y
      PM.copy$temp <- PM$temp
      PM.copy$PM10ago1 <- PM$PM10ago1
      PM.copy$PM10ago2 <- PM$PM10ago2
      PM.copy$PM10ago3 <- PM$PM10ago3
      PM.copy$PM10ago4 <- PM$PM10ago4
      PM.copy$PM10ago5 <- PM$PM10ago5
      PM.copy$PM10ago6 <- PM$PM10ago6
      
      set.seed(seed = 1234)
      
      index <- sample(x = 1:3, 
                      size = nrow(PM.copy), 
                      replace = TRUE, 
                      prob = c(0.5, 0.3, 0.2))
      train.data <- PM.copy[index == 1, ]
      valid.data <- PM.copy[index == 2, ]
      test.data <- PM.copy[index == 3, ]
      
      data.model <- lm(PM10 ~ ., data = train.data)
      
      predict.data <- predict(data.model, newdata = valid.data)
      valid.data$predict <- predict.data
      valid.data$Error <- (valid.data$PM10 - valid.data$predict) ** 2
      mse <- mean(valid.data$Error)
      
      rmse <- c(b, d, RMSE = sqrt(mse))
      RMSE <- rbind(RMSE, rmse)
      rmse <- c()
      valid.data$predict <- NULL
      valid.data$Error <- NULL
    }
  }
}
writexl::write_xlsx(RMSE, path = 'data/two102sqrt.xlsx', col_names = TRUE)
