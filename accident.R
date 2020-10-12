
rm(list = ls())

library(dplyr)
library(readxl)
library(XML)
library(rvest)

year = 2019 # 연도
acc_2019 <- read_excel('data/acc_2019.xls'); View(acc_2019)

city = '서울' # 도시
date <- unname(unlist(acc_2019[1, -c(1:4)])) # 날짜
data <- acc_2019 %>% 
  filter(시도 == '서울' & 사고년도 %in% c('사고건수', '사망자수', '부상자수')) %>% 
  dplyr::select(-c(1:4))

data <- as.data.frame(t(data))
rownames(data) <- date
View(data)
data[data == '-'] <- NA
View(data)

adata <- NULL; start = 1
for (i in 1:12){
  x <- data[c(start:(start+2))]
  colnames(x) <- c('사고건수', '사망자수', '부상자수')
  x <- x %>% 
    mutate(date = paste(year, '년', i, '월', rownames(x), sep=''),
           city = city) %>% 
    dplyr::select(c(4, 1:3, 5)) %>% 
    filter(!is.na(사고건수))
  adata <- rbind(adata, x)
  start = start + 3
}
View(adata)

#---------------------------------------------------------------------------
# 함수만들어보기

acc_data <- function (x,y) {
  year = x # 연도
  acc_year<- read_excel(paste0('data/acc_',year,'.xls'));
  city = y # 도시
  date <- unname(unlist(acc_year[1, -c(1:4)])) # 날짜
  data <- acc_year %>% 
    filter(시도 == city & 사고년도 %in% c('사고건수', '사망자수', '부상자수')) %>% 
    dplyr::select(-c(1:4))
  data <- as.data.frame(t(data))
  rownames(data) <- date
  data[data == '-'] <- NA
  adata <- NULL; start = 1
  for (i in 1:12){
    a <- data[c(start:(start+2))]
    colnames(a) <- c('사고건수', '사망자수', '부상자수')
    a <- a %>% 
      mutate(date = paste(year, '년', i, '월', rownames(a), sep=''),city = city) %>% 
      dplyr::select(c(4, 1:3, 5)) %>% 
      dplyr::filter(!is.na(사고건수))
    adata <- rbind(adata, a)
    start = start + 3
  }
  return(adata)
}

# x값(1번째 매개변수) <- 2017,2018,2019 입력
# y값(2번째 매개변수) <- "강원" "경기" "경남" "경북" "광주" "대구" "대전" "부산" "서울" "세종" "울산" "인천" "전남" "전북" "제주" "충남" "충북"

t1<-acc_data(2017,"강원")
t2<-acc_data(2018,"강원")
View(t1) ; View(t2)

#----------------------------------------------------------------------------------
# 년도-지역 변수, 데이터 만들기

acc_data_2017_서울 <- acc_data(2017, '서울')
acc_data_2017_경기 <- acc_data(2017, '경기')

years <- c(2017,2018,2019)
cityname <- c("강원", "경기", "경남", "경북", "광주", "대구", "대전", "부산", "서울", "세종", "울산", "인천", "전남", "전북", "제주", "충남", "충북")

for (y in years){
  for (i in cityname){
    assign(paste0('acc_data_',y,'_',i), acc_data(y, i))
  }
}

# acc_data_2017_경기 -> 년도, 지역 바꿔서 입력하면 됨

#------------------------------------------------------------------------------------
# 2017,2018,2019 각 년도별로 데이터 통합

#2017
for (i in cityname){
  acc_data_2017 <- rbind(acc_data_2017, acc_data(2017, i))
}

acc_data_2017

#2018
for (i in cityname){
  acc_data_2018 <- rbind(acc_data_2018, acc_data(2018, i))
}

acc_data_2018

#2019
for (i in cityname){
  acc_data_2019 <- rbind(acc_data_2019, acc_data(2019, i))
}

acc_data_2019

#-------------------------------------------------------------------------------
# 2017~2019 데이터 한 변수에 통합

years <- c(2017,2018,2019)
cityname <- c("강원", "경기", "경남", "경북", "광주", "대구", "대전", "부산", "서울", "세종", "울산", "인천", "전남", "전북", "제주", "충남", "충북")

accident_data <- NULL

for (y in years){
  for (i in cityname){
    accident_data <- rbind(accident_data, acc_data(y, i))
  }
}

accident_data

write.csv(accident_data, 'accident_data.csv')




