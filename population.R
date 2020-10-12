# 교통사고 데이터 + 인구 전처리
rm(list=ls())

library(dplyr)
library(lubridate)

accident <- read.csv('data/new_accident.csv')
head(accident)

population <- read.csv('data/population.csv')
head(population)

population <- population[-c(1),]; View(population)


# date 전처리
date <- colnames(population)[-c(1)]
date <- gsub('\\..', '-', substr(date, 2, 9))
date <- paste0(date, '-01')

# city
city <- c('서울', '부산', '대구', '인천', '광주', '대전',
         '울산', '세종', '경기', '강원', '충북', '충남',
         '전북', '전남', '경북', '경남', '제주')
length(city)

# cbind(date, city, 인구수)
# format(as.numeric(unlist(population[c(2), c(-1)])), big.mark = ',') -> x
# 
# a <- cbind(date, city=city[1], 
#            pop=format(as.numeric(unlist(population[c(2), c(-1)])), big.mark = ','))
# rbind(pop,a)
# 
# b <- cbind(city[2], unlist(population[c(3), c(-1)]))
# rbind(date, a, b)

population[c(1+1), c(-1)]

pop <- data.frame(date=c(), city=c(), pop=c()); pop

for(i in 1:length(city)){
  x <- cbind(date, city=city[i], 
             pop=format(as.numeric(unlist(population[c(i+1), c(-1)]))))
  pop <- rbind(pop,x)
}
View(pop)

write.csv(pop, 'data/new_population.csv')
