# 날씨 데이터 수집하기
rm(list = ls())

library(RSelenium)
library(dplyr)
library(XML)
library(rvest)

remDr <- remoteDriver(remoteServerAddr = "localhost", 
                      port = 4445, 
                      browserName = "chrome")
remDr$open()

url <- "http://www.weather.go.kr/weather/climate/past_table.jsp"
remDr$navigate(url)

# 2017.01 ~ 2019.12


## 1. 도시 별로 받아오기 ---------------------------------------------
weather_df <- NULL
city <- 'r'

for(cn in 1:1){
  # city 선택 (1:107)
  cityLink <- remDr$findElement('css',
                                paste('#observation_select1 > option:nth-child(',cn,')'))
  cityLink$clickElement()
  ccity <- unlist(cityLink$getElementText())
  print(paste('split 전 ccity:', ccity))
  ccity <- strsplit(ccity, split = "(", fixed = T)[[1]][1]; city # 도시
  print(paste('이전 city:', city, '/ 현재 ccity:', ccity))
  if(substr(ccity, 1, 1) == '-' || city == ccity){
    print('pass')
    next
  } else {
    city <- ccity
  }
  print(paste('사용될 city',city))
  submitLink <- remDr$findElements('css',
                                   '#content_weather > form > fieldset > input:nth-child(7)')
  remDr$executeScript("arguments[0].click();",submitLink)
  
  
  ## 2. 연도 별로 받아오기 --------------------------------------------
  yn <- 4
  ctdata <- NULL # city별 data

  while (TRUE) {
    # year 선택 (2017: 4/ 2018: 3/ 2019: 2)
    yearLink <- remDr$findElement('css',
                                  paste('#observation_select2 > option:nth-child(',yn,')'))
    yearLink$clickElement()
    year <- unlist(yearLink$getElementText()); year # 연도
    submitLink <- remDr$findElements('css',
                                     '#content_weather > form > fieldset > input:nth-child(7)')
    remDr$executeScript("arguments[0].click();",submitLink)

    ## 3. 요소 별로 받아오기 -------------------------------------------
    yrdata <- NULL # city별 year data

    for(eln in 1:9){
      # 요소 선택 (1:9)
      elemLink <- remDr$findElement('css',
                                    paste('#observation_select3 > option:nth-child(',eln,')'))
      elemLink$clickElement()
      wkind <- unlist(elemLink$getElementText()); wkind # 요소
      submitLink <- remDr$findElements('css',
                                       '#content_weather > form > fieldset > input:nth-child(7)')
      remDr$executeScript("arguments[0].click();",submitLink)

      # table로 받아오기
      doc <- read_html(remDr$getPageSource()[[1]])
      doc <- html_nodes(doc, 'table')[2]
      Sys.setlocale("LC_ALL", "English")
      table <- html_table(doc)
      Sys.setlocale("LC_ALL", "Korean")
      df <- as.data.frame(table)[-c(32),-c(1)]

      eldata <- NULL # city/year별 요소 data
      for (i in 1:12){
        x <- df[i]
        x[1] <- lapply(x[1], function(y){gsub('^write.*;+', '', y)})
        # nchar(x[,1])
        # if(nchar(x[]) >= 20){ # 풍속 처리
        #   x[i] <- lapply(x[i], function(y){gsub('^write.*;+', '', y)}); x
        # }

        colnames(x) <- wkind
        x <- x %>%
          mutate(date = paste(year, '년', i, '월', rownames(x),'일', sep='')) %>%
          select(c(2,1)) %>%
          filter(!is.na(x))
        eldata <- rbind(eldata, x)
      }
      #View(eldata)

      # yrdata = eldata + eldata + ...
      if(is.null(yrdata)){
        yrdata <- eldata
      } else{
        yrdata <- left_join(yrdata, eldata, by='date')
        #yrdata <- merge(yrdata, eldata, by='date', all=T)
      }

    }
    # View(yrdata)

    yrdata = cbind(yrdata, city)
    # ctdata = yrdata + yrdata + ...
    ctdata <- rbind(ctdata, yrdata)

    if(yn == 2){
      break
    } else{
      yn <- yn - 1
    }

  }
  View(ctdata)
  # str(ctdata)
  
  
  weather_df <- rbind(weather_df, ctdata)

}
View(weather_df)
# weather_df %>% 
#   filter(city == '고창') %>% 
#   count()
# 
# weather_df %>% 
#   filter(city == '서울') %>% 
#   count()


write.csv(weather_df, 'data/weather.csv')


table(weather_df$city)


## ===========================================
# date 수정
rm(list=ls())
weather_df <- read.csv('data/weather.csv')
weather_df <- subset(weather_df, select=-c(1))

weather_df <- weather_df %>% 
  mutate(
    d = sapply(date, function(x){paste(unlist(strsplit(x, '[년월일]+')), collapse ='-')}),
    date = as.Date(d)) %>% 
  select(-c(d))

View(weather_df)
table(weather_df$city)

write.csv(weather_df, 'data/weather.csv')
