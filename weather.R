# 날씨 데이터 전처리
rm(list = ls())

library(dplyr)

weather <- read.csv('data/weather.csv')
head(weather, 10)

# View(weather)
# sort(unique(weather$city))


# 1. 운량: 맑음(0~2), 구름조금(3~5), 구름많음(6~8), 흐림(9~10이상)
# 2. 평균기온, 강수량, 신적설, 평균풍속, 날씨
weather <- weather %>% 
  select(-c(일조시간, 최저기온, 최고기온, X))
head(weather)


# 3. 시도별 분류
# 세종 없음
# 고산, 북강릉, 북창원, 북춘천, 진도군 제외
kor <- list(
  
  서울 = c('서울'),
  부산 = c('부산'),
  대구 = c('대구'),
  인천 = c('강화', '백령도', '인천'),
  광주 = c('광주'),
  대전 = c('대전'),
  울산 = c('울산'),
  #세종
  경기 = c('동두천', '성산', '양평', '이천', '파주'),
  강원 = c('강릉', '대관령', '동해', '속초', '수원', '영월', '인제', '정선군', 
            '철원', '춘천', '태백', '홍천'),
  충북 = c('보은', '제천', '청주', '추풍령', '충주'),
  충남 = c('금산', '보령', '부여', '서산', '천안', '홍성'),
  전북 = c('고창', '고흥', '구미', '남원', '순창', '임실', '장수', '전주', '정읍'),
  전남 = c('강진군', '광양', '목포', '보성군', '순천', '여수', '영광', '완도',
             '장흥', '진도', '해남', '흑산도'),
  경북 = c('경주', '구미', '문경', '봉화', '상주', '안동', '영덕', '영주', '영천', 
            '울릉도', '울진', '의성', '청송군', '포항'),
  경남 = c('거제', '거창', '김해시', '남해', '밀양', '북창원', '산청', '양산', 
            '의령군', '진주', '창원', '통영', '함양군', '합천'),
  제주 = c('서귀포', '제주')
)
kor

new_weather <- NULL

for(sido in 1:length(kor)){
  #print(names(kor[sido]))
  weather %>%
    filter(city %in% unlist(kor[sido])) %>%
    group_by(date) %>%
    summarise('평균기온' = round(mean(평균기온, na.rm=T), 2),
              '강수량'  = round(mean(강수량, na.rm=T), 2),
              '적설량'  = round(mean(신적설, na.rm=T), 2),
              '풍속' = round(mean(평균풍속, na.rm=T), 2),
              '습도' = round(mean(상대습도, na.rm=T), 2),
              '운량' = round(mean(운량, na.rm=T), 2)) -> comb
  
  ncomb <- comb %>%
    mutate('날씨' = ifelse(운량 >= 9, '흐림', 
                           ifelse(운량 >= 6, '구름많음', 
                                    ifelse(운량 >= 3, '구름조금',
                                             ifelse(운량 >= 0, '맑음', NA))))) %>% 
    select(-c(운량))
  
  wdata <- cbind(ncomb, city=names(kor[sido]))
  new_weather <- rbind(new_weather, wdata)
}


View(new_weather)
str(new_weather)

new_weather %>% 
  group_by(city) %>% 
  count()

write.csv(new_weather, 'data/new_weather.csv')


