# 날씨 + 교통사고 데이터
rm(list = ls())

library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(reshape)
library(corrplot)

weather <- read.csv('data/new_weather.csv'); head(weather, 10)
accident <- read.csv('data/new_accident.csv')
population <- read.csv('data/new_population.csv')
rule <- read.csv('data/new_rule.csv')
# View(weather)
# View(accident)
# View(population)
# View(rule)

weather <- weather %>% select(-c(X))
accident <- accident %>% select(-c(X))
population <- population %>% select(-c(X))
rule <- rule %>% select(-c(X))


# 교통사고+날씨
ac_wt <- merge(weather, accident, by=c('date', 'city'))
head(ac_wt)



### 1. 강수량별 교통사고 ------------------------------------------
## 일자별 평균:  상관도낮음
ac_wt %>% 
  mutate(mday = paste(substr(date, 6, 11), '년', sep='')) %>% 
  group_by(mday) %>% 
  summarise(전국강수량 = mean(강수량, na.rm = T), # NA 제거
            전국사고건수 = sum(사고건수),
            전국사망률 = sum(사망자수)/(sum(사망자수)+sum(부상자수))) -> test

# 시각화
summary(test$전국강수량)
summary(test$전국사고건수)
ylim.r <- c(min(test$전국강수량, na.rm=T), max(test$전국강수량, na.rm=T))
ylim.a <- c(min(test$전국사고건수, na.rm=T), max(test$전국사고건수, na.rm=T))
b <- diff(ylim.r)/diff(ylim.a); a <- b*(ylim.r[1] - ylim.a[1])

ggplot(test, aes(mday, 전국강수량))+
  geom_col() +
  geom_line(aes(y=a+전국사고건수*b, group=1), col='blue', size=1)+
  scale_y_continuous("전국강수량", sec.axis = sec_axis(~ (. - a)/b, name = "전국사고건수")) +
  theme(axis.line.y.right = element_line(color = "blue"), 
        axis.ticks.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue"), 
        axis.title.y.right = element_text(color = "blue")) +
  ggtitle("전국 강수량과 교통사고 비교")

# 시각화
ggplot(test, aes(x=전국강수량, 전국사고건수)) +
  geom_point() -> g
ggplotly(g)


## 전국 월별 평균 교통사고 + 평균 강수량
ac_wt %>% 
  mutate(year = paste(substr(date, 1, 4), '년', sep=''),
         month = paste(substr(date, 6, 7),'월', sep='')) %>% 
  # filter(city == '제주') %>%
  group_by(month) %>% 
  summarise(전국강수량 = sum(강수량, na.rm = T), # NA 제거
            전국사고건수 = sum(사고건수),
            전국사망률 = sum(사망자수)/(sum(사망자수)+sum(부상자수))) -> test

# 시각화
summary(test$전국강수량)
summary(test$전국사고건수)
ylim.r <- c(min(test$전국강수량, na.rm=T), max(test$전국강수량, na.rm=T))
ylim.a <- c(min(test$전국사고건수, na.rm=T), max(test$전국사고건수, na.rm=T))
b <- diff(ylim.r)/diff(ylim.a); a <- b*(ylim.r[1] - ylim.a[1])

ggplot(test, aes(month, 전국강수량))+
  geom_col() +
  geom_line(aes(y=a+전국사고건수*b, group=1), col='blue', size=1) +
  scale_y_continuous("전국강수량", sec.axis = sec_axis(~ (. - a)/b, name = "전국사고건수")) +
  theme(axis.line.y.right = element_line(color = "blue"), 
        axis.ticks.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue"), 
        axis.title.y.right = element_text(color = "blue")) +
  ggtitle("전국 강수량과 교통사고 비교")


## 강수량(mm) 유무: 비가 오는날 일별 사고건수가 높음
summary(ac_wt$강수량) # 0, ~1, ~10, ~100, ~

ac_wt %>% 
  mutate(비 = ifelse(is.na(강수량), NA,
                    ifelse(강수량==0, '비 안오는 날', '비 오는 날'))) %>% 
  group_by(비) %>% 
  filter(!is.na(비)) %>% 
  # filter(!is.na(비) & city == '서울') %>% 
  summarise(n = n(),
            전국사고건수 = sum(사고건수),
            일별사고건수 = round(전국사고건수/n, 2),
            전국사망률 = round(sum(사망자수)/(sum(사망자수)+sum(부상자수))*100, 2)) -> test

# 시각화
ggplot(test, aes(비, 일별사고건수)) +
  geom_bar(stat="identity") + 
  geom_label(aes(label=일별사고건수)) +
  ggtitle('강수 유무에 따른 일별 사고건수') 


# 강수량(mm) 기준: 매우 많은 날 일별 사고건수가 높음
ac_wt %>% 
  mutate(비 = ifelse(is.na(강수량), NA,
                    ifelse(강수량==0, '없음',
                              ifelse(강수량 <= 1, '적음',
                                  ifelse(강수량 <= 10, '보통',
                                        ifelse(강수량 <= 100, '많음', '매우많음')))))) %>% 
  group_by(비) %>% 
  filter(!is.na(비)) %>%
  # filter(!is.na(비) & city == '제주') %>%
  summarise(n = n(),
            전국사고건수 = sum(사고건수),
            일별사고건수 = round(전국사고건수/n, 2),
            사망률 = round(sum(사망자수)/(sum(사망자수)+sum(부상자수))*100, 2)) -> test

# 시각화
ylim.r <- c(min(test$일별사고건수, na.rm=T), max(test$일별사고건수, na.rm=T))
ylim.a <- c(min(test$사망률, na.rm=T), max(test$사망률, na.rm=T))
b <- diff(ylim.r)/diff(ylim.a); a <- b*(ylim.r[1] - ylim.a[1])
a;b

ggplot(test, aes(비, 일별사고건수))+
  geom_col() +
  geom_line(aes(y=사망률*b, group=1), col='blue', size=1) +
  geom_label(aes(label=일별사고건수)) +
  scale_y_continuous("일별사고건수", sec.axis = sec_axis(~ ./b, name = "사망률")) +
  theme(axis.line.y.right = element_line(color = "blue"), 
        axis.ticks.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue"), 
        axis.title.y.right = element_text(color = "blue")) +
  scale_x_discrete(limits=c('없음', '적음', '보통', '많음', '매우많음')) +
  ggtitle("강수량과 교통사고 비교")



# 2. 날씨별 교통사고(운량기준) ------------------------------------------------
# 전국 날씨별 사고건수: 상관성 없음
# ex_날씨가 맑을 때나 흐릴 때 중 언제 사고가 더 발생하는지?
ac_wt %>% 
  group_by(날씨) %>% 
  summarise(n = n(),
            전국사고건수 = sum(사고건수),
            일별사고건수 = round(전국사고건수/n, 2),
            사망률 = round(sum(사망자수)/(sum(사망자수)+sum(부상자수))*100, 2))

# 지역별/연도별/월별(계절별)
# 지역별 연도별 데이터: 지역별 연도별 날씨 비중
# 월별 데이터: 월별 날씨 비중 vs 인구수 대비 사고건수




# 3. 일별 전국 교통사고  -----------------------------------------------------
# ex_비오는 날에는 어떤 교통사고가 많은지? 증가하는 교통사고는?
ac_wt %>% 
  # filter(city == '경기') %>% 
  group_by(date) %>% 
  summarise(강수량 = sum(강수량, na.rm = T),
            전국사고건수 = sum(사고건수),
            전국사망자수 = sum(사망자수),
            전국부상자수 = sum(부상자수)) -> ac_wt_all
# View(ac_wt_all)

ac_wt_r <- merge(ac_wt_all, rule, by='date')
# colnames(ac_wt_r)

## 비 ox: 신호위반
ac_wt_r %>% 
  mutate(비 = ifelse(is.na(강수량), NA,
                    ifelse(강수량==0, '비 안오는 날', '비 오는 날'))) %>% 
  group_by(비) %>% 
  summarise(n = n(),
            평균과속 = sum(과속, na.rm=T),
            평균중앙선침범 = sum(중앙선.침범, na.rm=T),
            평균신호위반 = sum(신호위반, na.rm=T),
            평균안전거리미확보 = sum(안전거리.미확보, na.rm=T),
            평균안전운전의무불이행 = sum(안전운전.의무.불이행, na.rm=T),
            
            과속 = round(평균과속/n, 2),
            '중앙선 침범' = round(평균중앙선침범/n, 2),
            신호위반 = round(평균신호위반/n, 2),
            '안전거리 미확보' = round(평균안전거리미확보/n, 2),
            '안전의무 불이행' = round(평균안전운전의무불이행/n, 2)) %>% 
  select(c(비, '과속', '중앙선 침범', '신호위반', '안전거리 미확보'))-> test
test <- as.data.frame(test)
test <- melt(test, 
     id.vars=c('비'),
     meaure.vars=c('과속', '중앙선 침범', '신호위반', '안전거리 미확보'))
colnames(test) <- c('강수', '위반내용', '사고건수'); test

# 시각화
ggplot(test, aes(x=위반내용, y=사고건수, fill=강수))+
  geom_bar(position='dodge', stat='identity') +
  ggtitle('강수 유무와 교통법규 위반')


# 비 정도: 과속, 중앙선, 신호위반반
ac_wt_r %>% 
  mutate(비 = ifelse(is.na(강수량), NA,
                    ifelse(강수량==0, '없음',
                              ifelse(강수량 <= 1, '적음',
                                        ifelse(강수량 <= 10, '보통',
                                                  ifelse(강수량 <= 100, '많음', '매우많음')))))) %>% 
  group_by(비) %>% 
  summarise(n = n(),
            평균과속 = sum(과속, na.rm=T),
            평균중앙선침범 = sum(중앙선.침범, na.rm=T),
            평균신호위반 = sum(신호위반, na.rm=T),
            평균안전거리미확보 = sum(안전거리.미확보, na.rm=T),
            평균안전운전의무불이행 = sum(안전운전.의무.불이행, na.rm=T),
            
            과속 = round(평균과속/n, 2),
            '중앙선 침범' = round(평균중앙선침범/n, 2),
            신호위반 = round(평균신호위반/n, 2),
            '안전거리 미확보' = round(평균안전거리미확보/n, 2),
            '안전의무 불이행' = round(평균안전운전의무불이행/n, 2)) %>% 
  select(c(비, '과속', '중앙선 침범', '신호위반', '안전거리 미확보'))-> test
test <- as.data.frame(test); test
test <- melt(test, 
             id.vars=c('비'),
             meaure.vars=c('과속', '중앙선 침범', '신호위반', '안전거리 미확보'))
colnames(test) <- c('강수', '위반내용', '사고건수'); test

# 시각화
ggplot(test, aes(x=위반내용, y=사고건수, fill=강수))+
  geom_bar(position='dodge', stat='identity') +
  scale_fill_discrete(labels=c('없음', '적음', '보통', '많음', '매우많음')) + 
  ggtitle('강수와 교통법규 위반') 


na_if(ac_wt_r, '-')



# 4. 풍속과 교통사고 ---------------------------------------------------------
## 월별 풍속과 교통사고 : 상관도 낮음
ac_wt %>% 
  mutate(year = paste(substr(date, 1, 4), '년', sep=''),
         month = paste(substr(date, 6, 7),'월', sep='')) %>% 
  group_by(month) %>% 
  summarise(전국풍속 = mean(풍속, na.rm = T), # NA 제거
            전국사고건수 = sum(사고건수),
            전국사망률 = sum(사망자수)/(sum(사망자수)+sum(부상자수))) -> test

# 시각화
ylim.r <- c(min(test$전국풍속, na.rm=T), max(test$전국풍속, na.rm=T))
ylim.a <- c(min(test$전국사고건수, na.rm=T), max(test$전국사고건수, na.rm=T))
b <- diff(ylim.r)/diff(ylim.a); a <- b*(ylim.r[1] - ylim.a[1])

ggplot(test, aes(month, 전국풍속))+
  geom_col() +
  geom_line(aes(y=전국사고건수*b, group=1), col='blue', size=1) +
  scale_y_continuous("전국풍속", sec.axis = sec_axis(~ ./b, name = "전국사고건수")) +
  theme(axis.line.y.right = element_line(color = "blue"), 
        axis.ticks.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue"), 
        axis.title.y.right = element_text(color = "blue")) +
  ggtitle("풍속과 교통사고")


## 풍속 기준과 교통사고
summary(ac_wt$풍속) # ~5, ~8, ~10, ~

ac_wt %>% 
  mutate(바람 = ifelse(is.na(풍속), NA,
                    ifelse(풍속 <= 5, '약함',
                              ifelse(풍속 <= 8, '보통',
                                        ifelse(풍속 <= 10, '강함', '매우강함'))))) %>% 
  group_by(바람) %>% 
  filter(!is.na(바람)) %>% 
  summarise(n = n(),
            전국사고건수 = sum(사고건수),
            일별사고건수 = round(전국사고건수/n, 2),
            사망률 = round(sum(사망자수)/(sum(사망자수)+sum(부상자수))*100, 2)) -> test

# 시각화
ylim.r <- c(min(test$일별사고건수, na.rm=T), max(test$일별사고건수, na.rm=T))
ylim.a <- c(min(test$사망률, na.rm=T), max(test$사망률, na.rm=T))
b <- diff(ylim.r)/diff(ylim.a); a <- b*(ylim.r[1] - ylim.a[1])
a;b

ggplot(test, aes(바람, 일별사고건수))+
  geom_col() +
  geom_line(aes(y=사망률*b, group=1), col='blue', size=1) +
  scale_y_continuous("일별사고건수", sec.axis = sec_axis(~ ./b, name = "사망률")) +
  theme(axis.line.y.right = element_line(color = "blue"), 
        axis.ticks.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue"), 
        axis.title.y.right = element_text(color = "blue")) +
  scale_x_discrete(limits=c('약함', '보통', '강함', '매우강함')) +
  ggtitle("풍속과 교통사고 비교")


## 풍속 정도: 과속, 중앙선, 신호위반
ac_wt %>% 
  group_by(date) %>% 
  summarise(풍속 = mean(강수량, na.rm = T),
               전국사고건수 = sum(사고건수),
               전국사망자수 = sum(사망자수),
               전국부상자수 = sum(부상자수)) -> ac_wt_all
# View(ac_wt_all)

ac_wt_r <- merge(ac_wt_all, rule, by='date')


ac_wt_r %>% 
  mutate(바람 = ifelse(is.na(풍속), NA,
                     ifelse(풍속 <= 5, '약함',
                              ifelse(풍속 <= 8, '보통',
                                       ifelse(풍속 <= 10, '강함', '매우강함'))))) %>% 
  group_by(바람) %>%  
  filter(!is.na(바람)) %>% 
  summarise(n = n(),
            # 교통 위반 건수 합치기
            평균과속 = sum(과속, na.rm=T),
            평균중앙선침범 = sum(중앙선.침범, na.rm=T),
            평균신호위반 = sum(신호위반, na.rm=T),
            평균안전거리미확보 = sum(안전거리.미확보, na.rm=T),
            평균안전운전의무불이행 = sum(안전운전.의무.불이행, na.rm=T),
            
            과속 = round(평균과속/n, 2),
            '중앙선 침범' = round(평균중앙선침범/n, 2),
            신호위반 = round(평균신호위반/n, 2),
            '안전거리 미확보' = round(평균안전거리미확보/n, 2),
            '안전의무 불이행' = round(평균안전운전의무불이행/n, 2)) %>% 
  select(c(바람, '과속', '중앙선 침범', '신호위반', '안전거리 미확보'))-> test
test <- as.data.frame(test); test
test <- melt(test, 
             id.vars=c('바람'),
             meaure.vars=c('과속', '중앙선 침범', '신호위반', '안전거리 미확보'))
colnames(test) <- c('풍속', '위반내용', '사고건수'); test

# 시각화
ggplot(test, aes(x=위반내용, y=사고건수, fill=풍속))+
  geom_bar(position='dodge', stat='identity') +
  scale_fill_discrete(labels=c('약함', '보통', '강함', '매우강함')) + 
  ggtitle('풍속와 교통법규 위반') 



## 5. 습도와 교통사고 ----------------------------------------------------
## 월별 습도과 교통사고 : 
ac_wt %>% 
  mutate(year = paste(substr(date, 1, 4), '년', sep=''),
         month = paste(substr(date, 6, 7),'월', sep='')) %>% 
  group_by(month) %>% 
  summarise(습도 = mean(습도, na.rm = T), # NA 제거
                전국사고건수 = sum(사고건수),
                전국사망률 = sum(사망자수)/(sum(사망자수)+sum(부상자수))) -> test

# 시각화
ylim.r <- c(min(test$습도, na.rm=T), max(test$습도, na.rm=T))
ylim.a <- c(min(test$전국사고건수, na.rm=T), max(test$전국사고건수, na.rm=T))
b <- diff(ylim.r)/diff(ylim.a); a <- b*(ylim.r[1] - ylim.a[1])

ggplot(test, aes(month, 습도))+
  geom_col() +
  geom_line(aes(y=0.5*a+전국사고건수*b, group=1), col='blue', size=1) +
  scale_y_continuous("습도", sec.axis = sec_axis(~ 0.5*a+./b, name = "전국사고건수")) +
  theme(axis.line.y.right = element_line(color = "blue"), 
        axis.ticks.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue"), 
        axis.title.y.right = element_text(color = "blue")) +
  ggtitle("습도과 교통사고")


## 습도 기준과 교통사고
summary(ac_wt$습도) # ~55, ~68, ~78, ~

ac_wt %>% 
  mutate(습도 = ifelse(is.na(습도), NA,
                     ifelse(습도 <= 55, '낮음',
                              ifelse(습도 <= 68, '보통',
                                       ifelse(습도 <= 78, '높음', '매우높음'))))) %>% 
  group_by(습도) %>% 
  filter(!is.na(습도)) %>% 
  summarise(n = n(),
            전국사고건수 = sum(사고건수),
            일별사고건수 = round(전국사고건수/n, 2),
            사망률 = round(sum(사망자수)/(sum(사망자수)+sum(부상자수))*100, 2)) -> test

# 시각화
ylim.r <- c(min(test$일별사고건수, na.rm=T), max(test$일별사고건수, na.rm=T))
ylim.a <- c(min(test$사망률, na.rm=T), max(test$사망률, na.rm=T))
b <- diff(ylim.r)/diff(ylim.a); a <- b*(ylim.r[1] - ylim.a[1])
a;b

ggplot(test, aes(습도, 일별사고건수))+
  geom_col() +
  geom_line(aes(y=사망률*b, group=1), col='blue', size=1) +
  scale_y_continuous("일별사고건수", sec.axis = sec_axis(~ ./b, name = "사망률")) +
  theme(axis.line.y.right = element_line(color = "blue"), 
        axis.ticks.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue"), 
        axis.title.y.right = element_text(color = "blue")) +
  scale_x_discrete(limits=c('낮음', '보통', '높음', '매우높음')) +
  ggtitle("습도와 교통사고 비교")


## 습도 정도: 과속, 중앙선, 신호위반
ac_wt %>% 
  group_by(date) %>% 
  summarise(습도 = mean(습도, na.rm = T),
              전국사고건수 = sum(사고건수),
              전국사망자수 = sum(사망자수),
              전국부상자수 = sum(부상자수)) -> ac_wt_all
# View(ac_wt_all)

ac_wt_r <- merge(ac_wt_all, rule, by='date')


ac_wt_r %>% 
  mutate(습도 = ifelse(is.na(습도), NA,
                     ifelse(습도 <= 55, '낮음',
                              ifelse(습도 <= 68, '보통',
                                       ifelse(습도 <= 78, '높음', '매우높음'))))) %>% 
  group_by(습도) %>%  
  filter(!is.na(습도)) %>% 
  summarise(n = n(),
            # 교통 위반 건수 합치기
            평균과속 = sum(과속, na.rm=T),
            평균중앙선침범 = sum(중앙선.침범, na.rm=T),
            평균신호위반 = sum(신호위반, na.rm=T),
            평균안전거리미확보 = sum(안전거리.미확보, na.rm=T),
            평균안전운전의무불이행 = sum(안전운전.의무.불이행, na.rm=T),
            
            과속 = round(평균과속/n, 2),
            '중앙선 침범' = round(평균중앙선침범/n, 2),
            신호위반 = round(평균신호위반/n, 2),
            '안전거리 미확보' = round(평균안전거리미확보/n, 2),
            '안전의무 불이행' = round(평균안전운전의무불이행/n, 2)) %>% 
  select(c(습도, '과속', '중앙선 침범', '신호위반', '안전거리 미확보'))-> test
test <- as.data.frame(test); test
test <- melt(test, 
             id.vars=c('습도'),
             meaure.vars=c('과속', '중앙선 침범', '신호위반', '안전거리 미확보'))
colnames(test) <- c('습도', '위반내용', '사고건수'); test

# 시각화
ggplot(test, aes(x=위반내용, y=사고건수, fill=습도))+
  geom_bar(position='dodge', stat='identity') +
  scale_fill_discrete(labels=c('낮음', '보통', '높음', '매우높음')) + 
  ggtitle('습도와 교통법규 위반') 



## 불쾌지수와 교통사고
# 불쾌지수 = 9/5*건구온도-0.55(1-상대습도)(9/5*온도-26)+32
#불쾌지수 = 9/5*평균기온-0.55(1-습도)(9/5*평균기온-26)+32,
# colSums(is.na(ac_wt))
# 
# ac_wt %>% 
#   mutate(불쾌지수 = 9/5*평균기온-0.55*(1-습도)*(9/5*평균기온-26)+32,
#          month = paste(substr(date, 6, 7),'월', sep='')) %>% 
#   group_by(month) %>% 
#   summarise(불쾌지수 = mean(불쾌지수, na.rm = T), # NA 제거
#               전국사고건수 = sum(사고건수),
#               전국사망률 = sum(사망자수)/(sum(사망자수)+sum(부상자수))) -> test
# 
# # 시각화
# ylim.r <- c(min(test$불쾌지수, na.rm=T), max(test$불쾌지수, na.rm=T))
# ylim.a <- c(min(test$전국사고건수, na.rm=T), max(test$전국사고건수, na.rm=T))
# b <- diff(ylim.r)/diff(ylim.a); a <- b*(ylim.r[1] - ylim.a[1])
# 
# ggplot(test, aes(month, 불쾌지수))+
#   geom_col() +
#   geom_line(aes(y=0.5*a+전국사고건수*b, group=1), col='blue', size=1) +
#   scale_y_continuous("불쾌지수", sec.axis = sec_axis(~ 0.5*a+./b, name = "전국사고건수")) +
#   theme(axis.line.y.right = element_line(color = "blue"), 
#         axis.ticks.y.right = element_line(color = "blue"),
#         axis.text.y.right = element_text(color = "blue"), 
#         axis.title.y.right = element_text(color = "blue")) +
#   ggtitle("불쾌지수와 교통사고")





# 강수량이 높은 달 또는 적설량 또는 풍속 > 사고랑 연관되는지 (상관계수)
# 어떤 요소가 가장 상관계수가 높은지? 

# 강수량이 적은데 사망률이 높았던 적은? 풍속이 세다
# >>상관분석 > 강수량 많을 수록, 풍속 셀수록(강수량이 높/낮고, 풍속이 셀 때?)


# 날씨 + 교통사고(전국 합계)
ac_wt <- merge(weather, accident, by=c('date', 'city'))
head(ac_wt)

ac_wt %>% 
  group_by(date) %>% 
  filter(강수량 >= 1 & 강수량 <= 60 & 사고건수<50) %>% 
  summarise(강수량 = mean(강수량, na.rm = T), # NA 제거
            평균기온 = mean(평균기온, na.rm=T),
            풍속 = mean(풍속, na.rm=T),
            습도 = mean(습도, na.rm=T),
            사고건수 = mean(사고건수),
            사망률 = sum(사망자수)/(sum(사망자수)+sum(부상자수))) -> test

# 결측값 처리
colSums(is.na(test))
#test$강수량 <- ifelse(is.na(test$강수량), mean(test$강수량, na.rm=T), test$강수량)

head(test)
test <- test[,-c(1)]

# 상관분석
weather_cor <- round(cor(test), 2)
corrplot(weather_cor,
         method = 'color',
         type='lower',
         addCoef.col = 'black', 
         tl.col = 'black',
         tl.srt = 45,
         diag = F)

ggplot(test, aes(강수량, 사고건수)) +
  geom_point(size=2, col='blue') +
  scale_y_continuous(limits = c(0, 50)) + 
  scale_x_continuous(limits = c(0, 50))


### 날씨 + 교통사고 + 교통법규
head(rule)

ac_wt %>% 
  select(c(date, city, 강수량, 평균기온, 풍속, 사고건수, 습도, 사망자수, 부상자수)) %>% 
  group_by(date) %>% 
  summarise(강수량 = round(mean(강수량, na.rm = T), 2),
            습도 = round(mean(습도, na.rm = T), 2),
            평균기온 = round(mean(평균기온, na.rm = T), 2),
            풍속 = round(mean(풍속, na.rm = T), 2),
            사고건수 = round(mean(사고건수, na.rm = T), 2),
            사망률per = round(sum(사망자수)/(sum(사망자수)+sum(부상자수))*100, 2)) -> ac_wt_new

merge(ac_wt_new, rule, by=c('date')) -> test  
head(test)

# 결측값 처리
colSums(is.na(test))
test$강수량 <- ifelse(is.na(test$강수량), mean(test$강수량, na.rm=T), test$강수량)
test$과속 <- ifelse(is.na(test$과속), mean(test$과속, na.rm=T), test$과속)
test <- test %>% 
  select(-c(사고건수, 사망률per))

# 상관분석
wacc_cor <- round(cor(test[,c(-1)]), 2)
corrplot(wacc_cor,
         method = 'color',
         type='lower',
         addCoef.col = 'black', 
         tl.col = 'black',
         tl.srt = 45,
         diag = F)









