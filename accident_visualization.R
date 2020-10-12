
# 어떤 달에 교통사고가 많은지? (2017~2019)

library(ggplot2)

library(showtext)
showtext_auto() 
font_add(family = "cat", regular = "fonts/HoonWhitecatR.ttf")
font_add(family = "dog", regular = "fonts/THEdog.ttf")
font_add(family = "maple", regular = "fonts/MaplestoryBold.ttf")

new_acc_data %>% 
  mutate(month = month(date)) %>%
  group_by( month) %>% 
  summarise(사고건수 = sum(사고건수)) -> acc_data_month

png("월별 교통사고 수.png", 500, 400)

xname <- acc_data_month$month  # X 축 값 설정위한 벡터
cols <- ifelse(acc_data_month$사고건수 == max(acc_data_month$사고건수), "brown4", "grey")
barplot(acc_data_month$사고건수, main="월별 교통사고 수", xlab="month", ylab="count", col=cols, names.arg=xname, ylim=c(0,80000), family="dog")

dev.off()


# 교통사고량이 연도별로 어떻게 변하는지?

new_acc_data %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>% 
  group_by(year, month) %>%
  summarise(사고건수 = sum(사고건수)) -> test

test <- data.frame(test)
test %>% 
  filter(year == 2017) %>% 
  select(-year) -> ex_2017

test <- data.frame(test)
test %>% 
  filter(year == 2018) %>% 
  select(-year) -> ex_2018

test <- data.frame(test)
test %>% 
  filter(year == 2019) %>% 
  select(-year) -> ex_2019

merge(ex_2017, ex_2018, by='month') -> step1
merge(step1, ex_2019, by='month') -> acc_year
names(acc_year) <- c('month',2017,2018,2019)
acc_year

acc_year_g <- acc_year[2:4]
acc_year_g2 <- t(acc_year_g)
str(acc_year_g2)

png("연도별 교통사고 수.png", 500, 400)

xname <- acc_year$month
barplot(acc_year_g2, main="월별 교통사고 수", xlab="month", ylab="count", col=rainbow(3), names.arg=xname, ylim=c(0,80000), family="dog")
legend("topright", names(acc_year_g), cex=0.8, fill=rainbow(3));

dev.off()

# 어느 지역이 가장 교통사고 비중이 높은지?

new_acc_data %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  group_by(city) %>% 
  summarise(사고건수 = sum(사고건수),
                사망자수 = sum(사망자수),
                부상자수 = sum(부상자수)) -> test

#View(test)
#str(test)

acc_city <- test %>% select(city, 사고건수)

png("도시별 교통사고 수.png", 800, 700)

cols <- ifelse(acc_city$사고건수 == max(acc_city$사고건수), "brown4", "grey")
pie(acc_city$사고건수, labels=paste(acc_city$city, "-", acc_city$사고건수), main="도시별 교통사고 수", col=cols, family="dog")

dev.off()

# 도시별 인구 추가
# 시도별 월별 인구
population %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  select(-c(date))-> test2

ac_pop <- merge(test, test2, by=c('year', 'month', 'city'))
View(ac_pop)

# 100만명당 사고건수/사망자수/부상자수
ac_pop %>% 
  mutate(accident_per_m = round(사고건수/pop*1000000,2),
         death_per_m = round(사망자수/pop*1000000,2),
         injury_per_m = round(부상자수/pop*1000000,2)) -> accident_pop_month

#View(accident_pop_month)

#names(accident_pop_month)

# city별 100만명 당 사고건수

accident_pop_month %>%
  group_by(city) %>%
  summarise(accident_per_m = sum(accident_per_m)) -> acc_per_m 

acc_per_m

png("도시별 100만명 당 사고건 수.png", 800, 700)

cols <- ifelse(acc_per_m$accident_per_m == max(acc_per_m$accident_per_m), "brown4", "grey")
pie(acc_per_m$accident_per_m, labels=paste(acc_per_m$city, "-", acc_per_m$accident_per_m), main="도시별 100만명 당 사고건 수", col=cols, family="dog")

dev.off()

# city별 100만명 당 사망자수

accident_pop_month %>%
  group_by(city) %>%
  summarise(death_per_m = sum(death_per_m)) -> dea_per_m 

#dea_per_m

png("도시별 100만명 당 사망자 수.png", 800, 700)

cols <- ifelse(dea_per_m$death_per_m == max(dea_per_m$death_per_m), "brown4", "grey")
pie(dea_per_m$death_per_m, labels=paste(dea_per_m$city, "-", dea_per_m$death_per_m), main="도시별 100만명 당 사망자 수", col=cols, family="dog")

dev.off()

# city별 100만명 당 부상자수

accident_pop_month %>%
  group_by(city) %>%
  summarise(injury_per_m = sum(injury_per_m)) -> inj_per_m 

inj_per_m

png("도시별 100만명 당 부상자 수.png", 500, 400)

cols <- ifelse(inj_per_m$injury_per_m == max(inj_per_m$injury_per_m), "brown4", "grey")
pie(inj_per_m$injury_per_m, labels=paste(inj_per_m$city, "-", inj_per_m$injury_per_m), main="도시별 100만명 당 부상자 수", col=cols, family="dog")

dev.off()


# 어느지역의 사망률이 높은지?
  
new_acc_data %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>%
  group_by(year, month, city) %>% 
  summarise(사고건수 = sum(사고건수),
                사망자수 = sum(사망자수),
                부상자수 = sum(부상자수)) -> acc_wdata
  
acc_wdata %>% 
  mutate('death_rate' = round(사망자수/(사망자수+부상자수),3)*100) %>% 
  group_by(city) %>% 
  summarise(death_rate = sum(death_rate)) -> acc_death_rate

png("도시별 사망률.png", 800, 700)

cols <- ifelse(acc_death_rate$death_rate == max(acc_death_rate$death_rate), "brown4", "grey")
pie(acc_death_rate$death_rate, labels=paste(acc_death_rate$city, "-", acc_death_rate$death_rate, "%"), main="도시별 사망률", col=cols, family="dog")

dev.off()


# 연도별 교통사고 수 비교

atest

new_acc_data %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>%
  group_by(year, month) %>% 
  summarise(사고건수 = sum(사고건수)) -> btest

btest %>% 
  group_by(year) %>% 
  summarise(사고건수 = sum(사고건수)) -> btest_n

png("연도별 교통사고 수.png", 500, 400)

xname <- btest_n$year  # X 축 값 설정위한 벡터
cols <- ifelse(btest_n$사고건수 == max(btest_n$사고건수), "brown4", "grey")
barplot(btest_n$사고건수, main="연도별 교통사고 수", xlab="year", ylab="count", col=cols, names.arg=xname, ylim=c(0,250000), family="dog")

dev.off()

# 연도별 + 월별 교통사고 수 비교


new_acc_data %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>%
  group_by(year, month) %>% 
  summarise(사고건수 = sum(사고건수)) -> atest

atest %>% 
  filter(year == 2017) -> atest2017

test1 <- atest2017[2,3]
names(test1) <- c(2017)
#test1 <- t(test1)

atest %>% 
  filter(year == 2018) -> atest2018

test2 <- atest2018[3]
names(test2) <- c(2018)
#test2 <- t(test2)

atest %>% 
  filter(year == 2019) -> atest2019

test3 <- atest2019[3]
names(test3) <- c(2019)
#test3 <- t(test3)

str(test1); str(test2); str(test3)

merge(atest2017,atest2018, by='month') -> total_test
merge(total_test,atest2019, by='month') -> total_test

names(total_test)

total_test %>% 
  select(-month,-year.x,-year.y,-year) -> total_test

names(total_test) <- c(2017,2018,2019)

t(total_test) -> total_test

#names(total_test) <- c(1:12)

str(total_test)
class(total_test)

colnames(total_test) <- c(1:12)


png("연도별, 월별 교통사고 수 비교-세로.png", 500, 400)

barplot(total_test, beside=T, main="연도별, 월별 교통사고 수 비교", xlab="month", ylab="count", ylim=c(0,25000), , family="dog")

dev.off()


png("연도별, 월별 교통사고 수 비교-가로.png", 500, 400)

barplot(total_test, horiz=T, beside=T, main="연도별, 월별 교통사고 수 비교", xlab="count", ylab="month", xlim=c(0,25000), , family="dog")

dev.off()





