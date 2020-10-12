
install.packages('lubridate')
library(lubridate)
library(dplyr)

new_acc_data <- read.csv('R miniproject/new_accident.csv')
View(new_acc_data)

# 연도별 사고건수 합

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

  
# 전국 기준 월별 
new_acc_data %>% 
  mutate(month = month(date)) %>% 
  group_by(month) %>% 
  summarise(사고건수 = sum(사고건수),
                사망자수 = sum(사망자수),
                부상자수 = sum(부상자수))

# 전국 기준 연도별
new_acc_data %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(사고건수 = sum(사고건수),
                사망자수 = sum(사망자수),
                부상자수 = sum(부상자수))

# 전국 기준 연도별, 월별
new_acc_data %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>%
  group_by(year, month) %>% 
  summarise(사고건수 = sum(사고건수),
                사망자수 = sum(사망자수),
                부상자수 = sum(부상자수))


# 전국 기준 일자별(교통사고 추이) > 시각화
new_acc_data %>% 
  group_by(date) %>% 
  summarise(사고건수 = sum(사고건수),
            사망자수 = sum(사망자수),
            부상자수 = sum(부상자수))

# 지역별
new_acc_data %>% 
  mutate(year = year(date)) %>% 
  group_by(city) %>% 
  summarise(사고건수 = sum(사고건수),
                사망자수 = sum(사망자수),
                부상자수 = sum(부상자수))

# 연도별, 지역별
new_acc_data %>% 
  mutate(year = year(date)) %>% 
  group_by(year, city) %>% 
  summarise(사고건수 = sum(사고건수),
            사망자수 = sum(사망자수),
            부상자수 = sum(부상자수))

# 월별, 지역별
new_acc_data %>% 
  mutate(month = month(date)) %>% 
  group_by(month, city) %>% 
  summarise(사고건수 = sum(사고건수),
                사망자수 = sum(사망자수),
                부상자수 = sum(부상자수))

# 일별, 지역별
new_acc_data %>% 
  group_by(date, city) %>% 
  summarise(사고건수 = sum(사고건수),
                사망자수 = sum(사망자수),
                부상자수 = sum(부상자수))







