# 교통 법규 데이터
rm(list = ls())

library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

year <- 2017
rule2017 <- read_csv('data/acc_rule_2017.csv')
rule <- cbind(year, rule2017)

year <- 2018
rule2018 <- read_csv('data/acc_rule_2018.csv')
rule <- rbind(rule, cbind(year, rule2018))

year <- 2019
rule2019 <- read_csv('data/acc_rule_2019.csv')
rule <- rbind(rule, cbind(year, rule2019))

View(rule)

colnames(rule)

rule %>% 
  filter(사고일!= '합계') %>% 
  mutate(date = paste(year, substring(사고월, 1, 2), substring(사고일, 1,2), sep='-')) %>% 
  select(c(date, 과속, '중앙선 침범', 신호위반, '안전거리 미확보', '안전운전 의무 불이행')) -> new_rule

new_rule <- na_if(new_rule, '-')

View(new_rule)


write.csv(new_rule, 'data/new_rule.csv')

1:5+10
ls()
