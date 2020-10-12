
# 국내 교통사고 수 단계구분도

# 전용 choroplethr 단계구분도
# choroplethr은 R에서 단계구분도 생성을 간략화하려는 목적으로 개발

# 시도별 교통사고 수 (전체인구)

install.packages("choroplethr")
install.packages("choroplethrAdmin1")
#install.packages("Kormaps") -> 설치 후 library 실행 안됨
library(choroplethr);library(choroplethrAdmin1);library(dplyr)

# 지도데이터 가져오기
data(admin1.regions)
head(admin1.regions)

admin.all <- tbl_df(admin1.regions)
admin.kr <-  filter(admin.all, grepl("south korea", country))

admin1_map("south korea")

add_city_a <- c(36682, 28429, 27452, 40705, 23658, 23368, 23127, 159538, 42510, 36065, 24049, 13029, 22292, 30434, 2463, 116678, 12604)
as.data.frame(add_city_a) -> add_city_a

admin.kr %>% 
  select(region) -> admin.kr

cbind(admin.kr, add_city_a) -> wpop.kr

names(wpop.kr) <- c("region","value")


png("시도별 교통사고 수(map).png", 500, 400)

admin1_choropleth(country.name = "south korea", 
                  df           = wpop.kr, 
                  title        = "시도별 교통사고 수", 
                  legend       = "교통 사고 횟수", 
                  num_colors   = 1)

dev.off()


# 100만명 기준 

accident_pop_month %>%
  group_by(city) %>%
  summarise(accident_per_m = sum(accident_per_m)) -> acc_per_m 

acc_per_m

add_city_b <- c(10620, 17810, 12957, 16511, 15827, 15128, 15815, 12282, 15859, 10694, 8146, 19713, 12100, 16153, 8266, 11881, 10873)

admin.kr %>% 
  select(region) -> admin.kr

cbind(admin.kr, add_city_b) -> pop_per_m.kr

names(pop_per_m.kr) <- c("region","value")


png("시도별 교통사고 수(100만 명 기준)(map).png", 500, 400)

admin1_choropleth(country.name = "south korea", 
                  df           = pop_per_m.kr, 
                  title        = "시도별 교통사고 수(100만 명 기준)", 
                  legend       = "교통 사고 횟수", 
                  num_colors   = 1)

dev.off()

# 사망률

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

admin.kr %>% 
  select(region) -> admin.kr

add_city_c <- c(28.6, 49.5, 86.1, 23.1, 25.9, 58.6, 24.3, 30.8, 66.1, 66.1, 34.8, 40.7, 81.2, 72.6, 55.8, 20.1, 38.9)

cbind(admin.kr, add_city_c) -> acc_death_rate.kr

names(acc_death_rate.kr) <- c("region","value")


png("시도별 사망률(map).png", 500, 400)

admin1_choropleth(country.name = "south korea", 
                  df           = acc_death_rate.kr, 
                  title        = "시도별 사망률", 
                  legend       = "사망률(단위 : %)", 
                  num_colors   = 1)

dev.off()


#

# 2010년 시/도, 시/군/구, 읍/면/동 행정구역지도 3개를 갖고 인구총조사(2010년) 기준 지리정보를 제공하는 R 팩키지를 카톨릭대학교 문건웅 교수님께서 개발하여 공개하였다.
# 그냥 install.packages("Kormaps") 설치 후 require(Kormaps) 실행하면 안되서 아래와 같이...
install.packages("devtools")
devtools::install_github("cardiomoon/Kormaps")
library(Kormaps)

library(choroplethr);library(choroplethrAdmin1);library(dplyr);library(ggmap);library(maptools);
library(RColorBrewer);library(scales)
require(Kormaps)

class(korpopmap1)
names(korpopmap1)

# 아래는 인구
tmp <- korpopmap1@data %>% select(21)
names(tmp) <- c("population")
tmp$population <- tmp$population / 10^4

kor.dat <- data.frame(NAME_1=korpopmap1$name_eng, id=korpopmap1$id)
kor.dat <- bind_cols(kor.dat, tmp)
kor.dat

# 에러...
korea.shp.f <- fortify(korpopmap1, region = "id")

merge.shp.coef<-merge(korea.shp.f, kor.dat, by="id", all.x=TRUE)
korea.population.2010 <-merge.shp.coef[order(merge.shp.coef$order), ] 

ggplot() +
  geom_polygon(data = acc_death_rate.kr, 
               aes(x = region, y = value, fill = value), 
               color = "black", size = 0.25) + 
  coord_map()



#shapefile

new_acc_data %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  group_by(city) %>% 
  summarise(사고건수 = sum(사고건수)) -> acc_map

str(acc_map)

install.packages("tidyverse")
install.packages("sf")

library(tidyverse)
library(sf)

korea_sf <- st_read("R miniproject/CTPRVN.shp")

korea_sf$CTP_KOR_NM <- iconv(korea_sf$CTP_KOR_NM, from = "CP949", to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE)

korea_sf <- st_transform(korea_sf, "+proj=longlat +datum=WGS84")

korea_sf %>% 
  select(CTP_KOR_NM) %>% 
  plot()

korea_df <- korea_sf %>% 
  st_set_geometry(NULL)

korea_df

# 지도 + 사고건 수

#korea_df[3]

#acc_map[1] -> city_add

korea_df %>% 
  arrange(CTP_KOR_NM) -> korea_df

cbind(korea_df,city_add) -> korea_df # -> 비교위해서

# CTP_KOR_NM, city 일치하는지 확인하고 CTP_KOR_NM 빼기 -> 안해도될듯
#korea_df %>% 
#  select(-CTP_KOR_NM) -> korea_df

# 사고건수 추가
#korea_df <- NULL

acc_map[2] -> acc_count

cbind(korea_df,acc_count) -> korea_df

korea_df

# 시도별 교통사고 수 시각화 ()

str(korea_df)
as_tibble(korea_df) -> korea_df
str(korea_df)

install.packages("extrafont")


library(extrafont)
loadfonts()

korea_df %>% 
  ggplot(aes(fill=`사고건수`)) +
  geom_sf() +
  theme_void(base_family="NanumGothic") +
  labs(title="시도별 교통사고 수", fill="교통사고수") + 
  theme(legend.position = "right") +
  scale_fill_gradient(low = "wheat1", high = "red", labels = scales::comma)






library(httr)
library(rvest)

Sys.setlocale("LC_ALL", "C")

wiki_url <- 'https://ko.wikipedia.org/wiki/%EB%8C%80%ED%95%9C%EB%AF%BC%EA%B5%AD%EC%9D%98_%ED%96%89%EC%A0%95_%EA%B5%AC%EC%97%AD'

pop_dat <- wiki_url %>% 
  read_html() %>% 
  html_node(xpath='//*[@id="mw-content-text"]/div/table[3]') %>% 
  html_table(fill=TRUE)

Sys.setlocale("LC_ALL", "Korean")

pop_df <- pop_dat %>% 
  dplyr::rename(CTP_KOR_NM = `행정 구역`) %>% 
  dplyr::mutate(`인구` = parse_number(`인구`)) %>% 
  dplyr::filter(!str_detect(CTP_KOR_NM, "계")) %>% 
  dplyr::select(CTP_KOR_NM, `인구`)

DT::datatable(pop_df)








