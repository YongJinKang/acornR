library(ggmap)
library(readxl)
library(dplyr)
library(tidyr)
###import subway address
raw.subway = read_excel('./Data2/4647,4661번 서울교통공사 역별 주소 현황 및 전화번호.xlsx')
View(raw.subway)
head(raw.subway)

subway.add = raw.subway %>%
  select(c(3,2,6)) %>%
  rename(line = 호선, station = 역명, address = 상세주소) %>%
  separate(address, c('address', 'etc'), sep = '[(]') %>%
  select(-etc)

subway.add


  #########################################
# generate coordinate with geocode

register_google(key='API key')
coordinate = subway.add$address %>% enc2utf8() %>% geocode()

## merging

subway.lon.lat = bind_cols(subway.add, coordinate)
subway.lon.lat
View(subway.lon.lat)

#########################################
# display subway position on map

map.seoul = get_googlemap('seoul',
                          maptype = 'roadmap',
                          zoom = 11)
ggmap(map.seoul) + geom_point(data = subway.lon.lat,
                              aes(x = lon, y = lat), color = 'blue')


install.packages("xlsx")
library(xlsx)
write.xlsx(subway.lon.lat, "df_seoul_subway.xlsx")
