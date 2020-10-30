library(ggmap)
library(readxl)
library(dplyr)
library(tidyr)
###import apartment address
#raw_apartment = read.csv('../../pandas/seoul_apartment.csv')
#View(raw_apartment)
#head(raw_apartment)
#dim(raw_apartment)
# = raw_apartment %>%
  #select(c(6,2,13)) %>%
 # rename(apartment = 단지명, address = 시군구, road = 도로명)
#View(apartment.add)  
#. 테이블에서 중복값 제거하여 독립적인 아파트 데이터 확보
#. 아파트 데이터에 주소포함하여 주소로 위경도 파악
#. 쪼인
#real_apartment <-raw_apartment %>%  select(-계약년월, -X,-계약일,-거래금액.만원., -층,-전용면적...)
#View(real_apartment)
#####################)####################
# generate coordinate with geocode



# = real_apartment[-which(duplicated(real_apartment$단지명)),]

#install.packages("xlsx")
#library(xlsx)
#write.xlsx(real_apartment, "real_apartment_real.xlsx")
#str(real_apartment)
real_apartment <- read_excel("real_apartment_real.xlsx")
View(real_apartment)
real_apartment <- real_apartment %>% select(-...1)



real_apartment$address <- paste(real_apartment$시군구, real_apartment$번지, sep = " ")
View(real_apartment)

real_apartment_location<-real_apartment %>% 
  select(-시군구,-번지,-본번,-부번,-건축년도)

View(real_apartment_location)

apartment.add = real_apartment_location %>%
  select(c(1,2,3)) %>%
  rename(apartment = 단지명, road = 도로명)


View(apartment.add)
## merging
register_google(key='API key')
coordinate = apartment.add$address %>% enc2utf8() %>% geocode()

apartment.lon.lat = bind_cols(apartment.add, coordinate)
apartment.lon.lat
View(subway.lon.lat)

#########################################
# display subway position on map

map.seoul = get_googlemap('seoul',
                          maptype = 'roadmap',
                          zoom = 11)
ggmap(map.seoul) + geom_point(data = apartment.lon.lat,
                              aes(x = lon, y = lat), color = 'blue')


install.packages("xlsx")
library(xlsx)
write.xlsx(apartment.lon.lat, "df_seoul_apartment.xlsx")
