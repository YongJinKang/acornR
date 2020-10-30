View(df_movie)

head(df_movie,10)
tail(df_movie,10)
dim(df_movie)
summary(df_movie)
str(df_movie)



library(dplyr)

df_movie %>% filter(movieNm == "수퍼맨 리턴즈")
