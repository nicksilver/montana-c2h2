library(ggplot2)
library(rnoaa)
library(dplyr)
library(ggmap)
library(ggrepel)

(stations <- ghcnd_stations())
stns.f <- stations %>% 
  filter(state == "MT") %>% 
  filter(first_year < 1990) %>%
  filter(last_year == 2019) %>%
  filter(element == "TMIN") %>%
  filter(wmo_id != "") %>%
  distinct(id, .keep_all = T)

data <- as.data.frame(stns.f)

mt.bbox <- c(left=-116.151, bottom=44.244, right=-103.897, top=49.168)
map <- get_stamenmap(bbox = mt.bbox, zoom=7, maptype = "toner")
ggmap(map) + 
  geom_point(data=data, aes(x=longitude, y=latitude), color='red') + 
  geom_text_repel(data=data, aes(x=longitude, y=latitude, label=name))
