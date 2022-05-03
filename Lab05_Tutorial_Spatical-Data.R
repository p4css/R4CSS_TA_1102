library(tidyverse)
library(sf)

## Background Map
### import data
sf_taiwan <-
  st_read(dsn = "data/Lab05/鄉鎮市區界線/", layer = "TOWN_MOI_1100415", quiet = T) %>%
  rename_with(~str_to_lower(.), everything()) %>% st_transform(3826)
df_vote <- read_csv("data/Lab05/df_president.csv")


sf_bigtaipei <- sf_taiwan %>% filter(str_detect(countyname, "北市|基隆")) %>% 
  st_simplify(dTolerance = 100) %>% st_transform(4326) %>%
  st_crop(xmin = 121, xmax = 122.1, ymin = 24.4, ymax = 25.4) %>%
  select(county = countyname, town = townname) %>% left_join(df_vote) %>%
  mutate(per_tsai = can_tsai/有效票數,
         per_han = can_han/有效票數)

sf_bigtaipei %>% ggplot() + geom_sf()

sf_bigtaipei %>% ggplot(aes(fill = per_tsai)) + geom_sf(color = NA) +
  scale_fill_gradient(low = "white", high = "#1B9431")

## Cartogram
### load cartogram
library(cartogram)

### transfrom data
president_cartogram <- cartogram_cont(sf_bigtaipei %>% st_transform(3826), "投票數", itermax=5)

### plot
president_cartogram %>%
  ggplot() +
  geom_sf(aes(fill = per_tsai) , size=0, alpha=0.9) +
  scale_fill_gradientn(colors = c("white","#1B9431")) +
  coord_sf() +
  theme_void()

