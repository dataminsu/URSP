library(ggplot2)
library(sf)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(rgdal)
library(sf)

test_shp <- readOGR("geocoding.shp", use_iconv=TRUE, encoding = "UTF-8")
plot(test_shp)

pivot_file <- st_pivot

install.packages("leaflet.minicharts")
library(leaflet.minicharts)

#data wrangling
filter_EA <- pivot_file %>%
  filter(unit == "EA") %>%
  rename(EA = quantity)

filter_EA <- filter_EA[,-5]

filter_kg <- pivot_file %>%
  filter(!unit %in% "EA")%>%
  rename(kg = quantity)

filter_kg <- filter_kg[,-5]

real_data <- unique(pivot_file[,c("location", "x", "y", "year")])

test_data <- merge(real_data, filter_EA,
                   by = c("location", "x","y","year"))

test_data <- merge(test_data, filter_kg,
                   by = c("location", "x", "y","year"))

pi_2019 <- test_data %>%
  filter(year == 2019)

pi_2019_2 <- pi_2019 %>%
  mutate(kg_10 = kg * 10)

#2019 map
colors = c("#FF0000", "#428EF4")

map_title_style <- tags$style(HTML('*{font-family: "Georgia"}')) #font type

map_title<- tags$div(
  map_title_style, HTML("Korean Marine Debris Bar Chart") #title name
)  

map <- pi_2019_2 %>%
    leaflet() %>% 
    addTiles() %>% 
    addProviderTiles(providers$CartoDB.Voyager) %>%
    addMinicharts(
    pi_2019_2$x, pi_2019_2$y, 
    chartdata = pi_2019_2[, c("EA", "kg_10")],
    colorPalette = colors,
    type = "bar",
    height = 45, width = 30,
    popup = popupArgs(html = paste0("Location: ", pi_2019_2$location, "<br/>",
                                    "EA: ", pi_2019_2$EA, "<br/>",
                                    "kg * 10: ", pi_2019_2$kg_10, " kg", " (real data: ", pi_2019_2$kg, ")"))
  )%>%
  addControl(map_title, position = "bottomleft")

map

map_2 <- read_sf("ctp_rvn.shp")


basemap = leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Voyager) 

g2 <- map_2["CTP_ENG_NM"]
df_g2 <- fortify(g2)

#g3 <- plot(g2, color = "black",
    # main = "Korea map")

g1 <- pi_2019_2 %>%
  ggplot(aes(x, y, color = kg))+
  geom_point(size = 2)+
  scale_color_viridis_c()+
  ggtitle("3D plot of Korea Marine Debris")+
  ylab("longitude")+ #y-label
  xlab("latitude") #x-label

g1

library(rayshader)

plot_gg(g1, zoom = 0.65)

plot_gg(map)
