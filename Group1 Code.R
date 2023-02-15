#import libraries
library(tidyverse)
library(readxl)
library(maptools)
library(plotly)
library(ggthemes)
library(gganimate)
library(shiny)
library(shinythemes)
library(readr)
library(rgdal)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(sp)
library(grid)
library(htmltools)
library(htmlwidgets)
library(htmltools)
library(leaflet.minicharts)
library(rayshader)

#import data
st1 <- read.csv("data1.csv", header = T, fileEncoding = "euc-kr")
st2 <- read.csv("data2.csv", header = T, fileEncoding = "euc-kr")
st3 = merge(st1, st2, by="지역")

##Data Wrangling
st_pivot <- st3 %>% pivot_longer(col = 2:35, names_to = "year", values_to = "quantity") %>%
  separate(year, c('First', 'Last')) %>%
  rename(year = First, unit = Last, location = 지역) %>% mutate(year = as.numeric(gsub("X", "", year))) %>%
  filter(!grepl('L', unit))

st_pivot <- read_csv("st_pivot.csv")

st_pivot[st_pivot == ""] <- NA
st_pivot$unit <- ifelse( is.na(st_pivot$unit), 'kg', st_pivot$unit)

st_pivot <- na.omit(st_pivot)

df <- st_pivot %>% pivot_wider(names_from = unit, values_from=quantity)

write.csv(df, "df.csv")

#---------------------------------------------------------------------Visualization 1
st <- read_csv("st_pivot.csv")
View(st)

tag_map_title <- tags$style(HTML(" 
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
")) #adding a title to our interactive map

title <- tags$div(
  tag_map_title, HTML("Marine Debris on The Korean Peninsula")
)  #save to dataframe to input into code later

mytext <- paste( 
  "Location: ", st$location, "<br/>", 
  "Quantity: ", st$quantity, "kg", "<br/>",
  "Year:", st$year) %>%
  lapply(htmltools::HTML)

map<- st %>%
  leaflet() %>%
  addTiles () %>%
  addCircleMarkers(lng = ~x, lat = ~y, popup = ~mytext,label = mytext, 
                   radius = ~(quantity/400), fillOpacity = 0.5) %>%
  addControl(title, position = "bottomleft") 


map

#---------------------------------------------------------------------Visualization 2
test_shp <- readOGR("geocoding.shp", use_iconv=TRUE, encoding = "UTF-8")
plot(test_shp)

pivot_file <- st_pivot

install.packages("leaflet.minicharts")

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

plot_gg(g1, zoom = 0.65)

plot_gg(map)

#---------------------------------------------------------------------Visualization 3
#Plot
unique(df$location)

country <- c("인천백령도 사곶해안","강화여차리갯벌","인천영종도 용유해변","인천덕적도 서포리해변", "안산말부흥",
             "태안백리포","태안안면도 바람아래해변","보령석대도","사천아두도","사천다사항해변",
             "부안변산","고창동호해변","영광백바위해변","신안임자도","진도하조도","신안고장리해변","해남예락해변",
             "해남송평해변","해남묵동리","완도신지도해변","고흥신흥","순천반월","여수백야도해변","여수백야도해변",
             "남해유구해변","통영망일봉","거제두모몽돌해변","마산봉암갯벌","부산해양대","울주진하해변","울산대왕암",
             "포항구룡포 대보해변","포항칠포","영덕고래불해변","울진후정", "동해노봉해변","강릉송정","속초청초")
df1 <- data.frame(country)

df <- df %>% rename("country" = "location")

df <- right_join(x=df1,y=df,by="country")

df <- df %>% rename("location" = "country")

xform <- list(categoryorder = "array",
              categoryarray = c("인천백령도 사곶해안","강화여차리갯벌","인천영종도 용유해변","인천덕적도 서포리해변", "안산말부흥",
                                "태안백리포","태안안면도 바람아래해변","보령석대도","사천아두도","사천다사항해변",
                                "부안변산","고창동호해변","영광백바위해변","신안임자도","진도하조도","신안고장리해변","해남예락해변",
                                "해남송평해변","해남묵동리","완도신지도해변", "고흥신흥","순천반월","여수백야도해변","여수백야도해변",
                                "남해유구해변","통영망일봉","거제두모몽돌해변","마산봉암갯벌","부산해양대","울주진하해변","울산대왕암",
                                "포항구룡포 대보해변","포항칠포","영덕고래불해변","울진후정", "동해노봉해변","강릉송정","속초청초"))


#UI
ui <- fluidPage(
  theme = shinytheme("darkly"), tags$head(tags$style('body {color:#EFFFE9;}')), 
  titlePanel(h1("Group Assignment CDS301", align = "center"),  windowTitle = "Plot3"), 
  fluidRow(width=12,
           sidebarLayout( 
             sidebarPanel(width = 3, style = "position:fixed; width:inherit;",
                          tags$head( 
                            tags$style("body {background-color: #303030; }")), 
                          selectInput("year",h2("Plot - Select Year"), c('2008','2009','2010','2011','2012',
                                                                         '2013','2014','2015','2016','2017',
                                                                         '2018','2019','2020'), selected = "2020"),
                          helpText(h4("You can choose one of years.")),
                          br(),
                          br(),
                          helpText(h3("About"),tags$br(),"The dataset is retrieved from 한국공공데이터포털",tags$br(),
                                   "The visualization is created by Minsu Kang.",tags$br(),
                                   "Published in 2022 November 28.")
             ),
             mainPanel(tags$head(
               tags$style("body {background-color: #2EC4B6; }")),
               br(),
               plotlyOutput("plot", height = "1000"),
               br(),
               br()
             ))))

#Server
server <- function(input, output) {
  
  dataInput <- reactive({
    if(input$year == '2020'){
      df 
    } else {subset(df %>% filter(df$year == input$year))}}) 
  
  output$plot <- renderPlotly({
    plot_ly(dataInput(), x = ~location, y = ~EA, type = "bar", name = "EA") %>% add_trace(y = ~kg, name = "kg") %>% 
      layout(title = list(text = "<b>Marine Debris Analysis<b>", font= list(family = "Times",
                                                                            size = 25, color = "#E71D36"), y = 0.98, x = 0.5, xanchor = 'center', yanchor =  'top'),
             font= list(family = "Arial", size = 20, color = "#011627"),
             xaxis = list(title = "<b>Location</b>", zeroline = T),
             yaxis = list(title = "<b>Quantity</b>", zeroline = T),
             showlegend = T,
             legend = list(title = list(text = "<b>Unit</b>")),
             plot_bgcolor = '#EFFFE9',
             paper_bgcolor = '#EFFFE9'
      ) %>% layout(xaxis=xform) #%>%  dd_segments(x = 15)
  })}
shinyApp(ui, server)
