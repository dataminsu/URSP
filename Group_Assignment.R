#######################
## Group Assignment
#######################

#import libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(rgdal)
library(sf)
library(maptools)
library(plotly)
library(ggthemes)
library(gganimate)
library(sf)
library(shiny)
library(shinythemes)

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


##Plot 3
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


#UI part
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

##
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

