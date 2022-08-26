library(dplyr)          #包含以下函數:filter() select() mutate() arrange() summarise() group_by()等等

library(shiny) 
library(shinydashboard) #分開寫法
library(shinyWidgets)   #設定網頁背景顏色等外觀

library(ggplot2)        #包含以下函數: ggplot() geom...

library(readxl)         #讀取excel檔案所需函數:read_excel()
library(kableExtra)     #展示實地記錄所使用表格函數:kable() kable_styling()等等

library(dygraphs)       #包含以下函數:dygraph() dyRangeSelector()等等
library(tidyverse)      #包含以下函數:read_rds()

library(zoo)            #包含以下函數:index()


#getwd()
#setwd("D:/Cody/project")

#windowsFonts(words = windowsFont("標楷體"))

#########################匯入檔案區
staticEB2 <- as.data.frame(read_excel("static_EB2.xlsx"))       #實地空間資訊調查記錄
EBday <- readRDS("EB_day_20180101_20210228_aug.rds")            #工二每日用電紀錄
xts_data_daily <- read_rds("money.rds")                         #電費

#########################數據處理
EBD <- data.frame(
  Time = EBday$datetime,
  Usage = EBday$EB
)

#########################函數創建
colcal <- function(a){                                          #列表警示函數
  m <- mean(a$class_time)
  z <- 0
  li <- c()
  for(i in index(a)){
    if(a$class_time[i] > m){
      li[z+1] <- i
      z = z + 1
    }
  }
  return(li)
}

###############################################網頁側邊(前端)
sidebar <- dashboardSidebar(                                    #選擇欄
  
  sidebarMenu(
    menuItem("控制選單",tabName = "dashboard"),
    dateRangeInput("date",h3("選擇日期(範圍)"),
                   min = min(EBD$Time),
                   max = max(EBD$Time),
                   start = min(EBD$Time),
                   end = max(EBD$Time)
    )
  ),
  selectInput("select", label = h3("分析模式"),
              #list第一個為下拉列表顯示，第二個為連結用tag
              choices = list(
                "電表數據" = "graph1", 
                #"峰值分布" = "peak_scatter", 
                "用電實地調查推測紀錄" = "record",
                "電費支出" = "BILL"), 
              selected = 1
  )
  
  
)

###############################################網頁主內容(前端)
body <- dashboardBody(                                         #網頁主內容排版
  ##第一個condition中input.???，???抓取你要連結的tag
  conditionalPanel(
    condition = "input.select == 'graph1'",
    fluidRow(
      column(12,plotOutput("graph")),
      column(12,plotOutput("peak"))
    )
  ),
  conditionalPanel(
    condition = "input.select == 'record'",
    fluidRow(
      column(6,tableOutput("recording")),
      column(6,plotOutput("page1"))
    )
  ),
  conditionalPanel(
    condition = "input.select == 'BILL'",
    h1(id = "4","電費支出",align = "center",style = 'color:blue;;font-weight:bold;'),
    dygraphOutput("expanse")
  )
)

board <- dashboardPage(dashboardHeader(title = "電表數據展示儀表板"),sidebar,body)
ui <- board

###############################################網頁內容(後端)
server <- function(input,output){
  ##折線圖#########
  graphing <- reactive({
    filter(EBD,
           between(EBD$Time,
                   input$date[1],
                   input$date[2])
    )
  })
  output$graph <- renderPlot({
    ggplot(graphing(),aes(x = Time,y = Usage),main = "電量分析") +
      geom_line(colour='green') + 
      theme(panel.background = element_rect(fill = 'black', colour = 'white')) +
      scale_x_date(date_labels = "%y-%m-%d") +
      labs(title = "電表數據") + 
      theme(plot.title = element_text(family = "words",color = "dark green",size = 24,face = "bold",hjust = 0.5))
  })
  ################
  
  
  ##峰值分布######
  
  peak_scatter <- reactive({
    filter(EBD,
           between(EBD$Time,
                   input$date[1],
                   input$date[2]),
           Usage > 1250
    )
  })
  output$peak <- renderPlot({
    ggplot(peak_scatter(),aes(x = Time,y = Usage)) +
      theme(panel.background = element_rect(fill = 'black', colour = 'white')) +
      geom_point(color = "orange") +
      geom_hline(yintercept=1500, linetype="dashed", color = "red") + 
      labs(title = "用電峰值分布") + 
      theme(plot.title = element_text(family = "words",color = "dark green",size = 24,face = "bold",hjust = 0.5))
    
  })
  
  
  
  ################
  
  ##KABLE#########
  
  output$recording <- function(){
    staticEB2 %>%
      kable("html") %>%
      kable_styling("striped",full_width = F) %>%
      column_spec(3:5,bold = T) %>%
      row_spec(c(colcal(staticEB2)),background = "red") %>%
      scroll_box(height = "600px",width = "600px")
  }
  
  
  ################
  
  
  
  ####電費圖片###########
  output$expanse <- renderDygraph({
    dygraph(xts_data_daily) %>%
      dyRangeSelector(height = 40) %>%
      dyOptions(fillGraph = TRUE,fillAlpha = 0.2,gridLineColor = "green",pointSize = 0.6) %>%
      dySeries(color = "orange") %>%
      dyLegend(show = "follow") %>%
      dyAxis("x", drawGrid = FALSE)
  })
  
  
  
  ##################################實地勘察數據處理
  ##table(staticEB2$category)
  #sum(table(table(staticEB2$category))) 找出有幾種不同資料
  category <- as.data.frame(cbind(c("assistant_room","classroom","department_office","lab","library","lounge","meeting_room","office"),rbind(0,0,0,0,0,0,0,0)))
  colnames(category) <- c("category","numbers")
  list <- c("assistant_room","classroom","department_office","lab","library","lounge","meeting_room","office")
  
  for(i in 1:length(staticEB2$category)){
    if(is.na(staticEB2$category[i]) == FALSE){
      for(j in 1:length(list)){
        if(staticEB2$category[i] == list[j]){
          category$numbers[j] = as.numeric(category$numbers[j]) + 1
        }
      }
    }
  }
  output$page1 <- renderPlot({
    ggplot(category,aes(x = category,y = numbers,fill = category)) +
      geom_bar(stat = "identity") + 
      geom_text(aes(label = numbers),vjust = 1.6,size = 5.5,color = "white") +
      labs(title = "空間分布資訊") + 
      theme(plot.title = element_text(family = "words",color = "dark green",size = 24,face = "bold",hjust = 0.5))
  })
  ##################################################
  
}

shinyApp(ui,server)