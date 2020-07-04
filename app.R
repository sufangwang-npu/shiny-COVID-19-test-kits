# This is a Shiny web application. 
# http://shiny.rstudio.com/

# packages used in this app
library(shiny)
library(shinydashboard,warn.conflicts=F)
library(ggplot2,warn.conflicts=F)
library(DT,warn.conflicts=F)
library(dplyr)
library(sp)
library(maptools)
library(maps)
#read data
data <- read.csv("data.csv", header = T)
data1 <- read.csv("data1.csv", header = T)
#define the format of date
data$Date <- as.Date(data$Date, format= "%Y-%m-%d")
# Define UI for application
ui <- fluidPage(

    dashboardPage(skin="blue",
        #application title          
        dashboardHeader(title = "Shiny-COVID-19-test-kits",titleWidth = 350),                
        #define sidedbar and items 
        dashboardSidebar( width = 200,                                                       
            sidebarMenu(
              menuItem("Kit-information", tabName = "Kit-information", icon = icon("vial")),
              menuItem("Data-analysis", tabName = "Data-analysis", icon = icon("chart-bar")),
              menuItem("Comparison", tabName = "Comparison", icon = icon("chart-line")),
              menuItem("more", tabName = "more", icon = icon("more")),
              menuItem(text = "Baidu", icon = icon("search"), href = "https://www.baidu.com/")
            )
        ),
        
        #Body part
        dashboardBody(                                                        
            tabItems(
                tabItem(tabName = "Kit-information",
                        h1("Overview"),
                        h4("The information mainly collect from China&U.S.A, update to 2020-06-24"),
                        fluidRow(
                          column(12,
                            #display a data table with download botton
                            datatable(caption = 'click row name to sort and type into boxes to search',
                                data[,1:8], extensions = 'Buttons',filter = 'top',width = 1100,
                                                options = list(
                                                               dom = 'lBfrtip',
                                                               scrollx=TRUE,
                                                               searchHighlight = TRUE,
                                                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                               lengthMenu = c(10, 20, 50, -1),
                                                               autoWidth = TRUE
                                                               )
                            )
                          )
                        )
                ),
                
                tabItem(tabName = "Data-analysis",
                        fluidRow(
                          tabBox(width = 12,height =  900,
                           tabPanel(title = "Map",
                             plotOutput("mp3"),
                             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                             #sliderinput of date range
                             sliderInput("Date1","Date",
                                      min = as.Date("2020-01-01"),
                                      max= as.Date("2020-06-30"),
                                      value = as.Date(c("2020-01-01","2020-06-30")),
                                      timeFormat ="%F"),
                             downloadButton(outputId = "down4", label = "Download the plot")
                                    ),
                           tabPanel(title = "Barplot",
                             plotOutput("Plot2"),
                             sliderInput("Date1","Date",
                                         min = as.Date("2020-01-01"),
                                         max= as.Date("2020-06-03"),
                                         value = as.Date(c("2020-01-01","2020-06-03")),
                                         timeFormat ="%F"),
                             downloadButton(outputId = "down1", label = "Download the plot")
                           ),
                           tabPanel(title = "Area",
                             plotOutput("Plot3"),
                             downloadButton(outputId = "down2", label = "Download the plot")
                             
                           )
                        )
                        )
                 ),
                 
                tabItem(tabName = "Comparison",
                        box(width = 6,
                            plotOutput("Plot1"),
                            downloadButton(outputId = "down3", label = "Download the plot")
                        ),
                        box(width = 6,
                            #select axis by selectinput
                            selectInput("selectx", h3("Select x"), 
                                        choices = list("date"=1 , "time"=2 ,
                                                       "sensitivity"=3 , "specificity"=4), selected = 2),
                            selectInput("selecty", h3("Select y"), 
                                        choices = list("date"=1 , "time"=2 ,
                                                       "sensitivity"=3 , "specificity"=4), selected = 3),
                            selectInput("selectz", h3("Select z"), 
                                        choices = list("Type"=1 , "Area"=2 
                                                       ),selected = 1)
                            
                        ),
                        
                        ),
                tabItem(tabName = "more",
                        h3("Please submit the information"),
                        fluidRow(
                            box(width = 12,
                                datatable(data1[,1:7],width = 1100,editable = 'cell',)
                           )
                        )
                )

                )
                  
            )
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Plot1 <- renderPlot({
         x<- switch(input$selectx,
                    "1"=data[,5],
                    "3"=data[,2],
                    "4"=data[,3],
                    "2"=data[,4]
         )
         y<- switch(input$selecty,
                    "1"=data[,5],
                    "3"=data[,2],
                    "4"=data[,3],
                    "2"=data[,4]
         )
         z<- switch(input$selectz,
                    "1"=data[,9],
                    "2"=data[,6]
         )
        ggplot(data, aes(x,y)) +geom_point(aes(colour = z),size=5)+theme(axis.text.x = element_text(angle = 90, hjust = 0.5,size = 16),axis.text.y = element_text(size = 16),axis.title.x =element_text(size=16), axis.title.y=element_text(size=16))
    })
    #filter data by date range
    Datemaster <- reactive({
      data %>% filter(Date>=input$Date1[1] & Date<=input$Date1[2])
    })
    #number-date of test kits barplot
    output$Plot2 <- renderPlot({
        Date <- Datemaster()[,5]
        ggplot(Datemaster(),aes(x=Date)) + geom_bar(aes(fill=Type))+scale_fill_manual(values=c("Viral tests" = "lightblue", "antibody tests" = "#F0E68c"))+theme(axis.text.x = element_text(angle = 45, hjust = 0.5))+scale_x_date(date_breaks = "1 week",date_minor_breaks = "1 day")
    })
    
   #number-area of test kit barplot 
    output$Plot3 <- renderPlot({
        Area <- data[,6]
        ggplot(data,aes(x=Area)) + geom_bar(aes(fill=Type))+scale_fill_manual(values=c("Viral tests" = "lightskyblue", "antibody tests" = "lightblue"))+coord_flip()
    })
    #map of test kits development
    output$mp3 <- renderPlot({
      mpx<-Datemaster()[,10]
      mpy<-Datemaster()[,11]
      mp<-NULL
      mapworld<-borders("world",color = "gray50",fill="white")
      mp<-ggplot()+mapworld+ylim(-60,90)
      mp2<-mp+geom_point(aes(x=mpx,y=mpy,size=Datemaster()[,12]),color="darkorange")+scale_size(range = c(2,4))
      mp3<-mp2+theme(legend.position = "none")#+theme(plot.margin = unit(rep(0,9),'lines'))
      mp3
    },height = 700,width=1600)
    #download botton
    output$down1 <- downloadHandler(
        filename =  function() {
            paste('data',Sys.Date(),'.pdf',sep='')
        },
        content = function(file) {
            pdf(file,width =13 ,height =6)
            Date<- Datemaster()[,5]
            print( ggplot(Datemaster(),aes(x=Date)) + geom_bar(aes(fill=Type))+scale_fill_manual(values=c("Viral tests" = "lightblue", "antibody tests" = "#F0E68c"))+theme(axis.text.x = element_text(angle = 45, hjust = 0.5))+scale_x_date(date_breaks = "1 week",date_minor_breaks = "1 day"))
            dev.off()
        }
        )
    output$down2 <- downloadHandler(
      filename =  function() {
        paste('data',Sys.Date(),'.pdf',sep='')
      },
      content = function(file) {
        pdf(file)
        Area <- data[,6]
        print(ggplot(data,aes(x=Area)) + geom_bar(aes(fill=Type))+scale_fill_manual(values=c("Viral tests" = "lightskyblue", "antibody tests" = "lightblue"))+coord_flip())
        dev.off()
      }
    )
    output$down3 <- downloadHandler(
      filename =  function() {
        paste('data',Sys.Date(),'.pdf',sep='')
      },
      content = function(file) {
        pdf(file,width =13 ,height =6)
        x<- switch(input$selectx,                # 
                   "1"=data[,5],
                   "3"=data[,2],
                   "4"=data[,3],
                   "2"=data[,4]
        )
        y<- switch(input$selecty,
                   "1"=data[,5],
                   "3"=data[,2],
                   "4"=data[,3],
                   "2"=data[,4]
        )
        z<- switch(input$selectz,
                   "1"=data[,9],
                   "2"=data[,6]
        )
        print(ggplot(data, aes(x,y)) +geom_point(aes(colour = z),size=5)+theme(axis.text.x = element_text(angle = 90, hjust = 0.5,size = 16),axis.text.y = element_text(size = 16),axis.title.x =element_text(size=16), axis.title.y=element_text(size=16)))
        dev.off()
      }
    )
    output$down4 <- downloadHandler(
      filename =  function() {
        paste('data',Sys.Date(),'.pdf',sep='')
      },
      content = function(file) {
        pdf(file,width =26 ,height =13)
        mpx<-Datemaster()[,10]
        mpy<-Datemaster()[,11]
        mp<-NULL
        mapworld<-borders("world",color = "gray50",fill="white")
        mp<-ggplot()+mapworld+ylim(-60,90)
        mp2<-mp+geom_point(aes(x=mpx,y=mpy,size=Datemaster()[,12]),color="darkorange")+scale_size(range = c(2,4))
        mp3<-mp2+theme(legend.position = "none")
        print(mp3)
        
        dev.off()
      }
    )
  }
# Run the application 
shinyApp(ui = ui, server = server)
