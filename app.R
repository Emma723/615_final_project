#

library(shiny)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rsconnect)
library(magrittr)
library(leaflet)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(tidyverse)
library(benford.analysis)

rsconnect::setAccountInfo(name='zhang-qixuan',
                          token='153B21B9F815EFDCCD137AF182E8541C',
                          secret='WFp3VGCdVk6oalz0vYODLzIMfPm9yr2XsetIJ5qi')

Bos_listing<-read.csv("Boston_listings.csv")
Boston_review<-read.csv("Boston_review.csv",stringsAsFactors = FALSE)


###Cleaning Dataset
Boston_listing1<-Bos_listing[,-c(2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30,31,32,33,35,36,37,38,39,41,42,43,44,45,46,47,48,51,58,59,60,62,63,64,66,67,68,69,70,72,73,74,75,76,78,79,86,87,88,89,90,91,92,93,94,95,96)]
#Delete NA of Boston_listing dataset,filter the price and number of reviews
Boston_listing11<-na.omit(Boston_listing1,cols="review_scores_rating")
Boston_listing111<-na.omit(Boston_listing11,cols="price")
Boston_listing1111<-na.omit(Boston_listing111,cols="review_scores_accuracy")
Boston_listing11111<-Boston_listing111 %>% filter(number_of_reviews>=3)
Boston_listing<-Boston_listing11111 %>% filter(price<=1000)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Airbnb in Boston "),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Airbnb Customers' Reviews in Boston",tabName = "about",icon=icon("info")),
      menuItem("Review Scores Rating Distribution", tabName = "review", icon = icon("angle-right")),
      menuItem("Geographic", tabName = "geo", icon = icon("line-chart")),
      menuItem("Exploratory Data Analysis",tabName = "EDA",icon = icon("th")),
      menuItem("Sentiment Analysis",tabName = "Sen",icon=icon("th")),
      menuItem("Benford Analysis",tabName = "ben",icon=icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # Boxes need to be put in a row (or column)
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About the Project", 
                  width = 20, 
                  height = 100,
                  solidHeader = TRUE, 
                  status = "primary",
                  collapsible  = TRUE,
                  h3("Abstract"),
                  print("This study investigates key factors that influence Airbnb customer experiences by analyzing big dataset of online review comments through the process of text mining and sentiment analysis.
                        Based on the key attributes, we will give some suggestions to future Airbnb hosts.
                        Compared with traditional study, the innovation of this study is using AFINN dataset to calculate sentiments scores of Airbnb customers' comments to study customers' opinions about Airbnb services. 
                        From our analysis, we found price is not the crucial factor influence customers' experience, 
                        and customers' overall review scores are based on location, accuracy, communication, room types and cleanliness. 
                        Methodologically, this study contributes to improve the satisfaction of customers and illustrate how big data can be used and visually interpreted in marketing studies.")
                  ))
              ),
              
      tabItem(tabName = "review",
              fluidRow(
                tabBox(width = NULL,
                       tabPanel( 
                                title="Review Distribution",
print("Initially,we made a histogram plot to describe reviews of Boston."),
                                plotOutput("plot1", height ="500px")
                       ))
              )
      ),
      tabItem(tabName = "geo",
              fluidRow(
                tabBox(width = NULL,
                       tabPanel(
                                title="Graphic",
print("To give our reader a more observable visualization, we made a leaflet to describe the overall review scores distribution in different area in Boston at first.With stronger and stronger of the green color, the overall review scores are higher."),
                                leafletOutput("plot2", height = "500px" )
                       ))
              )
      ),
      tabItem(tabName = "EDA",
              fluidRow(width = 20,
                       tabBox(width = NULL, 
                              tabPanel("Review Scores of Different Neibourhood",
print("Initially, we want to figure out overall review scores distribution of different neighborhoods. We made a bar plot to illustrate counts of different location related to their customers reviews scores for location,in we could find the best location of Airbnb apartments is in Allston. Because more than 2000 apartments located in Allston was given 10 scores by customers."),
                                       plotOutput("plot3",height="500px"), 
                                       side = "right", collapsible = TRUE,
                                       solidHeader = TRUE),
                              tabPanel("Review Scores of Different Property Type",
print("Based on the inspiration of neighborhood related to customers' reviews scores, we analyzed plots of property type related to customers' reviews."),
                                       plotOutput("plot3_1",height="500px"),
                                       side = "right", collapsible = TRUE,
                                       solidHeader = TRUE),
                              tabPanel("Evaluation of Different Room Type in Different Neighbourhood",
print("Furthermore, we want to analyze different room type within different neibourhood. Therefore, we made a plot displays customers' reviews for different room type in different neibourhood."),
                                       plotOutput("plot3_2",height = "500px"),
                                       side="right",collarpsible=TRUE,
                                       solidHeader=TRUE),
                              
                              tabPanel("Reviews Scores of Different Room Type",
print("We listed reviews scores of different room type as following."),
                                       plotOutput("plot4",height = "500px"),
                                       side="right",collapsible=TRUE,
                                       solidHeader=TRUE))
              )
              
      ),
      tabItem(tabName = "Sen",
              fluidRow(width=20,
                       tabBox(width = NULL,
                              tabPanel("High Review Scores Comments",
print("In the sentiment analysis section, we combined the high review scores with their comments to analyze the latent information in comments.In the following plot, we list TOP 30 frequency words of High Review Scores"),
                                       plotOutput("plot5",height = "500px"),
                                       side="right",collapsibe=TRUE,
                                       solidHeader=TRUE),
                              tabPanel("Low Review Scores Comments",
print("Additionally, I list low review scores comments as following."),
                                       plotOutput("plot6",height = "500px"),
                                       side="right",collapsible=TRUE,
                                       solidHeader=TRUE),
                              tabPanel("High Review Scores Wordcloud",
print("To make sentiment analysis more clearly, we list worldcloud for low review scores and high review scores as following."),
                                       plotOutput("plot7",height = "500px"),
                                       side="right",collapsible=TRUE,
                                       solidHeader=TRUE),
                              tabPanel("Low Reivew Scores Wordcloud",
                                       plotOutput("plot8",height="500px"),
                                       side="right",collapsible=TRUE,
                                       solidHeader=TRUE),
                              tabPanel("Distribution of Sentiment Score Of Comments For Different Room Type",
print("More specifically, we made a plot to display the distribution of sentiment scores of comments for different room type."),
                                       plotOutput("plot9",height="500px"),
                                       side="right",collapsible=TRUE,
                                       solidHeader=TRUE))
                       
              )
              
      ),
      
      tabItem(tabName = "ben",
              fluidRow(width=20,
                       tabBox(width = NULL,
                              tabPanel("Benford Analysis for Review Scores",
print("We applied Benford analysis to our dataset to confirm the accuracy of our dataset, unfortunately, the customers' reviews scores didn't following benford law. I illustrate these folloing cofirmation process in the report."),
                                       plotOutput("plot10",height="500px"),
                                       side="right",collapsibe=TRUE,
                                       solidHeader=TRUE),
                              tabPanel("Benford Analysis for Price",
                                       plotOutput("plot11",heigh="500px"),
                                       side="right",collapside=TRUE,
                                       solidHeader=TRUE))
                       
              ))       
      )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot1 <- renderPlot({
    b1<-Boston_listing %>% group_by(review_scores_rating)%>%
      summarise(count=n())
    ggplot(b1,aes(x=reorder(review_scores_rating,-count),y=count))+
      geom_histogram(binwidth = 0.09,stat="identity",fill="light green")+
      labs(title="Revies Rate Distribution of Boston",x="Review_Rate",y="Counts")+
      theme_minimal() +   
      theme(axis.title.x = element_text(face="bold",  size=12), 
            axis.title.y = element_text(face="bold",  size=12),
            plot.title = element_text(size=14, face="bold"),  
            axis.text.x  = element_text(vjust=0.5, size=10)) +
      theme(plot.title = element_text(hjust = 0.5)) 
  })
  output$plot2 <- renderLeaflet({
    pal <- colorQuantile(
      palette = "BuGn",
      domain = Boston_listing$review_scores_rating
    )
    leaflet(Boston_listing) %>% addTiles() %>%
      addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                 popup = ~price, radius = 50, 
                 color = ~pal(review_scores_rating), fillOpacity = 1)
    
  })
  output$plot3<-renderPlot({
    room_type<-Boston_listing %>% filter(review_scores_rating>=69)%>% 
      group_by(review_scores_rating,room_type)%>%
      summarise(count=n())
    pro_type<-Boston_listing %>% filter(review_scores_rating>=70)%>% 
      group_by(review_scores_rating,property_type)%>%
      summarise(count=n())
    n<-Boston_listing%>% filter(review_scores_location>=7)%>%
      group_by(neighbourhood_cleansed,review_scores_location)%>%
      summarise(count=n())
    ggplot(n,aes(x=review_scores_location,y=count,fill=neighbourhood_cleansed))+geom_bar(width=0.9,stat="identity")+
      labs(title = "Review Scores Location Of Different Neighbourhood",x="Review Scores Location",y="count")+
      theme_minimal()
  })
  output$plot3_1<-renderPlot({
    pro_type<-Boston_listing %>% filter(review_scores_rating>=70)%>% 
      group_by(review_scores_rating,property_type)%>%
      summarise(count=n())
    ggplot(pro_type,aes(x=review_scores_rating,y=count,fill=property_type))+
      geom_bar(width=0.9,stat="identity")+
      labs(title="Review Scores Rating of Different Property Type",x="Review Scores Rating",y="count")+theme_minimal()
    
  })
  output$plot3_2<- renderPlot({
    ggplot(Boston_listing,aes(x=neighbourhood_cleansed,y=review_scores_rating,fill=room_type))+geom_bar(width = 0.9,stat="identity")+theme(axis.text.x = element_text(angle=45,vjust = 0.75,size = 10))+
      labs(title = "Evaluation of Different Room Type in Different Neighbourhood")
  })
  output$plot4<-renderPlot({
    ggplot(Boston_listing,aes(x=room_type,y=review_scores_rating,fill=room_type))+
      geom_bar(width = 0.9,stat="identity")+
      labs(title = "Reviews socres of Different Room Type")
  })
  output$plot5<-renderPlot({
  review_words_high <- read.csv("review_words_high.csv")
      ggplot(review_words_high)+geom_bar(mapping = aes(x=reorder(word, count), y=count),
                        stat="identity", fill = "#FFCCFF") +
      coord_flip() +
      labs(title="Comments Of High Rating",
           y="Word count", x="Words") +
      theme_minimal()
  })

  output$plot6<-renderPlot({
    review_words_low <- read.csv("review_words_low.csv")
      ggplot(review_words_low) +
      geom_bar(mapping = aes(x=reorder(word, count), y=count),
               stat="identity", fill = "light green") +
      coord_flip() +
      labs(title="Comments Of Low Rating",y="Word count", x="Words") +
      theme_minimal()
  })
  
  output$plot7<-renderPlot({
    cloud_high <- read.csv("cloud_high.csv")
    wordcloud(words = cloud_high$word, freq = cloud_high$no_rows, min.freq =3,
              max.words=120, random.order=FALSE, random.color=FALSE, rot.per=0.33, 
              colors=brewer.pal(1, "Dark2"))
  })
  
  output$plot8<-renderPlot({
    cloud_low <- read.csv("cloud_low.csv")
    wordcloud(words = cloud_low$word, freq = cloud_low$no_rows, min.freq =3,
              max.words=120, random.order=FALSE, random.color=FALSE, rot.per=0.33, 
              colors=brewer.pal(1, "Dark2"))
   
  })
  
  output$plot9<-renderPlot({
    reviews_sentiment <- read.csv("reviews_sentiment.csv")
    
    ggplot(reviews_sentiment, aes(x=sentiment))+
      geom_histogram(binwidth = 0.09, aes(fill = room_type))+
      labs(title="Distribution of Sentiment Score Of Comments For Different Room Type",
           x="Mean AFFIN Score", y="Count") +
      theme_minimal()
  })
  output$plot10 <-renderPlot({
    ben_review <- benford(Boston_listing$review_scores_rating, number.of.digits = 2)
    plot(ben_review)
  })
  output$plot11<-renderPlot({
    ben_price<-benford(Boston_listing$price, number.of.digits = 2)
    plot(ben_price)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

