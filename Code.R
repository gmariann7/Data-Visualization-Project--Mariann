library(tidyverse)
#tidyverse does include ggplot2
library(ggplot2)
#library that was included in sample for density plots 
library(dplyr)
publishersdataset_df <- read_csv("dataset/publishersdataset.csv")

#DataVis1 
#makes a table that gives the mean daily author revenue per publisher type
publishersdataset_df %>% 
  group_by(publisher.type)%>% 
  summarize(
    mean_daily_author_revenue = mean(`daily_average.author_revenue`)
  ) %>%
  
  #creates the barplot from the table with the publisher type as the x axis and y as mean daily author revenue
  ggplot() + 
  geom_bar(
    mapping = aes(x = publisher.type, y= mean_daily_author_revenue),
    stat= "identity", fill = "lightblue", position = position_dodge())+ 
  
  #label the x and y axis and title
  labs(
    x= "publisher type", 
    y = "mean daily author revenue",
    title = "The Mean Daily Author Revenue by Publisher Type"
  ) + 
  
  #rotates the x axis text 90 degrees so that it is vertical
  theme(axis.text.x= element_text(size=10,angle=90, hjust=1)
  )

#saves data into a png file called datavis1
ggsave("datavis1.png", width = 5, height = 5, 
       path ="../plots" )



#DataVis2
#this dataset is filtering out the publisher and author revenue that exceeds 100
publishersdataset_df %>% 
  filter(daily_average.publisher_revenue < 100 & daily_average.author_revenue < 100 )%>%
  ggplot()  + 
  
  #adds the labels on each density plot
  annotate("text", x=45, y= 0.04, label= "publisher", parse= TRUE, size=2.5, color = "blue") + 
  annotate("text", x=22, y= 0.065, label= "author", parse= TRUE, size=2.5, color = "purple") +
  
  #makes a density plot for both publisher revenue and author revenue
  geom_density(aes(x=daily_average.publisher_revenue), fill="lightblue", color = "blue", alpha = 0.8)  + 
  geom_density(aes(x=daily_average.author_revenue),fill="pink", color = "purple", alpha = 0.8)+
  
  #adds the x axis and y axis labels 
  labs(x="revenue", y = "density") +
  
  # adds the table title 
  ggtitle("Distribution of Publisher vs Author Revenue ")  +
  
  #makes a density ploy for each publisher 
  facet_wrap(~sold_by) 

#saves data into a png called datavis2
ggsave("datavis2.png", width = 8, height = 5, 
       path ="../plots" )