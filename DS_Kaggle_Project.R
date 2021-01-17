kaggle<-read.csv(file= "C:/Users/sofia/Desktop/kaggle-survey-2020/kaggle_survey_2020_responses_bearb.csv", header=T)
View(kaggle)

names(kaggle)

# load packages
library(dplyr)
library(ggplot2)

table(kaggle$Program_language_Python)
class(kaggle$Age)
kaggle$Age<-as.numeric(kaggle$Age)
mean(kaggle$Age, na.rm=T)
table(kaggle$Age)

kaggle$Age_group<-recode_factor(kaggle$Age, 
                          "18-21" = "1",
                          "22-24" = "2",
                          "25-29" = "3",
                          "30-34" = "4",
                          "35-39" = "5",
                          "40-44"  = "6",
                          "45-49" = "7",
                          "50-54" = "8",
                          "55-59" = "9",
                          "60-69" = "10",
                          "70+" = "11")
kaggle$Age_group <- as.numeric(kaggle$Age_group)                           

kaggle$Education<-ordered(kaggle$Education, levels = c("No formal education past high school", 
                                     "Some college/university study without earning a bachelor's degree", 
                                     "Professional degree",
                                     "Bachelor's degree",
                                     "Master's degree",
                                     "Doctoral degree",
                                     "I prefer not to answer"))

kaggle$Gender_short <- 
ex<-kaggle %>% group_by(Gender)                          
barplot(table(kaggle$Gender))
ggplot(kaggle, aes(Gender,
                   ))

ex <- kaggle%>% group_by(kaggle$Gender) %>% summarise(no_rows = length(kaggle$Education))

Gender_short<-subset(kaggle,Gender=="Man" | Gender=="Woman") %>% select(Education)

ggplot(Gender_short, aes(x=Education)) + 
      geom_bar(aes(fill = Education)) +
  facet_wrap(~ Gender)+ 
  theme_classic()+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank())
       
       
myGraph <- ggplot(kaggle, aes(x= Gender, ..count..))
myGraph + geom_bar(aes(fill=kaggle$Education))+ 
  labs(title = "Frequency of Genders",
       x = "Gender",
       y = "Frequency") +
  theme_classic() + 
  facet_wrap(~ Gender)
