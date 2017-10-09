#cleaning the environment
rm(list=ls())

#set working directory 
setwd("D:/data science/EDWISOR PROJECT 2 - TEXT MINING/Text Classification - HealthCare")

#check working directory
getwd()

#loading the libraries
library(stringr)
library(tm)
library(wordcloud)
library(slam)
# library(sentiment)
library(ggplot2)
library(dplyr)

#loading the data
text_data = read.csv("data.csv",na.strings = c(" ", "", "NA","N/A","null"))


#-------------------------  EXPLORATORY DATA ANALYSIS -----------------------------------# 
#get dimension of the data
dim(text_data)
#structure of the data
str(text_data)
#first five entries in the data
head(text_data)


#-----------------------------------------------------------------------------------#

#calculate missing value
# sum(is.na(text_data))
# 
# missing_values <- text_data %>% summarize_each(funs(sum(is.na(.))/n()))

#--------------------     UNIVARIATE ANALYSIS -----------------------#

# 1. Categories
class(text_data$categories)   #is factor

table(text_data$categories) 
#now noise will be removed

text_data$categories[ text_data$categories %in% c("asK_A_DOCTOR")] = "ASK_A_DOCTOR"

text_data$categories[ text_data$categories %in% c("JUNK","mISCELLANEOUS")] = "MISCELLANEOUS"

text_data$categories = as.factor(as.character(text_data$categories))

text_data %>%
  ggplot(aes(x=categories)) +
  geom_bar(fill="Red")+
  ylab("Count")

#------------------- sub categories ------------------------------#
table(text_data$sub_categories)
length(unique(table(text_data$sub_categories)))

text_data$sub_categories[ text_data$sub_categories %in% c("mEDICATION RELATED")] = "MEDICATION RELATED"

text_data$sub_categories[ text_data$sub_categories %in% c("JUNK")] = "OTHERS"
text_data$sub_categories = as.factor(as.character(text_data$sub_categories))

freq =as.data.frame( table(text_data$sub_categories))

# barplot(table(text_data$sub_categories),horiz = T)

ggplot(data=freq, aes(x=Var1,y=Freq,fill = "red")) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Distribution of Subcategories")

#----------------------------------   Previous Appointement       --------------------------#

table(text_data$previous_appointment)

text_data$previous_appointment[ text_data$previous_appointment %in% c( "NO")] = "No"
text_data$previous_appointment[ text_data$previous_appointment %in% c("yes","YES")] = "Yes"

text_data$previous_appointment[is.na(text_data$previous_appointment)] = "No"
text_data$previous_appointment = as.factor(as.character(text_data$previous_appointment))



text_data %>%
  ggplot(aes(x=previous_appointment)) +
  geom_bar(fill="Blue")+
  ylab("Count")


write.csv(text_data,"pp_data.csv")
