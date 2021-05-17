#Import libraries
#install.packages('corrgram')
#install.packages('corrplot')
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ISLR)
library(corrplot)
library(corrgram)
library(car)

##Import data

dataset<-Carseats
dataset2<-Carseats

##Exploratory Data Analysis
str(Carseats)
summary(Carseats)


###CORR
#Option 1 use dyplr
Carseats_corr<-select(Carseats, Sales, CompPrice, Income, Advertising, Population, Price, Age, Education)
corrgram(Carseats_corr,order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt)

#Option 2 filter dataframe
# Get numeric columns
Carseats.cols <- sapply(Carseats, is.numeric)
# Filter For correlation
Carseats_corr2 <- cor(Carseats[,Carseats.cols])
corrgram(Carseats_corr2,order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt)


##Model

#option 1 use factor varialbes
regression1 = lm(Sales~., Carseats)
summary(regression1)
plot(regression1)

#option 2 create our own variables
#Manual encode variables with dplyr
dataset<- dataset %>%
  mutate(Bad_Shelve = ifelse(ShelveLoc=="Bad",1,0)) %>%
  mutate(Medium_Shelve = ifelse(ShelveLoc=="Medium",1,0)) %>%
  mutate(Urban = ifelse(Urban=='Yes', 1, 0))  %>%
  mutate(US = ifelse(US=='Yes', 1, 0))

dataset<-subset(dataset, select=-c(ShelveLoc))

regression2 = lm(Sales~., dataset)
summary(regression2)
vif(regression2)



##Interaction Terms
regression4 = lm(Sales~Price + Bad_Shelve + Medium_Shelve , dataset)
summary(regression4)

dataset3<- dataset %>%
  mutate(Bad_price = Price*Bad_Shelve) %>%
  mutate(Medium_price = Price*Medium_Shelve)

regression5 = lm(Sales~Price + Bad_Shelve + Medium_Shelve + Bad_price + Medium_price, dataset3)
summary(regression5)


