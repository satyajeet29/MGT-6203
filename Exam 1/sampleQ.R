library(ISLR)
data = College

#lin reg Personal with Room.board
lm1 = lm(formula = data$Personal ~ data$Room.Board)
summary(lm1) #.039, .038



#Question 1-5:
lm1 = lm(formula = data$Personal ~ data$Room.Board)
summary(lm1) #.03977, .03853

lnlog = lm(formula = data$Personal ~ log(data$Room.Board))
summary(lnlog) #.04037, .0391

loglog = lm(formula = log(data$Personal) ~ log(data$Room.Board))
summary(loglog) #.04489, .04366

logln = lm(formula =  log(data$Personal) ~ data$Room.Board)
summary(logln) #.04304, .04181

#Question 6-10
data = read.csv('binary.csv')

bn = glm(formula = data$ï..admit ~ data$gre + data$gpa, family = "binomial")
summary(bn)

library("ROCR")
prob = predict(bn, newdata = ., type = "response")
problabls = ifelse(prob >= 0.28, 1, 0)
pred = prediction(problabls, data$ï..admit)
auc = performance(pred, measure = 'auc')
auc@y.values

#Question 11-16
library(PerformanceAnalytics)
library(xts)
library(lubridate)

data = read.csv('Berkshire.csv')
data$Date = as.Date(data$Date, "%m/%d/%Y")

d1 <- data %>%
  filter(Date <= '2005-12-31')

table.Stats(d1$BrkRet)

mean(d1$BrkRet-d1$MKT)

Return.cumulative(d1$BrkRet,geometric = TRUE)*10000

(prod(d1$BrkRet+1)-1)*10000

ts=xts(d1[,-1],order.by = d1[,1],)
SharpeRatio(ts$BrkRet,ts$RF)
SharpeRatio(ts$MKT,ts$RF)

#Question 17-18
data = read.csv('Factor_HiTec.csv')

lm1 = lm(data$HiTec_rf ~ data$Mkt_rf + data$SMB + data$HML + data$Mom + data$BAB + data$QMJ)
summary(lm1)

#Question 19-20
data = read.csv('UPS_KO.csv')

d2 <- data %>%
  filter(Date >=201504) %>%
  filter(Date <=201811)

upsl= lm(d2$UPS ~ d2$Mkt_RF + d2$SMB + d2$HML)
kol= lm(d2$KO ~ d2$Mkt_RF + d2$SMB + d2$HML)


library(stargazer)
summary(upsl)
summary(kol)