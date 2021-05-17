install.packages('ISLR')

library('ISLR')
library(dplyr)

data('Carseats')

lm1 = lm(formula = Sales ~ Price, data = Carseats)
summary(lm1)

Cars2 = Carseats %>%
  mutate("Bad_Shelf" = ifelse(ShelveLoc == "Bad", 1,0)) %>%
  mutate("Good_Shelf" = ifelse(ShelveLoc == "Good", 1,0))

lm2 = lm(formula = Sales ~ Price + Bad_Shelf + Good_Shelf, data = Cars2)
summary(lm2)

PriceDemand = read.csv("PriceDemand.csv", header = TRUE)

lm3 = lm(formula = Qty~Price, data = PriceDemand)
summary(lm3)

lm4 = lm(formula = Qty~log(Price), data = PriceDemand)
summary(lm4)
#When you do a log of anything, a 1% change equates to a change of .01 * the coefficient. 

lm5 = lm(formula = log(Qty)~Price, data = PriceDemand)
summary(lm5)
#On the other hand, when the log is the dependent, a unit change int he independent will equal the coefficient * 100
#percenetage change of y. it'll change log(y) by exactly b1 units, but that changes to a percenetage change of y. 

lm6 = lm(formula = log(Qty)~log(Price), data = PriceDemand)
summary(lm6)
#This menas 