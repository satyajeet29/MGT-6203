

#Q1
library(ISLR)
data = Auto
lm1 = lm(data$mpg ~ data$horsepower)
summary(lm1)
#Q2
data = mtcars
data <- data %>%
  mutate(amF = ifelse(am == 1, 1,0)) %>%
  mutate(vsF = ifelse(vs == 1, 1,0)) %>%
  mutate(vshp = vs*hp)

lm1 = lm(data$mpg ~ data$hp)
lm2 = lm(data$mpg ~ data$hp + as.factor(data$am) + as.factor(data$vs))
lm3 = lm(data$mpg ~ data$hp + as.factor(data$am) + as.factor(data$vs) + data$vshp)
summary(lm1)
summary(lm2)
summary(lm3)
#Q3
library(MASS)
?Boston
data = Boston
data <- data %>%
  mutate(Result = ifelse(medv > 30, 1, 0)) %>%
  dplyr::select(-medv)

lgt <- glm(data$Result~., data = data, family = "binomial")
summary(lgt)
data <- data %>%
  mutate(logpred = predict(lgt, newdata = data,type = "response")) %>%
  mutate(Result2 = ifelse(logpred >= .5, 1, 0))
#Q4
TP = data %>%
  filter(Result == 1) %>%
  filter(Result2 == 1) %>%
  summarize(count = n())

TN = data %>%
  filter(Result == 0) %>%
  filter(Result2 == 0) %>%
  summarize(count = n())

FN = data %>%
  filter(Result == 1) %>%
  filter(Result2 == 0) %>%
  summarize(count = n())

FP = data %>%
  filter(Result == 0) %>%
  filter(Result2 == 1) %>%
  summarize(count = n())
#Q5
data <- read.csv("abalone.csv",stringsAsFactors = FALSE)
lm1 = lm(data$Rings ~ data$Diameter + data$Height)
summary(lm1)
#Q6
data1 <- data %>%
  filter(Type != "F") %>%
  mutate(Type = ifelse(Type == "M", 1, 0))
lm2mutate(Type = ifelse(Type == "M", 1, 0)) = lm(data1$Diameter ~ as.factor(data1$Type))
summary(lm2)
data2 <- data %>%
  filter(Type != "M") %>%
  mutate(Type = ifelse(Type == "F", 1, 0))
lm3 = lm(data2$Diameter ~ as.factor(data2$Type))
summary(lm3)
#Q7
data <- read.csv("Admissions.csv")
lgt = glm(data$Admitted~., data = data, family = "binomial")
data <- data %>%
  mutate(logprd = predict(lgt, newdata = ., type = "response"))%>%
  mutate(Admitted2 = ifelse(logprd > .75,1,0))
sum(data$Admitted == data$Admitted2)/NROW(data)
#Q8
if (!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics") 
library(PerformanceAnalytics) 
data = managers
table.Stats(data)
#Q9
MarketExcess = data$`SP500 TR`- data$`US 10Y TR`
HAM1Exc = data$HAM1 - data$`US 10Y TR`
HAM2Exc = data$HAM2 - data$`US 10Y TR`
HAM3Exc = data$HAM3 - data$`US 10Y TR`
HAM4Exc = data$HAM4 - data$`US 10Y TR`

alph1 = lm(HAM1Exc ~ MarketExcess)
alph2 = lm(HAM2Exc ~ MarketExcess)
alph3 = lm(HAM3Exc ~ MarketExcess)
alph4 = lm(HAM4Exc ~ MarketExcess)

summary(alph1)#.004
summary(alph2)#.007
summary(alph3)#.005
summary(alph4)#.003
#Q10
Return.cumulative(data$HAM1, geometric = TRUE)
(prod(data$HAM1+1))*8000
#Q11
TreynorRatio(data$HAM4, data$`SP500 TR`, data$`US 10Y TR`) #.075
TreynorRatio(data$HAM3, data$`SP500 TR`, data$`US 10Y TR`) #.14
#Q12 + Q13
data <- read.csv("Final_Exam_Factors.csv")
data <- data %>%
  mutate(NV_rf = NVDA - RF) %>%
  mutate(INT_rf = INTC - RF)

fac1 = lm(NV_rf ~ MKT_RF+SMB+HML+QMJ+BAB+MOM, data = data)
fac2 = lm(INT_rf ~ MKT_RF+SMB+HML+QMJ+BAB+MOM, data = data)
summary(fac1)
summary(fac2)
#Q14
data <- read.csv("KAG_wrangled_dataset.csv",stringsAsFactors = FALSE)

data %>% 
  filter(Impressions > 10000)%>%
  group_by(gender) %>%
  summarize(counts = n())
#Q15
data %>%
  filter(xyz_campaign_id == 916) %>%
  mutate(ConvRate = ifelse(Total_Conversion == 0, 0, Approved_Conversion/Total_Conversion)) %>%
  summarize(avg = mean(ConvRate))
#Q16
data %>%
  mutate(CPC = ifelse(Clicks == 0, 0, Spent/Clicks)) %>%
  group_by(gender, age) %>%
  summarize(avg = mean(CPC))

data %>%
  group_by(gender, age) %>%
  summarize(totClick = sum(Clicks),
            totSpent = sum(Spent)) %>%
  mutate(CPC = totSpent/totClick)

#Q17+Q18 in xlsx scratch

#19
data <- read.csv("sample_data.csv",stringsAsFactors = FALSE)
data <- data %>%
  dplyr::select(-sample)
data %>%
  mutate(xbar = rowMeans(data))
rowMeans(data)

#Q20
library(lubridate)
library(fpp2)
data <-read.csv("Store_Demand_Final.csv")
data$Date <- mdy(data$Date)
class(data$Date)
data <- xts(data[-1],order.by = data$Date)
ts1 <- ses(data, alpha = .25, h = 5)
#Q21
accuracy(ts1)
#Q22
plot(ts1)