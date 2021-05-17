library(dplyr)

data <- read.csv("KAG_conversion_data_wrangled.csv",stringsAsFactors = FALSE)

data %>%
  filter(Spent == 0)%>%
  group_by(campaign_id) %>%
  summarize(count = n())

data %>%
  filter(Spent == 0)%>%
  arrange(desc(Impressions)) %>%
  select(ad_id, Impressions)