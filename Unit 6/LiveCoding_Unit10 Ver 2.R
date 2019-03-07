#requires packages "dplyr" and "ggplot2"
#install.package("dplyr")
#install.package("ggplot2"")

library(dplyr)
library(ggplot2)
library(tidyr)

#UNIT 6 Live Session Code! Including Breakouts!

#Sample?
#Population?

#load first day dataset
fileLocation <- "http://stat.columbia.edu/~rachel/datasets/nyt1.csv"
nytData <- read.csv(url(fileLocation))
head(nytData)

#stratified Age groups
str(nytData)
nytData$ageGroup <- cut(nytData$Age, c(-Inf, 18, 24, 34, 44, 54, 64, Inf))
levels(nytData$ageGroup) <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")

#Transform gender to a factor variable
nytData$Gender = factor(nytData$Gender, levels=c(1,0), labels = c("male", "female"))
head(nytData)

#plot ageGroup and impressions
ggplot(data=nytData, aes(x=ageGroup, y=Impressions, fill=ageGroup)) + geom_bar(stat="identity") + theme_bw()

#plot ageGroup and impressions
nytData %>% filter(Impressions>0 & Signed_In == 1) %>% ggplot(aes(x=ageGroup, y=Impressions, fill=ageGroup)) + geom_bar(stat="identity") + theme_bw()

#(CTR = clicks/impressions) ; no CTR if there are no impressions
#conclusion <18 and 65+ have highest CTR

nytData %>% filter(Impressions>0 & Signed_In == 1) %>% 
  group_by(ageGroup) %>% 
  summarise(Impressions = sum(Impressions), Clicks = sum(Clicks)) %>% 
  ggplot(aes(x=ageGroup, y=Clicks/Impressions, fill=ageGroup)) + geom_bar(stat="identity") + theme_bw()
