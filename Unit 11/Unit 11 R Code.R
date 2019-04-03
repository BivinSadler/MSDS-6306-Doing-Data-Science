---
  title: "R Notebook"
output: html_notebook
editor_options: 
  chunk_output_type: inline
---
  
  # Sample R Code for Dealing with the NYT API
  
  ```{r}
library(dplyr)
library(tidyr)
library(plyr)
library(jsonlite)

NYTIMES_KEY = "1dd4756fa8394d538b5db5ecb658cf0b" #Your Key Here … get from NTY API website

# Let's set some parameters
term <- "central+park+jogger" # Need to use + to string together separate words
begin_date <- "19890419"
end_date <- "19890901"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

initialQuery <- jsonlite::fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1)

pages <- list()
for(i in 0:maxPages){
  nytSearch <- jsonlite::fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  message("Retrieving page ", i)
  pages[[i+1]] <- nytSearch 
  Sys.sleep(10) 
}


allNYTSearch <- rbind_pages(pages)


# Visualize coverage by section
allNYTSearch %>% 
  group_by(response.docs.type_of_material) %>%
  dplyr::summarize(count=n()) %>%
  mutate(percent = (count / sum(count))*100) %>%
  ggplot() +
  geom_bar(aes(y=percent, x=response.docs.type_of_material, fill=response.docs.type_of_material), stat = "identity") + coord_flip()


#Make another column of News versus Other ... The labels

allNYTSearch$NewsOrOther = ifelse(allNYTSearch$response.docs.type_of_material == "News","News","Other")


# Visualize coverage of News or Other
allNYTSearch %>% 
  group_by(NewsOrOther) %>%
  dplyr::summarize(count=n()) %>%
  mutate(percent = (count / sum(count))*100) %>%
  ggplot() +
  geom_bar(aes(y=percent, x=NewsOrOther, fill=NewsOrOther), stat = "identity") + coord_flip()




#This function returns P(News | Keyword) 
#P(News|KW) = P(KW|News)* P(News) / P(KW)
Pnews_word = function(key_word = "jogging", trainingSet)
{
  print(key_word)
  NewsGroup = trainingSet[trainingSet$NewsOrOther == "News",]
  OtherGroup = trainingSet[trainingSet$NewsOrOther == "Other",]
  
  pNews = dim(NewsGroup)[1] / (dim(NewsGroup)[1] + dim(OtherGroup)[1])
  pOther = 1 - pNews
  
  pKWGivenNews = length(grep(paste("\\b",key_word,"\\b",sep=""),NewsGroup$response.docs.headline.main,ignore.case = TRUE))/dim(NewsGroup)[1]
  pKWGivenOther = length(grep(paste("\\b",key_word,"\\b",sep=""),OtherGroup$response.docs.headline.main,ignore.case = TRUE))/dim(OtherGroup)[1]
  
  pKW = length(grep(paste("\\b",key_word,"\\b",sep=""),trainingSet$response.docs.headline.main,ignore.case = TRUE))/dim(trainingSet)[1]
  
  pNewsGivenKW = pKWGivenNews*pNews/pKW
  pOtherGivenKW = pKWGivenOther*pOther/pKW
  
  return(pNewsGivenKW)
}

theScoreHolderNews = c()
theScoreHolderOther = c()
articleScoreNews = 0;
articleScoreOther = 0;


for (i in 1 : dim(allNYTSearch)[1])  #This loop iterates over the articles
{
  
  articleScoreNews = 0; 
  articleScoreOther = 0;
  #strsplit(gsub("[^[:alnum:] ]", "", str), " +")
  #strsplit(allNYTSearch$response.docs.headline.main[i],split = " ")
  theText = unlist(strsplit(gsub("[^[:alnum:] ]", "", allNYTSearch$response.docs.headline.main[i]), " +"))
  for(j in 1 : length(theText))  #This loop iterates over the headline (each word)
  {
    articleScoreNews = articleScoreNews + Pnews_word(theText[j],allNYTSearch)
    articleScoreOther = articleScoreOther + (1 - Pnews_word(theText[j],allNYTSearch))
  }
  theScoreHolderNews[i] = articleScoreNews
  theScoreHolderOther[i] = articleScoreOther
}

# Classify the aricle as News or Other based on a given piece of information from the article.
allNYTSearch$Classified = ifelse(theScoreHolderNews > theScoreHolderOther,"News","Other")

#Confusion Matrix
table(allNYTSearch$NewsOrOther,allNYTSearch$Classified)
