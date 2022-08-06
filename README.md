# Programming-and-Text-Analytics-with-R
This course work includes dataset of “ted_talks” including speakers with their given talks on different times. By presenting and comparing word frequency and sentiment analysis, it was noticed in both ted1 and ted2 dataset that they have almost similar sentiments. In sentiment analysis, all three lexicons which are used presented the results depending on both ted1 and ted2 dataset. Bing lexicon divided the positive and negative sentiments after that afinn lexicon stated the score between positive and negative sentiments and nrc provided several sentiments.


---
title: "MA331_coursework_Project"
subtitle: "Text analytics of the TED talks by Sherwind Nuland and Ben Saunders"
author: "2111168_Ahmad_Cheema"
date: "18/11/2022"
output: html_document
---

## Introduction:

This course work includes dataset of “ted_talks” including speakers with their given talks on different times, every student is assigned of two speakers and our task is to present and compare the word frequency and sentiment analysis of transcripts of both ted speakers. The talks of ted speakers are purified from data set “ted_talks” and in this course work my allocated two speakers are “Sherwin Nuland” and “Ben Saunders”. Ted is a platform or a hub where we can find information of the Teds where world leading thinkers are asked to give their talks of their life experiences. We have two ted talks of each speaker, “Sherwin Nuland” topics are “The extraordinary power of ordinary people” given in February 2003 and “How electroshock therapy changes me” given in February 2001. Likewise, “Ben Saunders” also has two topics which are “Why did I ski to the North Pole” given in February 2005 and “To the south pole and back-the hardest 105 days of my life” which was given in March 2014. 

Word frequency is done by using tidy data theory which is used to handle data easier and more effective, and it includes packages such as dplyr, ggplot, tidy verse and tidytext. Tidy text package does not allow us to keep text data in tidy form during an analysis. In Sentiment analysis, there are so many methods exist for estimating the emotion in text. We are using three lexicons which are briefly explained in methods, these three lexicons are based on single words and contain many English language words assigned to positive and negative sentiments possibly like fear, anger, surprise and anticipation. We will see how far these sentiments are evident in the dataset of two talks of both speakers. The objective of this study is to examine whether the assumptions mentioned above are true by comparing the two-ted speaker’s dataset.

Let’s find out if both texts express similar or different sentiments.

## Methods: 

As we are asked to perform word frequency and sentiment analysis, at first step we are going to load required packages in R studio of dsEssex and tidyverse which are here used to load dataset ted_talks and then we will filter our two ted speakers and then load packages and libraries of dplyr, textdata, ggplot2, stringr, tidyverse and tidytext.  We will perform analysis on both ted talks of speakers and then present and compare outputs of frequency of words and sentiment analysis. We are naming here “ted1” as “Sherwin Nuland” and “ted2” as “Ben Saunders” for the better compatibility in results. Then using stop words lexicon which is a function used to return vector of stop words in a language and here to filter stop words using snowball lexicon among tokens in ted1 & ted2 dataset to visualise frequent occurring words. We used slice max function on both ted1 and ted2 speaker’s dataset to select the top rows ordered by n. 
In string check, Using mutates here to override the existing columns or bars in both ted1 & ted2 speakers’ dataset. After getting summary of both speakers, we will head straight to ggplot2 function because we are using tidyverse tools here. Both ted1 and ted2 datasets are prepared for sentiment analysis by applying these all above steps. 



```{r setup, include=FALSE, echo=FALSE}
knitr::opts_chunk$set(echo = FALSE)
###install.packages "ggrepel"###
###load the required packages###

library(dsEssex)
require(dsEssex)
load("C:/Users/cahma/Downloads/ted_talks_1.rda")

###load the 'ted_talks' data###
data(ted_talks)

```



```{r include=FALSE}
knitr::opts_chunk$set(echo = FALSE)

###library(scales)###
###library(plotly)###
###library(ggrepel)###
###library(here)###
###library(gradethis)###
library(dplyr)
##package for string processing###
library(stringr)
##package for lexicons###
library(textdata)
##package for visualization###
library(ggplot2)

library(tidyverse)
library(tidytext)

###filter speakers###
MyData <- ted_talks %>%
  filter(speaker %in% c("Sherwin Nuland", "Ben Saunders"))
MyData
```


```{r include=FALSE, results='hide'}
###remove stopwords###
library(stopwords)

tidy_talks <- MyData %>% unnest_tokens (word, text)
stopwords()
stopwords_getlanguages(source = "snowball")
tidy_talks <- tidy_talks %>% anti_join(get_stopwords() )
```                         
```{r echo=FALSE,results='hide'}
###filter out data for two speakers###
tidy_talks1<- tidy_talks %>% filter(speaker %in% "Sherwin Nuland")
tidy_talks2<- tidy_talks %>% filter(speaker %in% "Ben Saunders")

###string check###
tidy_talks1 <- tidy_talks1 %>%
  mutate(word = str_trim(word))%>%
  filter(!str_detect(word, pattern="\\d"))%>%
  mutate(word = str_replace_all(word, pattern="_", replacement = ""))
tidy_talks1 <- tidy_talks1 %>%
  mutate(word = str_trim(word))%>%
  filter(!str_detect(word, pattern="\\d"))%>%
  mutate(word = str_replace_all(word, pattern="_", replacement = ""))
```


```{r echo=FALSE,results='hide'}
###count words for Sherwind Nuland###
tidy_talk1_count<-tidy_talks1 %>% count(word, sort = TRUE) %>%

slice_max(n, n = 20)  
###select the top 20 rows ordered by n###
```

```{r echo=FALSE,results='hide'}
###count words for Ben Saunders###
tidy_talk2_count<-tidy_talks2 %>% count(word, sort = TRUE) %>%
slice_max(n, n = 20)  
###select the top 20 rows ordered by n###

```
 


```{r echo=FALSE,results='hide', fig.keep='all',figures-side, fig.show="hold", out.width=450}
library(ggplot2)
ted1 <- head(tidy_talk1_count,n=10)
###ted1 <- head(tidy_talk1_count,n=10)

###plotting graph for ted1###
###ggplot(head(tidy_talk1_count,n=10), aes(n,word))+
  ggplot(ted1, aes(x=word, y=n)) +
  geom_bar(stat="identity", color="black", fill="brown", width=0.60)+
    labs(caption ="Fig 1 most frequent words in Ted1", x = "Word", y = "Frequency")+
  theme_minimal(base_size=12)+
  theme(plot.caption = element_text(hjust=0.5, size=rel(1)))
  theme_minimal(base_size = 12)
 # geom_col() +
  #labs(y = NULL)
  
  ted2 <- head(tidy_talk2_count,n=10)
###ted2 <- head(tidy_talk2_count,n=10)###

##plotting graph for ted2###
###ggplot(head(tidy_talk2_count,n=10), aes(n,word))+
  ggplot(ted2, aes(x=word, y=n)) +
  geom_bar(stat="identity", color="black", fill="yellow", width=0.60)+
    labs(caption ="Fig 2 most frequent words in Ted2", x = "Word", y = "Frequency")+
  theme_minimal(base_size=12)+
  theme(plot.caption = element_text(hjust=0.5, size=rel(1)))
  theme_minimal()
 # geom_col() +
  #labs(y = NULL)

```

By visualising, we got 10 most frequent word of ted1 and ted2 as we can see in Fig 1 and Fig 2.  We are analysing number of words in both ted1 and ted2 datasets. We are naming here ted1 as Sherwin Nuland and ted2 as Ben Saunders for the better compatibility in results. Ted1 has total of 1128 words and ted2 has 1448 words. 
Fig 1 and Fig 2 shows the 10 most frequent words in ted1 and ted2 datasets. The most frequent words in ted1 dataset are can, course, every, got, know, one, people, said, time and well. The frequency of the word one is notably high compared to other words in the data, which is 28 times. Likewise, in the ted2 dataset frequent words are back, one, pole, ice, got, just, see, expedition, last and can. The higher frequency word also in this dataset is one which is 40 times comparing to other word in ted2 dataset. These frequent words mention to main events on which talks are based on both speakers.














## Sentiment Analysis
Now performing sentiment analysis on both ted1 and ted2 speaker’s dataset using some different lexicons which are loaded within the packages are bing, afinn and nrc. Initially downloading the bing which is used to divide words into positive and negative sentiments. Then afinn lexicon which used to assign the score between -5 and 5, like -5 negative and 5 positive sentiments. Eventually to provide several sentiments like to extract sentiments in words we will download and use third lexicon nrc. 



```{r echo=FALSE,results='hide'}
ted_ted1 <- MyData %>%
  filter(speaker %in% c("Sherwin Nuland"))

ted_ted2 <- MyData %>%
  filter(speaker %in% c("Ben Saunders"))

###extracting tokens from text in both the speakers
###extract vector of words from texts and convert into lower case
###ted2###
ted_ted2_token_df<-ted_ted2 %>%
  unnest_tokens(word, text, token = "words", to_lower = TRUE)

###ted1###
ted_ted1_token_df<-ted_ted1 %>% 
  unnest_tokens(word, text, token = "words", to_lower = TRUE)

###ted2###
ted_ted2_token_df<- ted_ted2_token_df %>%
  mutate(word = str_trim(word))%>%
  filter(!str_detect(word, pattern="\\d"))%>%
  mutate(word = str_replace_all(word, pattern="_", replacement = ""))

###ted1###
ted_ted1_token_df <- ted_ted1_token_df %>%
  mutate(word = str_trim(word))%>%
  filter(!str_detect(word, pattern="\\d"))%>%
  mutate(word = str_replace_all(word, pattern="_", replacement = ""))

###Word count of all the words and arranging in descending order###
###ted2###
ted_ted2_token_df <- ted_ted2_token_df %>% 
  count(word) %>%
  arrange(desc(n))

###ted1###
ted_ted1_token_df<-ted_ted1_token_df %>% 
  count(word) %>%
  arrange(desc(n))
count(ted_ted2_token_df)
count(ted_ted1_token_df)
```




```{r,echo=FALSE}
###Sentiment analysis###
###downloading lexicons required### 
##downloading bing###   
###classifying words into positive and negative###
bing <- get_sentiments("bing")

###downloading afinn lexicon### 
###give positive and negative scores for words###
afinn <- get_sentiments("afinn") %>%
  select(word, value)

##downloading nrc lexicon### 
###extract more sentiments in words###
nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)
```


```{r, echo=FALSE, results='hide'}
###visualizing sentiments###
###get the positive and negative sentiments in texts and merge the 2 objects using inner join###

###ted1###
ted1_bing<- ted_ted1_token_df %>% 
  inner_join(bing, by = "word") 

###head of the dataframe###
head(ted1_bing)

###ted2###
###using inner join to merge the 2 objects###
ted2_bing<- ted_ted2_token_df %>% 
  inner_join(bing, by = "word") 

###head of the dataframe###
head(ted2_bing)
```


```{r, echo=FALSE}
###plotting the top 10 negative and positive words in ted1 data###
###the 10 most frequently occurring words with positive sentiment### 
ted1_positive<-ted1_bing %>% filter(sentiment=="positive") %>% head(n=10)
###the 10 most frequently occurring words with negative sentiment###
ted1_negative<-ted1_bing%>% filter(sentiment=="negative") %>% head(n=10)
###binding both dataframes together into one###
ted1_pos_neg<-bind_rows(ted1_positive,ted1_negative)
###Plott###
ted1_pos_neg %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +    
###reorder the columns in plot to descending order of n and fill different colours based on different sentiments### 
  geom_col(show.legend = FALSE) +          
  ###type of plot###
  facet_wrap(~sentiment, scales = "free_y") +    ###plot 2 plots in a row###
  labs(x = "Word",y = "Frequency")+      #label x and y axis
  coord_flip()+          
  ###flip the coordinates###
  labs(caption ="Fig 3 positive and negative sentiments in ted1", x = "Word", y = "Frequency")+
  theme_minimal(base_size=12)+
  theme(plot.caption = element_text(hjust=1, size=rel(1)))
```


```{r, echo=FALSE}
###plotting the top 10 negative and positive words in ted2 data###
###10 most frequently occurring words with positive sentiment###
ted2_positive<-ted2_bing %>% filter(sentiment=="positive") %>% head(n=10)
###10 most frequently occurring words with negative sentiment###
ted2_negative<-ted2_bing %>% filter(sentiment=="negative") %>% head(n=10)
###binding both the dataframes together into one###
ted2_pos_neg<-bind_rows(ted2_positive,ted2_negative)
###Plot###
ted2_pos_neg %>%
  ggplot(aes(x = reorder(word, n),y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Word",y = "Contribution to sentiment")+
  coord_flip()+
  labs(caption ="Fig 4 positive and negative senitments in ted2", x = "Word", y = "Frequency")+
  theme_minimal(base_size=12)+
  theme(plot.caption = element_text(hjust=1, size=rel(1)))
```



Figs 3 and 4 show 10 most frequent words with negative and positive sentiments in ted1 and ted2 datasets of speakers. It is evident from the graph that depression, depressed, bad, terrible, difficult, death, disadvantaged, blow and awful are the most frequent negative words in ted1 overall 71 and on the right side well, like, better, work, right, love, great, wonderful, patient and clear are most frequent positive word of ted1 overall 67. While, dragging, limits, dangerous, wrong, hard, cold, worst, radical, died and bad are the negative words in ted2 overall 99 and second graph showing like, pretty, right, top, interesting, led, enough, worked, thank and best are positive words in ted2 overall 73. As obvious frequency of positive and negative sentiments is greater of ted2 than ted1 speaker dataset.





## Results
In ted_talks dataset we have chosen two speakers who are called as ted1 and ted2 in this coursework report and as mentioned above ted2 is greater than ted1 data. By presenting and comparing word frequency and sentiment analysis, it was noticed in both ted1 and ted2 dataset that they have almost similar sentiments. In sentiment analysis, all three lexicons which are used presented the results depending on both ted1 and ted2 dataset. Bing lexicon divided the positive and negative sentiments after that afinn lexicon stated the score between positive and negative sentiments and nrc provided several sentiments as shown in Fig 7 and Fig 8. It is engrossing to see the sentiment scores that ted2 has higher positive and negative words compared to ted1 dataset. 


Summary of Ted1

```{r echo=FALSE}
###summary of ted1###
summary(tidy_talk1_count)
```

Summary of Ted2

```{r echo=FALSE}
###summary of ted1###
summary(tidy_talk1_count)
```

Sentiments of Ted1

```{r, echo=FALSE}
###count of negative and positive words in ted1 data###
###count of positive and negative words###
table(ted1_bing$sentiment)
###number of positive and negative words### 
count_ted1_negative<-sum(ted1_bing$sentiment=="negative")
count_ted1_positive<-sum(ted1_bing$sentiment=="positive")
###length of total words###
count_ted1_words<-length(ted1_bing$sentiment)
###proportion of positive and negative words in the text###
negative_prop_ted1<-count_ted1_negative/count_ted1_words
positive_prop_ted1<-count_ted1_positive/count_ted1_words
```


Sentiments of Ted2

```{r, echo=FALSE}
###count of negative and positive words in ted2 data###
table(ted2_bing$sentiment)
###proportion of positive and negative words### 
count_ted2_negative<-sum(ted2_bing$sentiment=="negative")
count_ted2_positive<-sum(ted2_bing$sentiment=="positive")
count_ted2_words<-length(ted2_bing$sentiment)
###proportion of positive and negative words in the text###
negative_prop_ted2<-count_ted2_negative/count_ted2_words
positive_prop_ted2<-count_ted2_positive/count_ted2_words
```

```{r, echo=FALSE, fig.keep='last'}
###positive and negative scores 
###get the positive and negative sentiment scores for the words in texts###
ted1_afinn<- ted_ted1_token_df %>% 
  inner_join(afinn, by = "word") 
###head of the dataframe###
#head(ted1_afinn)
###get the summary statistics for the  sentiment scores###
#summary(ted1_afinn)
###plot boxplot to visualize the summary statistics###
boxplot(ted1_afinn$value, main="Fig 5 Sentiment scores for Sherwind Nuland")
###number of words in each sentiment score###
#table(ted1_afinn$value)

```


```{r, echo=FALSE, fig.keep='last'}
###same for ted2###
ted2_afinn<- ted_ted2_token_df %>% 
  inner_join(afinn, by = "word") 
#head(ted2_afinn)
#summary(ted2_afinn)
boxplot(ted2_afinn$value, main="Fig 6 Sentiment scores for Ben Saunders")
###number of words in each sentiment score###
#table(ted2_afinn$value)  
```

 Fig 5 and Fig 6 shows the dispersal of negative and positive sentiments of both ted1 and ted2 speakers’ dataset. Sentiment scores for ted1 data are given by -4 to 4 and the median score is observed to be between 0 and 2. Secondly, for ted2 data is given by -4 and 4 and median score is between 0 and 2. Sentiment score given by afinn lexicon is comparatively same in both ted1 and ted2 dataset.




```{r,echo=FALSE, results='hide'}
###other sentiments (nrc lexicon)###
###ted1 data###
###get the sentiments for the words common in ted1 text and the lexicon objects by merging using inner join###
ted1_nrc<-ted_ted1_token_df %>% 
  inner_join(nrc, by = "word") 
ted2_nrc<-ted_ted2_token_df %>% 
  inner_join(nrc, by = "word") 
###head of the dataframe###
head(ted1_nrc)
###count of each sentiment in the dataframe###
table(ted1_nrc$sentiment)
```

```{r, echo=FALSE}
###filter negative and positive sentiments from this dataframe### 
###as we have already analysed it###
ted1_nrc<-ted1_nrc%>%
  filter(!sentiment == "negative",
         !sentiment == "positive")
###plot the frequency of each sentiment in the ted1 data###
ggplot(ted1_nrc, aes(x=sentiment, fill = sentiment))+
  geom_bar()+       
  ###type of plot###
  coord_flip()+    
  ###flip coordinates###
  labs(caption ="Fig 7 Frequency of sentiments in ted1", x = "Sentiment", y = "Frequency")+
  theme_minimal(base_size=12)+
  theme(plot.caption = element_text(hjust=0.5, size=rel(1)))

```

```{r, echo=FALSE}
###filter negative and positive sentiments from this dataframe### 
###as we have already analysed it###
ted2_nrc<-ted2_nrc%>%
  filter(!sentiment == "negative",
         !sentiment == "positive")
###plot the frequency of each sentiment in the ted1 data###
ggplot(ted1_nrc, aes(x=sentiment, fill = sentiment))+
  geom_bar()+       
  ###type of plot###
  coord_flip()+    
  ###flip coordinates###
  labs(caption ="Fig 8 Frequency of sentiments in ted1", x = "Sentiment", y = "Frequency")+
  theme_minimal(base_size=12)+
  theme(plot.caption = element_text(hjust=0.5, size=rel(1)))

```

Fig 7 and Fig 8 shows the frequency of sentiment given by the nrc lexicon in both ted1 and ted2 datasets. Trust appears to be the dominant sentiment in ted1 data and also predominant in ted2 data as shown in Fig 8.



```{r,echo=FALSE}
###count of other sentiments recognized by the nrc lexicon in ted1 data###
table(ted1_nrc$sentiment)
###count of other sentiments recognized by the nrc lexicon in ted1 data###
###filter and get a sum of each sentiment in the data###
count_ted1_anger<-sum(ted1_nrc$sentiment=="anger")
count_ted1_anticipation<-sum(ted1_nrc$sentiment=="anticipation")
count_ted1_disgust<-sum(ted1_nrc$sentiment=="disgust")
count_ted1_fear<-sum(ted1_nrc$sentiment=="fear")
count_ted1_joy<-sum(ted1_nrc$sentiment=="joy")
count_ted1_sadness<-sum(ted1_nrc$sentiment=="sadness")
count_ted1_surprise<-sum(ted1_nrc$sentiment=="surprise")
count_ted1_trust<-sum(ted1_nrc$sentiment=="trust")
###total count of sentiments recognized by nrc lexicon in ted1 data###
count_ted1_nrc<-length(ted1_nrc$sentiment)

```

```{r, echo=FALSE}
###count of other sentiments recognized by the nrc lexicon in ted2 data###
table(ted2_nrc$sentiment)
###count of other sentiments recognized by the nrc lexicon in ted1 data###
###filter and get a sum of each sentiment in the data###
count_ted2_anger<-sum(ted2_nrc$sentiment=="anger")
count_ted2_anticipation<-sum(ted2_nrc$sentiment=="anticipation")
count_ted2_disgust<-sum(ted2_nrc$sentiment=="disgust")
count_ted2_fear<-sum(ted2_nrc$sentiment=="fear")
count_ted2_joy<-sum(ted2_nrc$sentiment=="joy")
count_ted2_sadness<-sum(ted2_nrc$sentiment=="sadness")
count_ted2_surprise<-sum(ted2_nrc$sentiment=="surprise")
count_ted2_trust<-sum(ted2_nrc$sentiment=="trust")
###total count of sentiments recognized by nrc lexicon in ted1 data###
count_ted2_nrc<-length(ted2_nrc$sentiment)
```

## Discussion:
Sentiment analysis provides us different ways to understand the point of view expressed in these talks of speakers. We surveyed how to approach sentiment analysis using tidy data principles and many other packages used in this coursework. Initially we stated, the ted1 data taken from dataset ted_talks have lesser words than ted2. It was found that both the texts have almost same sentiments and words. Ted2 has higher negative emotion in talks and the positive sentiments than ted1. Due to limited words in the dictionary our sentiment analysis is also restricted because all words are not included. Sentiment analysis provides us different ways to understand the point of view expressed in these talks of speakers.
