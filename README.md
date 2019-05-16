# R
data analysis
---
title: "MLB_Pitch"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##load library
```{r libs, message=FALSE}
library(ggplot2)
library(dplyr)
library(tidyr)
```

##load csv
```{r}
atbat <- read.csv('~/downloads/mlb-pitch-data-20152018/atbats.csv')
#pitch <- read.csv('~/downloads/mlb-pitch-data-20152018/pitches.csv')
head(atbat)
#head(pitch)
```

##arrange data
```{r}
event <- atbat %>%
  select(event) %>%
  distinct()
subset(event)
#new_df <- df %>%
#  group_by(User_ID, Gender, Age, City_Category, Stay_In_Current_City_Years, Marital_Status = as.factor(Marital_Status)) %>%
#  summarize(Purchase = sum(Purchase))
#head(new_df)

```{r}
team <- atbat %>%
  select(top, event) #%>%
  #filter(top == "TRUE")
table(team)
summary(team)
```

##draw
```{r}
team <- team %>% mutate(cnt = 1)
head(team)
count <- aggregate(cnt ~ event + top, team, sum)
head(count)
ggplot(count, aes(x = event, y = cnt, col = top)) + geom_point()
```

##draw
```{r}
batter <- atbat %>%
  select(batter_id, event)
table(batter)
batter <- batter %>% mutate(cnt = 1)
head(batter)
count2 <- aggregate(cnt ~ event + batter_id, batter, sum)
head(count2)
ggplot(count2, aes(x = batter_id, y = cnt, col = event)) + geom_point()
ggplot(count2, aes(x = event, y = cnt, col = batter_id)) + geom_point()
```

##simplify
```{r}
count2 <-count2 %>% mutate(nicebat = TRUE)
head(count2)
#if(count2$event == 'Groundout' | count2$event == 'Strikeout' | count2$event == 'Runner Out' | count2$event == 'Flyout' | count2$event == 'Forceout' | count2$event == 'Pop Out' | count2$event == 'Lineout' | count2$event == 'Grounded Into DP' | count2$event == 'Bunt Groundout' | count2$event == 'Double Play' | count2$event == 'Fielders Choice Out' | count2$event == 'Bunt Pop Out' | count2$event == 'Strikeout - DP' | count2$event == 'Batter Interference' | count2$event == 'Sac Fly DP' | count2$event == 'Bunt Lineout' | count2$event == 'Sacrifice Bunt DP' | count2$event == 'Triple Play'){count2$nicebat = FALSE}

#if(count2$event == 'Groundout'){count2$nicebat = FALSE}
#if(count2$event == 'Strikeout'){count2$nicebat = FALSE}
#if(count2$event == 'Runner Out'){count2$nicebat = FALSE}
#if(count2$event == 'Flyout'){count2$nicebat = FALSE}
#if(count2$event == 'Forceout'){count2$nicebat = FALSE}
#if(count2$event == 'Pop Out'){count2$nicebat = FALSE}
#if(count2$event == 'Lineout'){count2$nicebat = FALSE}
#if(count2$event == 'Grounded Into DP'){count2$nicebat = FALSE}
#if(count2$event == 'Bunt Groundout'){count2$nicebat = FALSE}
#if(count2$event == 'Double Play'){count2$nicebat = FALSE}
#if(count2$event == 'Fielders Choice Out'){count2$nicebat = FALSE}
#if(count2$event == 'Bunt Pop Out'){count2$nicebat = FALSE}
#if(count2$event == 'Strikeout - DP'){count2$nicebat = FALSE}
#if(count2$event == 'Batter Interference'){count2$nicebat = FALSE}
#if(count2$event == 'Sac Fly DP'){count2$nicebat = FALSE}
#if(count2$event == 'Bunt Lineout'){count2$nicebat = FALSE}
#if(count2$event == 'Sacrifice Bunt DP'){count2$nicebat = FALSE}
#if(count2$event == 'Triple Play'){count2$nicebat = FALSE}

if(count2$event == 'Groundout'){count2$nicebat = FALSE}else{if(count2$event == 'Strikeout'){count2$nicebat = FALSE}else{if(count2$event == 'Runner Out'){count2$nicebat = FALSE}else{if(count2$event == 'Flyout'){count2$nicebat = FALSE}else{if(count2$event == 'Forceout'){count2$nicebat = FALSE}else{if(count2$event == 'Pop Out'){count2$nicebat = FALSE}else{if(count2$event == 'Lineout'){count2$nicebat = FALSE}else{if(count2$event == 'Grounded Into DP'){count2$nicebat = FALSE}else{if(count2$event == 'Bunt Groundout'){count2$nicebat = FALSE}else{if(count2$event == 'Double Play'){count2$nicebat = FALSE}else{if(count2$event == 'Fielders Choice Out'){count2$nicebat = FALSE}else{if(count2$event == 'Bunt Pop Out'){count2$nicebat = FALSE}else{if(count2$event == 'Strikeout - DP'){count2$nicebat = FALSE}else{if(count2$event == 'Batter Interference'){count2$nicebat = FALSE}else{if(count2$event == 'Sac Fly DP'){count2$nicebat = FALSE}else{if(count2$event == 'Bunt Lineout'){count2$nicebat = FALSE}else{if(count2$event == 'Sacrifice Bunt DP'){count2$nicebat = FALSE}else{if(count2$event == 'Triple Play'){count2$nicebat = FALSE}}}}}}}}}}}}}}}}}}

count3 <- aggregate(cnt ~ batter_id + nicebat, count2, sum)
str(count3)
c<-count3%>%filter(nicebat==TRUE)
head(c)
subset(c)
ggplot(count3, aes(x = as.factor(batter_id), y = cnt, col = nicebat)) + geom_point()
part <- count3 %>% filter(batter_id < 500000)
ggplot(part, aes(x = as.factor(batter_id), y = cnt, col = nicebat)) + geom_point()
```
