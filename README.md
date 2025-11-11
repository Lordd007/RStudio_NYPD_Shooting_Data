# RStudio_NYPD_Shooting_Data
This report will analyse the Historic NYPD Shooting Incident Data self-reported by the NYPD from 2006 to 2022.


---
title: "NYPD Shooting Incident Data (Historic)"
author: "David Lord"
date: "`r Sys.Date()`"
output: html_document
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(lubridate)
```

Data was inputted from dataset titled NYPD Shooting Incident Data (Historic).

This report will analyse the Historic NYPD Shooting Incident Data self-reported by the NYPD from 2006 to 2022.

**Problem Statement**: Are individuals who identify as Black disproportionately more susceptible to becoming victims of crime in New York? Are there any other demographic groups similarly affected?

**Summary**: My analysis revealed that **70%** of crime had victims that identified as Black, despite comprising only **20%** of the population. Additionally, I found that **90%** of victims were male, while males constitute only **47%** of the population.  

### Import Data from City of New York
```{r get_nypd_data}
## Get Current Data
url_in<-"https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv"
```


### Read in Data and select fields that will be relevant to race:
```{r import_data,message=FALSE}
nypd<-read.csv(url_in, stringsAsFactors = TRUE) %>%
  select(-c(X_COORD_CD:Lon_Lat, OCCUR_TIME, INCIDENT_KEY, LOC_OF_OCCUR_DESC,LOC_CLASSFCTN_DESC,JURISDICTION_CODE,LOCATION_DESC))
```


### Change Columns and Convert Date type:

```{r updateCols}
nypd <- nypd %>% 
    rename("Date" = "OCCUR_DATE" ,
          # "Time" = "OCCUR_TIME" ,
           "Murder" = "STATISTICAL_MURDER_FLAG" ,
           "Victim_Sex" = "VIC_SEX" ,
           "Victim_Race" = "VIC_RACE",
           "Victim_Age_Group"="VIC_AGE_GROUP") 
colnames(nypd) <- tolower(colnames(nypd))
#Convert string dates into date type
nypd$date<-mdy(nypd$date)
```

### Data cleaning and standardizing

In cases where the fields were "UNKNOWN" or null, the fields were converted to "UNKNOWN" or "U"
This resulted in a large number of UNKNOWNS in the data set.  However it should not be excluded initially without keeping in mind how it may make the rest of the data appear. 
```{r cleanData}
#Data cleaning and standardizing
nypd$perp_race[is.na(nypd$perp_race)] <- "UNKNOWN"
nypd$perp_race[(nypd$perp_race=="(null)")] <- "UNKNOWN"
nypd$perp_age_group[(nypd$perp_age_group=="(null)")] <- "UNKNOWN"
nypd$perp_sex[is.na(nypd$perp_sex)] <- "U"
nypd$perp_sex[nypd$perp_sex=='(null)'] <- "U"
nypd$perp_age_group[is.na(nypd$perp_age_group)] <- "UNKNOWN"
nypd$victim_age_group[nypd$victim_age_group=='1022'] <- "UNKNOWN"

```

### Summary of the Data

```{r summary}
#Cleaned
summary(nypd)
<img width="745" height="219" alt="image" src="https://github.com/user-attachments/assets/a5c654eb-5b90-4c5a-b05a-05106dd8b34b" />

```


### Does an individual's race play a role?
In order to investigate my first task, I looked at the victim's race and the perpetrator's race from the data and graphed the number of instances.  

```{r vis_perpetratrsAndVictim, echo=FALSE}
victim_race_tbl <- table(nypd$victim_race)
#You can adjust the size of the margins using the notation par(mar = c(bottom, left, top, right) where the arguments bottom , left , top and right are the size of the corresponding margins. By default R sets these margins as mar = c(5.1, 4.1, 4.1, 2.1) with these numbers specifying the number of lines in each margin.
par(bg = "white",mar=c(10,14.1,4.1,2.1))
 barplot(victim_race_tbl, col = gray.colors(length(victim_race_tbl)),xlim=c(0,20000),horiz=TRUE,las=2, cex.names = .8,cex.axis = .8)
 title(main = "Victims by Race", cex.main = 2, font.main = 3) 
 
 #Get the number of perps by race from nypd table
 perp_race_tbl <- table(nypd$perp_race)
 #There were a significant amount of empty cells which were added to 'UNKNOWN'
 perp_race_tbl[7]<-perp_race_tbl[7]+perp_race_tbl[1]
 #Empty cells and null were removed.
 perp_race_tbl <- perp_race_tbl[-c(1,2)]
 
#You can adjust the size of the margins using the notation par(mar = c(bottom, left, top, right) where the arguments bottom , left , top and right are the size of the corresponding margins. By default R sets these margins as mar = c(5.1, 4.1, 4.1, 2.1) with these numbers specifying the number of lines in each margin.
par(bg = "white",mar=c(10,14.1,4.1,2.1))
 barplot(perp_race_tbl, col = gray.colors(length(perp_race_tbl)),xlim=c(0,20000),horiz=TRUE,las=2, cex.names = .8,cex.axis = .8)
 title(main = "Perpetrators by Race", cex.main = 2, font.main = 3) 
```

People who identified as Black are much more affected by any other category. **70%** of the crime had victims that identified as Black, despite comprising only **20%** of the population. Interesting, the majority of perpetrators' race was identified as Black or Unknown.   This is a call to action to improve the identification of race and record-keeping. 


### Is there a connection between Victim's and Perpetrator's race?

```{r vis_victim_vs_perp, echo=FALSE}

victim_vs_perp_race_df <- data.frame(Victim_Race= nypd$victim_race,
                   Perpetrator_Race= nypd$perp_race)
for (i in 1:nrow(victim_vs_perp_race_df )){
         if(victim_vs_perp_race_df$Perpetrator_Race[i]==''|
            victim_vs_perp_race_df$Perpetrator_Race[i]=='(null)')
           { 
         victim_vs_perp_race_df$Perpetrator_Race[i]='UNKNOWN' 
         }
}
 #Drops unused levels
victim_vs_perp_race_df$Perpetrator_Race<-droplevels(victim_vs_perp_race_df$Perpetrator_Race)
 
#Create a heatmap  
table(victim_vs_perp_race_df ) %>%
  as.data.frame() %>%
  ggplot() + aes(x=Victim_Race, y =Perpetrator_Race, fill=Freq)%>%
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Perpetrator Race", x = "Victim Race", fill = "Frequency", title ="Perpetrator and Victim Race")

``` 

There seems to be a strong correlation between attacks on Black victims by perpetrators whose race was identified as Black or is not known.  I will report my findings and hope to bring awareness of this issue.  I would also like to gather more information on how the data is reported as it could be that the police are not called as often racial disturbances. 


### Does an individual's sex play a role?

In order to investigate my second task, I looked at the victim's sex and the perpetrator's sex from the data and graphed the number of instances.  


```{r vis_perpetratrsAndVictimSex, echo=FALSE}
victim_sex_tbl <- table(nypd$victim_sex)
sex<-c("Female", "Male", "Unknown")
#You can adjust the size of the margins using the notation par(mar = c(bottom, left, top, right) where the arguments bottom , left , top and right are the size of the corresponding margins. By default R sets these margins as mar = c(5.1, 4.1, 4.1, 2.1) with these numbers specifying the number of lines in each margin.
par(bg = "white",mar=c(10,14.1,4.1,2.1))
 barplot(victim_sex_tbl, names.arg = sex, col = gray.colors(length(victim_sex_tbl)), ylim=c(0,30000), cex.names = .8,cex.axis = .8)
 title(main = "Victims by Sex", cex.main = 2, font.main = 3) 
 
 #Get the number of perps by race from nypd table
 perp_sex_tbl <- table(nypd$perp_sex)
 #There were a significant amount of empty cells which were added to 'UNKNOWN'
 perp_sex_tbl[5]<-perp_sex_tbl[5]+perp_sex_tbl[1]
 #Empty cells and null were removed.
 perp_sex_tbl <- perp_sex_tbl[-c(1,2)]
 
#You can adjust the size of the margins using the notation par(mar = c(bottom, left, top, right) where the arguments bottom , left , top and right are the size of the corresponding margins. By default R sets these margins as mar = c(5.1, 4.1, 4.1, 2.1) with these numbers specifying the number of lines in each margin.
par(bg = "white",mar=c(10,14.1,4.1,2.1))
 barplot(perp_sex_tbl, , names.arg = sex,  col = gray.colors(length(perp_sex_tbl)), ylim=c(0,30000), cex.names = .8,cex.axis = .8)
 title(main = "Perpetrators by Sex", cex.main = 2, font.main = 3) 
```

 I found that **90%** of victims were male, while males constitute only **47%** of the population. Again, there is an issue with recording incomplete information of the perpetrators, which hopefully can be improved on in future. 



### Which Boros has the highest murder rate?

My investigation naturally lead to inquiry on where are the crimes occurring? Does it occur in all boroughs equally or are some more effected than others? 
I looked at the number of murders in New York and found the percentages of murders for each borough.

```{r vis_boro_vs_murderPercent, echo=FALSE}
murder_by_boro <- nypd %>%
    group_by(boro) %>%
    summarize(murder = sum(murder=="true"))%>%
    mutate(murder_percent = murder/5266 *100) %>%
    mutate("murder rate" = murder/(5266+22046) *100) %>%
    mutate("Borough" = boro) %>%
    select(boro,murder,"murder rate", murder_percent) %>%
    ungroup() 

#murder_by_boro
barplot(murder_by_boro$murder_percent,names.arg = murder_by_boro$boro, xlab = "Boroughs", cex.names = .8, ylab="Percent of Murders", main="Which boroughs have the greatest percent of murders of New York?")
``` 

Brooklyn has the greatest proportion of murders that occurred in New York, but is also the most populated. Interestingly to note is that the Bronx is the 4th in population but second in terms of murder rate.    So while Brooklyn appears to be the most dangerous borough to live in, the Bronx may be more dangerous. 

<!-- However in terms of total victims, both the Bronx and Brooklyn have similar proportions of victims.  -->
<!-- ```{r pieGraph, echo=FALSE} -->



<!-- boro_tbl <- table(nypd$boro) -->

<!-- par(bg = "white") -->
<!--  pie(boro_tbl, col = rainbow(length(boro_tbl)), labels=c(""), radius = .8, font=2,cex=.3)  -->
<!--  title(main = "Victims by Boro", cex.main = 2, font.main = 3)  -->
<!--  legend("right",legend = str_to_title(names(boro_tbl)),cex=.7, fill=rainbow(length(boro_tbl))) -->
<!-- ``` -->


## Sources of Bias

There are several sources of bias in the data collected.  Many of perpetrators' race and sex could not be identified or were left empty.  If one only looks at the races provided, they may assume that people who are identified as "Black", are the most common perpetrators of crimes.  The race was also reported by the victims and/or police offers who may have misidentified races or have their own biases.  

For my own personal biases, I was surprised by the distributions of crimes by race and sex.  I thought the distribution would be more similar to the demographics of New York, but Blacks seem to be over represented as both victims and perpetrators.  I also was surprised by how distributions of murders across the boroughs.  Based on movies and media, I thought the Bronx and Queens were the most dangerous areas, but Brooklyn did have the largest proportion of murders.  Queens has a population that is approximately 85% of Brooklyn but has a much lower murder percent.   I am aware that by looking at only one data source, the data could lead to false assumptions.  The next steps are to look for additional sources to compare it to this source and improve the models to better represent the population of New York. 
