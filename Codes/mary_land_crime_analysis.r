install.packages("tidyverse")
install.packages("lubridate")
crime_data < read.csv("D:\\Work\\Jenga School\\JAD_402\\maryland_crime.csv")
crime_data < read.csv("D:\\Work\\Jenga School\\JAD_402\\maryland_crime.csv")
crime_data <- read.csv("D:\\Work\\Jenga School\\JAD_402\\maryland_crime.csv")
head(crime_data)
library(tidyverse)
library(lubridate)
#Load data
crime_data <- read.csv("D:\\Work\\Jenga School\\JAD_402\\maryland_crime.csv")
violent_crime_data <- crime_data %>% select(JURISDICTION ,YEAR,POPULATION, crime_rate = 'VIOLENT CRIME RATE.PER.100.000.PEOPLE')%>%
mutate(YEAR_2 = year(mdy_hms(YEAR)))
violent_crime_data <- crime_data %>% select(JURISDICTION ,YEAR,POPULATION, crime_rate = crime_data$VIOLENT.CRIME.RATE.PER.100.000.PEOPLE)%>%
mutate(YEAR_2 = year(mdy_hms(YEAR)))
violent_crime_data <- crime_data %>% select(JURISDICTION ,YEAR,POPULATION, crime_rate = 'VIOLENT.CRIME.RATE.PER.100.000.PEOPLE')%>%
mutate(YEAR_2 = year(mdy_hms(YEAR)))
# Look at few values of the data
head(violent_crime_data)
library(ggplot2)
ggplot(violent_crime_data, aes(x= YEAR_2, y = crime_rate, group = JURISDICTION))+
geom_line()+stat_smooth(method = "lm", se =FALSE, size = 0.5)
?ggplot
#TASK 3: Build a LMER
install.packages("lmerTest")
lmer_model_otput<- lmer(crime_rate ~ YEAR_2 + (YEAR_2 |JURISDICTION), data = violent_crime_data)
)
install.packages("lmerTest")
library(lmerTest)
install.packages("lmerTest")
library(lmerTest)
lmer_model_otput<- lmer(crime_rate ~ YEAR_2 + (YEAR_2 |JURISDICTION), data = violent_crime_data)
print(lmer_model_otput)
violent_crime_data %>% violent_crime_data %>% mutate(YEAR_3 = YEAR_2 -min(YEAR_2))
violent_crime_data <- violent_crime_data %>% mutate(YEAR_3 = YEAR_2 -min(YEAR_2))
violent_crime_data <- violent_crime_data %>% mutate(YEAR_3 = YEAR_2 -min(YEAR_2))
#Install needed packages
install.packages("tidyverse")
#Load the required packages
library(tidyverse)
iolent_crime_data <- violent_crime_data %>% mutate(YEAR_3 = YEAR_2 -min(YEAR_2))
head(violent_crime_data)
violent_crime_data <- violent_crime_data %>% mutate(YEAR_3 = YEAR_2 -min(YEAR_2))
head(violent_crime_data)
summary(violent_crime_data)
lmer_model_otput<- lmer(crime_rate ~ YEAR_3 + (YEAR_3 |JURISDICTION), data = violent_crime_data)
print(lmer_model_otput)
lmer_model_otput<- lmer(crime_rate ~ YEAR_2 + (YEAR_2 |JURISDICTION), data = violent_crime_data)
print(lmer_model_otput)
summary(lmer_model_otput)
fixef(lmer_model_otput)
ranef(lmer_model_otput)
#TASK 8: Maryland map data
install.packages("usmap")
library(usmap)
map_data<- us_map(regions = "counties", include = "MD")
map_data

