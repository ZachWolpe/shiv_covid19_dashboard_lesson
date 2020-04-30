

# ---- Dependencies ----x
library(tidyverse)
library(ggplot2)



# Read in Data
setwd("~/Desktop/Shiv Covid")
CovidDataSet <- read.csv('data/CovidDataSet.csv')
GlobalConfirmed <- read.csv('data/GlobalConfirmed.csv')
GlobalDeaths <- read.csv('data/GlobalDeaths.csv')
GlobalRecovered <- read.csv('data/GlobalRecovered.csv')


head(CovidDataSet)
head(GlobalConfirmed)
head(GlobalDeaths)
head(GlobalRecovered)


read.csv('data/novel-corona-virus-2019-dataset/time_series_covid_19_deaths_US.csv')


list.files('data')


# ---- Fix Datatypes ----x

filter(GlobalConfirmed, Country.Region=='South Africa') %>%
  mutate(date=as.Date(as.character(Date), "%Y-%m-%d"),
         value=as.numeric(as.character(Value))) %>%
  # plot
  ggplot(aes(x=date, y=value)) + 
  geom_point(col='steelblue', alpha=0.7) +
  geom_line(aes(x=date, y=value), col='steelblue', alpha=0.7) +
  theme_minimal() + ggtitle('South Africa Covid Cases') +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(y='Confirmed Cases', x='Date')




filter(GlobalDeaths, Country.Region=='South Africa') %>%
  mutate(date=as.Date(as.character(Date), "%Y-%m-%d"),
         value=as.numeric(as.character(Value))) %>%
  # plot
  ggplot(aes(x=date, y=value)) + 
  geom_point(col='darkred', alpha=0.6) +
  geom_line(aes(x=date, y=value), col='darkred', alpha=0.6) +
  theme_minimal() + ggtitle('Deaths in South Africa') +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(y='Deaths', x='Date')



filter(GlobalRecovered, Country.Region=='South Africa') %>%
  mutate(date=as.Date(as.character(Date), "%Y-%m-%d"),
         value=as.numeric(as.character(Value))) %>%
  # plot
  ggplot(aes(x=date, y=value)) + 
  geom_point(col='darkgreen', alpha=0.6) +
  geom_line(aes(x=date, y=value), col='darkgreen', alpha=0.6) +
  theme_minimal() + ggtitle('Recovered in South Africa') +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(y='Recovered', x='Date')



# ---- One Graph ----x
a <- transmute(
  GlobalConfirmed, date=as.Date(as.character(Date), "%Y-%m-%d"),
  country=Country.Region,
  value=as.numeric(as.character(Value)),
  group='confirmed')
b <- transmute(
  GlobalDeaths, date=as.Date(as.character(Date), "%Y-%m-%d"),
  country=Country.Region,
  value=as.numeric(as.character(Value)),
  group='deaths')
c <- transmute(
  GlobalRecovered, date=as.Date(as.character(Date), "%Y-%m-%d"),
  country=Country.Region,
  value=as.numeric(as.character(Value)),
  group='recovered')
  




# filter for SA first if you have memory issues
data <- full_join(a, b, by=c('date','country', 'group', 'value'))
data <- full_join(data, c, by=c('date','country', 'group', 'value'))

filter(data, country=='South Africa') %>%
ggplot() + 
  geom_point(aes(x=date, y=value, col=group), alpha=0.6) +
  geom_line(aes(x=date, y=value, col=group), alpha=0.6) +
  theme_minimal() + ggtitle('South Africa') +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(y='Recovered', x='Date')




# ---- Model ----x


# ---- Fix Datatypes ----x

us_data <- filter(GlobalConfirmed, Country.Region=='US') %>%
  transmute(date=as.Date(as.character(Date), "%Y-%m-%d"),
         value=as.numeric(as.character(Value))) 

us_data %>%
  # plot
  ggplot(aes(x=date, y=value)) + 
  geom_point(col='steelblue', alpha=0.7) +
  geom_line(aes(x=date, y=value), col='steelblue', alpha=0.7) +
  theme_minimal() + ggtitle('US Confirmed') +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(y='Confirmed Cases', x='Date')



























