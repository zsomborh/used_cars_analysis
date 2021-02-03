rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(tidyverse)
library(moments)

df <- read_csv('https://raw.githubusercontent.com/zsomborh/ba_da1/master/homework/data/used_cars.csv')

#rewriting Diesel so that we don't have issues with encoding
df[df$Engine == 'Dízel' & !is.na(df$Engine),]$Engine <- 'Diesel'

#clean prices
df <- df %>% 
    separate(Prices , 'Fizetend', into = c("first","second")) %>% 
    mutate(Prices = ifelse(!is.na(second), second,first),
           Prices = as.numeric(gsub('[^0-9\\.]','',Prices)),
           first=NULL,
           second=NULL) %>%
    separate(Year, sep = '/', into = c('Year','Month'), fill = 'right') %>% 
    filter(!is.na(Prices)) %>% 
    filter(!is.na(Year)) 
    


df <- df %>% 
    mutate(
        Displacement = as.numeric(Displacement),
        Power_kW = as.numeric(Power_kW),
        Horsepower = as.numeric(Horsepower),
        Km = as.numeric(Km),
        PictureCount = as.numeric(PictureCount),
        Engine = as.factor(Engine),
        Year = as.numeric(Year),
        Month = as.numeric(Month)
    )

getwd()
write_csv(df,'../data-clean/used_cars_clean.csv')
