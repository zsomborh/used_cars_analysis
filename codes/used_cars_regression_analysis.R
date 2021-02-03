rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(data.table)
library(tidyverse)
library(moments)
library(xtable)
library(ggpubr)
library(estimatr)
library(texreg)
library(lubridate)

df <- read_csv('../data-clean/used_cars_clean.csv')

#create Age variable - Since we don't have all montly data, this will be a simle difference between year of today and when the model came out

df$Age <- year(today())-df$Year


age_df <- df %>% summarise(
    variable = 'Age',
    mean     = mean(Age),
    median   = median(Age),
    std      = sd(Age),
    iq_range = IQR(Age), 
    min      = min(Age),
    max      = max(Age),
    skew     = skewness(Age),
    numObs   = sum( !is.na( Age ) ) )

price_df <- df %>% summarise(
    variable = 'Price',
    mean     = mean(Prices),
    median   = median(Prices),
    std      = sd(Prices),
    iq_range = IQR(Prices, na.rm= TRUE), 
    min      = min(Prices),
    max      = max(Prices),
    skew     = skewness(Prices),
    numObs   = sum( !is.na( Prices ) ) )

df_summary <- age_df %>% add_row( price_df ) 
xtb <- xtable(df_summary,type = "latex", caption = "Summary statistics of examined variables")


ggplot( df[df$Engine == 'Diesel' | df$Engine == 'Benzin',] , aes( x = Prices, colour = Engine, fill = Engine ) ) +
    geom_histogram( aes(x = Prices) , alpha = 0.5, binwidth = 500000) +
    geom_density(aes(x=Prices)) +
    scale_x_continuous(labels = comma)+ 
    scale_y_continuous(labels = comma)+ 
    facet_wrap(~Engine)+
    labs(x='Price', y= 'Count')+ theme_bw()

p1<- ggplot(df, aes(x = Age, y= Prices))+ 
    stat_summary_bin(fun='mean', bins =2, color = 'orange', size = 7, geom = 'point')+ 
    theme_bw() +
    scale_y_continuous(labels = comma, limits = c(40000,14000000))+
    xlim(0,max(df$Age))

p2<- ggplot(df, aes(x = Age, y= Prices))+ 
    stat_summary_bin(fun='mean', bins =4, color = 'orange', size = 7, geom = 'point')+ 
    theme_bw() +
    scale_y_continuous(labels = comma, limits = c(40000,14000000))+
    xlim(0,max(df$Age))

ggarrange(p1,p2,nrow=1)


p1<- ggplot(df, aes(x = Age, y= Prices)) + geom_point() + geom_smooth(method="loess") +theme_bw()

p2<- ggplot(df, aes(x = Age, y= Prices)) + geom_point() + geom_smooth(method = lm, color = 'red') +theme_bw()

ggarrange(p1,p2,nrow=1)

# Run  simple linear regression

reg1 <- lm_robust( Prices ~ Age , data = df , se_type = "HC2" )
summary(reg1)

#Finding best deals

# Get the predicted y values from the model
df$reg1_y_pred <- reg1$fitted.values
# Calculate the errors of the model
df$reg1_res <- df$Prices - df$reg1_y_pred 

# Find countries with largest negative errors
top_df <- df %>% top_n( -10 , reg1_res ) %>% select(ID, Age,Prices, Km, Power_kW, reg1_y_pred,reg1_res)
colnames(top_df)[6:7] <- c('Pred', 'Residual') 
xtb <- xtable(top_df,type = "latex", caption = "Best Renault Megane deal proposed by simple linear OLS based on Age-Price relationship")

# Modeling with non-linearity
df <- df %>%
    mutate(
        Age_sq = Age **2,
        Age_cb = Age **3, 
        Prices_ln = log(Prices)
    )

# visulaise non-parametric 
p1<- ggplot(df, aes(x = Age, y= Prices_ln)) + geom_point() + geom_smooth(method="loess") +theme_bw()
p2<- ggplot(df, aes(x = Age, y= Prices_ln)) + geom_point() + geom_smooth(method = lm, color = 'red') +theme_bw()
p3<- ggplot(df, aes(x = Age, y= Prices_ln)) + geom_point() + geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'orange') +theme_bw()
p4<- ggplot(df, aes(x = Age, y= Prices)) + geom_point() + geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'lightgreen') +theme_bw()
p5<- ggplot(df, aes(x = Age, y= Prices_ln)) + geom_point() + geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'gold') +theme_bw()
p6<- ggplot(df, aes(x = Age, y= Prices)) + geom_point() + geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'gray') +theme_bw()
ggarrange(p1,p2,p3,p4,p5,p6,nrow=2, ncol = 3)

# Based on the above, best models look to be log-level linear regression, level-level quadratic and third order polynomial
# We will compare these to the original 


reg2 <- lm_robust( Prices ~ Age + Age_sq , data = df , se_type = "HC2" )
reg3 <- lm_robust( Prices ~ Age + Age_sq + Age_cb , data = df , se_type = "HC2" )
reg4 <- lm_robust( Prices_ln ~ Age , data = df , se_type = "HC2" )

data_out <- '../out/'
out1<- htmlreg( list(reg1 , reg2 , reg3 , reg4),
         type = 'html',
         custom.model.names = c("Price~Age","Price~Age - quadratic",
                                "Price~Age - cubic","Log Price~Age"),
         caption = "Modeling pattern of association between a car's price and age")


out2 <-  gsub("108", format(1011288, big.mark = "' "), out1) #

# let's use the log-level model as it has best fit+nicer interpretation

# Get the predicted y values from the model
df$reg4_y_pred <- reg4$fitted.values
# Calculate the errors of the model
df$reg4_res <- df$Prices - df$reg4_y_pred 

top_df <- df %>% top_n( -10 , reg4_res ) %>% select(ID, Age,Prices, Km, Power_kW, reg4_y_pred,reg4_res)
colnames(top_df)[6:7] <- c('Pred', 'Residual') 
xtb <- xtable(top_df,type = "latex", caption = "Best Renault Megane deal proposed by simple linear OLS based on Age-Price relationship")
