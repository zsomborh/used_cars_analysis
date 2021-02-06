rm(list = ls())

library(data.table)
library(tidyverse)
library(moments)
library(estimatr)
library(lubridate)
library(caret)
library(rattle)
library(Hmisc)
require(scales)

df<- read_csv('https://raw.githubusercontent.com/zsomborh/used_cars_analysis/main/data/clean/used_cars_clean.csv')


# Data Quality ------------------------------------------------------------


colSums(is.na(df))

sum(duplicated(df))


# Checking variables ------------------------------------------------------

# Engine

df<- df %>% 
    mutate(
        Engine = factor(ifelse(Engine %in% c('Benzin/Gáz', 'Benzin'),'Petrol',Engine))) %>% 
    filter(
        !Engine == 'LPG')


ggplot(df, aes(x = factor(Engine), y = Prices,
                     fill = factor(Engine), color=factor(Engine))) +
    geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
    scale_color_manual( name = '', values = c('black', 'black')) +
    scale_fill_manual( name = '', values = c('navyblue', 'purple4')) +
    stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
    labs(x = "Engine types",y = "Price (HUF)")+
    theme_minimal() +
    theme(legend.position = c(0.15,0.95)) +
    scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 5000000), breaks = seq(0,5000000, 1000000))


# Age was thoroughly analysed in an earlier exercise - we won't need Year or Month

df <- df %>%  mutate( 
    Age = as.numeric(year(today())-Year), 
    Month = NULL,
    Year = NULL)

# Displacement 

ggplot(df, aes(x = Displacement))+
    geom_histogram( aes(y = ..density..) , alpha = 1, binwidth = 100, color = 'black', fill = 'navyblue') +
    geom_density( aes(y = ..density..) , alpha = .5 , bw = 50, color = 'black', fill='steelblue') +
    labs(x='Displacement distribution',y='Density') + theme_minimal() 

# Horsepower - we won't use power KW as we discovered earlier that they are exactly the same with Horsepower

ggplot(df, aes(x = Horsepower))+
    geom_histogram( aes(y = ..density..) , alpha = 1, binwidth = 10, color = 'black', fill = 'purple4') +
    geom_density( aes(y = ..density..) , alpha = .5 , bw = 10, color = 'black', fill='orchid') +
    labs(x='Horsepower distribution',y='Density') + theme_minimal() 

# Km 

ggplot(df, aes(x = Km))+
    geom_histogram( aes(y = ..density..) , alpha = 1, binwidth = 10000, color = 'black', fill = 'green4') +
    geom_density( aes(y = ..density..) , alpha = .5 , bw = 10000, color = 'black', fill='seagreen') +
    labs(x='Km distribution',y='Density') + theme_minimal() 

#There is one Renault Megane with an extremely high KM - it is a 2016 model, it is very unlikely that
#It has this many Km so this is likely to be an error - I will impute the mean of KM for this car

df[df$Km == max(df$Km),'Km'] <- mean(df$Km)

ggplot(df, aes(x = Km))+
    geom_histogram( aes(y = ..density..) , alpha = 1, binwidth = 10000, color = 'black', fill = 'green4') +
    geom_density( aes(y = ..density..) , alpha = .5 , bw = 10000, color = 'black', fill='seagreen') +
    labs(x='Km distribution',y='Density') + theme_minimal() 

skewness(df$Km)
kurtosis(df$Km)
# Without this extreme value distribution now looks normal -skewness and kurtosis tell the same

#PictureCount

hist(df$PictureCount)


# Feature engineering -----------------------------------------------------

# log transformation for price and add age in quadratic form

df <- df %>% mutate(
    ln_Prices = log(Prices)
)

# sq transformatio for all numeric vars
colnames(df)


df<- df %>% mutate(
    sq_Displacement = Displacement ** 2, 
    sq_Horsepower = Horsepower ** 2, 
    sq_Km = Km **2, 
    sq_PictureCount = PictureCount **2, 
    sq_Age = Age **2,
    cb_Displacement = Displacement ** 2, 
    cb_Horsepower = Horsepower ** 2, 
    cb_Km = Km **2, 
    cb_PictureCount = PictureCount **2, 
    cb_Age = Age **2
    
)

sq_vars <- df %>% select(starts_with('sq')) %>% colnames()
cb_vars <- df %>% select(starts_with('cb')) %>% colnames()



# Looking at price distributions, there seems to be some luxuries cars that don't fit the general pattern both in terms of price and displacement

df <- df %>% mutate(
    is_luxury = ifelse(Engine == 'Petrol' & Prices >7000000 | Engine == 'Diesel' & Prices > 5000000,1,0)
    )

sum(df$is_luxury)/nrow(df)

# define variable sets for incrementally more complex models


target_var1 <- c('Prices')
target_var2 <- c('ln_Prices')

first_case <- c('Age')
second_case <- c('Age', 'Displacement', 'Horsepower', 'Km', 'PictureCount')

# Added interaction sets 
interactions1 <- c(paste0('Engine * (',paste(second_case, collapse = '+'),')'))
interactions2 <- c(paste0('Age * (',paste(second_case[2:length(third_case)], collapse = '+'),')'))
interactions3 <- c(paste0('is_luxury * (',paste(second_case, collapse = '+'),')'))


third_case <- c(second_case, sq_vars)
fourth_case <- c(third_case,interactions1)
fifth_case <- c(fourth_case,cb_vars)
sixth_case <- c(fifth_case,interactions2, interactions3)


# Modeling  ---------------------------------------------------------------

#  create holdout set 


set.seed(7)

train_indices <- as.integer(createDataPartition(df$Prices, p = 0.85, list = FALSE))
df_train <- df[train_indices, ]
df_holdout <- df[-train_indices, ]


# train control is 5 fold cross validation
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)

# Starting off with simple OLSs 


set.seed(7)
system.time({
    ols_model1 <- train(
        formula(paste0("ln_Prices ~", first_case)),
        data = df_train,
        method = "lm",
        trControl = train_control
    )
})

set.seed(7)
system.time({
    ols_model2 <- train(
        formula(paste0("ln_Prices ~", paste0(second_case, collapse = " + "))),
        data = df_train,
        method = "lm",
        trControl = train_control
    )
})

set.seed(7)
system.time({
    ols_model3 <- train(
        formula(paste0("ln_Prices ~", paste0(third_case, collapse = " + "))),
        data = df_train,
        method = "lm",
        trControl = train_control
    )
})

set.seed(7)
system.time({
    ols_model4 <- train(
        formula(paste0("ln_Prices ~", paste0(fourth_case, collapse = " + "))),
        data = df_train,
        method = "lm",
        trControl = train_control
    )
})

set.seed(7)
system.time({
    ols_model5 <- train(
        formula(paste0("ln_Prices ~", paste0(fifth_case, collapse = " + "))),
        data = df_train,
        method = "lm",
        trControl = train_control
    )
})

set.seed(7)
system.time({
    ols_model6 <- train(
        formula(paste0("ln_Prices ~", paste0(sixth_case, collapse = " + "))),
        data = df_train,
        method = "lm",
        trControl = train_control
    )
})



# Model performance and model choice --------------------------------------



temp_models <-
    list("OLS1" = ols_model1,
         "OLS2" = ols_model2,
         "OLS3" = ols_model3,
         "OLS4" = ols_model4,
         "OLS5" = ols_model5,
         'OLS6' = ols_model6)

result_temp <- resamples(temp_models) %>% summary()


result_rmse <- imap(temp_models, ~{mean(result_temp$values[[paste0(.y,"~RMSE")]])}) %>% unlist() %>% as.data.frame() %>% rename("CV RMSE" = ".")



result_holdout <- map(temp_models, ~{
    RMSE(predict(.x, newdata = df_holdout), df_holdout[["ln_Prices"]])
}) %>% unlist() %>% as.data.frame() %>%
    rename("Holdout RMSE" = ".")

result_rmse
result_holdout



# Price prediction --------------------------------------------------------

reg <- lm(formula(paste0("ln_Prices ~", paste0(third_case, collapse = " + "))), data=df)

df <- df %>%  mutate ( 
    pred_ln_Prices = predict(ols_model3,newdata = df) ,
    pred_level_Prices = exp(pred_ln_Prices) * exp(RMSE(pred_ln_Prices, ln_Prices)**2)
    )

ggplot(df, aes(x = Prices, y = pred_level_Prices))+
    geom_point(color = 'navyblue', shape =1, size = 1.5 )+
    geom_abline(color = 'purple4', size = 1.4)+
    theme_minimal()+
    labs(x = 'Price', y = 'Predicted price')+
    scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) 


pred_ln_new_80 <- predict(reg, newdata = df, se.fit = TRUE,interval = 'prediction', level =  0.8)

df<- df %>% mutate(
    pred_level_lwr_80 = exp(pred_ln_new_80$fit[,'lwr']) *  exp(RMSE(pred_ln_Prices, ln_Prices)**2),
    pred_level_upr_80 = exp(pred_ln_new_80$fit[,'upr']) *  exp(RMSE(pred_ln_Prices, ln_Prices)**2)
)

df$pred_ln_Prices

colors <- c("Actual" = "navyblue", "Predicted" = "purple3", "lower 80" = "green4", 'upper 80' = 'gold2')


ggplot(df, aes(x = Age))+
    geom_smooth(aes(x = Age, y = Prices, color = 'Actual'), method="loess", se=F, size=1.2, na.rm=T) + 
    geom_smooth(aes(x = Age, y = pred_level_Prices, color = 'Predicted'), method="loess", se=F, size=1.2, na.rm=T)  +
    geom_smooth(aes(x = Age, y = pred_level_lwr_80, color = 'lower 80'), method="loess", se=F, size=1.2, na.rm=T)  +
    geom_smooth(aes(x = Age, y = pred_level_upr_80, color = 'upper 80'), method="loess", se=F, size=1.2, na.rm=T)  +
    geom_point(aes (x = Age, y = Prices), color = 'navyblue', shape =1, size = 1.5) + theme_minimal() + labs(x = 'Age', y= 'Prices', color = 'Legend') +
    scale_color_manual(values = colors) + scale_y_continuous(labels = comma) 
