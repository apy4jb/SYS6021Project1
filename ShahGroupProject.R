---
title: "Preet-Group Project 1"
output: html_notebook
--
traindir <- "/Users/preetthegr8est/Desktop/UVA/fall 23/Stats/lab2/TrainData"
sourcedir <-"/Users/preetthegr8est/Desktop/UVA/fall 23/Stats/lab2"

setwd(sourcedir)
source("AccidentInput.r")

library(dplyr)
library(ggplot2)
library(car)
# Load a list of data frames for each year of accident data
acts <- file.inputl(traindir)

# combine into one data frame
totacts <- combine.data(acts)

#Create the Casualty variable
df2 <- totacts
df2$Casualty <- df2$TOTKLD + df2$TOTINJ
#Remove data without a 'Casualty'
df2 <- df2[!is.na(df2$Casualty), ]
df2 <- df2 %>% filter(Casualty > 0)

# Remove duplicate reports
df2 <- df2 %>% distinct(INCDTNO, YEAR, MONTH, DAY, TIMEHR, TIMEMIN, .keep_all = TRUE)

# Define the model
model <- lm(Casualty ~ TRNSPD, data = df2)

# Print a summary of the model
summary(model)

# Residual analysis
hist(model$residuals)
qqnorm(model$residuals)
plot(model$fitted.values, model$residuals)
# Cook's distance plot
plot(cooks.distance(model))
# Leverage vs. standardized residuals plot
plot(model, which = 5)
crPlots(model)
# Model diagnostics
AIC(model)


#Hypothesis: Train Speed influences the number of casualties in accidents. 
#Null Hypothesis: There is no significant correlation between train speed and the number of casualties in accidents.
#Alternative Hypothesis: Train speed is significantly correlated with the number of casualties in accidents.

# Based on the p-value for TRNSPD, we can reject the null hypothesis and conclude that
# there is a statistically significant correlation between train speed and the number of casualties in extreme railroad accidents.
# However, the low R-squared value suggests that train speed explains only a very small proportion of the variation in casualties.



