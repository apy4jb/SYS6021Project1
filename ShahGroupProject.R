---
title: "Preet-Group Project 1"
output: html_notebook
--
  
#Hypothesis: The time of day influences the number of casualties in accidents. 
#Null Hypothesis: There is no significant difference in the number of casualties in daytime accidents compared to nighttime accidents. 
#Alternative Hypothesis: Daytime accidents have significantly different casualty counts compared to nighttime accidents.
  

traindir <- "/Users/preetthegr8est/Desktop/UVA/fall 23/Stats/lab2/TrainData"
sourcedir <-"/Users/preetthegr8est/Desktop/UVA/fall 23/Stats/lab2"

setwd(sourcedir)
source("AccidentInput.r")

library(dplyr)
library(ggplot2)
library(car)
#Load a list of data frames for each year of accident data
acts <- file.inputl(traindir)

#Combine into one data frame
totacts <- combine.data(acts)

#3b
#To initiate an initial investigation, it is essential to begin by refining the training dataset.
#This entails the creation of a new variable named "Causality," which sums the total number of fatalities (TOTKLD) and injuries (TOTINJ).
df2 <- totacts
df2$Casualty <- df2$TOTKLD + df2$TOTINJ

#We then remove data points without any casualties and filter out records with null, empty, or duplicate entries from our dataset. 
df2 <- df2[!is.na(df2$Casualty), ]
df2 <- df2 %>% filter(Casualty > 0)
df2 <- df2 %>% distinct(INCDTNO, YEAR, MONTH, DAY, TIMEHR, TIMEMIN, .keep_all = TRUE)


#We then create two separate dataframes for "AM" and "PM" incidents
am_data <- df2 %>% filter(AMPM == "AM")
pm_data <- df2 %>% filter(AMPM == "PM")

#Assuming the sun rises at 6 am and sets at 6 pm 
#we create a new dataframe "dark" for when the sun is down
dark <- am_data %>% filter(TIMEHR <= 6)
dark1 <- pm_data %>% filter(TIMEHR > 6)
dark <- rbind(dark, dark1)

#and "bright" for when the sun is up
bright <- pm_data %>% filter(TIMEHR <= 6)
bright1 <- am_data %>% filter(TIMEHR > 6)
bright <- rbind(bright, bright1)


# Create a binary variable 'AMPM' for 'dark'
dark$AMPM <- 1  # 1 represents "dark"
bright$AMPM <- 0  # 0 represents "bright"

# Combine "dark" and "bright"
combined_data <- rbind(dark, bright)


# Create a combined dataframe
combined_data <- rbind(dark, bright)
combined_data <- rbind(
  data.frame(Data_Type = "Dark", Casualty = dark$Casualty),
  data.frame(Data_Type = "Bright", Casualty = bright$Casualty)
)

#3c
# Create a grouped Bar Graph to compare the casualties in the dark vs in the light
ggplot(combined_data, aes(x = Data_Type, y = Casualty, fill = Data_Type)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Time of Day",
    y = "Casualty",
    title = "Comparison of Casualties during Dark and Bright"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Dark" = "blue", "Bright" = "orange"))

# Perform a t-test to test the significance of the hypothesis
t_test_result <- t.test(dark$Casualty, bright$Casualty)
t_test_result

# Box plot to visualize the distribution of casualties for each group
boxplot(Casualty ~ Data_Type, data = combined_data, 
        col = c("Dark" = "blue", "Bright" = "orange"),
        main = "Casualty Distribution by Daylight Condition",
        xlab = "Daylight Condition",
        ylab = "Casualty")

# Add significance markers (if the t-test is significant)
if (t_test_result$p.value < 0.05) {
  text(1.5, max(combined_data$Casualty), "Significant", pos = 3)
  abline(h = max(combined_data$Casualty) + 10, col = "red")
} else {
  text(1.5, max(combined_data$Casualty), "Not Significant", pos = 3)
}

#The p-value associated with the test is 0.3074. This p-value is greater than the significance level of 0.05.
#The 95 percent confidence interval for the difference in means is [-0.8371525, 2.6548912]. 
#This interval includes zero, further suggesting no statistically significant difference in means.

# Linear Model
lm_model <- lm(Casualty ~ AMPM, data = combined_data)
summary(lm_model)

#The F-statistic tests whether the addition of the "AMPM" variable as a predictor is significant. 
#In this case, the F-statistic is 1.026, and the associated p-value is 0.3113, which is greater than 0.05.
#The Adjusted R-squared and the Multiple R-squared values are close to zero. 
#This indicates that the model explains very little of the variance in the number of casualties.

#Diagnostic Plots
par(mfrow = c(2, 2))
plot(lm_model, which = 1)
plot(lm_model, which = 2)
plot(lm_model, which = 3)
plot(lm_model, which = 4)

#From the diagnostic plots we can conclude non-linearity, non-normality, and heteroscedasticity.
#The Cook's Distance Plot suggests non-influential observations.


#Conclusion:
#We fail to reject the null hypothesis. therefore, there is no strong evidence to suggest that the mean number of casualties in the dark significantly differs from the mean number of casualties in the day light.

#Reccomendation:
#The objective of this hypothesis was to investigate the potential relationship between sunlight and casualties, aiming to provide safety-related recommendations to the Federal Railroad Administration (FRA). 
#However, our findings indicate that there is no statistically significant correlation between the time of day, particularly sunlight, and the number of casualties in railroad incidents.

#In light of these results, we recommend exploring alternative variables and factors to improve railroad safety, aligning with the primary mission of the FRA.
#Nevertheless, we also suggest that certain factors be continuously monitored, as they may still hold significance in influencing safety outcomes in the future.


#https://uknowledge.uky.edu/ktc_researchreports/1069/





