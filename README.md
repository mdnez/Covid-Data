# Covid-Data
Death rates by age and gender
rm(list = ls()) #Removes all variables stored previously
library(Hmisc)
COVID19_data <- read.csv("~/Maurica/Projects/COVID19_line_list_data.csv")
describe(COVID19_data) ##Hmisc command

##Clean up death column because it had 14 distinct values
COVID19_data$death_dummy <- as.integer(COVID19_data$death !=0)

##Death Rate: sum death/total
sum(COVID19_data$death_dummy) / nrow(COVID19_data) # 0.058

##AGE
#Do more older people die from covid?
dead = subset(COVID19_data, death_dummy == 1)
alive = subset(COVID19_data, death_dummy == 0)
mean(dead$age) ###NA result
mean(alive$age) ###NA result
##Remove NA
mean(dead$age, na.rm = TRUE) # 68.58%
mean(alive$age, na.rm = TRUE) # 48.07
## Test statistical significance
t.test(alive$age, dead$age, alternative = "two.sided",
       conf.level = 0.95) # 48% 68%
t.test(alive$age, dead$age, alternative = "two.sided",
       conf.level = 0.99) # 48% 68%
##Criteria: p-value < 0.05 reject null hypothesis
##Results: p-value ~ 0 reject null hypothesis
##Therefore, results are statistically significant


##Gender
#Gender has no impact on death from covid?
men = subset(COVID19_data, gender == "male")
women = subset(COVID19_data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) # 8%
mean(women$death_dummy, na.rm = TRUE) # 3%
## Test statistical significance
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided",
       conf.level = 0.95) # 0.08 0.03
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided",
       conf.level = 0.99) # 0.08 0.03
# 99% confidence: men have from 0.8% to 8.8% higher chance of 
# dying
# p-value = 0.002 < 0.05, this is statistically significant
