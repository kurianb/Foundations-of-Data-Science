##Load libraries

library(dplyr)
library(readr)
library(tidyr)

##Read data
titanic_original <- read_csv("titanic_original.csv")
str(titanic_original)
summary(titanic_original)

##Replace missing values in embarked with S

titanic_clean <- titanic_original %>% replace_na(list(embarked = "S"))
summary(titanic_clean)

#Replace missing age with mean of age column without NAs. It will not be appropriate to update age with 0 or negative values. 
#Rather than randomly selecting one positive value the best option is to take the average age
mean_age <- mean(!is.na(titanic_clean$age))
#mean_age <- mean( titanic_clean$age, na.rm = TRUE)
titanic_clean$age[ is.na(titanic_clean$age) ] <- mean_age
#titanic_clean <- titanic_clean %>% replace_na(list(age = mean_age))
sum(is.na(titanic_clean$age)) # Test to check if NA values were updated with mean age

# Replace missing values in the Boat column with "None"
titanic_clean <- titanic_clean %>% replace_na(list(boat = "None"))
sum(is.na(titanic_clean$boat)) # test to check the total number of NAs

#  Add has_cabin_number flag
titanic_clean <- titanic_clean %>% mutate(has_cabin_number = 1*(!is.na(cabin)))

#Save cleaned file
write.csv(titanic_clean, file="titanic_clean.csv")