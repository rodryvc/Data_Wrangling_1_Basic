library(dplyr)
library(tidyr)

titanic_log <- read.csv("titanic_original.csv")
summary(titanic_log)
str(titanic_log)

##Port of embarkation
#The embarked column has one missing value, which is known to 
#correspond to a passenger who actually embarked at Southampton. Find the missing value and replace it with S.
titanic_log$embarked
table(titanic_log$embarked)
levels(titanic_log$embarked)
titanic_log$embarked[titanic_log$embarked == ""] = NA
which(is.na(titanic_log$embarked))

titanic_log$embarked[169] <- "S"
titanic_log$embarked[285] <- "S"
titanic_log$embarked[1310] <- "S"

##Age. You'll notice that a lot of the values in the Age coluAge.mn are missing. While there are many ways to fill 
#these missing values, using the mean or median of the rest of the values is quite common in such cases.
#1.	Calculate the mean of the Age column and use that value to populate the missing values
#2.	Think about other ways you could have populated the missing values in the age column. 
#Why would you pick any of those over the mean (or not)?

#First explore if there are any outliers in the data, this means ages that do not make any sense
summary(titanic_log$age)

which(is.na(titanic_log$age))
titanic_log$age[which(is.na(titanic_log$age))] <- mean(titanic_log$age, na.rm = TRUE)
titanic_log$age

##Lifeboat. You're interested in looking at the distribution of passengers in different lifeboats, but as we know, many passengers did not make it to a boat
#This means that there are a lot of missing values in the boat column. Fill these empty slots with a dummy value e.g. NA

levels(titanic_log$boat)
titanic_log$boat[titanic_log$boat == ""] = NA
titanic_log$boat

##Cabin. Create a dummy variable for filled and empty values. 
levels(titanic_log$cabin)
has_cabin <- ifelse((titanic_log$cabin == ""),0,1)
titanic_log_with_cabin <- mutate(titanic_log, has_cabin_number = has_cabin)             
summary(titanic_log_with_cabin)
titanic_log_with_cabin$has_cabin_number

#Write to csv
write.csv(titanic_log_with_cabin, file = "titanic_clean.csv")

#Some visualizations
# Check out the structure of titanic
str(titanic_log)

# Use ggplot() for the first instruction
ggplot(titanic_log, aes(x = factor(pclass), fill = factor(sex))) + geom_bar(position = "dodge")

# Use ggplot() for the second instruction
ggplot(titanic_log, aes(x = factor(pclass), fill = factor(sex))) + 
  geom_bar(position = "dodge") + 
  facet_grid(. ~ survived)

# Position jitter (use below)
posn.j <- position_jitter(0.5, 0)

# Use ggplot() for the last instruction
ggplot(titanic_log, aes(x = factor(pclass), y = age, col = factor(sex))) + 
  geom_jitter(size = 3, alpha = 0.5, position = posn.j) + 
  facet_grid(. ~ survived)


