library(stringr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(readr)
library(mltools)
library(data.table)

# Explain the data 
# Response variable is math.score

# Read Data

read.csv('StudentsPerformance.csv')

sp = read.csv('StudentsPerformance.csv')


# number of rows in sp
# number of columns in sp
len_cols = length(names(sp))
print(c("number of rows:", nrow(sp)))
print(c("number of columns:", len_cols))

# check if there are NA values in dataframe

if (any(is.na(sp)) == TRUE) {
  na_cols = colnames(sp)[colSums(is.na(sp)) > 0]
  
}
print(c("Column with na values are:", na_cols))

# Columns that have na values are math.score, reading.score, writing.score

# Response variable has NA values, remove the data when response variables have na values
sp = sp[!is.na(sp$math.score),]
print(c("number of rows:", nrow(sp)))

# Fill NA values for other variables using appropriate conditions
# Fill na values for Reading score by using median values for Male and Female

# median of female reading score
med_female_rs = median(sp$reading.score[sp$gender=='female'],na.rm=TRUE)
# median for male reading score
med_male_rs = median(sp$reading.score[sp$gender=='male'], na.rm=TRUE)

sp$reading.score[is.na(sp$reading.score) & (sp$gender == "female")] <-med_female_rs
sp$reading.score[is.na(sp$reading.score) & (sp$gender == "male")] <-med_male_rs

# Do the same for the writing Score

med_female_ws = median(sp$writing.score[sp$gender=='female'],na.rm=TRUE)
# median for male reading score
med_male_ws = median(sp$writing.score[sp$gender=='male'], na.rm=TRUE)

sp$writing.score[is.na(sp$writing.score) & (sp$gender == "female")] <-med_female_ws
sp$writing.score[is.na(sp$writing.score) & (sp$gender == "male")] <-med_male_ws

# find categorical columns 
y = c()
for (i in names(sp)){
  x <- class(sp[[i]])
  if (x == "factor"){
    y = c(y, i)
  }
}

# Remove any categorical columns that have more than 10 categories
for (i in y){
  x = length(names(table(sp$test.preparation.course)))
  if (x > 10){
    sp[[i]] <- NULL
  }
    
}
  

# check if there is a difference of mean between girls and boys math score
gend <- sp%>% select(gender, math.score)
gend <- gend %>% group_by(gender) %>% summarise_all(mean)
gend <- data.frame(gend)
d <- melt(gend)
ggplot(data = d,mapping = aes(x = gender, 
                              y = value, fill = variable)) + 
  geom_col(position = position_dodge())


# check if the difference betwween the mean of math.score is significant
t.test(math.score ~ gender, data = sp, paired = FALSE)

# p-value is much less than 0.05, hence the different statistically significant

# check if there is a difference of mean between girls and boys reading score
gend <- sp%>% select(gender, reading.score)
gend <- gend %>% group_by(gender) %>% summarise_all(mean)
gend <- data.frame(gend)
d <- melt(gend)
ggplot(data = d,mapping = aes(x = gender, 
                              y = value, fill = variable)) + 
  geom_col(position = position_dodge())

# check if the difference betwween the mean of reading.scoreis significant
t.test(reading.score ~ gender, data = sp, paired = FALSE)
# p-value is much less than 0.05, hence the different statistically significant

# USe only columns that have less than or equal to five categories

# One hot encode the categorical variables for medeling in R
table_sp <- one_hot(data.table(sp), dropCols = TRUE)

# Convert back to data frame (I am more comfortable)
sp = data.frame(table_sp)

# Print the correlation between all the variables with the response variable

for (i in names(sp)){
  correlation_res <- cor(sp[[i]], sp$math.score)
  print(c("corr with respose of", i, correlation_res))
}

# Build a simple regression model
linearMod <- lm(math.score ~ ., sp)
print(summary(linearMod))

# get all dependent variables
sp_dependent <- sp
sp_dependent$math.score <- NULL
predicted <- predict(linearMod, sp_dependent)

print(c("correlation between predicted and actual:", cor(predicted, sp$math.score)))

# Plot actual vs predicted
plot(sp$math.score, predicted)
abline(lm(sp$math.score~predicted))








