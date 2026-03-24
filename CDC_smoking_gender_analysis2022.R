# New project CDC
# load the tidyverse
library(tidyverse)

# Load the file
library(haven)
CDC_hdata <- read_xpt(file.choose("LLCP2022.XPT"))

# Go through data
glimpse(CDC_hdata)
View(CDC_hdata)
dim(CDC_hdata)
str(CDC_hdata)
summary(CDC_hdata)
names(CDC_hdata)

# Research question
#Are males more likely to smoke than females?

# Check the variables
attr(CDC_hdata$SMOKDAY2, "label")
attr(CDC_hdata$SMOKE100, "label")
attr(CDC_hdata$COLGSEX1, "label")
attr(CDC_hdata$CELLSEX1, "label")

# check the number of people in each variable
table(CDC_hdata$COLGSEX1)
table(CDC_hdata$CELLSEX1)

# Using only CELLSEX1 variable , valeaning it and creating a new one with
# 3, 7, 9 replaced by NA

sex_clean <- CDC_hdata$CELLSEX1

sex_clean[sex_clean %in% c(3, 7, 9)] <- NA

sex_clean <- factor(sex_clean,
                    levels = c(1, 2),
                    labels = c("Male", "Female"))

table(sex_clean)

# to create the smoking variable
# check what each variable says
attr(CDC_hdata$SMOKDAY2, "label")
attr(CDC_hdata$SMOKE100, "label")

# check the people
table(CDC_hdata$SMOKDAY2)
table(CDC_hdata$SMOKE100)

# Read the notes in word file for the nexxt step
smoke_binary <- NA

# Assign current smoker as 1
smoke_binary[CDC_hdata$SMOKDAY2 %in% c(1,2)] <- 1

# Assign non current smoker as 0
smoke_binary[CDC_hdata$SMOKE100 == 2 | CDC_hdata$SMOKDAY2 == 3] <- 0

# Convert 0 and 1 into labels
smoke_binary <- factor(smoke_binary,
                       levels = c(0,1),
                       labels = c("No", "Yes"))


# check the variable
table(smoke_binary, useNA = "ifany")
prop.table(table(smoke_binary, useNA = "ifany"))
table(sex_clean)

# Chi - square test
chisq.test(table(sex_clean, smoke_binary))

# logistic regression

model1 <- glm(smoke_binary ~ sex_clean,
           family = binomial )

summary(model1)

# To change the comparision from female to male
sex_clean <- relevel(sex_clean, ref = "Female")

model1 <- glm(smoke_binary ~ sex_clean,
              family = binomial)
summary(model1)

# convert it into odds ratio
OR <- exp(coef(model1))
write.csv(OR, "odds_ratio_results.csv")

CI <- exp(confint(model1))
write.csv(CI, "confidence_intervals.csv")









