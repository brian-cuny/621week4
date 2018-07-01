library(tidyverse)
library(caret)

insurance <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621week4\\insurance_training_data.csv') %>%
  select(-INDEX) %>% #don't need index
  mutate(INCOME = as.numeric(str_replace_all(INCOME, '\\D', '')),
         PARENT1 = factor(ifelse(PARENT1 == 'Yes', 1, 0)),
         HOME_VAL = as.numeric(str_replace_all(HOME_VAL, '\\D', '')),
         MSTATUS = factor(ifelse(MSTATUS == 'Yes', 1, 0)),
         MALE = ifelse(SEX == 'M', 1, 0),
         EDUCATION = factor(ifelse(EDUCATION == 'z_High School', 'High School', EDUCATION)),
         JOB = factor(ifelse(JOB == 'z_Blue Collar', 'Blue Collar', JOB)),
         PRIVATE = factor(ifelse(CAR_USE == 'Private', 1, 0)),
         BLUEBOOK = as.numeric(str_replace_all(BLUEBOOK, '\\D', '')),
         CAR_TYPE = factor(ifelse(CAR_TYPE == 'z_SUV', 'SUV', CAR_TYPE)),
         RED_CAR = factor(ifelse(RED_CAR == 'yes', 1, 0)),
         OLDCLAIM = as.numeric(str_replace_all(OLDCLAIM, '\\D', '')),
         REVOKED = factor(ifelse(REVOKED == 'Yes', 1, 0)),
         URBAN = factor(ifelse(URBANICITY == 'Highly Urban/ Urban', 1, 0))) %>%
  select(-SEX, -URBANICITY, -CAR_USE)

#small number of missing values, impute them
insurance %>%
  map_dbl(~sum(is.na(.))/nrow(insurance))

VIM::aggr(insurance[, c(-1, -2)], col=c('navyblue', 'yellow'),
          numbers=TRUE, sortVars=TRUE, 
          labels=names(insurance[, c(-1, -2)]), cex.axis=.7,
          gap=3, ylab=c('Missing Data', 'Pattern'), combined=TRUE)

library(mice)
imputed.data <- mice::mice(insurance[, c(-1, -2)], m=5, maxit=50, method='pmm', seed=500)

#imputed values, no more missing
insurance.complete <- cbind(insurance[, c(1, 2)], complete(imputed.data, 1))


#logistic training set for TARGET_FLAG
part <- caret::createDataPartition(insurance.complete$TARGET_FLAG, p=0.8, list=FALSE)
log.training <- insurance.complete[, -2] %>%
  filter(row_number() %in% part)
log.testing <- insurance.complete[, -2] %>%
  filter(!row_number() %in% part)

#linear training set for TARGET_AMT
lm.insurance.complete <- insurance.complete %>%
  filter(TARGET_AMT != 0)
part <- caret::createDataPartition(lm.insurance.complete$TARGET_AMT, p=0.8, list=FALSE)
lin.training <- lm.insurance.complete[, -1] %>%
  filter(row_number() %in% part)
lin.testing <- lm.insurance.complete[, -1] %>%
  filter(!row_number() %in% part)

ggplot(lin.training, aes(TRAVTIME, log(TARGET_AMT))) +
  geom_point() +
  geom_smooth(method='lm')

#KIDSDRIV not continuous...what do?
#HOMEKIDS not continuous
#YOJ not continous




