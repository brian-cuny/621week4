library(tidyverse)
library(caret)

insurance <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621week4\\insurance_training_data.csv') %>%
  select(-INDEX) %>% #don't need index
  mutate(INCOME = as.numeric(str_replace_all(INCOME, '\\D', '')),
         PARENT1.CAT = factor(ifelse(PARENT1 == 'Yes', 1, 0)),
         HOME_VAL = as.numeric(str_replace_all(HOME_VAL, '\\D', '')),
         MSTATUS.CAT = factor(ifelse(MSTATUS == 'Yes', 1, 0)),
         MALE.CAT = factor(ifelse(SEX == 'M', 1, 0)),
         EDUCATION.CAT = factor(ifelse(EDUCATION == 'z_High School', 'High School', EDUCATION)),
         JOB.CAT = factor(ifelse(JOB == 'z_Blue Collar', 'Blue Collar', JOB)),
         PRIVATE.CAT = factor(ifelse(CAR_USE == 'Private', 1, 0)),
         BLUEBOOK = as.numeric(str_replace_all(BLUEBOOK, '\\D', '')),
         CAR_TYPE.CAT = factor(ifelse(CAR_TYPE == 'z_SUV', 'SUV', CAR_TYPE)),
         RED_CAR.CAT = factor(ifelse(RED_CAR == 'yes', 1, 0)),
         OLDCLAIM = as.numeric(str_replace_all(OLDCLAIM, '\\D', '')),
         REVOKED.CAT = factor(ifelse(REVOKED == 'Yes', 1, 0)),
         URBAN.CAT = factor(ifelse(URBANICITY == 'Highly Urban/ Urban', 1, 0))) %>%
  select(-SEX, -URBANICITY, -CAR_USE, -MSTATUS, -PARENT1, -JOB, -CAR_TYPE, -REVOKED, -RED_CAR, -EDUCATION)

#small number of missing values, impute them
insurance %>%
  map_dbl(~sum(is.na(.))/nrow(insurance))

VIM::aggr(insurance[, c(-1, -2)], col=c('navyblue', 'yellow'),
          numbers=TRUE, sortVars=TRUE, 
          labels=names(insurance[, c(-1, -2)]), cex.axis=.7,
          gap=3, ylab=c('Missing Data', 'Pattern'), combined=TRUE)

set.seed(123)
library(mice)
imputed.data <- mice::mice(insurance[, c(-1, -2)], m=5, maxit=50, method='pmm', seed=500)

#imputed values, no more missing
insurance.complete <- cbind(insurance[, c(1, 2)], complete(imputed.data, 1))


#logistic training set for TARGET_FLAG
set.seed(1)
part <- caret::createDataPartition(insurance.complete$TARGET_FLAG, p=0.8, list=FALSE)
log.training <- insurance.complete[, -2] %>%
  filter(row_number() %in% part)
log.testing <- insurance.complete[, -2] %>%
  filter(!row_number() %in% part)


# logistic regression -----------------------------------------------------

Predictor.Discrete.Disp <- function(x){
  require(gridExtra)
  plot.1 <- ggplot(log.training, aes_string(x)) +
    geom_bar()
  
  plot.2 <- 
    log.training %>%
    mutate(TARGET_FLAG = factor(TARGET_FLAG)) %>%
    ggplot(aes_string('TARGET_FLAG', x)) +
    geom_boxplot() +
    geom_jitter(alpha=0.2)
  
  grid.arrange(plot.1, plot.2, ncol=2)
}

Predictor.Disp <- function(x){
  require(gridExtra)
  plot.1 <- ggplot(log.training, aes_string(x)) +
    geom_density()
  
  plot.2 <- 
    log.training %>%
    mutate(TARGET_FLAG = factor(TARGET_FLAG)) %>%
    ggplot(aes_string('TARGET_FLAG', x)) +
    geom_boxplot() +
    geom_point(alpha=0.2)
  
  grid.arrange(plot.1, plot.2, ncol=2)
}

#predictor exploration

Predictor.Discrete.Disp('KIDSDRIV')
#Kidsdriv is discrete, heavily skewed as nearly all policy holders have 0 kids driving.
#can't use boxcox as it requires positive data

Predictor.Disp('AGE')
#nearly normal, no outliers. Good to go

Predictor.Discrete.Disp('HOMEKIDS')
#Like Kidsdriv, this is discrete, heavily skewed and mostly 0.

Predictor.Disp('YOJ')
#Nearly normal other than people without jobs. Boxplots indicate no meaning in the data

Predictor.Disp('INCOME')
#heavily skewed, no real difference in boxplot

geoR::boxcoxfit(log.training$INCOME + 1)
#suggests log transformation

log.training <- log.training %>%
  mutate(l.INCOME = log(INCOME + 1))
Predictor.Disp('l.INCOME')
#nicely normal, against expect for people without jobs

cor(log.training$YOJ, log.training$INCOME)
#decent correlation between YOJ and INCOME, may need to be addressed

#PARENT1, categorical data

Predictor.Disp('HOME_VAL')
#nearly normal except for people who do not own homes

geoR::boxcoxfit(log.training$HOME_VAL + 1)
#not log transform. Maybe make categorical?

table(SINGLE_PARENT=log.training$PARENT1, MARRIED=log.training$MSTATUS)
table(KIDS = log.training$HOMEKIDS, MARRIED=log.training$MSTATUS)

#Married w/children
#Married wo/children
#Not married w/ children
#Not married wo/ children

#what if i combined homekids, parent1 and mstatus into 1 categorical variable that is 0: single, no kids 1: single, w/ kids, 2: married, no kids, 3: married, w/kids

log.training <- log.training %>%
  mutate(HOMELIFE.CAT = factor(ifelse(HOMEKIDS == 0 & MSTATUS.CAT == 0, 0, ifelse(HOMEKIDS != 0 & MSTATUS.CAT == 0, 1, ifelse(HOMEKIDS == 0 & MSTATUS.CAT == 1, 2, 3))))) %>%
  select(-HOMEKIDS, -MSTATUS.CAT)

#HOMELIFE is categorical now

#EDUCATION is categorical

#JOB is categorical

Predictor.Disp('TRAVTIME')
#nearly normal, no apparent difference in boxplot

geoR::boxcoxfit(log.training$TRAVTIME)
#maybe sqrt transformation?

Predictor.Disp('BLUEBOOK')
#slightly skew, no apparent different in boxplot

geoR::boxcoxfit(log.training$BLUEBOOK)
#maybe sqrt transformation?

Predictor.Disp('TIF')

geoR::boxcoxfit(log.training$TIF)
#can be made normal with transformation

#cartype is categorical

#red_car is categorical

Predictor.Disp('OLDCLAIM')
#change to categorical

log.training <- log.training %>%
  mutate(OLDCLAIM.CAT = factor(ifelse(OLDCLAIM == 0, 0, 1)))

#oldclaim.cat is categorical

#claim frequency is oldclaim.cat, can be dropped

#male is categorical, derived from sex

#private is categorical, derived from car_use

#urban is categorical, derived from urbanicity


#Model 1 -- all categorical

model.1 <- glm(TARGET_FLAG ~ MALE.CAT + EDUCATION.CAT + JOB.CAT + PRIVATE.CAT + CAR_TYPE.CAT + REVOKED.CAT + URBAN.CAT + HOMELIFE.CAT + OLDCLAIM.CAT,
               family=binomial, data=log.training)
summary(model.1)

drop1(model.1) #suggestions dropping none
MASS::stepAIC(model.1) #only drops RED_CAR
MASS::stepAIC(model.1, k=log(nrow(log.training))) #drops RED_CAR and JOB 
#lasso suggests dropping RED_CAR and JOB lawyer and CAR_TYPE Panel_truck
#manual dropping suggests RED_CAR only

library(lars)
model.1.lasso <- lars(model.matrix(~ MALE.CAT + EDUCATION.CAT + JOB.CAT + PRIVATE.CAT + CAR_TYPE.CAT + 
                                     RED_CAR.CAT + REVOKED.CAT + URBAN.CAT + HOMELIFE.CAT + OLDCLAIM.CAT, log.training), log.training$TARGET_FLAG)
plot(model.1.lasso)
set.seed(123)
cvlmod <- cv.lars(model.matrix(~ MALE.CAT + EDUCATION.CAT + JOB.CAT + PRIVATE.CAT + CAR_TYPE.CAT + 
                    RED_CAR.CAT + REVOKED.CAT + URBAN.CAT + HOMELIFE.CAT + OLDCLAIM.CAT, log.training), log.training$TARGET_FLAG)
cvlmod$index[which.min(cvlmod$cv)] #0.9292929
predict(model.1.lasso, s=0.9292929, type='coef', mode='fraction')$coef

car::vif(model.1) #correlation isn't an issue

#Model 2 -- everything



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




