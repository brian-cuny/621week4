library(tidyverse)
library(caret)

insurance <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621week4\\insurance_training_data.csv') %>%
  select(-INDEX) %>% #don't need index
  mutate(TARGET_FLAG = factor(TARGET_FLAG),
         INCOME = as.numeric(str_replace_all(INCOME, '\\D', '')),
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

table(KIDSDRIV = log.training$KIDSDRIV, TARGET=log.training$TARGET_FLAG)


Predictor.Disp('AGE')
#nearly normal, no outliers. Good to go
#don't anticipate this helping as the data is nearly identical in both distributions


Predictor.Disp('YOJ')
#Nearly normal other than people without jobs. Boxplots indicate no meaning in the data
#add logs odds?

Predictor.Disp('INCOME')
#skewed, equal variance, no real difference in boxplot

geoR::boxcoxfit(log.training$INCOME+1)
#suggests sqrt transformation


Predictor.Disp('l.INCOME')
#nicely normal, against expect for people without jobs

Predictor.Disp('HOME_VAL')
#nearly normal except for people who do not own homes

geoR::boxcoxfit(log.training$HOME_VAL + 1)
#log transformation

Predictor.Disp('l.HOME_VAL')

Predictor.Disp('TRAVTIME')
#nearly normal, no apparent difference in boxplot

geoR::boxcoxfit(log.training$TRAVTIME)
#maybe sqrt transformation?

Predictor.Disp('s.TRAVTIME')

Predictor.Disp('BLUEBOOK')
#slightly skew, no apparent different in boxplot

geoR::boxcoxfit(log.training$BLUEBOOK)
#maybe sqrt transformation?

Predictor.Disp('TIF')

geoR::boxcoxfit(log.training$TIF)
#can be made normal with transformation

Predictor.Disp('OLDCLAIM')
#heavily, heavily skewed. Most people don't make claims.

Predictor.Discrete.Disp('CLM_FREQ')

Predictor.Disp('MVR_PTS')
#heavily skewed, apparent difference

Predictor.Disp('CAR_AGE')
#bimodal, equal variance

#MALE.CAT
table(MALE = log.training$MALE.CAT, TARGET=log.training$TARGET_FLAG)

#Education.CAT
table(EDUCATION = log.training$EDUCATION.CAT, TARGET = log.training$TARGET_FLAG)

#PRIVATE.CAT
table(PRIVATE = log.training$PRIVATE.CAT, TARGET = log.training$TARGET_FLAG)

#CAR_TYPE.CAT
table(CAR_TYPE = log.training$CAR_TYPE.CAT, TARGET = log.training$TARGET_FLAG)

#RED_CAR.CAT
table(RED_CAR = log.training$RED_CAR.CAT, TARGET = log.training$TARGET_FLAG)

#REVOKED.CAT
table(REVOKED = log.training$REVOKED.CAT, TARGET = log.training$TARGET_FLAG)

#URBAN.CAT
table(URBAN = log.training$URBAN.CAT, TARGET = log.training$TARGET_FLAG)

#Model 1 -- all categorical
log.training <- log.training %>%
  mutate(TARGET_FLAG = factor(TARGET_FLAG))

ctrl <- trainControl(method='repeatedcv', number=10, savePredictions=TRUE)
mod_fit <- train(TARGET_FLAG ~ PARENT1.CAT + MSTATUS.CAT + MALE.CAT + EDUCATION.CAT + JOB.CAT + PRIVATE.CAT + CAR_TYPE.CAT + 
                   REVOKED.CAT + URBAN.CAT, data=log.training, method='glm', family='binomial',
                 trControl=ctrl, tuneLength=5)
summary(mod_fit)

model.1.full <- glm(TARGET_FLAG ~ PARENT1.CAT + MSTATUS.CAT + MALE.CAT + EDUCATION.CAT + JOB.CAT + PRIVATE.CAT + CAR_TYPE.CAT + 
                      RED_CAR.CAT + REVOKED.CAT + URBAN.CAT, family=binomial, data=log.training)
summary(model.1.full)

model.1 <- glm(TARGET_FLAG ~ PARENT1.CAT + MSTATUS.CAT + MALE.CAT + EDUCATION.CAT + JOB.CAT + PRIVATE.CAT + CAR_TYPE.CAT + 
                 REVOKED.CAT + URBAN.CAT, data=log.training)
summary(model.1)

drop1(model.1.full) #suggestions dropping none
MASS::stepAIC(model.1.full) #only drops RED_CAR
MASS::stepAIC(model.1.full, k=log(nrow(log.training))) #drops RED_CAR
#lasso suggests dropping JOB lawyer and CAR_TYPE Panel_truck, which we can't do
#manual dropping suggests RED_CAR only

library(lars)
model.1.lasso <- lars(model.matrix(~ PARENT1.CAT + MSTATUS.CAT + MALE.CAT + EDUCATION.CAT + JOB.CAT + PRIVATE.CAT + CAR_TYPE.CAT + 
                                     RED_CAR.CAT + REVOKED.CAT + URBAN.CAT, log.training), as.numeric(log.training$TARGET_FLAG))
plot(model.1.lasso)
set.seed(123)
cvlmod <- cv.lars(model.matrix(~ PARENT1.CAT + MSTATUS.CAT + MALE.CAT + EDUCATION.CAT + JOB.CAT + PRIVATE.CAT + CAR_TYPE.CAT + 
                                 RED_CAR.CAT + REVOKED.CAT + URBAN.CAT, log.training), as.numeric(log.training$TARGET_FLAG))
cvlmod$index[which.min(cvlmod$cv)] #0.9292929
predict(model.1.lasso, s=0.9292929, type='coef', mode='fraction')$coef

#Following the various suggestions, we will only drop RED_CAR.
anova(model.1, test='Chisq')

car::vif(model.1) #correlation isn't an issue

#This is a valid model and our first one

#Model 2 -- everything

#possible transformations
log.training.mod <- log.training %>%
  mutate(l.YOJ = log(YOJ + 1),
         s.INCOME = sqrt(INCOME),
         l.HOME_VAL = log(HOME_VAL + 1),
         s.TRAVTIME = sqrt(TRAVTIME))

log.training <- log.training %>%
  mutate(s.INCOME = sqrt(INCOME),
         s.BLUEBOOK = sqrt(BLUEBOOK))

model.2.full <- glm(TARGET_FLAG ~ KIDSDRIV + I(KIDSDRIV ^ 2) + AGE + HOMEKIDS + I(HOMEKIDS ^ 2) + YOJ + INCOME + 
                      HOME_VAL + TRAVTIME + BLUEBOOK + TIF + OLDCLAIM + CLM_FREQ + I(CLM_FREQ ^ 2) +
                      MVR_PTS + I(MVR_PTS ^ 2) + CAR_AGE + PARENT1.CAT + MSTATUS.CAT + MALE.CAT + EDUCATION.CAT + JOB.CAT +
                      PRIVATE.CAT + CAR_TYPE.CAT + RED_CAR.CAT + REVOKED.CAT + URBAN.CAT, data=log.training, family=binomial)
summary(model.2.full)

model.2 <- glm(TARGET_FLAG ~ . +I(KIDSDRIV^2) -AGE -YOJ +I(CLM_FREQ^2) -MVR_PTS -CAR_AGE -MALE.CAT -JOB.CAT -RED_CAR.CAT +I(MVR_PTS^2), 
               data=log.training, family=binomial)
summary(model.2)

drop1(model.2.full) #doesn't want to drop any -- 2nd pass still drops none
MASS::stepAIC(model.2.full) #drops AGE, YOJ, CAR_AGE, MALE.CAT, RED_CAR -- 2nd pass drops AGE, HOMEKIDS^2, YOJ, MVR_PTS, CAR_AGE, MALE.CAT, RED_CAR
MASS::stepAIC(model.2.full, k=log(nrow(log.training))) #drops AGE, YOJ, CAR_AGE, MALE.CAT, JOB, RED_CAR -- 2nd pass drops KIDSDRIV^2, AGE, HOMEKIDS, HOMEKIDS^2, 
                                                       #YOJ, CLM_FREQ^2, MVR_PTS, CAR_AGE, MALE.CAT, JOB.CAT, RED_CAR
#lasso suggests nothing
#manual dropping suggests RED_CAR, AGE, YOJ, CAR_AGE, MALE.CAT, HOMEKIDS

#Following stepAIC we will remove 5 variables.
anova(model.2.full, model.2)

library(lars)
model.2.lasso <- lars(model.matrix(~ KIDSDRIV + AGE + HOMEKIDS + YOJ + INCOME + HOME_VAL + TRAVTIME + BLUEBOOK + TIF + OLDCLAIM + CLM_FREQ +
                                     MVR_PTS + CAR_AGE + PARENT1.CAT + MSTATUS.CAT + MALE.CAT + EDUCATION.CAT + JOB.CAT +
                                     PRIVATE.CAT + CAR_TYPE.CAT + RED_CAR.CAT + REVOKED.CAT + URBAN.CAT, log.training), log.training$TARGET_FLAG)
plot(model.1.lasso)
set.seed(123)
cvlmod <- cv.lars(model.matrix(~ KIDSDRIV + AGE + HOMEKIDS + YOJ + INCOME + HOME_VAL + TRAVTIME + BLUEBOOK + TIF + OLDCLAIM + CLM_FREQ +
                                 MVR_PTS + CAR_AGE + PARENT1.CAT + MSTATUS.CAT + MALE.CAT + EDUCATION.CAT + JOB.CAT +
                                 PRIVATE.CAT + CAR_TYPE.CAT + RED_CAR.CAT + REVOKED.CAT + URBAN.CAT, log.training), log.training$TARGET_FLAG)
cvlmod$index[which.min(cvlmod$cv)] #0.9292929
predict(model.1.lasso, s=0.9292929, type='coef', mode='fraction')$coef

#Verifying model

model.2.check <- log.training %>%
  select(-TARGET_FLAG, -AGE, -YOJ, -CAR_AGE, -MALE.CAT, -RED_CAR.CAT) %>%
  mutate(KIDSDRIV2 = KIDSDRIV*KIDSDRIV,
         CLM_FREQ2 = CLM_FREQ*CLM_FREQ,
         MVR_PTS2 = MVR_PTS*MVR_PTS) %>%
  select_if(is.numeric)
predictors <- colnames(model.2.check)
probability <- predict(model.2, type='response')

model.2.check <- model.2.check %>%
  mutate(logit = log(probability/(1-probability))) %>%
  gather(key='predictors', value='predictor.value') 

ggplot(model.2.check, aes(logit, predictor.value)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method='loess') +
  theme_bw() +
  facet_wrap(~predictors, scales = 'free_y')

#suggests that we add a quadradic term for clm_freq, homekids, kidsdriv, and mvr_pts
car::vif(model.2)
car::marginalModelPlots(model.2)

Marginal.Model <- function(p){
  yhat <- predict(model.2, type='response')
  p <- ggplot(log.training, aes_string(p, 'TARGET_FLAG')) +
    geom_point() + 
    geom_smooth(method='gam', se=FALSE) +
    geom_smooth(aes(y=yhat), method='gam', se = FALSE, color='red', linetype='dotted')
  return(p)
}

alr3::residual.plots(model.2)

Marginal.Model('KIDSDRIV')
Marginal.Model('INCOME')
Marginal.Model('HOME_VAL')
Marginal.Model('TRAVTIME')
Marginal.Model('BLUEBOOK')
Marginal.Model('TIF')
Marginal.Model('OLDCLAIM')
Marginal.Model('CLM_FREQ')

car::vif(model.2)
car::marginalModelPlots(model.2)


#Model 3

#I will use BIC to reduce categories at much as possible.

model.2 <- glm(TARGET_FLAG ~ . +I(KIDSDRIV^2) -AGE -YOJ +I(CLM_FREQ^2) -MVR_PTS -CAR_AGE -MALE.CAT -JOB.CAT -RED_CAR.CAT +I(MVR_PTS^2), 
               data=log.training, family=binomial)
summary(model.2)




p <- ifelse(predict(model.2, newdata=log.testing, type='response') > 0.5, 1, 0)
x <- table(predicted = p, actual = log.testing$TARGET_FLAG)

caret::confusionMatrix(x)


termplot(model.2, partial.resid=TRUE, smooth=panel.smooth, terms=1:6)





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





