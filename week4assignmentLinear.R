library(tidyverse)
library(caret)

#create training/testing partition
set.seed(1)
lm.insurance.complete <- insurance.complete %>%
  filter(TARGET_AMT != 0)
part <- caret::createDataPartition(lm.insurance.complete$TARGET_AMT, p=0.8, list=FALSE)
lin.training <- lm.insurance.complete[, -1] %>%
  filter(row_number() %in% part)
lin.testing <- lm.insurance.complete[, -1] %>%
  filter(!row_number() %in% part)

ggplot(lin.training, aes(PARENT1.CAT, log(TARGET_AMT))) +
  geom_point() +
  geom_smooth(method='lm')

#KIDSDRIV
#somewhat heteroscadastic
#maybe log transform kidsdriv

#AGE is good
#HOMEKIDS is good
#YOJ is good
#log(INCOME + 1) is ok-ish
#log(HOME_VAL+1) is ok-ish
#travtime is good
#log(BLUEBOOK) is good
#tif is good
#log(OLDCLAIM + 1) is good
#CLM_FREQ is good
#MVR_PTS is good
#CAR_AGE is good

#MODEL 1, everything log target

ln.model.1.full <- lm(log(TARGET_AMT) ~ ., data=lin.training)

summary(ln.model.1.full)

MASS::stepAIC(ln.model.1.full, trace=0)

ln.model.1 <- lm(log(TARGET_AMT) ~ BLUEBOOK + CLM_FREQ + MVR_PTS + MSTATUS.CAT + RED_CAR.CAT, data=lin.training)
summary(ln.model.1)
plot(ln.model.1)

temp <- lin.training %>%
  mutate(TARGET_AMT = log(TARGET_AMT))
pairs(temp[, c(1, 9, 12, 13, 16, 22)])

car::mmps(ln.model.1)

#MODEL 2, 
ln.model.2 <- lm(log(TARGET_AMT) ~ log(BLUEBOOK), data=lin.training)
summary(ln.model.2)

anova(ln.model.1, ln.model.2)

car::mmps(ln.model.2)
plot(ln.model.2)

#MODEL 3, robust regression

car::avPlots(ln.model.2)

ln.model.3 <- MASS::rlm(TARGET_AMT ~ ., data=lin.training)
summary(ln.model.3)





