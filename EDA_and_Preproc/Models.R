library(dplyr)
library(survey)
library(tidyr)
library(knitr)
library(questionr)
library(stargazer)
library(mlbench)
library(caret)
library(carData)
library(MASS)
library(caTools)
library(randomForest)

options(max.print=100000)
cdc_data2 = read.csv('survey_answers.csv')


head(cdc_data2)

sum(is.na(cdc_data2))
colnames(cdc_data2)

cdc_split = sample.split(cdc_data2,SplitRatio = 0.3)
train = subset(cdc_data2,cdc_split==TRUE)
test = subset(cdc_data2,cdc_split==FALSE)



####### Survey
############## data with Non missing weights
options(survey.lonely.psu = "adjust")

## Survey design
des = svydesign(ids=~1, strata=~stratum, weights=~weight, data = cdc_data2[is.na(cdc_data2$weight) == F,])
des2 = svydesign(ids=~1, strata=~stratum, weights=~weight, data = train[is.na(train$weight) == F,])
des3 = svydesign(ids=~1, strata=~stratum, weights=~weight, data = test[is.na(test$weight) == F,])
svymean(~weapons_all, des)

colnames(cdc_data2)

##### Models
set.seed(7)


### TRAIN full #######################
olr.red_full = svyolr(weapons_all~. -psu-stratum-X-X.1, des2)
summary(olr.red_full)

(ctable <- coef(summary(olr.red_full)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))

## combined table xonfidence intervals and odds ratios
(ci <- confint(olr.red_full))
confint.default(olr.red_full)
exp(coef(olr.red_full))


### Reduced Ordinal logistic regression Survey##########
## TRAIN
olr.red_train = svyolr(weapons_all ~ raceeth +  weapons_toschool + weapons_gun + phys_fight + sad + smoke_ever + drv_text_email + phys_fight_sch + bullied_sch + bullied_elec +
                         smoke_days + vape_days + drink_days + chew_tobac + drug_inhalents + drug_cocaine + drug_X + drug_heroin + grade_descript + drug_hallucigens + sex_id + age +
                         drug_meth + drug_X + weight_percept + grade + 
                         sports + Sex, design = des2)

summary(olr.red_train)
(ctable <- coef(summary(olr.red_train)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(olr.red_train))
confint.default(olr.red_train)
exp(coef(olr.red_train))

## Identifying feature importance based on randomForest
fit_rf = randomForest(weapons_all~raceeth +  weapons_toschool + weapons_gun + phys_fight + sad + smoke_ever + drv_text_email + phys_fight_sch + bullied_sch + bullied_elec +
                        smoke_days + vape_days + drink_days + chew_tobac + drug_inhalents + drug_cocaine + drug_X + drug_heroin + grade_descript + drug_hallucigens + sex_id + age +
                        drug_meth + drug_X + weight_percept + grade + 
                        sports + Sex, data = train)
# Create an importance based on mean decreasing gini
importance(fit_rf)
varImpPlot(fit_rf)
print(fit_rf)

## TEST
olr.red_test = svyolr(weapons_all ~ raceeth +  weapons_toschool + weapons_gun + phys_fight + sad + smoke_ever + drv_text_email + phys_fight_sch + bullied_sch + bullied_elec +
                        smoke_days + vape_days + drink_days + chew_tobac + drug_inhalents + drug_cocaine + drug_X + drug_heroin + grade_descript + drug_hallucigens + sex_id + age +
                        drug_meth + drug_X + weight_percept + grade + 
                        sports + Sex, design = des3)
summary(olr.red_test)
(ctable <- coef(summary(olr.red_test)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table confidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(olr.red_test))
confint.default(olr.red_test)
exp(coef(olr.red_test))


##################################################q14

### TRAIN full #######################

olr.red_full = svyolr(weapons_gun~. -psu-stratum-X-X.1, des2)
summary(olr.red_full)
(ctable <- coef(summary(olr.red_full)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(olr.red_full))
confint.default(olr.red_full)
exp(coef(olr.red_full))


### Reduced Ordinal logistic regression Survey##########
## TRAIN
olr.red_train = svyolr(weapons_gun ~ raceeth +  weapons_toschool + phys_fight  + smoke_ever + drv_text_email + phys_fight_sch +
                         smoke_days +  drink_days +  grade_descript + sex_id + age +
                          weight_percept + grade + Sex, design = des2)

summary(olr.red_train)
(ctable <- coef(summary(olr.red_train)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(olr.red_train))
confint.default(olr.red_train)
exp(coef(olr.red_train))

## Identifying feature importance based on randomForest
fit_rf = randomForest(weapons_gun ~ raceeth +  weapons_toschool + phys_fight  + smoke_ever + drv_text_email + phys_fight_sch +
                        smoke_days +  drink_days +  grade_descript + sex_id + age +
                        weight_percept + grade + Sex, data = train)
# Create an importance based on mean decreasing gini
importance(fit_rf)
varImpPlot(fit_rf)
print(fit_rf)

## TEST
olr.red_test = svyolr(weapons_gun ~raceeth + Hispanic.Latino + weapons_toschool + weapons_gun + phys_fight + sad + smoke_ever +
                        smoke_first + vape_purchase + chew_tobac + drug_inhalents + drug_heroin + drug_meth + drug_X + weight_percept +
                        sports + Sex, design = des3)
summary(olr.red_test)
(ctable <- coef(summary(olr.red_test)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table confidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(olr.red_test))
confint.default(olr.red_test)
exp(coef(olr.red_test))

##################################################q16
colnames(cdc_data2)
### TRAIN full #######################

olr.red_full = svyolr(inj_weapon~. -X-psu-weight-stratum, design = des2)
summary(olr.red_full)
(ctable <- coef(summary(olr.red_full)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(olr.red_full))
confint.default(olr.red_full)
exp(coef(olr.red_full))


### Reduced Ordinal logistic regression Survey##########
## TRAIN
olr.red_train = svyolr(weapons_gun ~ smoke_days + vape_purchase + marijuana + drug_X + drugs_sch + sex_partners + PE_class +
                         + tone_muscles + weapons_all,  design = des2)

summary(olr.red_train)
(ctable <- coef(summary(olr.red_train)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(olr.red_train))
confint.default(olr.red_train)
exp(coef(olr.red_train))

## Identifying feature importance based on randomForest
fit_rf = randomForest(weapons_gun ~ smoke_days + vape_purchase + marijuana + drug_X + drugs_sch + sex_partners + PE_class +
                        + tone_muscles + weapons_all, data = train)
# Create an importance based on mean decreasing gini
importance(fit_rf)
varImpPlot(fit_rf)
print(fit_rf)

## TEST
olr.red_test = svyolr(weapons_all ~raceeth + Hispanic.Latino + weapons_toschool + weapons_gun + phys_fight + sad + smoke_ever +
                        smoke_first + vape_purchase + chew_tobac + drug_inhalents + drug_heroin + drug_meth + drug_X + weight_percept +
                        sports + Sex, design = des3)
summary(olr.red_test)
(ctable <- coef(summary(olr.red_test)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table confidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(olr.red_test))
confint.default(olr.red_test)
exp(coef(olr.red_test))

