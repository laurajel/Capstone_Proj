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


### TRAIN full ############################################################ q12

### Reduced Ordinal logistic regression Survey##########
## TRAIN
olr.red_train = svyolr(weapons_all ~ raceeth + grade_descript + weapons_toschool + phys_fight + grade + age + 
                         sex_id + Sex, design = des2)
summary(olr.red_train)
(ctable <- coef(summary(olr.red_train)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(olr.red_train))
confint.default(olr.red_train)
exp(coef(olr.red_train))

### importance carat train
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(weapons_all~raceeth + grade_descript + weapons_toschool + phys_fight + grade + age + 
                 sex_id + Sex, data=train, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


## TEST
olr.red_test = svyolr(weapons_all ~ raceeth + grade_descript + weapons_toschool + phys_fight + grade + age + 
                        sex_id + Sex, design = des3)
summary(olr.red_test)
(ctable <- coef(summary(olr.red_test)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table confidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(olr.red_test))
confint.default(olr.red_test)
exp(coef(olr.red_test))


########################################################### q14

### TRAIN full #######################

### Reduced Ordinal logistic regression Survey##########
## TRAIN
olr.red_train = svyolr(weapons_gun ~ raceeth + height + indv_weight +
                         unsafe + inj_weapon + phys_fight + sex_id + age + weight_percept + grade + Sex, design = des2)

summary(olr.red_train)
(ctable <- coef(summary(olr.red_train)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(olr.red_train))
confint.default(olr.red_train)
exp(coef(olr.red_train))

### importance carat train
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(weapons_gun ~ raceeth + height + indv_weight +
                 unsafe + inj_weapon + phys_fight + sex_id + age + weight_percept + grade + Sex,
               data=train, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

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

##################################################q17
colnames(cdc_data2)
### TRAIN full #######################

### Reduced Ordinal logistic regression Survey##########
## TRAIN
olr.red_train = svyolr(phys_fight~ raceeth + Sex + age + grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                         weapons_all + weapons_toschool + weapons_gun + unsafe + inj_weapon + viol_dating1 + bullied_sch + bullied_elec +
                         sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                         marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise,
                          design = des2)

summary(olr.red_train)
(ctable <- coef(summary(olr.red_train)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(olr.red_train))
confint.default(olr.red_train)
exp(coef(olr.red_train))

### importance carat train
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(phys_fight~ raceeth + Sex + age + grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                 weapons_all + weapons_toschool + weapons_gun + unsafe + inj_weapon + viol_dating1 + bullied_sch + bullied_elec +
                 sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                 marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise,
               data=train, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

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

##################################################q20
colnames(cdc_data2)
### TRAIN full #######################

### Reduced Ordinal logistic regression Survey##########
## TRAIN
olr.red_train = svyolr(viol_dating2~ raceeth + Sex + age + grade + height + indv_weight + 
                         weapons_all + weapons_toschool + unsafe + inj_weapon + bullied_sch + bullied_elec +
                         sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days + sex_drugs + 
                         marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise + sex_contact,
                       design = des2)

summary(olr.red_train)
(ctable <- coef(summary(olr.red_train)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(olr.red_train))
confint.default(olr.red_train)
exp(coef(olr.red_train))

### importance carat train
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(viol_dating2~ raceeth + Sex + age + grade + height + indv_weight + 
                 weapons_all + weapons_toschool + unsafe + inj_weapon + bullied_sch + bullied_elec +
                 sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days + sex_drugs + 
                 marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise,
               data=train, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

colnames(train)

################################################### VIol dating 1
unique(train$viol_dating1)

levels(train$viol_dating1)[1] = "0"
levels(train$viol_dating1)[2] = "1"


glm.train = svyglm(viol_dating1 ~ raceeth + Sex + age + grade + sex_id + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                     weapons_all + weapons_toschool + weapons_gun + unsafe + inj_weapon + bullied_sch + bullied_elec +
                     sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                     marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise + sex_contact,
                   design=des2, family=binomial)
summary(glm.train)

### importance carat train
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(viol_dating1 ~ raceeth + Sex + age + grade + sex_id + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                 weapons_all + weapons_toschool + weapons_gun + unsafe + inj_weapon + bullied_sch + bullied_elec +
                 sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                 marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise,
               data=train, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance) 

####################################################q23
unique(train$bullied_sch)

levels(train$bullied_sch)[1] = "0"
levels(train$bullied_sch)[2] = "1"



glm.train = svyglm(bullied_sch ~ raceeth + Sex + age + grade + sex_id + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                     weapons_all + weapons_toschool + weapons_gun + unsafe + inj_weapon + viol_dating1 + bullied_sch + bullied_elec +
                     sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                     marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise,
                    design=des2, family=binomial)
summary(glm.train)  
  
  
### importance carat train
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(bullied_sch ~ raceeth + Sex + age + grade + sex_id + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                 weapons_all + weapons_toschool + weapons_gun + unsafe + inj_weapon + viol_dating1 + bullied_sch + bullied_elec +
                 sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                 marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise,
               data=train, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance) 

################################################# q24  
unique(train$bullied_elec)

levels(train$bullied_elec)[1] = "0"
levels(train$bullied_elec)[2] = "1"
  
glm.train = svyglm(bullied_elec ~ raceeth + Sex + age + grade + sex_id + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                     weapons_all + weapons_toschool + weapons_gun + unsafe + inj_weapon + viol_dating1 + bullied_sch + bullied_elec +
                     sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                     marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise,
                   design=des2, family=binomial)
summary(glm.train)


### importance carat train
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(bullied_elec ~ raceeth + Sex + age + grade + sex_id + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                 weapons_all + weapons_toschool + weapons_gun + unsafe + inj_weapon + viol_dating1 + bullied_sch + bullied_elec +
                 sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                 marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise,
               data=train, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance) 
  
