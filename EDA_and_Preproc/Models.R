library(ggplot2)
library(plotly)
library(dplyr)
library(survey)
library(formattable)
library(tidyr)
library(data.table)
library(randomForest)
library(knitr)
library(questionr)
library(stargazer)


cdc_data = read.csv('cdc_data_noNA.csv')
cdc_data2 = read.csv('survey_answers.csv')

head(cdc_data)
head(cdc_data2)

sum(is.na(cdc_data))
####### Survey
############## data with Non missing weights
options(survey.lonely.psu = "adjust")

## Survey design
des<-svydesign(ids=~1, strata=~stratum, weights=~weight, data = cdc_data[is.na(cdc_data$weight)==F,] )
des2 = svydesign(ids=~1, strata=~stratum, weights=~weight, data = cdc_data2[is.na(cdc_data2$weight)==F,] )
summary(des)
svymean(~q12, des)
##### Models

#### Simple Linear models
SLR<-lm(q12~raceeth, data=cdc_data, weights = weight)
SLR3<-svyglm(q12~raceeth, des, family=gaussian)
summary(SLR)
summary(SLR3)

############### ALL Weapons (q12, weapons_all)
#### GLM
model.full = lm(q12 ~ ., data =cdc_data)
summary(model.full)

reduced_MLR1 = cdc_data %>% 
  select(raceeth, q2 , q7 , q10 , q12,  q14 , q17 , q18 , q21 , q24 , q25 ,
           q29 , q30 , q31 , q36 , q37 , q39 , q40 , q41 , q43 , q44 , q47  , q51 ,q52 , q54, q55,
           q60, q71, q72, q73, q74, q76, q77, q79, q82, q83, q86, q91,
           q95, q96, q97, q98, q99, age, grade, weight, psu)

no_wights = lm(q12~., data=reduced_MLR1)
summary(no_wights)

model.reduced=lm(q12~., data=reduced_MLR1, weights = weight)
summary(model.reduced)

model.reduced2<-svyglm(q12 ~ q2 + q7 + q10 + q13 + q14 + q17 + q18 + q21 + q24 + q25 +
                        q29 + q30 + q31 + q36 + q37 + q39 + q40 + q41 + q43 + q44 + q47 +q51 +q52 + q54 + q55 +
                       q60 + q71 +q72+ q73+ q74 + q76 + q77 + q79 + q82 + q83 + q86 + q91 +
                     q95 + q96 + q97 + q98 +q99 + psu+ age + grade, data = reduced_MLR1, design = des)
summary(model.reduced2)

model.olr = svyolr(factor(q12) ~ q2 + q7 + q13 + q14 + q17 + q18 + q31 + q36 + q37 + q39 + q40 +  q43 + q44 + q47 +q51 + q54 + 
                     q72+ q73+  q77 +  q82 + q83 + q86 + q91 + q95 +  q97 + q98 + psu+ age + grade, design = des)

summary(model.olr)
head(reduced_MLR1)

### Ordinal logistic regression

olr = svyolr(factor(weapons_all) ~ Sex + indv_weight+ weapons_toschool+phys_fight + phys_fight_sch + 
               smoke_ever + smoke_first + chew_tobac + quit_all_tobac + drink_ever + drink_purchase + 
               drink_binge + marijuana_age + drug_heroin + drug_synth_mj + PE_class + sports + dentist + drug_hallucigens
               + diff_concentration, design = des2)

summary(olr)
(ctable <- coef(summary(olr)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(olr))
confint.default(olr)

exp(coef(olr))
