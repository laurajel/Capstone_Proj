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

head(cdc_data)

####### Survey
############## data with Non missing weights
options(survey.lonely.psu = "adjust")

## Survey design
des<-svydesign(ids=~1, strata=~stratum, weights=~weight, data = cdc_data[is.na(cdc_data$weight)==F,] )
summary(des)
svymean(~q13, des)



table.weight = wtd.table(cdc_data$q13, cdc_data$raceeth, weights = cdc_data$weight)
formattable(table.weight)

#### Summary stats ####################
### proportions weighted, unweighted
table = prop.table(wtd.table(cdc_data$q13, cdc_data$raceeth, weights = cdc_data$weight), margin=2)
prop.table(wtd.table(cdc_data$q13, cdc_data$raceeth), margin=2)
formattable(table)

### std. error or percentages
n<-table(is.na(cdc_data$q13)==F)
n

p<-prop.table(wtd.table(cdc_data$q13, cdc_data$raceeth, weights = cdc_data$weight), margin=2)[2,]
formattable(p)
se<-sqrt((p*(1-p))/n)

tble = data.frame(proportion=p, se=se)
formattable(tble)

svy = svytable(~q13+raceeth, design = des)
formattable(svy)

prop.table(svytable(~q13+raceeth, design = des), margin = 2)

svytble = svyby(formula = ~q13, by = ~raceeth, design = des, FUN = svymean)
formattable(svytble)

##### Models

#### Simple Linear models
SLR1<-lm(q13~raceeth, data=cdc_data, subset = )
SLR2<-lm(q13~raceeth, data=cdc_data, weights = weight)
SLR3<-svyglm(q13~raceeth,des, family=gaussian)
summary(SLR2)
summary(SLR1)
summary(SLR3)


#### GLM
model.full = lm(q13 ~ ., data =cdc_data, weights = weight)
summary(model.full)

reduced_MLR1 = cdc_data %>% 
  select(q2, q7,q10,q11,q12,q13,q14,q15,q16,q17,q18,q21,q25,
         q26,q27,q29,q37,q39,q42,q44,q45, q49, q51,q52,q54,q55,q56,q57,q58,
         q66,q70,q72, q83, q90,q91,q93,q98, BMIPCT,weight, sex_id, psu, age, grade )

no_wights = lm(q13~., data=reduced_MLR1)
summary(no_wights)

model.reduced=lm(q13~., data=reduced_MLR1, weights = weight)
summary(model.reduced)

reduced_MLR2<-svyglm(q13 ~ q2 + q7 + q10 + q11 + q12 + q13 + q14 + q15 + q16 + q17 + q18 + q21 + q25 +
                     q26 + q27 + q29 + q37 + q39 + q42 + q44 + q45 + q49 + q51 + q52 + q54 + q55 +q56 +q57 + q58 +
                     q66 + q70 +q72+ q83+ q90+q91+q93+q98+ BMIPCT+weight+ sex_id+ psu+ age+ grade, data = reduced_MLR1, design = des)
summary(reduced_MLR2)



### rnaming vars for Visualizations & MLR 
### 
##########################################################
### Imputing real value names from survey
reduced_MLR1 = reduced_MLR1 %>% 
  mutate(q2 = replace(q2, q2 == 1.0, "Female")) %>%
  mutate(q2 = replace(q2, q2 == 2.0, "Male"))
reduced_MLR1$q2 = as.factor(reduced_MLR1$q2)


reduced_MLR1 = reduced_MLR1 %>%
  mutate(q10 = replace(q10, q10 == 1.0, "I did not drive past 30 days")) %>%
  mutate(q10 = replace(q10, q10 == 2.0, "0 times")) %>%
  mutate(q10 = replace(q10, q10 == 3.0, "1 time")) %>%
  mutate(q10 = replace(q10, q10 == 4.0, "2 or 3 times")) %>%
  mutate(q10 = replace(q10, q10 == 5.0, "4 or 5 times")) %>%
  mutate(q10 = replace(q10, q10 == 6.0, "6 or more times"))
reduced_MLR1$q10 = as.factor(reduced_MLR1$q10)  

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q11 = replace(q11, q11 == 1.0, "Did not drive in last 30 days")) %>%
  mutate(q11 = replace(q11, q11 == 2.0, "0 days")) %>%
  mutate(q11 = replace(q11, q11 == 3.0, "1 or 2 days")) %>%
  mutate(q11 = replace(q11, q11 == 4.0, "3 to 5 days")) %>%
  mutate(q11 = replace(q11, q11 == 5.0, "6 to 9 days")) %>%
  mutate(q11 = replace(q11, q11 == 6.0, "10 or 19 days")) %>%
  mutate(q11 = replace(q11, q11 == 7.0, "20 to 29 days")) %>%
  mutate(q11 = replace(q11, q11 == 8.0, "All 30 days"))
reduced_MLR1$q11 = as.factor(reduced_MLR1$q11)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q12 = replace(q12, q12 == 1.0, "0 days")) %>%
  mutate(q12 = replace(q12, q12 == 2.0, "1 day")) %>%
  mutate(q12 = replace(q12, q12 == 3.0, "2 or 3 days")) %>%
  mutate(q12 = replace(q12, q12 == 4.0, "4 to 5 days")) %>%
  mutate(q12 = replace(q12, q12 == 5.0, "6 or more days")) 
reduced_MLR1$q12 = as.factor(reduced_MLR1$q12)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q13 = replace(q13, q13 == 1.0, "0 days")) %>%
  mutate(q13 = replace(q13, q13 == 2.0, "1 day")) %>%
  mutate(q13 = replace(q13, q13 == 3.0, "2 or 3 days")) %>%
  mutate(q13 = replace(q13, q13 == 4.0, "4 to 5 days")) %>%
  mutate(q13 = replace(q13, q13 == 5.0, "6 or more days")) 
reduced_MLR1$q13 = as.factor(reduced_MLR1$q13)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q14 = replace(q14, q14 == 1.0, "0 days")) %>%
  mutate(q14 = replace(q14, q14 == 2.0, "1 day")) %>%
  mutate(q14 = replace(q14, q14 == 3.0, "2 or 3 days")) %>%
  mutate(q14 = replace(q14, q14 == 4.0, "4 to 5 days")) %>%
  mutate(q14 = replace(q14, q14 == 5.0, "6 or more days")) 
reduced_MLR1$q14 = as.factor(reduced_MLR1$q14)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q15 = replace(q15, q15 == 1.0, "0 days")) %>%
  mutate(q15 = replace(q15, q15 == 2.0, "1 day")) %>%
  mutate(q15 = replace(q15, q15 == 3.0, "2 or 3 days")) %>%
  mutate(q15 = replace(q15, q15 == 4.0, "4 to 5 days")) %>%
  mutate(q15 = replace(q15, q15 == 5.0, "6 or more days")) 
reduced_MLR1$q15 = as.factor(reduced_MLR1$q15)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q16 = replace(q16, q16 == 1.0, "0 times")) %>%
  mutate(q16 = replace(q16, q16 == 2.0, "1 time")) %>%
  mutate(q16 = replace(q16, q16 == 3.0, "2 or 3 times")) %>%
  mutate(q16 = replace(q16, q16 == 4.0, "4 or 5 times")) %>%
  mutate(q16 = replace(q16, q16 == 5.0, "6 or 7 times")) %>%
  mutate(q16 = replace(q16, q16 == 6.0, "8 or 9 times")) %>%
  mutate(q16 = replace(q16, q16 == 7.0, "10 or 11 times")) %>%
  mutate(q16 = replace(q16, q16 == 8.0, "12 or more times"))
reduced_MLR1$q16 = as.factor(reduced_MLR1$q16)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q17 = replace(q17, q17 == 1.0, "0 times")) %>%
  mutate(q17 = replace(q17, q17 == 2.0, "1 time")) %>%
  mutate(q17 = replace(q17, q17 == 3.0, "2 or 3 times")) %>%
  mutate(q17 = replace(q17, q17 == 4.0, "4 or 5 times")) %>%
  mutate(q17 = replace(q17, q17 == 5.0, "6 or 7 times")) %>%
  mutate(q17 = replace(q17, q17 == 6.0, "8 or 9 times")) %>%
  mutate(q17 = replace(q17, q17 == 7.0, "10 or 11 times")) %>%
  mutate(q17 = replace(q17, q17 == 8.0, "12 or more times"))
reduced_MLR1$q17 = as.factor(reduced_MLR1$q17)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q18 = replace(q18, q18 == 1.0, "0 times")) %>%
  mutate(q18 = replace(q18, q18 == 2.0, "1 time")) %>%
  mutate(q18 = replace(q18, q18 == 3.0, "2 or 3 times")) %>%
  mutate(q18 = replace(q18, q18 == 4.0, "4 or 5 times")) %>%
  mutate(q18 = replace(q18, q18 == 5.0, "6 or 7 times")) %>%
  mutate(q18 = replace(q18, q18 == 6.0, "8 or 9 times")) %>%
  mutate(q18 = replace(q18, q18 == 7.0, "10 or 11 times")) %>%
  mutate(q18 = replace(q18, q18 == 8.0, "12 or more times"))
reduced_MLR1$q18 = as.factor(reduced_MLR1$q18)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q21 = replace(q21, q21 == 1.0, "I did not date in the past 12 months")) %>%
  mutate(q21 = replace(q21, q21 == 2.0, "0 times")) %>%
  mutate(q21 = replace(q21, q21 == 3.0, "1 time")) %>%
  mutate(q21 = replace(q21, q21 == 4.0, "2 or 3 times")) %>%
  mutate(q21 = replace(q21, q21 == 5.0, "4 or 5 times")) %>%
  mutate(q21 = replace(q21, q21 == 6.0, "6 or more times"))
reduced_MLR1$q21 = as.factor(reduced_MLR1$q21)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q25 = replace(q25, q25 == 1.0, "Yes")) %>%
  mutate(q25 = replace(q25, q25 == 2.0, "No")) 
reduced_MLR1$q25 = as.factor(reduced_MLR1$q25)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q26 = replace(q26, q26 == 1.0, "Yes")) %>%
  mutate(q26 = replace(q26, q26 == 2.0, "No")) 
reduced_MLR1$q26 = as.factor(reduced_MLR1$q26)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q27 = replace(q27, q27 == 1.0, "Yes")) %>%
  mutate(q27 = replace(q27, q27 == 2.0, "No"))
reduced_MLR1$q27 = as.factor(reduced_MLR1$q27)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q29 = replace(q29, q29 == 1.0, "No attempt in 12 months")) %>%
  mutate(q29 = replace(q29, q29 == 2.0, "Yes")) %>%
  mutate(q29 = replace(q29, q29 == 3.0, "No"))
reduced_MLR1$q29 = as.factor(reduced_MLR1$q29)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q37 = replace(q37, q37 == 1.0, "0 days")) %>%
  mutate(q37 = replace(q37, q37 == 2.0, "1 or 2 days")) %>%
  mutate(q37 = replace(q37, q37 == 3.0, "3 to 5 days")) %>%
  mutate(q37 = replace(q37, q37 == 4.0, "6 to 9 days")) %>%
  mutate(q37 = replace(q37, q37 == 5.0, "10 to 19 days")) %>%
  mutate(q37 = replace(q37, q37 == 6.0, "20 to 29 days")) %>%
  mutate(q37 = replace(q37, q37 == 7.0, "All 30 days")) 
reduced_MLR1$q37 = as.factor(reduced_MLR1$q37)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q39 = replace(q39, q39 == 1.0, "Did not use tobacco products")) %>%
  mutate(q39 = replace(q39, q39 == 2.0, "Yes")) %>%
  mutate(q39 = replace(q39, q39 == 3.0, "No")) 
reduced_MLR1$q39 = as.factor(reduced_MLR1$q39)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q42 = replace(q42, q42 == 1.0, "0 days")) %>%
  mutate(q42 = replace(q42, q42 == 2.0, "1 or 2 days")) %>%
  mutate(q42 = replace(q42, q42 == 3.0, "3 to 5 days")) %>%
  mutate(q42 = replace(q42, q42 == 4.0, "6 to 9 days")) %>%
  mutate(q42 = replace(q42, q42 == 5.0, "10 to 19 days")) %>%
  mutate(q42 = replace(q42, q42 == 6.0, "20 to 29 days")) %>%
  mutate(q42 = replace(q42, q42 == 7.0, "All 30 days"))
reduced_MLR1$q42 = as.factor(reduced_MLR1$q42)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q44 = replace(q44, q44 == 1.0, "0 days")) %>%
  mutate(q44 = replace(q44, q44 == 2.0, "1 day")) %>%
  mutate(q44 = replace(q44, q44 == 3.0, "2 days")) %>%
  mutate(q44 = replace(q44, q44 == 4.0, "3 to 5 days")) %>%
  mutate(q44 = replace(q44, q44 == 5.0, "6 to 9 days")) %>%
  mutate(q44 = replace(q44, q44 == 6.0, "10 to 19 days")) %>%
  mutate(q44 = replace(q44, q44 == 7.0, "20 or more days"))
reduced_MLR1$q44 = as.factor(reduced_MLR1$q44)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q45 = replace(q45, q45 == 1.0, "I did not drink")) %>%
  mutate(q45 = replace(q45, q45 == 2.0, "1 or 2 drinks")) %>%
  mutate(q45 = replace(q45, q45 == 3.0, "3 drinks")) %>%
  mutate(q45 = replace(q45, q45 == 4.0, "4 drinks")) %>%
  mutate(q45 = replace(q45, q45 == 5.0, "5 drinks")) %>%
  mutate(q45 = replace(q45, q45 == 6.0, "6 or 7 drinks")) %>%
  mutate(q45 = replace(q45, q45 == 7.0, "8 or 9 drinks")) %>%
  mutate(q45 = replace(q45, q45 == 8.0, "10 or more drinks"))
reduced_MLR1$q45 = as.factor(reduced_MLR1$q45)


reduced_MLR1 = reduced_MLR1 %>%
  mutate(q49 = replace(q49, q49 == 1.0, "0 times")) %>%
  mutate(q49 = replace(q49, q49 == 2.0, "1 or 2 times")) %>%
  mutate(q49 = replace(q49, q49 == 3.0, "3 to 9 times")) %>%
  mutate(q49 = replace(q49, q49 == 4.0, "10 to 19 times")) %>%
  mutate(q49 = replace(q49, q49 == 5.0, "20 to 39 times")) %>%
  mutate(q49 = replace(q49, q49 == 6.0, "40 or more times"))
reduced_MLR1$q49 = as.factor(reduced_MLR1$q49)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q51 = replace(q51, q51 == 1.0, "0 times")) %>%
  mutate(q51 = replace(q51, q51 == 2.0, "1 or 2 times")) %>%
  mutate(q51 = replace(q51, q51 == 3.0, "3 to 9 times")) %>%
  mutate(q51 = replace(q51, q51 == 4.0, "10 to 19 times")) %>%
  mutate(q51 = replace(q51, q51 == 5.0, "20 to 39 times")) %>%
  mutate(q51 = replace(q51, q51 == 6.0, "40 or more times"))
reduced_MLR1$q51 = as.factor(reduced_MLR1$q51)


reduced_MLR1 = reduced_MLR1 %>%
  mutate(q52 = replace(q52, q52 == 1.0, "0 times")) %>%
  mutate(q52 = replace(q52, q52 == 2.0, "1 or 2 times")) %>%
  mutate(q52 = replace(q52, q52 == 3.0, "3 to 9 times")) %>%
  mutate(q52 = replace(q52, q52 == 4.0, "10 to 19 times")) %>%
  mutate(q52 = replace(q52, q52 == 5.0, "20 to 39 times")) %>%
  mutate(q52 = replace(q52, q52 == 6.0, "40 or more times"))
reduced_MLR1$q52 = as.factor(reduced_MLR1$q52)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q54 = replace(q54, q54 == 1.0, "0 times")) %>%
  mutate(q54 = replace(q54, q54 == 2.0, "1 or 2 times")) %>%
  mutate(q54 = replace(q54, q54 == 3.0, "3 to 9 times")) %>%
  mutate(q54 = replace(q54, q54 == 4.0, "10 to 19 times")) %>%
  mutate(q54 = replace(q54, q54 == 5.0, "20 to 39 times")) %>%
  mutate(q54 = replace(q54, q54 == 6.0, "40 or more times"))
reduced_MLR1$q54 = as.factor(reduced_MLR1$q54)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q55 = replace(q55, q55 == 1.0, "0 times")) %>%
  mutate(q55 = replace(q55, q55 == 2.0, "1 or 2 times")) %>%
  mutate(q55 = replace(q55, q55 == 3.0, "3 to 9 times")) %>%
  mutate(q55 = replace(q55, q55 == 4.0, "10 to 19 times")) %>%
  mutate(q55 = replace(q55, q55 == 5.0, "20 to 39 times")) %>%
  mutate(q55 = replace(q55, q55 == 6.0, "40 or more times"))
reduced_MLR1$q55 = as.factor(reduced_MLR1$q55)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q56 = replace(q56, q56 == 1.0, "0 times")) %>%
  mutate(q56 = replace(q56, q56 == 2.0, "1 or 2 times")) %>%
  mutate(q56 = replace(q56, q56 == 3.0, "3 to 9 times")) %>%
  mutate(q56 = replace(q56, q56 == 4.0, "10 to 19 times")) %>%
  mutate(q56 = replace(q56, q56 == 5.0, "20 to 39 times")) %>%
  mutate(q56 = replace(q56, q56 == 6.0, "40 or more times"))
reduced_MLR1$q56 = as.factor(reduced_MLR1$q56)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q57 = replace(q57, q57 == 1.0, "0 times")) %>%
  mutate(q57 = replace(q57, q57 == 2.0, "1 time")) %>%
  mutate(q57 = replace(q57, q57 == 3.0, "2 or more times"))
reduced_MLR1$q57 = as.factor(reduced_MLR1$q57)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q58 = replace(q58, q58 == 1.0, "Yes")) %>%
  mutate(q58 = replace(q58, q58 == 2.0, "No"))
reduced_MLR1$q58 = as.factor(reduced_MLR1$q58)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q66 = replace(q66, q66 == 1.0, "No Contact")) %>%
  mutate(q66 = replace(q66, q66 == 2.0, "Females")) %>%
  mutate(q66 = replace(q66, q66 == 3.0, "Males")) %>%
  mutate(q66 = replace(q66, q66 == 4.0, "Females and Males")) 
reduced_MLR1$q66 = as.factor(reduced_MLR1$q66)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q70 = replace(q70, q70 == 1.0, "did not drink fruit juice")) %>%
  mutate(q70 = replace(q70, q70 == 2.0, "1 - 3 times in last week")) %>%
  mutate(q70 = replace(q70, q70 == 3.0, "4 - 6 times in last week")) %>%
  mutate(q70 = replace(q70, q70 == 4.0, "1 time a day")) %>%
  mutate(q70 = replace(q70, q70 == 5.0, "2 times a day")) %>%
  mutate(q70 = replace(q70, q70 == 6.0, "3 times a day")) %>%
  mutate(q70 = replace(q70, q70 == 7.0, "4 or more times a day")) 
reduced_MLR1$q70 = as.factor(reduced_MLR1$q70)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q72 = replace(q72, q72 == 1.0, "did not eat green salad juice")) %>%
  mutate(q72 = replace(q72, q72 == 2.0, "1 - 3 times in last week")) %>%
  mutate(q72 = replace(q72, q72 == 3.0, "4 - 6 times in last week")) %>%
  mutate(q72 = replace(q72, q72 == 4.0, "1 time a day")) %>%
  mutate(q72 = replace(q72, q72 == 5.0, "2 times a day")) %>%
  mutate(q72 = replace(q72, q72 == 6.0, "3 times a day")) %>%
  mutate(q72 = replace(q72, q72 == 7.0, "4 or more times a day"))
reduced_MLR1$q72 = as.factor(reduced_MLR1$q72)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q83 = replace(q83, q83 == 1.0, "0 teams")) %>%
  mutate(q83 = replace(q83, q83 == 2.0, "1 team")) %>%
  mutate(q83 = replace(q83, q83 == 3.0, "2 teams")) %>%
  mutate(q83 = replace(q83, q83 == 4.0, "3 or more teams"))
reduced_MLR1$q83 = as.factor(reduced_MLR1$q83)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q90 = replace(q90, q90 == 1.0, "Did not drive last 30 days")) %>%
  mutate(q90 = replace(q90, q90 == 2.0, "0 times")) %>%
  mutate(q90 = replace(q90, q90 == 3.0, "1 time")) %>%
  mutate(q90 = replace(q90, q90 == 4.0, "2 or 3 times")) %>%
  mutate(q90 = replace(q90, q90 == 5.0, "4 or 5 times")) %>%
  mutate(q90 = replace(q90, q90 == 6.0, "6 or more times"))
reduced_MLR1$q90 = as.factor(reduced_MLR1$q90)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q91 = replace(q91, q91 == 1.0, "0 times")) %>%
  mutate(q91 = replace(q91, q91 == 2.0, "1 or 2 times")) %>%
  mutate(q91 = replace(q91, q91 == 3.0, "3 to 9 times")) %>%
  mutate(q91 = replace(q91, q91 == 4.0, "10 to 19 times")) %>%
  mutate(q91 = replace(q91, q91 == 5.0, "20 to 39 times")) %>%
  mutate(q91 = replace(q91, q91 == 6.0, "40 or more times"))
reduced_MLR1$q91 = as.factor(reduced_MLR1$q91)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q93 = replace(q93, q93 == 1.0, "did not drink water in last week")) %>%
  mutate(q93 = replace(q93, q93 == 2.0, "1 - 3 times in last week")) %>%
  mutate(q93 = replace(q93, q93 == 3.0, "4 - 6 times in last week")) %>%
  mutate(q93 = replace(q93, q93 == 4.0, "1 time a day")) %>%
  mutate(q93 = replace(q93, q93 == 5.0, "2 times a day")) %>%
  mutate(q93 = replace(q93, q93 == 6.0, "3 times a day")) %>%
  mutate(q93 = replace(q93, q93 == 7.0, "4 or more times a day"))
reduced_MLR1$q93 = as.factor(reduced_MLR1$q93)

reduced_MLR1 = reduced_MLR1 %>%
  mutate(q98 = replace(q98, q98 == 1.0, "Yes")) %>%
  mutate(q98 = replace(q98, q98 == 2.0, "No")) 
reduced_MLR1$q98 = as.factor(reduced_MLR1$q98)

head(reduced_MLR1)
### Ordinal logistic regression

reduced_MLR1$q13 = as.factor(reduced_MLR1$q13)
class(reduced_MLR1$q13)
unique(reduced_MLR1$q10)

m<-svyglm(q13 ~ q2  + q7 + q12  + q16 + q17 + q18  + q25 +
            q26 +  q58 + q66 +q72+ q83+ q90+q91+q93+q98, data = reduced_MLR1, design = des)

m1<-svyolr(factor(q13)~ q2  + q7 + q12  + q16 + q17 + q18  + q25 +
            q26 +  q58 + q66 +q72+ q83+ q90 + q91 + q93 + q98 , design = des)

summary(m1)
(ctable <- coef(summary(m1)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m1))
confint.default(m1)

exp(coef(m1))

##########################################################################

model.reduced=glm(q13~., data=reduced_MLR1, family = "binomial", weights = weight)
summary(model.reduced)


cdc_data = cdc_data %>%
  mutate(q14 = replace(q14, q14 == 1.0, "0 times")) %>%
  mutate(q14 = replace(q14, q14 == 2.0, "1 time")) %>%
  mutate(q14 = replace(q14, q14 == 3.0, "2 or 3 times")) %>%
  mutate(q14 = replace(q14, q14 == 4.0, "4 or 5 times")) %>%
  mutate(q14 = replace(q14, q14 == 5.0, "6 or more times"))

cdc_data = cdc_data %>%
  mutate(q15 = replace(q15, q15 == 1.0, "0 times")) %>%
  mutate(q15 = replace(q15, q15 == 2.0, "1 time")) %>%
  mutate(q15 = replace(q15, q15 == 3.0, "2 or 3 times")) %>%
  mutate(q15 = replace(q15, q15 == 4.0, "4 or 5 times")) %>%
  mutate(q15 = replace(q15, q15 == 5.0, "6 or more times"))

cdc_data = cdc_data %>%
  mutate(q16 = replace(q16, q16 == 1.0, "0 times")) %>%
  mutate(q16 = replace(q16, q16 == 2.0, "1 time")) %>%
  mutate(q16 = replace(q16, q16 == 3.0, "2 or 3 times")) %>%
  mutate(q16 = replace(q16, q16 == 4.0, "4 or 5 times")) %>%
  mutate(q16 = replace(q16, q16 == 5.0, "6 or 7 times")) %>%
  mutate(q16 = replace(q16, q16 == 6.0, "8 or 9 times")) %>%
  mutate(q16 = replace(q16, q16 == 7.0, "10 or 11 times")) %>%
  mutate(q16 = replace(q16, q16 == 8.0, "12 or more times"))

cdc_data = cdc_data %>%
  mutate(q17 = replace(q17, q17 == 1.0, "0 times")) %>%
  mutate(q17 = replace(q17, q17 == 2.0, "1 time")) %>%
  mutate(q17 = replace(q17, q17 == 3.0, "2 or 3 times")) %>%
  mutate(q17 = replace(q17, q17 == 4.0, "4 or 5 times")) %>%
  mutate(q17 = replace(q17, q17 == 5.0, "6 or 7 times")) %>%
  mutate(q17 = replace(q17, q17 == 6.0, "8 or 9 times")) %>%
  mutate(q17 = replace(q17, q17 == 7.0, "10 or 11 times")) %>%
  mutate(q17 = replace(q17, q17 == 8.0, "12 or 7more times"))

cdc_data = cdc_data %>%
  mutate(q18 = replace(q18, q18 == 1.0, "0 times")) %>%
  mutate(q18 = replace(q18, q18 == 2.0, "1 time")) %>%
  mutate(q18 = replace(q18, q18 == 3.0, "2 or 3 times")) %>%
  mutate(q18 = replace(q18, q18 == 4.0, "4 or 5 times")) %>%
  mutate(q18 = replace(q18, q18 == 5.0, "6 or 7 times")) %>%
  mutate(q18 = replace(q18, q18 == 6.0, "8 or 9 times")) %>%
  mutate(q18 = replace(q18, q18 == 7.0, "10 or 11 times")) %>%
  mutate(q18 = replace(q18, q18 == 8.0, "12 or 7more times"))

cdc_data = cdc_data %>%
  mutate(q19 = replace(q19, q19 == 1.0, "Yes")) %>%
  mutate(q19 = replace(q19, q19 == 2.0, "No")) 

cdc_data = cdc_data %>%
  mutate(q20 = replace(q20, q20 == 1.0, "0 times")) %>%
  mutate(q20 = replace(q20, q20 == 2.0, "1 time")) %>%
  mutate(q20 = replace(q20, q20 == 3.0, "2 or 3 times")) %>%
  mutate(q20 = replace(q20, q20 == 4.0, "4 or 5 times")) %>%
  mutate(q20 = replace(q20, q20 == 5.0, "6 or more times"))

cdc_data = cdc_data %>%
  mutate(q21 = replace(q21, q21 == 1.0, "I did not date in the past 12 months")) %>%
  mutate(q21 = replace(q21, q21 == 2.0, "0 times")) %>%
  mutate(q21 = replace(q21, q21 == 3.0, "1 time")) %>%
  mutate(q21 = replace(q21, q21 == 4.0, "2 or 3 times")) %>%
  mutate(q21 = replace(q21, q21 == 5.0, "4 or 5 times")) %>%
  mutate(q21 = replace(q21, q21 == 6.0, "6 or more times"))
  
cdc_data = cdc_data %>%
    mutate(q22 = replace(q22, q22 == 1.0, "I did not date in the past 12 months")) %>%
    mutate(q22 = replace(q22, q22 == 2.0, "0 times")) %>%
    mutate(q22 = replace(q22, q22 == 3.0, "1 time")) %>%
    mutate(q22 = replace(q22, q22 == 4.0, "2 or 3 times")) %>%
    mutate(q22 = replace(q22, q22 == 5.0, "4 or 5 times")) %>%
    mutate(q22 = replace(q22, q22 == 6.0, "6 or more times"))

cdc_data = cdc_data %>%
  mutate(q23 = replace(q23, q23 == 1.0, "Yes")) %>%
  mutate(q23 = replace(q23, q23 == 2.0, "No")) 

cdc_data = cdc_data %>%
  mutate(q24 = replace(q24, q24 == 1.0, "Yes")) %>%
  mutate(q24 = replace(q24, q24 == 2.0, "No")) 


  
## EDA, Demo
summarize(cdc_data, N_hat = sum(cdc_data$weight))

### weights
ggplot(data = cdc_data, mapping = aes(x = weight, color = raceeth)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data = cdc_data, mapping = aes(x = weight, color = age)) +
  geom_freqpoly(binwidth = 0.1)


ggplot(data = cdc_data, mapping = aes(x = weight, color = grade)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data = cdc_data, mapping = aes(x = weight, color = Sex)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data = cdc_data, mapping = aes(x = weight, color = sex_id)) +
  geom_freqpoly(binwidth = 0.1)

##### Stacked bar Demo
### weighted 
cdc_data %>%
  ggplot(aes(x = q14, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  coord_flip() +
  ggtitle("Gun Carry and Ethnicity Weighted") + 
  labs(y="Count", x = "Carried Gun")

cdc_data %>%
  ggplot(aes(x = q14, weight = weight)) +
  geom_bar(aes(fill = age), position = "fill") +
  coord_flip() +
  ggtitle("Gun Carry and Age Weighted") + 
  labs(y="Count", x = "Carried Gun")

cdc_data %>%
  ggplot(aes(x = q13, weight = weight)) +
  geom_bar(aes(fill = sex_id), position = "fill") +
  coord_flip() +
  ggtitle("Weapons and Sexual Identity Weighted") + 
  labs(y="Count", x = "Weapons Carry")

cdc_data %>%
  ggplot(aes(x = q13, weight = weight)) +
  geom_bar(aes(fill = grade), position = "fill") +
  coord_flip() +
  ggtitle("Weapons and Grade Weighted") + 
  labs(y="Count", x = "Weapons Carry")

cdc_data %>%
  ggplot(aes(x = q13, weight = weight)) +
  geom_bar(aes(fill = Sex), position = "fill") +
  coord_flip() +
  ggtitle("Weapons and Gender Weighted") + 
  labs(y="Count", x = "Weapons Carry")


box = ggplot(data = cdc_data) +
  geom_boxplot(
    mapping=aes(
      x=reorder(q13, q7, FUN = median), weight = weight,
      y = q7
    )) 
box + ggtitle("Weapons and Body Weight Weighted") + 
      labs(y="Count", x = "Weapons Carry") 
  

##### Stacked bar Demo
### unweighted 
cdc_data %>%
  ggplot(aes(x = q13)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  coord_flip() +
  ggtitle("Weapons and Ethnicity Uneighted") + 
  labs(y="Count", x = "Weapons Carry")

cdc_data %>%
  ggplot(aes(x = q13)) +
  geom_bar(aes(fill = age), position = "fill") +
  coord_flip() +
  ggtitle("Weapons and Age Unweighted") + 
  labs(y="Count", x = "Weapons Carry")

cdc_data %>%
  ggplot(aes(x = q13)) +
  geom_bar(aes(fill = sex_id), position = "fill") +
  coord_flip() +
  ggtitle("Weapons and Sexual Identity Unweighted") + 
  labs(y="Count", x = "Weapons Carry")

cdc_data %>%
  ggplot(aes(x = q13)) +
  geom_bar(aes(fill = grade), position = "fill") +
  coord_flip() +
  ggtitle("Weapons and Grade Unweighted") + 
  labs(y="Count", x = "Weapons Carry")

cdc_data %>%
  ggplot(aes(x = q13)) +
  geom_bar(aes(fill = Sex), position = "fill") +
  coord_flip() +
  ggtitle("Weapons and Gender Unweighted") + 
  labs(y="Count", x = "Weapons Carry")


box2 = ggplot(data = cdc_data) +
  geom_boxplot(
    mapping=aes(
      x=reorder(q13, q7, FUN = median),
      y = q7
    )
  )
box2 + ggtitle("Weapons and Body Weight Unweighted") + 
  labs(y="Count", x = "Weapons Carry") 


#### Stacked by other survey Qs

cdc_data %>%
  ggplot(aes(x = q16, weight = weight )) +
  geom_bar(aes(fill = sex_id), position = "fill") +
  coord_flip() +
  ggtitle("Weapons and Grade Unweighted") + 
  labs(y="Count", x = "Weapons Carry")


##### tables, chisq
table(cdc_data$raceeth, cdc_data$q13)
chisq.test(table(cdc_data$raceeth, cdc_data$q13)) 

table(cdc_data$age, cdc_data$q13)
chisq.test(table(cdc_data$age, cdc_data$q13)) 

table(cdc_data$sex_id, cdc_data$q13)
chisq.test(table(cdc_data$sex_id, cdc_data$q13)) 

table(cdc_data$grade, cdc_data$q13)
chisq.test(table(cdc_data$grade, cdc_data$q13)) 

table(cdc_data$Sex, cdc_data$q13)
chisq.test(table(cdc_data$Sex, cdc_data$q13)) 

formattable(tbl)
#######################3
GBarChart <- data.frame(grade = names(table(groupM$grade)),
                        GroupM = as.numeric(table(groupM$grade)),
                        GroupF = as.numeric(table(groupF$grade)))


plot_ly(GBarChart,
        x = ~grade,
        y = ~GroupM,
        type = "bar",
        name = "Male",
        marker = list(color = "rgba(53, 61, 219, 0.7)",
                      line = list(color = "rgba(53, 61, 219, 0.5)",
                                  width = 1.5))
) %>% 
  add_trace(y = ~GroupF,
            name = "Female",
            marker = list(color = "rgba(219, 53, 133, 0.7)",
                          line = list(color = "rgba(219, 53, 133, 0.5)",
                                      width = 1.5))
  ) %>% 
  layout(yaxis = list(title = "Students"),
         barmode = "stack",
         xaxis = list(title = "Percent",
                      categoryorder = "array",
                      categoryarray = gBarChart$grade))
