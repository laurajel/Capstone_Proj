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
library(RColorBrewer)
library(randomForest)
library(mice)
library(missForest)
library(formattable)

options(max.print=100000)
#cdc_data2 = read.csv('survey_answers.csv')
cdc_NA = read.csv('MICE_Impute.csv')


#head(cdc_data2)

head(cdc_NA)

cdc_NA = dplyr::select(cdc_NA, -1)
head(cdc_NA)


sum(is.na(cdc_NA))
#colnames(cdc_data2)

#cdc_split = sample.split(cdc_data2,SplitRatio = 0.3)
#train = subset(cdc_data2,cdc_split==TRUE)
#test = subset(cdc_data2,cdc_split==FALSE)


############## MICE imputation and missing
library(VIM)
md.pattern(cdc_NA)

#resetPar <- function() {
 # dev.new()
  #op <- par(no.readonly = TRUE)
  #dev.off()
  #op
#}
#par(resetPar())

#mice_plot <- aggr(cdc_NA, col=c('navyblue','yellow'),
 #                 numbers=TRUE, sortVars=TRUE,
  #                labels=names(cdc_NA), cex.axis=.3,
   #               gap=3, ylab=c("Missing data","Pattern"))

imputed_Data <- mice(cdc_NA, m=2, maxit = 2,method = "cart", seed = 500)

sum(is.na(imputed_Data))

######################################## Split

cdc_NA_split = sample.split(cdc_NA, SplitRatio = 0.3)
train_na = subset(cdc_NA, cdc_NA_split==TRUE)
test_na = subset(cdc_NA, cdc_NA_split==FALSE)



####### Survey
############## data with Non missing weights
options(survey.lonely.psu = "adjust")

## Survey design
des = svydesign(ids=~1, strata=~stratum, weights=~weight, data = cdc_NA[is.na(cdc_NA$weight) == F,])
des2 = svydesign(ids=~1, strata=~stratum, weights=~weight, data = train_na[is.na(train_na$weight) == F,])
des3 = svydesign(ids=~1, strata=~stratum, weights=~weight, data = test_na[is.na(test_na$weight) == F,])
svymean(~weapons_all, des)

colnames(cdc_data2)
######################################## Models q12


fit = with(data = train_na, exp = svyolr(weapons_all ~ raceeth + Sex + Age + Grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                                           unsafe + bullied_sch + bullied_elec + sad + suicide_consider + suicide_plan + suicide_attempt +  suicide_treat + smoke_days + vape_days + drink_days +
                                           marijuana_use + drug_cocaine   
                                            , design = des2))
summary(fit)


(ctable <- coef(summary(fit)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit))
confint.default(fit)
exp(coef(fit))


weapons_all_sum = stargazer(ctable, summary = FALSE,type = c('text'),
                    title = 'Summary of Weapons all',
                    rownames = TRUE)

write.table(ctable, file = "Weapons_all_sum.txt", sep = ",", quote = FALSE, row.names = TRUE)
######################################## q14

fit2 = with(data =train_na, exp = svyolr(weapons_gun ~ raceeth  + Sex+ Age + Grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                                           unsafe + bullied_sch + bullied_elec + sad  + suicide_consider + suicide_plan + suicide_attempt + suicide_treat +
                                          smoke_days +  drink_days + marijuana_use + drug_cocaine   , design = des2))
summary(fit2)

(ctable <- coef(summary(fit2)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit2))
confint.default(fit2)
exp(coef(fit2))

weapons_gun_sum = stargazer(ctable, summary = FALSE,type = c('text'),
                            title = 'Summary of Weapons gun',
                            rownames = TRUE)

write.table(ctable, file = "Weapons_gun_sum.txt", sep = ",", quote = FALSE, row.names = TRUE)

######################################## q17

fit3 = with(data = train_na, exp = svyolr(weapons_toschool ~ raceeth +  Sex+ Age + Grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                                            unsafe + bullied_sch + bullied_elec + sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat  , design = des2))
summary(fit3)
(ctable <- coef(summary(fit3)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit3))
confint.default(fit3)
exp(coef(fit3))

weapons_gun_sum = stargazer(ctable, summary = FALSE,type = c('text'),
                            title = 'Summary of Weapons gun',
                            rownames = TRUE)

write.table(ctable, file = "Weapons_tosch_sum.txt", sep = ",", quote = FALSE, row.names = TRUE)

###### injured weapon

fit10 = with(data = train_na, exp = svyolr(inj_weapon ~ raceeth +  Sex+ Age + Grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                                             unsafe + bullied_sch + bullied_elec + sad + suicide_consider   , design = des2))
summary(fit10)
(ctable <- coef(summary(fit10)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit10))
confint.default(fit10)
exp(coef(fit10))

colnames(train_na)

write.table(ctable, file = "Weapons_inj_sum.txt", sep = ",", quote = FALSE, row.names = TRUE)



### TRAIN full ############################################################ q19

### Reduced Ordinal logistic regression Survey##########
## TRAIN
unique(train_na$viol_dating1)

levels(train_na$viol_dating1)[1] = "0"
levels(train_na$viol_dating1)[2] = "1"

fit4 = with(data = train_na, exp = svyglm(viol_dating1 ~ raceeth + Sex+ Age + Grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                                                unsafe + bullied_sch + bullied_elec + sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                                                marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise + sex_contact, design = des2, family=binomial))
summary(fit4)
(ctable <- coef(summary(fit4)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit4))
confint.default(fit4)
exp(coef(fit4))

dating_viol1_sum = stargazer(ctable, summary = FALSE,type = c('text'),
                            title = 'Summary of Weapons gun',
                            rownames = TRUE)

write.table(ctable, file = "dating_viol1.txt", sep = ",", quote = FALSE, row.names = TRUE)
########### dv_2
fit5 = with(data = train_na, exp = svyolr(viol_dating2 ~ raceeth + Sex+ Age + Grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email+
                                          unsafe + bullied_sch + bullied_elec + sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat  ,  des2))
summary(fit5)
(ctable <- coef(summary(fit5)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit5))
confint.default(fit5)
exp(coef(fit5))


write.table(ctable, file = "dating_viol2.txt", sep = ",", quote = FALSE, row.names = TRUE)

####### dv_3
fit6 = with(data = train_na, exp = svyolr(viol_dating3 ~ raceeth + Sex+ Age + Grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                                            unsafe + bullied_sch + bullied_elec + sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat+ sex_contact, des2))
summary(fit6)
(ctable <- coef(summary(fit6)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit6))
confint.default(fit6)
exp(coef(fit6))


write.table(ctable, file = "dating_viol3.txt", sep = ",", quote = FALSE, row.names = TRUE)

##### dv_4

fit7 = with(data = train_na, exp = svyolr(viol_dating4 ~ raceeth + Sex+ Age + Grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                                            unsafe + bullied_sch + bullied_elec + sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat+ sex_contact, des2))
summary(fit7)
(ctable <- coef(summary(fit7)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit7))
confint.default(fit7)
exp(coef(fit7))


write.table(ctable, file = "dating_viol4.txt", sep = ",", quote = FALSE, row.names = TRUE)


#########################################################q23
### Reduced Ordinal logistic regression Survey##########
## TRAIN
unique(train_na$bullied_sch)

levels(train_na$bullied_sch)[1] = "0"
levels(train_na$bullied_sch)[2] = "1"

fit8 = with(data = train_na, exp = svyglm(bullied_sch ~ raceeth + Sex + sex_id + Age + Grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                                            unsafe + bullied_sch + bullied_elec + sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                                            marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise + sex_contact, design = des2, family=binomial))
summary(fit8)
(ctable <- coef(summary(fit8)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit8))
confint.default(fit8)
exp(coef(fit8))

write.table(ctable, file = "bully_sch.txt", sep = ",", quote = FALSE, row.names = TRUE)
#############################################q24
unique(train_na$bullied_elec)

levels(train_na$bullied_elec)[1] = "0"
levels(train_na$bullied_elec)[2] = "1"

fit9 = with(data = train_na, exp = svyglm(bullied_elec~ raceeth + Sex + sex_id + Age + Grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                                            unsafe + bullied_sch + bullied_elec + sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                                            marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise + sex_contact, design = des2, family=binomial))
summary(fit9)
(ctable <- coef(summary(fit9)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit9))
confint.default(fit9)
exp(coef(fit9))

write.table(ctable, file = "bully_elec.txt", sep = ",", quote = FALSE, row.names = TRUE)

### importance carat train ## test
#control <- trainControl(method="repeatedcv", number=10, repeats=3)
#model <- train(weapons_all~raceeth + Sex + age + grade + height + unsafe + marijuana_use +  drink_drv + drink_days + drug_cocaine + 
#                 unsafe + drug_inhalents + drug_heroin + drug_meth  + drug_X , data=cdc_data2, method="lvq", preProcess="scale", trControl=control)
#importance <- varImp(model, scale=FALSE)
# summarize importance
#print(importance)
# plot importance
#plot(importance)


###################################




