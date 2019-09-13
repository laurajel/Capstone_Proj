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


###################### clustering
library(flexclust)

cdc_clus = read.csv('XXHq_2017.csv')
head(cdc_clus)

cdc_clus = select(cdc_clus, -1, -3, -4, -5, -6, -108, -109)
cdc_clus = select(cdc_clus, -6)
head(cdc_clus)

val = unique(cdc_clus[!is.na(cdc_clus)])
mode = val[which.max(tabulate(match(cdc_clus, val)))]
cdc_clus[is.na(cdc_clus)] = mode 

sum(is.na(cdc_clus))
head(cdc_clus)

cdc_violence1 = cdc_clus %>%
  select(q1, q2, q3, q6, q7, q8,q9,q10,q11,q12,q13,q14,q15,q16,q17,q18,q19,q23,q24,
         q25,q26,q30,q44,q46,q49, q50, q51, q52, q53, q55, q56, q59,q67, q88, q89)

library(igraph)
library(RColorBrewer)

mat <- cor(t(cdc_violence1))
mat[mat<0.50] <- 0

coul <- brewer.pal(nlevels(as.factor(cdc_violence1$q2)), "Set2")
network <- graph_from_adjacency_matrix( mat, weighted=T, mode="undirected", diag=F)
plot(network)

# Map the color to cylinders
my_color <- coul[as.numeric(as.factor(cdc_violence$q2))]


# plot
par(bg="grey13", mar=c(0,0,0,0))
set.seed(4)
plot(network, 
     vertex.size=12,
     vertex.color=my_color, 
     vertex.label.cex=0.7,
     vertex.label.color="white",
     vertex.frame.color="transparent"
)

# title and legend
text(0,0,"mtcars network",col="white", cex=1.5)
legend(x=-0.2, y=-0.12, 
       legend=paste( levels(as.factor(mtcars$cyl)), " cylinders", sep=""), 
       col = coul , 
       bty = "n", pch=20 , pt.cex = 2, cex = 1,
       text.col="white" , horiz = F)

#######################################################################################
cdc_data2 = read.csv('survey_answers.csv')


install.packages("collapsibleTree")
library(collapsibleTree) 

cdc_weapons = cdc_data2 %>%
  select(raceeth, weapons_all, age, grade, Sex)

# Represent this tree:
p <- collapsibleTree(cdc_weapons, c( "raceeth", "Sex", "age", "grade", "weapons_all"))
p

#######################################################################################
install.packages("networkD3")
library(networkD3)














cdc.scaled = as.data.frame(scale(cdc_violence))
summary(cdc.scaled)
sapply(cdc.scaled, sd)

d = dist(cdc.scaled)

fit.single = hclust(d, method = "single")
fit.complete = hclust(d, method = "complete")
fit.average = hclust(d, method = "average")

plot(fit.single, hang = -1, main = "Dendrogram of Single Linkage")
plot(fit.complete, hang = -1, main = "Dendrogram of Complete Linkage")
plot(fit.average, hang = -1, main = "Dendrogram of Average Linkage")

clusters.average = cutree(fit.average, k = 5)
table(clusters.average)

aggregate(cdc_clus, by = list(cluster = clusters.average), median)

aggregate(cdc.scaled, by = list(cluster = clusters.average), median)

par(mfrow = c(1, 1))
plot(fit.average, hang = -1, main = "Dendrogram of Average Linkage\n5 Clusters")
rect.hclust(fit.average, k = 5)







