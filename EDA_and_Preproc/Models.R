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
                                           unsafe + bullied_sch + bullied_elec +
                                           sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
 
                                          marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise, design = des2))
summary(fit)
(ctable <- coef(summary(fit)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit))
confint.default(fit)
exp(coef(fit))



######################################## q14

fit2 = with(data =train_na, exp = svyolr(weapons_gun ~ raceeth + Sex+ Age + Grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                                                unsafe + bullied_sch + bullied_elec + sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                                                marijuana_use + drug_cocaine, design = des2))
summary(fit2)
(ctable <- coef(summary(fit2)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit2))
confint.default(fit2)
exp(coef(fit2))

######################################## q17

fit3 = with(data = train_na, exp = svyolr(phys_fight ~ raceeth + Sex+ Age + Grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                                                unsafe + bullied_sch + bullied_elec + sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                                                marijuana_use + drug_cocaine, design = des2))
summary(fit3)
(ctable <- coef(summary(fit3)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit3))
confint.default(fit3)
exp(coef(fit3))


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


#########################################################q23
### Reduced Ordinal logistic regression Survey##########
## TRAIN
unique(train_na$bullied_sch)

levels(train_na$bullied_sch)[1] = "0"
levels(train_na$bullied_sch)[2] = "1"

fit5 = with(data = train_na, exp = svyglm(bullied_sch ~ raceeth + Sex + sex_id + Age + Grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                                            unsafe + bullied_sch + bullied_elec + sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                                            marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise + sex_contact, design = des2, family=binomial))
summary(fit5)
(ctable <- coef(summary(fit5)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit5))
confint.default(fit5)
exp(coef(fit5))


#############################################q24
unique(train_na$bullied_elec)

levels(train_na$bullied_elec)[1] = "0"
levels(train_na$bullied_elec)[2] = "1"

fit6 = with(data = train_na, exp = svyglm(bullied_elec~ raceeth + Sex + sex_id + Age + Grade + height + indv_weight + seatbelt_wear + pass_drink_drv + drink_drv + drv_text_email + 
                                            unsafe + bullied_sch + bullied_elec + sad + suicide_consider + suicide_plan + suicide_attempt + suicide_treat + smoke_days + vape_days + drink_days +
                                            marijuana_use + drug_cocaine + drug_inhalents + drug_heroin + drug_meth  + drug_X + drug_Rx_abuse + exercise + sex_contact, design = des2, family=binomial))
summary(fit6)
(ctable <- coef(summary(fit6)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table xonfidence intervals and odds ratios
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(fit6))
confint.default(fit6)
exp(coef(fit6))


### importance carat train ## test
#control <- trainControl(method="repeatedcv", number=10, repeats=3)
#model <- train(weapons_all~raceeth + Sex + age + grade + height + unsafe + marijuana_use +  drink_drv + drink_days + drug_cocaine + 
#                 unsafe + drug_inhalents + drug_heroin + drug_meth  + drug_X , data=cdc_data2, method="lvq", preProcess="scale", trControl=control)
#importance <- varImp(model, scale=FALSE)
# summarize importance
#print(importance)
# plot importance
#plot(importance)

###################### clustering
library(flexclust)

cdc_clus = read.csv('XXHq_2017.csv')
head(cdc_clus)

cdc_clus = dplyr::select(cdc_clus, -1, -3, -4, -5, -6, -108, -109)
cdc_clus = dplyr::select(cdc_clus, -6)
head(cdc_clus)

val = unique(cdc_clus[!is.na(cdc_clus)])
mode = val[which.max(tabulate(match(cdc_clus, val)))]
cdc_clus[is.na(cdc_clus)] = mode 

sum(is.na(cdc_clus))
head(cdc_clus)

cdc_violence1 = cdc_clus %>%
  dplyr::select(q1, q2, q3, q6, q7, q8,q9,q10,q11,q12,q13,q14,q15,q16,q17,q18,q19,q23,q24,
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
  dplyr::select(raceeth, weapons_all, age, grade, Sex, weight)

# Represent this tree:
p <- collapsibleTree(cdc_weapons, c( "raceeth", "Sex", "age", "grade", "weapons_all"), weights = weight)
p

###################################




