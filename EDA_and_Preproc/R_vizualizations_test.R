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

####### Survey
############## data with Non missing weights
options(survey.lonely.psu = "adjust")

## Survey design
des<-svydesign(ids=~1, strata=~stratum, weights=~weight, data = cdc_data2[is.na(cdc_data2$weight)==F,] )
summary(des)
svymean(~weapons_all, des)

table.weight = wtd.table(cdc_data2$weapons_all, cdc_data2$raceeth, weights = cdc_data2$weight)
formattable(table.weight)

#### Summary stats  all weapons####################
### proportions weighted, unweighted
table = prop.table(wtd.table(cdc_data2$weapons_all, cdc_data2$raceeth, weights = cdc_data2$weight), margin=2)
prop.table(wtd.table(cdc_data2$weapons_all, cdc_data2$raceeth), margin=2)
formattable(table)

### std. error or percentages
n<-table(is.na(cdc_data2$weapons_all)==F)
n

p<-prop.table(wtd.table(cdc_data2$weapons_all, cdc_data2$raceeth, weights = cdc_data2$weight), margin=2)[2,]
formattable(p)
se<-sqrt((p*(1-p))/n)

tble = data.frame(proportion=p, se=se)
formattable(tble)

svy = svytable(~weapons_all+raceeth, design = des)
formattable(svy)

svy = prop.table(svytable(~weapons_all+raceeth, design = des), margin = 2)
formattable(svy)

svytble = svyby(formula = ~weapons_all, by = ~raceeth, design = des, FUN = svymean)
formattable(svytble)

## EDA, Demo
summarize(cdc_data2, N_hat = sum(cdc_data2$weight))

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

##### Stacked bar Demo >>> weapons_all
### weighted 
cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Ethnicity Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")

cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = age), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Age Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")

cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = sex_id), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Sexual Identity Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")

cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = grade), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Grade Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")

cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = Sex), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Gender Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")


box = ggplot(data = cdc_data2) +
  geom_boxplot(
    mapping=aes(
      x=reorder(weapons_all, indv_weight, FUN = median), weight = weight,
      y = indv_weight
    )) 
box + ggtitle("Weapons Carriy and Body Weight Weighted") + 
      labs(y="Count", x = "Number of Days Carried Weapon in past 30 Days") 

#### Prop tables
  

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
  ggplot(aes(x = weapons_all, weight = weight )) +
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
