library(ggplot2)
library(plotly)
library(dplyr)
library(survey)
library(formattable)
library(tidyr)
library(data.table)
library(knitr)
library(questionr)
library(stargazer)

##Visualizations ########### q12
### Import Data
cdc_data2 = read.csv('MICE_Impute.csv')

##### Stacked bar Demo >>> weapons_all
### weighted 


race = cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Ethnicity Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")
ggplotly(race)


age = cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = Age), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Age Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")
ggplotly(age)

Sex_ID= cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = sex_id), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Sexual Identity Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")
ggplotly(Sex_ID)

Grade = cdc_data2 %>%
ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = Grade), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Grade Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")
ggplotly(Grade)

Sex = cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = Sex), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Gender Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")
ggplotly(Sex)

  
#### ##################################### q 14

race14 = cdc_data2 %>%
  ggplot(aes(x = weapons_gun, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  coord_flip() +
  ggtitle("Gun Carry and Ethnicity Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Gun in past 30 Days")
ggplotly(race14)


age14 = cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = Age), position = "fill") +
  coord_flip() +
  ggtitle("Gun Carry and Age Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Gun in past 30 Days")
ggplotly(age14)

Sex_ID14= cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = sex_id), position = "fill") +
  coord_flip() +
  ggtitle("Gun Carry and Sexual Identity Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Gun in past 30 Days")
ggplotly(Sex_ID14)

Grade14 = cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = Grade), position = "fill") +
  coord_flip() +
  ggtitle("Gun Carry and Grade Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Gun in past 30 Days") 
ggplotly(Grade14)

Sex14 = cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = Sex), position = "fill") +
  coord_flip() +
  ggtitle("Gun Carry and Gender Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Gun in past 30 Days")
ggplotly(Sex14)

####################################### q 17
race = cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Ethnicity Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")
ggplotly(race)


age = cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = Age), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Age Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")
ggplotly(age)

Sex_ID= cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = sex_id), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Sexual Identity Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")
ggplotly(Sex_ID)

Grade = cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = Grade), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Grade Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")
ggplotly(Grade)

Sex = cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = Sex), position = "fill") +
  coord_flip() +
  ggtitle("Weapons Carry and Gender Weighted") + 
  labs(y="Proportion", x = "Number of Days Carried Weapon in past 30 Days")
ggplotly(Sex)


#### ##################################### q 14

race17 = cdc_data2 %>%
  ggplot(aes(x = phys_fight, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  coord_flip() +
  ggtitle("Physical Fights and Ethnicity Weighted") + 
  labs(y="Proportion", x = "Number of Times Participated in Fight in past 12 Months")
ggplotly(race17)


age17 = cdc_data2 %>%
  ggplot(aes(x = phys_fight, weight = weight)) +
  geom_bar(aes(fill = Age), position = "fill") +
  coord_flip() +
  ggtitle("Physical Fights and Age Weighted") + 
  labs(y="Proportion", x = "Number of Times Participated in Fight in past 12 Months")
ggplotly(age17)

Sex_ID17= cdc_data2 %>%
  ggplot(aes(x = phys_fight, weight = weight)) +
  geom_bar(aes(fill = sex_id), position = "fill") +
  coord_flip() +
  ggtitle("Physical Fights and Sexual Identity Weighted") + 
  labs(y="Proportion", x = "Number of Times Participated in Fight in past 12 Months")
ggplotly(Sex_ID17)

Grade17 = cdc_data2 %>%
  ggplot(aes(x = phys_fight, weight = weight)) +
  geom_bar(aes(fill = Grade), position = "fill") +
  coord_flip() +
  ggtitle("Physical Fighting and Grade Weighted") + 
  labs(y="Proportion", x = "Number of Times Participated in Fight in past 12 Months") 
ggplotly(Grade17)

Sex17 = cdc_data2 %>%
  ggplot(aes(x = phys_fight, weight = weight)) +
  geom_bar(aes(fill = Sex), position = "fill") +
  coord_flip() +
  ggtitle("Physical Fighting and Gender Weighted") + 
  labs(y="Proportion", x = "Number of Times Participated in Fight in past 12 Months")
ggplotly(Sex17)


########################## 19

race19 = cdc_data2 %>%
  ggplot(aes(x = viol_dating1, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  ggtitle("Dating Violence and Ethnicity Weighted") + 
  labs(y="Proportion", x = "Experienced Dating Violence")
ggplotly(race19)


age19 = cdc_data2 %>%
  ggplot(aes(x = viol_dating1, weight = weight)) +
  geom_bar(aes(fill = Age), position = "fill") +
  ggtitle("Dating Violence and Age Weighted") + 
  labs(y="Proportion", x = "Experienced Dating Violence")
ggplotly(age19)

Sex_ID19= cdc_data2 %>%
  ggplot(aes(x = viol_dating1, weight = weight)) +
  geom_bar(aes(fill = sex_id), position = "fill") +
  ggtitle("Dating Violence and Sexual Identity Weighted") + 
  labs(y="Proportion", x = "Experienced Dating Violence")
ggplotly(Sex_ID19)

Grade19 = cdc_data2 %>%
  ggplot(aes(x = viol_dating1, weight = weight)) +
  geom_bar(aes(fill = Grade), position = "fill") +
  ggtitle("Dating Violence and Grade Weighted") + 
  labs(y="Proportion", x = "Experienced Dating Violence") 
ggplotly(Grade19)

Sex19 = cdc_data2 %>%
  ggplot(aes(x = viol_dating1, weight = weight)) +
  geom_bar(aes(fill = Sex), position = "fill") +
  ggtitle("Dating Violence by Sex Weighted") + 
  labs(y="Proportion", x = "Experienced Dating Violence")
ggplotly(Sex19)


################################### q23
race23 = cdc_data2 %>%
  ggplot(aes(x = bullied_sch, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  ggtitle("Bullying at school and Ethnicity Weighted") + 
  labs(y="Proportion", x = "Experienced Bullying at School")
ggplotly(race23)


age23 = cdc_data2 %>%
  ggplot(aes(x = bullied_sch, weight = weight)) +
  geom_bar(aes(fill = Age), position = "fill") +
  ggtitle("Bullying at school and Age Weighted") + 
  labs(y="Proportion", x = "Experienced Bullying at School")
ggplotly(age23)

Sex_ID23= cdc_data2 %>%
  ggplot(aes(x = bullied_sch, weight = weight)) +
  geom_bar(aes(fill = sex_id), position = "fill") +
  ggtitle("Bullying at school and Sexual Identity Weighted") + 
  labs(y="Proportion", x = "Experienced Bullying at School")
ggplotly(Sex_ID23)

Grade23 = cdc_data2 %>%
  ggplot(aes(x = bullied_sch, weight = weight)) +
  geom_bar(aes(fill = Grade), position = "fill") +
  ggtitle("Bullying at school and Grade Weighted") + 
  labs(y="Proportion", x = "Experienced Bullying at School") 
ggplotly(Grade23)

Sex23 = cdc_data2 %>%
  ggplot(aes(x = bullied_sch, weight = weight)) +
  geom_bar(aes(fill = Sex), position = "fill") +
  ggtitle("Bullying at school by Sex Weighted") + 
  labs(y="Proportion", x = "Experienced Bullying at school")
ggplotly(Sex23)

Sad23 = cdc_data2 %>%
  ggplot(aes(x = bullied_sch, weight = weight)) +
  geom_bar(aes(fill = sad), position = "fill") +
  ggtitle("Electronic Bullying by Sex Weighted") + 
  labs(y="Proportion", x = "Experienced Electronic Bullying")
ggplotly(Sad23)

suicide23 = cdc_data2 %>%
  ggplot(aes(x = bullied_sch, weight = weight)) +
  geom_bar(aes(fill = suicide_consider), position = "fill") +
  ggtitle("Electronic Bullying by Sex Weighted") + 
  labs(y="Proportion", x = "Experienced Electronic Bullying")
ggplotly(suicide23)

################################### Electronic bullying q 24

race24 = cdc_data2 %>%
  ggplot(aes(x = bullied_elec, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  ggtitle("Electronic bullying and Ethnicity Weighted") + 
  labs(y="Proportion", x = "Experienced Bullying Electronically")
ggplotly(race24)


age24 = cdc_data2 %>%
  ggplot(aes(x = bullied_elec, weight = weight)) +
  geom_bar(aes(fill = Age), position = "fill") +
  ggtitle("Electronic Bullying and Age Weighted") + 
  labs(y="Proportion", x = "Experienced Electronic Bullying")
ggplotly(age24)

Sex_ID24= cdc_data2 %>%
  ggplot(aes(x = bullied_elec, weight = weight)) +
  geom_bar(aes(fill = sex_id), position = "fill") +
  ggtitle("Electronic Bullying and Sexual Identity Weighted") + 
  labs(y="Proportion", x = "Experienced Electronic Bullying")
ggplotly(Sex_ID24)

Grade24 = cdc_data2 %>%
  ggplot(aes(x = bullied_elec, weight = weight)) +
  geom_bar(aes(fill = Grade), position = "fill") +
  ggtitle("Electronic Bullying and Grade Weighted") + 
  labs(y="Proportion", x = "Experienced Electronic Bullying") 
ggplotly(Grade24)

Sex24 = cdc_data2 %>%
  ggplot(aes(x = bullied_elec, weight = weight)) +
  geom_bar(aes(fill = Sex), position = "fill") +
  ggtitle("Electronic Bullying by Sex Weighted") + 
  labs(y="Proportion", x = "Experienced Electronic Bullying")
ggplotly(Sex24)

Sad24 = cdc_data2 %>%
  ggplot(aes(x = bullied_elec, weight = weight)) +
  geom_bar(aes(fill = sad), position = "fill") +
  ggtitle("Electronic Bullying by Sex Weighted") + 
  labs(y="Proportion", x = "Experienced Electronic Bullying")
ggplotly(Sad24)


suicide24 = cdc_data2 %>%
  ggplot(aes(x = bullied_elec, weight = weight)) +
  geom_bar(aes(fill = suicide_consider), position = "fill") +
  ggtitle("Electronic Bullying by Sex Weighted") + 
  labs(y="Proportion", x = "Experienced Electronic Bullying")
ggplotly(suicide24)

unique(cdc_data2$inj_weapon)
