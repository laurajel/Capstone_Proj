library(data.table)
library(tidyverse)
library(plotly)
library(googleVis)
library(ggmap)
library(leaflet)
library(ggthemes)
library(dplyr)
#FOR LOADING AND AGGREGATING DATA AND SELECTIZED OPTIONS


## DATA LOAD AND PREP

demo <- read.csv("../EDA_and_Preproc/demographic.csv")
answers <- read.csv("../DATA/weighted_2017.csv")

cdc_data2 = read.csv("../DATA/MICE_Impute.csv")

total <- nrow(demo)

gender <- demo %>% 
  group_by(sex) %>% 
  summarise(total = n())

girls <- gender %>% 
  filter(sex == "Female") %>% 
  select(total)

boys <- gender %>% 
  filter(sex == "Male") %>% 
  select(total)


median_grade <- median(na.omit(substr(demo$grade,1, str_locate(demo$grade, "th grade")[,1]-1)))
median_age <- median(na.omit(demo$age))


#RACE
raceeth <- demo %>% 
  select(raceeth,sex) %>% 
  group_by(raceeth,sex) %>% 
  summarise(total = n())


raceeth_gender <- dcast(raceeth, raceeth +  total ~ sex)

#AGES
ages <- demo %>% 
  select(age,sex) %>% 
  group_by(age,sex) %>% 
  summarise(total = n())


age_gender <- dcast(ages, age +  total ~ sex)


#

height_kde <- demo %>% 
  filter(sex == "Male" | sex == "Female") 



kde_height <- ggplot(height_kde, aes(x = height)) + 
  geom_density(aes(fill = sex), alpha = 0.5) + 
  ggtitle("KDE Of Heights by Gender")

kde_height <- ggplotly(kde_height)



kde_weight <- ggplot(height_kde, aes(x = weight)) + 
  geom_density(aes(fill = sex, color= sex), alpha = 0.5) + 
  ggtitle("KDE Of Weight by Gender")

kde_weight <- ggplotly(kde_weight)



## PLOTS 
gender_race_plot <- raceeth_gender %>% 
  plot_ly() %>%
  add_trace(x = ~raceeth, y = ~Male, type = 'bar', 
            text = ~Male, textposition = 'auto',
            name = "Male",
            marker = list(color = 'rgb(65,105,225)',
                          line = list(color = 'rgb(65,105,225)', width = 1.5))) %>%
  add_trace(x = ~raceeth, y = ~Female, type = 'bar', 
            text = ~Female, textposition = 'auto',
            name = "Female",
            marker = list(color = 'rgb(220,20,60)',
                          line = list(color = 'rgb(220,20,60)', width = 1.5))) %>%
  layout(title = "Race and Ethnicity By Gender",
         barmode = 'group',
         xaxis = list(title = ""),
         yaxis = list(title = ""))



age_gender_plot <- age_gender %>% 
  plot_ly() %>%
  add_trace(x = ~age, y = ~Male, type = 'bar', 
            text = ~Male, textposition = 'auto',
            name = "Male",
            marker = list(color = 'rgb(65,105,225)',
                          line = list(color = 'rgb(65,105,225)', width = 1.5))) %>%
  add_trace(x = ~age, y = ~Female, type = 'bar', 
            text = ~Female, textposition = 'auto',
            name = "Female",
            marker = list(color = 'rgb(220,20,60)',
                          line = list(color = 'rgb(220,20,60)', width = 1.5))) %>%
  layout(title = "Age By Gender",
         barmode = 'group',
         xaxis = list(title = ""),
         yaxis = list(title = ""))


###### PLOTS violence
### all weapons
weapons_all = cdc_data2 %>%
  ggplot(aes(x = weapons_all, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  coord_flip() +
  labs(y="Proportion", x = "Number of Days Carried Weapon")

wep_all <- ggplotly(weapons_all)

### guns
weapons_guns = cdc_data2 %>%
  ggplot(aes(x = weapons_gun, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  coord_flip() +
  labs(y="Proportion", x = "Number of Days Carried Gun")

wep_guns <- ggplotly(weapons_guns)

#### Weapon School
weapons_sch = cdc_data2 %>%
  ggplot(aes(x = weapons_toschool, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  coord_flip() +
  labs(y="Proportion", x = "Number of Days Carried Weapon to School")

wep_sch <- ggplotly(weapons_sch)

#### injured or threatened
weapons_inj = cdc_data2 %>%
  ggplot(aes(x = inj_weapon, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  coord_flip() +
  labs(y="Proportion", x = "Number of Days Injured by Weapon at School")

wep_inj <- ggplotly(weapons_inj)

#### Dating violence 
#### rape
dating_viol = cdc_data2 %>%
  ggplot(aes(x = viol_dating1, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  labs(y="Proportion", x = "Forced to have Intercorse")

dv_1 = ggplotly(dating_viol)

#### forced sexual things
dating_viol2 = cdc_data2 %>%
  ggplot(aes(x = viol_dating2, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  coord_flip() +
  labs(y="Proportion", x = "Forced to do Other Sexual Things")

dv_2 = ggplotly(dating_viol2)

#### forced to do sexual things (dating)
dating_viol3 = cdc_data2 %>%
  ggplot(aes(x = viol_dating3, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  coord_flip() +
  labs(y="Proportion", x = "Forced to do Other Sexual Things")

dv_3 = ggplotly(dating_viol3)

#### injured by someone you were dating
dating_viol4 = cdc_data2 %>%
  ggplot(aes(x = viol_dating4, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  coord_flip() +
  labs(y="Proportion", x = "Forced to do Other Sexual Things")

dv_4 = ggplotly(dating_viol4)


####### bullying 
bullying_elec = cdc_data2 %>%
  ggplot(aes(x = bullied_elec, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  labs(y="Proportion", x = " Bullied ")

bully_elec = ggplotly(bullying_elec)

#### bully school
bullying_sch = cdc_data2 %>%
  ggplot(aes(x = bullied_sch, weight = weight)) +
  geom_bar(aes(fill = raceeth), position = "fill") +
  labs(y="Proportion", x = " Bullied")

bully_sch = ggplotly(bullying_sch)

