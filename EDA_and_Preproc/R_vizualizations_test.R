library(ggplot2)
library(plotly)
library(dplyr)

weapons = read.csv('weapon_carry.csv')
violence = read.csv('violence.csv')
bullied = read.csv('bullied.csv')


head(weapons)
head(violence)
head(bullied)

summary(weapons)

##### weapon carry


weapons = weapons %>%
  filter(Gun_carry & oth_weapons != "1")
weapons
weapons 
agg = count(weapons, Gun_carry)
head(agg)
agg = count_(weapons, names(weapons))
head(agg)

xtabs(n ~ Gun_carry + Sex + raceeth, data = agg)
  
q = ggplot(weapons, aes(x = Gun_carry, fill = Sex)) +
  geom_bar(position ="dodge")
q
p = ggplot(weapons, aes(x = oth_weapons, fill = Sex)) +
  geom_bar(position ="dodge") 
p  

weapons %>%
  ggplot(aes(x=Gun_carry, y = weight, fill = Sex)) +
  geom_boxplot()
