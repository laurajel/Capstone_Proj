library(plotly)
library(dplyr)

demo = read.csv("/Users/ktread/Capstone_Proj/EDA_and_Preproc/demographic.csv")
View(demo)

race <- demo %>% 
  group_by(race) %>% 
  summarise(total = n())
  

View(race)

p <- plot_ly(
  x = race$race,
  y = race$total,
  name = "SF Zoo",
  type = "bar"
)

p