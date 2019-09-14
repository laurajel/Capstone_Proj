library(data.table)
library(tidyverse)
library(plotly)
library(googleVis)
library(ggmap)
library(leaflet)

#FOR LOADING AND AGGREGATING DATA AND SELECTIZED OPTIONS

demo <- read.csv("/Users/ktread/Capstone_Proj/EDA_and_Preproc/demographic.csv")

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

  

#EXAMPLES

#choice <- ( c("Black","Asian/Pacific Islander"
#              ,"White"                 
#              ,"Hispanic/Latino"   
#              ,"Native American"))


#police_detail <- police %>% 
#  select("Armed"               
#         ,"City"                
##         ,"Flee"                
#         ,"Body_camera"           
#         ,"Juvenile"              
#         ,"Signs_of_mental_illness" 
#         ,"State_name"              
#         ,"Threat_level"           
#         ,"Region" )






