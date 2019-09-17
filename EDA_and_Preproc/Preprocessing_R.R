library(dplyr)
library(tidyr)


cdc_data = read.csv('XXHq_2017.csv')


head(cdc_data)


## removing index variable X from python clean
cdc_data2 = dplyr::select(cdc_data, -1, -3, -4,-5, -6, -11)
head(cdc_data2)
###################### raceeth

cdc_data2 = cdc_data2 %>% 
  mutate(`raceeth` = replace(`raceeth`, `raceeth` == 1.0, "American Indian/Alaskan Native")) %>%
  mutate(`raceeth` = replace(`raceeth`, `raceeth` == 2.0, "Asian")) %>%
  mutate(`raceeth` = replace(`raceeth`, `raceeth` == 3.0, "African American")) %>%
  mutate(`raceeth` = replace(`raceeth`, `raceeth` == 4.0, "Native Hawaiian/Pac Isld")) %>%
  mutate(`raceeth` = replace(`raceeth`, `raceeth` == 5.0, "White")) %>%
  mutate(`raceeth` = replace(`raceeth`, `raceeth` == 6.0, "Hispanic/Latino")) %>%
  mutate(`raceeth` = replace(`raceeth`, `raceeth` == 7.0, "Multiple Hisp/Lat")) %>%
  mutate(`raceeth` = replace(`raceeth`, `raceeth` == 8.0, "Multiple non-Hisp/Lat"))

### Imputing columns with survey values - q1
colnames(cdc_data2)[colnames(cdc_data2) == "q1"] = "Age"
head(cdc_data2)
cdc_data2 = cdc_data2 %>% 
  mutate(`Age` = replace(`Age`, `Age` == 1.0, "12 or younger")) %>%
  mutate(`Age` = replace(`Age`, `Age` == 2.0, "13 years old")) %>%
  mutate(`Age` = replace(`Age`, `Age` == 3.0, "14 years old")) %>%
  mutate(`Age` = replace(`Age`, `Age` == 4.0, "15 years old")) %>%
  mutate(`Age` = replace(`Age`, `Age` == 5.0, "16 years old")) %>%
  mutate(`Age` = replace(`Age`, `Age` == 6.0, "17 years old")) %>%
  mutate(`Age` = replace(`Age`, `Age` == 7.0, "18 or older")) 

cdc_data2$`Age` = as.factor(cdc_data2$`Age`)
head(cdc_data2)
############################# q2
colnames(cdc_data2)[colnames(cdc_data2) == "q2"] = "Sex"
head(cdc_data2)
cdc_data2 = cdc_data2 %>% 
  mutate(`Sex` = replace(`Sex`, `Sex` == 1.0, "F")) %>%
  mutate(`Sex` = replace(`Sex`, `Sex` == 2.0, "M")) 

cdc_data2$`Sex` = as.factor(cdc_data2$`Sex`)
head(cdc_data2)
############################### q3

colnames(cdc_data2)[colnames(cdc_data2) == "q3"] = "Grade"
head(cdc_data2)
cdc_data2 = cdc_data2 %>% 
  mutate(`Grade` = replace(`Grade`, `Grade` == 1.0, "9th")) %>%
  mutate(`Grade` = replace(`Grade`, `Grade` == 2.0, "10th")) %>%
  mutate(`Grade` = replace(`Grade`, `Grade` == 3.0, "11th")) %>%
  mutate(`Grade` = replace(`Grade`, `Grade` == 4.0, "12th")) %>%
  mutate(`Grade` = replace(`Grade`, `Grade` == 5.0, "ungraded or other")) 


cdc_data2$`Grade` = as.factor(cdc_data2$`Grade`)
head(cdc_data2)

################################## q4

# Rename Column - q4
colnames(cdc_data2)[colnames(cdc_data2) == "q4"] = "Hispanic/Latino"
head(cdc_data2)
# Impute survey answers = q4 
cdc_data2 = cdc_data2 %>% 
  mutate(`Hispanic/Latino` = replace(`Hispanic/Latino`, `Hispanic/Latino` == 1.0, "Yes")) %>%
  mutate(`Hispanic/Latino` = replace(`Hispanic/Latino`, `Hispanic/Latino` == 2.0, "No"))
cdc_data2$`Hispanic/Latino` = as.factor(cdc_data2$`Hispanic/Latino`)
head(cdc_data2)

# Rename Column - q6 
colnames(cdc_data2)[colnames(cdc_data2) == 'q6'] = "height"
head(cdc_data2)
# Rename Column - q7
colnames(cdc_data2)[colnames(cdc_data2) == "q7"] = "indv_weight"
head(cdc_data2)

# Rename Column - q8 
colnames(cdc_data2)[colnames(cdc_data2) == "q8"] = "seatbelt_wear"
# Impute survey answers- q8
cdc_data2 = cdc_data2 %>%
  mutate(seatbelt_wear = replace(seatbelt_wear, seatbelt_wear == 1.0, "Never")) %>%
  mutate(seatbelt_wear = replace(seatbelt_wear, seatbelt_wear == 2.0, "Rarely")) %>%
  mutate(seatbelt_wear = replace(seatbelt_wear, seatbelt_wear == 3.0, "Sometimes")) %>%
  mutate(seatbelt_wear = replace(seatbelt_wear, seatbelt_wear == 4.0, "Most of the Time")) %>%
  mutate(seatbelt_wear = replace(seatbelt_wear, seatbelt_wear == 5.0, "Always")) 
cdc_data2$seatbelt_wear = as.factor(cdc_data2$seatbelt_wear) 




head(cdc_data2)

# Rename Column - q9 
colnames(cdc_data2)[colnames(cdc_data2) == "q9"] = "pass_drink_drv"
head(cdc_data2)
# Impute survey answers - q9
cdc_data2 = cdc_data2 %>%
  mutate(pass_drink_drv = replace(pass_drink_drv, pass_drink_drv == 1.0, "0 times")) %>%
  mutate(pass_drink_drv = replace(pass_drink_drv, pass_drink_drv == 2.0, "1 time")) %>%
  mutate(pass_drink_drv = replace(pass_drink_drv, pass_drink_drv == 3.0, "2 or 3 times")) %>%
  mutate(pass_drink_drv = replace(pass_drink_drv, pass_drink_drv == 4.0, "4 or 5 times")) %>%
  mutate(pass_drink_drv = replace(pass_drink_drv, pass_drink_drv == 5.0, "6 or more times")) 
cdc_data2$pass_drink_drv = as.factor(cdc_data2$pass_drink_drv) 
head(cdc_data2)


# Rename Column - q10
colnames(cdc_data2)[colnames(cdc_data2) == "q10"] = "drink_drv"
head(cdc_data2)
# impute survey answers - q10
cdc_data2 = cdc_data2 %>%
  mutate(drink_drv = replace(drink_drv, drink_drv == 1.0, "I did not drive in last 30")) %>%
  mutate(drink_drv = replace(drink_drv, drink_drv == 2.0, "0 times")) %>%
  mutate(drink_drv = replace(drink_drv, drink_drv == 3.0, "1 time")) %>%
  mutate(drink_drv = replace(drink_drv, drink_drv == 4.0, "2 or 3 times")) %>%
  mutate(drink_drv = replace(drink_drv, drink_drv == 5.0, "4 or 5 times")) %>%
  mutate(drink_drv = replace(drink_drv, drink_drv == 6.0, "6 or more times")) 
cdc_data2$drink_drv = as.factor(cdc_data2$drink_drv) 
head(cdc_data2)


# Rename Column - q11
colnames(cdc_data2)[colnames(cdc_data2) == "q11"] = "drv_text_email"
head(cdc_data2)
# Impute Survey answers - q11
cdc_data2 = cdc_data2 %>%
  mutate(drv_text_email = replace(drv_text_email, drv_text_email == 1.0, "I did not drive in last 30")) %>%
  mutate(drv_text_email = replace(drv_text_email, drv_text_email == 2.0, "0 days")) %>%
  mutate(drv_text_email = replace(drv_text_email, drv_text_email == 3.0, "1 or 2 days")) %>%
  mutate(drv_text_email = replace(drv_text_email, drv_text_email == 4.0, "3 to 5 days")) %>%
  mutate(drv_text_email = replace(drv_text_email, drv_text_email == 5.0, "6 to 9 days")) %>%
  mutate(drv_text_email = replace(drv_text_email, drv_text_email == 6.0, "10 to 19 days")) %>%
  mutate(drv_text_email = replace(drv_text_email, drv_text_email == 7.0, "20 to 29 days")) %>%
  mutate(drv_text_email = replace(drv_text_email, drv_text_email == 8.0, "All 30 days"))
cdc_data2$drv_text_email = as.factor(cdc_data2$drv_text_email) 
head(cdc_data2)

# Rename Column - q12
colnames(cdc_data2)[colnames(cdc_data2) == "q12"] = "weapons_all"
head(cdc_data2)
# Impute Survey answers - q12
cdc_data2 = cdc_data2 %>%
  mutate(weapons_all = replace(weapons_all, weapons_all == 1.0, "0 days")) %>%
  mutate(weapons_all = replace(weapons_all, weapons_all == 2.0, "1 day")) %>%
  mutate(weapons_all = replace(weapons_all, weapons_all == 3.0, "2 or 3 days")) %>%
  mutate(weapons_all = replace(weapons_all, weapons_all == 4.0, "4 to 5 days")) %>%
  mutate(weapons_all= replace(weapons_all, weapons_all == 5.0, "6 or more days")) 
cdc_data2$weapons_all = as.factor(cdc_data2$weapons_all)
head(cdc_data2)

# Rename Column - q13
colnames(cdc_data2)[colnames(cdc_data2) == "q13"] = "weapons_toschool"
head(cdc_data2)
# Impute survey answers - q13
cdc_data2 = cdc_data2 %>%
  mutate(weapons_toschool = replace(weapons_toschool, weapons_toschool == 1.0, "0 days")) %>%
  mutate(weapons_toschool = replace(weapons_toschool, weapons_toschool == 2.0, "1 day")) %>%
  mutate(weapons_toschool = replace(weapons_toschool, weapons_toschool == 3.0, "2 or 3 days")) %>%
  mutate(weapons_toschool = replace(weapons_toschool, weapons_toschool == 4.0, "4 to 5 days")) %>%
  mutate(weapons_toschool= replace(weapons_toschool, weapons_toschool == 5.0, "6 or more days")) 
cdc_data2$weapons_toschool = as.factor(cdc_data2$weapons_toschool)
head(cdc_data2)

# Rename Column - q14
colnames(cdc_data2)[colnames(cdc_data2) == "q14"] = "weapons_gun"
head(cdc_data2)
# Impute survey answers = q14
cdc_data2 = cdc_data2 %>%
  mutate(weapons_gun = replace(weapons_gun, weapons_gun == 1.0, "0 days")) %>%
  mutate(weapons_gun = replace(weapons_gun, weapons_gun == 2.0, "1 day")) %>%
  mutate(weapons_gun = replace(weapons_gun, weapons_gun == 3.0, "2 or 3 days")) %>%
  mutate(weapons_gun = replace(weapons_gun, weapons_gun == 4.0, "4 to 5 days")) %>%
  mutate(weapons_gun= replace(weapons_gun, weapons_gun == 5.0, "6 or more days")) 
cdc_data2$weapons_gun = as.factor(cdc_data2$weapons_gun)
head(cdc_data2)

# Rename Column = q15
colnames(cdc_data2)[colnames(cdc_data2) == "q15"] = "unsafe"
head(cdc_data2)
# Impute Survey values - q 15
cdc_data2 = cdc_data2 %>%
  mutate(unsafe = replace(unsafe, unsafe== 1.0, "0 days")) %>%
  mutate(unsafe = replace(unsafe, unsafe == 2.0, "1 day")) %>%
  mutate(unsafe = replace(unsafe, unsafe == 3.0, "2 or 3 days")) %>%
  mutate(unsafe = replace(unsafe, unsafe == 4.0, "4 to 5 days")) %>%
  mutate(unsafe = replace(unsafe, unsafe == 5.0, "6 or more days")) 
cdc_data2$unsafe = as.factor(cdc_data2$unsafe)
head(cdc_data2)

# RImpute Survey answers - q16
cdc_data2 = cdc_data2 %>%
  mutate(q16 = replace(q16, q16 == 1.0, "0 times")) %>%
  mutate(q16 = replace(q16, q16 == 2.0, "1 time")) %>%
  mutate(q16 = replace(q16, q16 == 3.0, "2 or 3 times")) %>%
  mutate(q16 = replace(q16, q16 == 4.0, "4 or 5 times")) %>%
  mutate(q16 = replace(q16, q16 == 5.0, "6 or 7 times")) %>%
  mutate(q16 = replace(q16, q16 == 6.0, "8 or 9 times")) %>%
  mutate(q16 = replace(q16, q16 == 7.0, "10 or 11 times")) %>%
  mutate(q16 = replace(q16, q16 == 8.0, "12 or more times"))
cdc_data2$q16 = as.factor(cdc_data2$q16)
head(cdc_data2)
# Rename Column- q16
colnames(cdc_data2)[colnames(cdc_data2) == "q16"] = "inj_weapon"
head(cdc_data2)

# Impute survy answers - q17
cdc_data2 = cdc_data2 %>%
  mutate(q17 = replace(q17, q17 == 1.0, "0 times")) %>%
  mutate(q17 = replace(q17, q17 == 2.0, "1 time")) %>%
  mutate(q17 = replace(q17, q17 == 3.0, "2 or 3 times")) %>%
  mutate(q17 = replace(q17, q17 == 4.0, "4 or 5 times")) %>%
  mutate(q17 = replace(q17, q17 == 5.0, "6 or 7 times")) %>%
  mutate(q17 = replace(q17, q17 == 6.0, "8 or 9 times")) %>%
  mutate(q17 = replace(q17, q17 == 7.0, "10 or 11 times")) %>%
  mutate(q17 = replace(q17, q17 == 8.0, "12 or more times"))
cdc_data2$q17 = as.factor(cdc_data2$q17)
head(cdc_data2)
# Rename column - q17
colnames(cdc_data2)[colnames(cdc_data2) == "q17"] = "phys_fight"
head(cdc_data2)

# Impute survey answers - q18
cdc_data2 = cdc_data2 %>%
  mutate(q18 = replace(q18, q18 == 1.0, "0 times")) %>%
  mutate(q18 = replace(q18, q18 == 2.0, "1 time")) %>%
  mutate(q18 = replace(q18, q18 == 3.0, "2 or 3 times")) %>%
  mutate(q18 = replace(q18, q18 == 4.0, "4 or 5 times")) %>%
  mutate(q18 = replace(q18, q18 == 5.0, "6 or 7 times")) %>%
  mutate(q18 = replace(q18, q18 == 6.0, "8 or 9 times")) %>%
  mutate(q18 = replace(q18, q18 == 7.0, "10 or 11 times")) %>%
  mutate(q18 = replace(q18, q18 == 8.0, "12 or more times"))
cdc_data2$q18 = as.factor(cdc_data2$q18)
# Rename column - q18
colnames(cdc_data2)[colnames(cdc_data2) == "q18"] = "phys_fight_sch"
head(cdc_data2)

# Impute survey answers - q19
cdc_data2 = cdc_data2 %>% 
  mutate(q19 = replace(q19, q19 == 1.0, "Yes")) %>%
  mutate(q19 = replace(q19, q19 == 2.0, "No"))
cdc_data2$q19 = as.factor(cdc_data2$q19)
head(cdc_data2)
# Rename Column - q19
colnames(cdc_data2)[colnames(cdc_data2) == "q19"] = "viol_dating1"
head(cdc_data2)

# Impute Survey answers - q20
cdc_data2 = cdc_data2 %>%
  mutate(q20 = replace(q20, q20 == 1.0, "0 times")) %>%
  mutate(q20 = replace(q20, q20 == 2.0, "1 time")) %>%
  mutate(q20 = replace(q20, q20 == 3.0, "2 or 3 times")) %>%
  mutate(q20 = replace(q20, q20 == 4.0, "4 or 5 times")) %>%
  mutate(q20 = replace(q20, q20 == 5.0, "6 or more times"))
cdc_data2$q20 = as.factor(cdc_data2$q20)
head(cdc_data2)
# rename Column
colnames(cdc_data2)[colnames(cdc_data2) == "q20"] = "viol_dating2"
head(cdc_data2)

# Impute Survey answers - q21
cdc_data2 = cdc_data2 %>%
  mutate(q21 = replace(q21, q21 == 1.0, "I did not date in past 12 mo")) %>%
  mutate(q21 = replace(q21, q21 == 2.0, "0 times")) %>%
  mutate(q21 = replace(q21, q21 == 3.0, "1 time")) %>%
  mutate(q21 = replace(q21, q21 == 4.0, "2 or 3 times")) %>%
  mutate(q21 = replace(q21, q21 == 5.0, "4 or 5 times")) %>%
  mutate(q21 = replace(q21, q21 == 6.0, "6 or more times"))
cdc_data2$q21 = as.factor(cdc_data2$q21)

head(cdc_data2)
# rename col - q21
colnames(cdc_data2)[colnames(cdc_data2) == "q21"] = "viol_dating3"
head(cdc_data2)

# Impute Survey answers - q22
cdc_data2 = cdc_data2 %>%
  mutate(q22 = replace(q22, q22 == 1.0, "I did not date in past 12 mo")) %>%
  mutate(q22 = replace(q22, q22 == 2.0, "0 times")) %>%
  mutate(q22 = replace(q22, q22 == 3.0, "1 time")) %>%
  mutate(q22 = replace(q22, q22 == 4.0, "2 or 3 times")) %>%
  mutate(q22 = replace(q22, q22 == 5.0, "4 or 5 times")) %>%
  mutate(q22 = replace(q22, q22 == 6.0, "6 or more times"))
cdc_data2$q22 = as.factor(cdc_data2$q22)
head(cdc_data2)
# rename col - q22
colnames(cdc_data2)[colnames(cdc_data2) == "q22"] = "viol_dating4"
head(cdc_data2)

# Impute survey answers - q23
cdc_data2 = cdc_data2 %>% 
  mutate(q23 = replace(q23, q23 == 1.0, "Yes")) %>%
  mutate(q23 = replace(q23, q23 == 2.0, "No"))
cdc_data2$q23 = as.factor(cdc_data2$q23)
head(cdc_data2)
# Rename Column - q23
colnames(cdc_data2)[colnames(cdc_data2) == "q23"] = "bullied_sch"
head(cdc_data2)

# Impute survey answers - q24
cdc_data2 = cdc_data2 %>% 
  mutate(q24 = replace(q24, q24 == 1.0, "Yes")) %>%
  mutate(q24 = replace(q24, q24 == 2.0, "No"))
cdc_data2$q24 = as.factor(cdc_data2$q24)
head(cdc_data2)
# Rename Column - q24
colnames(cdc_data2)[colnames(cdc_data2) == "q24"] = "bullied_elec"
head(cdc_data2)

# Impute survey answers - q25
cdc_data2 = cdc_data2 %>% 
  mutate(q25 = replace(q25, q25 == 1.0, "Yes")) %>%
  mutate(q25 = replace(q25, q25 == 2.0, "No"))
cdc_data2$q25 = as.factor(cdc_data2$q25)
head(cdc_data2)
# Rename Column - q25
colnames(cdc_data2)[colnames(cdc_data2) == "q25"] = "sad"
head(cdc_data2)

# Impute survey answers - q26
cdc_data2 = cdc_data2 %>% 
  mutate(q26 = replace(q26, q26 == 1.0, "Yes")) %>%
  mutate(q26 = replace(q26, q26 == 2.0, "No"))
cdc_data2$q26 = as.factor(cdc_data2$q26)
head(cdc_data2)
# Rename Column - q26
colnames(cdc_data2)[colnames(cdc_data2) == "q26"] = "suicide_consider"
head(cdc_data2)

# Impute survey answers - q27
cdc_data2 = cdc_data2 %>% 
  mutate(q27 = replace(q27, q27 == 1.0, "Yes")) %>%
  mutate(q27 = replace(q27, q27 == 2.0, "No"))
cdc_data2$q27 = as.factor(cdc_data2$q27)
head(cdc_data2)
# Rename Column - q27
colnames(cdc_data2)[colnames(cdc_data2) == "q27"] = "suicide_plan"
head(cdc_data2)

# Impute survey answers - q28
cdc_data2 = cdc_data2 %>% 
  mutate(q28 = replace(q28, q28 == 1.0, "0 times")) %>%
  mutate(q28 = replace(q28, q28 == 2.0, "1 time")) %>%
  mutate(q28 = replace(q28, q28 == 3.0, "2 or 3 times")) %>%
  mutate(q28 = replace(q28, q28 == 4.0, "4 or 5 times")) %>%
  mutate(q28 = replace(q28, q28 == 5.0, "6 or more times"))
cdc_data2$q28 = as.factor(cdc_data2$q28)
head(cdc_data2)
# Rename Column - q28
colnames(cdc_data2)[colnames(cdc_data2) == "q28"] = "suicide_attempt"
head(cdc_data2)

# Impute survey answers - q29
cdc_data2 = cdc_data2 %>% 
  mutate(q29 = replace(q29, q29 == 1.0, "Did not attempt 12 mo")) %>%
  mutate(q29 = replace(q29, q29 == 2.0, "Yes")) %>%
  mutate(q29 = replace(q29, q29 == 3.0, "No"))
cdc_data2$q29 = as.factor(cdc_data2$q29)
head(cdc_data2)
# Rename Column - q29
colnames(cdc_data2)[colnames(cdc_data2) == "q29"] = "suicide_treat"
head(cdc_data2)

# Impute survey answers - q30
cdc_data2 = cdc_data2 %>% 
  mutate(q30 = replace(q30, q30 == 1.0, "Yes")) %>%
  mutate(q30 = replace(q30, q30 == 2.0, "No"))

cdc_data2$q30 = as.factor(cdc_data2$q30)

head(cdc_data2)
# Rename Column - q30
colnames(cdc_data2)[colnames(cdc_data2) == "q30"] = "smoke_ever"
head(cdc_data2)

# Impute Survey answers - q31
cdc_data2 = cdc_data2 %>%
  mutate(q31 = replace(q31, q31 == 1.0, "I have never smoked")) %>%
  mutate(q31 = replace(q31, q31 == 2.0, "8 years or younger")) %>%
  mutate(q31 = replace(q31, q31 == 3.0, "9 to 10 years old")) %>%
  mutate(q31 = replace(q31, q31 == 4.0, "11 or 12 years old")) %>%
  mutate(q31 = replace(q31, q31 == 5.0, "13 or 14 years old")) %>%
  mutate(q31 = replace(q31, q31 == 6.0, "15 or 16 years old")) %>%
  mutate(q31 = replace(q31, q31 == 7.0, "17 years or older"))
cdc_data2$q31 = as.factor(cdc_data2$q31)
# Rename Column - q31
colnames(cdc_data2)[colnames(cdc_data2) == "q31"] = "smoke_first"
head(cdc_data2)

# Impute Survey answers - q32
cdc_data2 = cdc_data2 %>%
  mutate(q32 = replace(q32, q32 == 1.0, "0 days")) %>%
  mutate(q32 = replace(q32, q32 == 2.0, "1 to 2 days")) %>%
  mutate(q32 = replace(q32, q32 == 3.0, "3 to 5 days")) %>%
  mutate(q32 = replace(q32, q32 == 4.0, "6 to 9 days")) %>%
  mutate(q32 = replace(q32, q32 == 5.0, "10 to 19 days")) %>%
  mutate(q32 = replace(q32, q32 == 6.0, "20 to 29 days")) %>%
  mutate(q32 = replace(q32, q32 == 7.0, "All 30 days"))
cdc_data2$q32 = as.factor(cdc_data2$q32)
# Rename Column - q32
colnames(cdc_data2)[colnames(cdc_data2) == "q32"] = "smoke_days"
head(cdc_data2)

# Impute Survey answers - q33
cdc_data2 = cdc_data2 %>%
  mutate(q33 = replace(q33, q33 == 1.0, "Did not smoke past 30")) %>%
  mutate(q33 = replace(q33, q33 == 2.0, "< 1 cig")) %>%
  mutate(q33 = replace(q33, q33 == 3.0, "1 cig")) %>%
  mutate(q33 = replace(q33, q33 == 4.0, "2 to 5 cigs")) %>%
  mutate(q33 = replace(q33, q33 == 5.0, "6 to 10 cigs")) %>%
  mutate(q33 = replace(q33, q33 == 6.0, "11 to 20 cigs")) %>%
  mutate(q33 = replace(q33, q33 == 7.0, "20 < cigs"))
cdc_data2$q33 = as.factor(cdc_data2$q33)
# Rename Column - q33
colnames(cdc_data2)[colnames(cdc_data2) == "q33"] = "smoke_perday"
head(cdc_data2)

cdc_data2 = cdc_data2 %>% 
  mutate(q34 = replace(q34, q34 == 1.0, "Yes")) %>%
  mutate(q34 = replace(q34, q34 == 2.0, "No"))

cdc_data2$q34 = as.factor(cdc_data2$q34)
head(cdc_data2)
# Rename Column - q34
colnames(cdc_data2)[colnames(cdc_data2) == "q34"] = "vape"
head(cdc_data2)

# Impute Survey answers - q35
cdc_data2 = cdc_data2 %>%
  mutate(q35 = replace(q35, q35 == 1.0, "0 days")) %>%
  mutate(q35 = replace(q35, q35 == 2.0, "1 to 2 days")) %>%
  mutate(q35 = replace(q35, q35 == 3.0, "3 to 5 days")) %>%
  mutate(q35 = replace(q35, q35 == 4.0, "6 to 9 days")) %>%
  mutate(q35 = replace(q35, q35 == 5.0, "10 to 19 days")) %>%
  mutate(q35 = replace(q35, q35 == 6.0, "20 to 29 days")) %>%
  mutate(q35 = replace(q35, q35 == 7.0, "All 30 days"))
cdc_data2$q35 = as.factor(cdc_data2$q35)
# Rename Column - q35
colnames(cdc_data2)[colnames(cdc_data2) == "q35"] = "vape_days"
head(cdc_data2)

# Impute Survey answers - q36
cdc_data2 = cdc_data2 %>%
  mutate(q36 = replace(q36, q36 == 1.0, "Did not use past 30")) %>%
  mutate(q36 = replace(q36, q36 == 2.0, "In Store")) %>%
  mutate(q36 = replace(q36, q36 == 3.0, "Internet")) %>%
  mutate(q36 = replace(q36, q36 == 4.0, "Other Person")) %>%
  mutate(q36 = replace(q36, q36 == 5.0, "Borrowed")) %>%
  mutate(q36 = replace(q36, q36 == 6.0, "Person > 18")) %>%
  mutate(q36 = replace(q36, q36 == 7.0, "Stolen")) %>%
  mutate(q36 = replace(q36, q36 == 8.0, "Other"))
cdc_data2$q36 = as.factor(cdc_data2$q36)
# Rename Column - q36
colnames(cdc_data2)[colnames(cdc_data2) == "q36"] = "vape_purchase"
head(cdc_data2)


# Impute Survey answers - q37
cdc_data2 = cdc_data2 %>%
  mutate(q37 = replace(q37, q37 == 1.0, "0 days")) %>%
  mutate(q37 = replace(q37, q37 == 2.0, "1 to 2 days")) %>%
  mutate(q37 = replace(q37, q37 == 3.0, "3 to 5 days")) %>%
  mutate(q37 = replace(q37, q37 == 4.0, "6 to 9 days")) %>%
  mutate(q37 = replace(q37, q37 == 5.0, "10 to 19 days")) %>%
  mutate(q37 = replace(q37, q37 == 6.0, "20 to 29 days")) %>%
  mutate(q37 = replace(q37, q37 == 7.0, "All 30 days"))
cdc_data2$q37 = as.factor(cdc_data2$q37)
# Rename Column - q35
colnames(cdc_data2)[colnames(cdc_data2) == "q37"] = "chew_tobac"
head(cdc_data2)

# Impute Survey answers - q38
cdc_data2 = cdc_data2 %>%
  mutate(q38 = replace(q38, q38 == 1.0, "0 days")) %>%
  mutate(q38 = replace(q38, q38 == 2.0, "1 to 2 days")) %>%
  mutate(q38 = replace(q38, q38 == 3.0, "3 to 5 days")) %>%
  mutate(q38 = replace(q38, q38 == 4.0, "6 to 9 days")) %>%
  mutate(q38 = replace(q38, q38 == 5.0, "10 to 19 days")) %>%
  mutate(q38 = replace(q38, q38 == 6.0, "20 to 29 days")) %>%
  mutate(q38 = replace(q38, q38 == 7.0, "All 30 days"))
cdc_data2$q38 = as.factor(cdc_data2$q38)
# Rename Column - q38
colnames(cdc_data2)[colnames(cdc_data2) == "q38"] = "oth_tobac"
head(cdc_data2)

cdc_data2 = cdc_data2 %>% 
  mutate(q39 = replace(q39, q39 == 1.0, "did not use tobac")) %>%
  mutate(q39 = replace(q39, q39 == 2.0, "Yes")) %>%
  mutate(q39 = replace(q39, q39 == 3.0, "No"))
cdc_data2$q39 = as.factor(cdc_data2$q39)
head(cdc_data2)
# Rename Column - q39
colnames(cdc_data2)[colnames(cdc_data2) == "q39"] = "quit_all_tobac"
head(cdc_data2)

# Impute Survey answers - q40
cdc_data2 = cdc_data2 %>%
  mutate(q40 = replace(q40, q40 == 1.0, "0 days")) %>%
  mutate(q40 = replace(q40, q40 == 2.0, "1 to 2 days")) %>%
  mutate(q40 = replace(q40, q40 == 3.0, "3 to 9 days")) %>%
  mutate(q40 = replace(q40, q40 == 4.0, "10 to 19 days")) %>%
  mutate(q40 = replace(q40, q40 == 5.0, "20 to 39 days")) %>%
  mutate(q40 = replace(q40, q40 == 6.0, "40 to 99 days")) %>%
  mutate(q40 = replace(q40, q40 == 7.0, " > 100 days"))
cdc_data2$q40 = as.factor(cdc_data2$q40)
# Rename Column - q40
colnames(cdc_data2)[colnames(cdc_data2) == "q40"] = "drink_ever"
head(cdc_data2)

# Impute Survey answers - q41
cdc_data2 = cdc_data2 %>%
  mutate(q41 = replace(q41, q41 == 1.0, "I have never drank")) %>%
  mutate(q41 = replace(q41, q41 == 2.0, "8 years or younger")) %>%
  mutate(q41 = replace(q41, q41 == 3.0, "9 to 10 years old")) %>%
  mutate(q41 = replace(q41, q41 == 4.0, "11 or 12 years old")) %>%
  mutate(q41 = replace(q41, q41 == 5.0, "13 or 14 years old")) %>%
  mutate(q41 = replace(q41, q41 == 6.0, "15 or 16 years old")) %>%
  mutate(q41 = replace(q41, q41 == 7.0, "17 years or older"))
cdc_data2$q41 = as.factor(cdc_data2$q41)
# Rename Column - q41
colnames(cdc_data2)[colnames(cdc_data2) == "q41"] = "drink_age"
head(cdc_data2)

# Impute Survey answers - q42
cdc_data2 = cdc_data2 %>%
  mutate(q42 = replace(q42, q42 == 1.0, "0 days")) %>%
  mutate(q42 = replace(q42, q42 == 2.0, "1 to 2 days")) %>%
  mutate(q42 = replace(q42, q42 == 3.0, "3 to 5 days")) %>%
  mutate(q42 = replace(q42, q42 == 4.0, "6 to 9 days")) %>%
  mutate(q42 = replace(q42, q42 == 5.0, "10 to 19 days")) %>%
  mutate(q42 = replace(q42, q42 == 6.0, "20 to 29 days")) %>%
  mutate(q42 = replace(q42, q42 == 7.0, "All 30 days"))
cdc_data2$q42 = as.factor(cdc_data2$q42)
# Rename Column - q42
colnames(cdc_data2)[colnames(cdc_data2) == "q42"] = "drink_days"
head(cdc_data2)

# Impute Survey answers - q43
cdc_data2 = cdc_data2 %>%
  mutate(q43 = replace(q43, q43 == 1.0, "Did not use past 30")) %>%
  mutate(q43 = replace(q43, q43 == 2.0, "In Store")) %>%
  mutate(q43 = replace(q43, q43 == 3.0, "Bar/rest")) %>%
  mutate(q43 = replace(q43, q43 == 4.0, "Event")) %>%
  mutate(q43 = replace(q43, q43 == 5.0, "Other Person")) %>%
  mutate(q43 = replace(q43, q43 == 6.0, "Given")) %>%
  mutate(q43 = replace(q43, q43 == 7.0, "Stolen")) %>%
  mutate(q43 = replace(q43, q43 == 8.0, "Other"))
cdc_data2$q43 = as.factor(cdc_data2$q43)
# Rename Column - q43
colnames(cdc_data2)[colnames(cdc_data2) == "q43"] = "drink_purchase"
head(cdc_data2)

# Impute Survey answers - q44
cdc_data2 = cdc_data2 %>%
  mutate(q44 = replace(q44, q44 == 1.0, "0 days")) %>%
  mutate(q44 = replace(q44, q44 == 2.0, "1 day")) %>%
  mutate(q44 = replace(q44, q44 == 3.0, "2 days")) %>%
  mutate(q44 = replace(q44, q44 == 4.0, "3 to 5 days")) %>%
  mutate(q44 = replace(q44, q44 == 5.0, "6 to 9 days")) %>%
  mutate(q44 = replace(q44, q44 == 6.0, "10 to 19 days")) %>%
  mutate(q44 = replace(q44, q44 == 7.0, "20 or more days"))
cdc_data2$q44 = as.factor(cdc_data2$q44)
# Rename Column - q44
colnames(cdc_data2)[colnames(cdc_data2) == "q44"] = "drink_binge"
head(cdc_data2)

# Impute Survey answers - q45
cdc_data2 = cdc_data2 %>%
  mutate(q45 = replace(q45, q45 == 1.0, "Did not use past 30")) %>%
  mutate(q45 = replace(q45, q45 == 2.0, "1 or 2 drinks")) %>%
  mutate(q45 = replace(q45, q45 == 3.0, "3 drinks")) %>%
  mutate(q45 = replace(q45, q45 == 4.0, "4 drinks")) %>%
  mutate(q45 = replace(q45, q45 == 5.0, "5 drinks")) %>%
  mutate(q45 = replace(q45, q45 == 6.0, "6 or 7 drinks")) %>%
  mutate(q45 = replace(q45, q45 == 7.0, "8 or 9 drinks")) %>%
  mutate(q45 = replace(q45, q45 == 8.0, " > 10 drinks"))
cdc_data2$q45 = as.factor(cdc_data2$q45)
# Rename Column - q45
colnames(cdc_data2)[colnames(cdc_data2) == "q45"] = "drink_inrow"
head(cdc_data2)

# Impute Survey answers - q46
cdc_data2 = cdc_data2 %>%
  mutate(q46 = replace(q46, q46 == 1.0, "0 times")) %>%
  mutate(q46 = replace(q46, q46 == 2.0, "1 to 2 times")) %>%
  mutate(q46 = replace(q46, q46 == 3.0, "3 to 9 times")) %>%
  mutate(q46 = replace(q46, q46 == 4.0, "10 to 19 times")) %>%
  mutate(q46 = replace(q46, q46 == 5.0, "20 to 39 times")) %>%
  mutate(q46 = replace(q46, q46 == 6.0, "40 to 99 times")) %>%
  mutate(q46 = replace(q46, q46 == 7.0, " > 100 times"))
cdc_data2$q46 = as.factor(cdc_data2$q46)
# Rename Column - q46
colnames(cdc_data2)[colnames(cdc_data2) == "q46"] = "marijuana"
head(cdc_data2)

# Impute Survey answers - q47
cdc_data2 = cdc_data2 %>%
  mutate(q47 = replace(q47, q47 == 1.0, "I have never used")) %>%
  mutate(q47 = replace(q47, q47 == 2.0, "8 years or younger")) %>%
  mutate(q47 = replace(q47, q47 == 3.0, "9 to 10 years old")) %>%
  mutate(q47 = replace(q47, q47 == 4.0, "11 or 12 years old")) %>%
  mutate(q47 = replace(q47, q47 == 5.0, "13 or 14 years old")) %>%
  mutate(q47 = replace(q47, q47 == 6.0, "15 or 16 years old")) %>%
  mutate(q47 = replace(q47, q47 == 7.0, "17 years or older"))
cdc_data2$q47 = as.factor(cdc_data2$q47)
# Rename Column - q47
colnames(cdc_data2)[colnames(cdc_data2) == "q47"] = "marijuana_age"
head(cdc_data2)

# Impute Survey answers - q48
cdc_data2 = cdc_data2 %>%
  mutate(q48 = replace(q48, q48 == 1.0, "0 times")) %>%
  mutate(q48 = replace(q48, q48 == 2.0, "1 to 2 times")) %>%
  mutate(q48 = replace(q48, q48 == 3.0, "3 to 9 times")) %>%
  mutate(q48 = replace(q48, q48 == 4.0, "10 to 19 times")) %>%
  mutate(q48 = replace(q48, q48 == 5.0, "20 to 39 times")) %>%
  mutate(q48 = replace(q48, q48 == 6.0, "40 or more times")) 
cdc_data2$q48 = as.factor(cdc_data2$q48)
# Rename Column - q48
colnames(cdc_data2)[colnames(cdc_data2) == "q48"] = "marijuana_use"
head(cdc_data2)

# Impute Survey answers - q49
cdc_data2 = cdc_data2 %>%
  mutate(q49 = replace(q49, q49 == 1.0, "0 times")) %>%
  mutate(q49 = replace(q49, q49 == 2.0, "1 to 2 times")) %>%
  mutate(q49 = replace(q49, q49 == 3.0, "3 to 9 times")) %>%
  mutate(q49 = replace(q49, q49 == 4.0, "10 to 19 times")) %>%
  mutate(q49 = replace(q49, q49 == 5.0, "20 to 39 times")) %>%
  mutate(q49 = replace(q49, q49 == 6.0, "40 or more times")) 
cdc_data2$q49 = as.factor(cdc_data2$q49)
# Rename Column - q49
colnames(cdc_data2)[colnames(cdc_data2) == "q49"] = "drug_cocaine"
head(cdc_data2)

# Impute Survey answers - q50
cdc_data2 = cdc_data2 %>%
  mutate(q50 = replace(q50, q50 == 1.0, "0 times")) %>%
  mutate(q50 = replace(q50, q50 == 2.0, "1 to 2 times")) %>%
  mutate(q50 = replace(q50, q50 == 3.0, "3 to 9 times")) %>%
  mutate(q50 = replace(q50, q50 == 4.0, "10 to 19 times")) %>%
  mutate(q50 = replace(q50, q50 == 5.0, "20 to 39 times")) %>%
  mutate(q50 = replace(q50, q50 == 6.0, "40 or more times")) 
cdc_data2$q50 = as.factor(cdc_data2$q50)
# Rename Column - q50
colnames(cdc_data2)[colnames(cdc_data2) == "q50"] = "drug_inhalents"
head(cdc_data2)

# Impute Survey answers - q51
cdc_data2 = cdc_data2 %>%
  mutate(q51 = replace(q51, q51 == 1.0, "0 times")) %>%
  mutate(q51 = replace(q51, q51 == 2.0, "1 to 2 times")) %>%
  mutate(q51 = replace(q51, q51 == 3.0, "3 to 9 times")) %>%
  mutate(q51 = replace(q51, q51 == 4.0, "10 to 19 times")) %>%
  mutate(q51 = replace(q51, q51 == 5.0, "20 to 39 times")) %>%
  mutate(q51 = replace(q51, q51 == 6.0, "40 or more times")) 
cdc_data2$q51 = as.factor(cdc_data2$q51)
# Rename Column - q51
colnames(cdc_data2)[colnames(cdc_data2) == "q51"] = "drug_heroin"
head(cdc_data2)

# Impute Survey answers - q52
cdc_data2 = cdc_data2 %>%
  mutate(q52 = replace(q52, q52 == 1.0, "0 times")) %>%
  mutate(q52 = replace(q52, q52 == 2.0, "1 to 2 times")) %>%
  mutate(q52 = replace(q52, q52 == 3.0, "3 to 9 times")) %>%
  mutate(q52 = replace(q52, q52 == 4.0, "10 to 19 times")) %>%
  mutate(q52 = replace(q52, q52 == 5.0, "20 to 39 times")) %>%
  mutate(q52 = replace(q52, q52 == 6.0, "40 or more times")) 
cdc_data2$q52 = as.factor(cdc_data2$q52)
# Rename Column - q52
colnames(cdc_data2)[colnames(cdc_data2) == "q52"] = "drug_meth"
head(cdc_data2)


# Impute Survey answers - q53
cdc_data2 = cdc_data2 %>%
  mutate(q53 = replace(q53, q53 == 1.0, "0 times")) %>%
  mutate(q53 = replace(q53, q53 == 2.0, "1 to 2 times")) %>%
  mutate(q53 = replace(q53, q53 == 3.0, "3 to 9 times")) %>%
  mutate(q53 = replace(q53, q53 == 4.0, "10 to 19 times")) %>%
  mutate(q53 = replace(q53, q53 == 5.0, "20 to 39 times")) %>%
  mutate(q53 = replace(q53, q53 == 6.0, "40 or more times")) 
cdc_data2$q53 = as.factor(cdc_data2$q53)
# Rename Column - q53
colnames(cdc_data2)[colnames(cdc_data2) == "q53"] = "drug_X"
head(cdc_data2)

# Impute Survey answers - q54
cdc_data2 = cdc_data2 %>%
  mutate(q54 = replace(q54, q54 == 1.0, "0 times")) %>%
  mutate(q54 = replace(q54, q54 == 2.0, "1 to 2 times")) %>%
  mutate(q54 = replace(q54, q54 == 3.0, "3 to 9 times")) %>%
  mutate(q54 = replace(q54, q54 == 4.0, "10 to 19 times")) %>%
  mutate(q54 = replace(q54, q54 == 5.0, "20 to 39 times")) %>%
  mutate(q54 = replace(q54, q54 == 6.0, "40 or more times")) 
cdc_data2$q54 = as.factor(cdc_data2$q54)
# Rename Column - q54
colnames(cdc_data2)[colnames(cdc_data2) == "q54"] = "drug_synth_mj"
head(cdc_data2)

# Impute Survey answers - q55
cdc_data2 = cdc_data2 %>%
  mutate(q55 = replace(q55, q55 == 1.0, "0 times")) %>%
  mutate(q55 = replace(q55, q55 == 2.0, "1 to 2 times")) %>%
  mutate(q55 = replace(q55, q55 == 3.0, "3 to 9 times")) %>%
  mutate(q55 = replace(q55, q55 == 4.0, "10 to 19 times")) %>%
  mutate(q55 = replace(q55, q55 == 5.0, "20 to 39 times")) %>%
  mutate(q55 = replace(q55, q55 == 6.0, "40 or more times")) 
cdc_data2$q55 = as.factor(cdc_data2$q55)
# Rename Column - q51
colnames(cdc_data2)[colnames(cdc_data2) == "q55"] = "drug_pills"
head(cdc_data2)

# Impute Survey answers - q56
cdc_data2 = cdc_data2 %>%
  mutate(q56 = replace(q56, q56 == 1.0, "0 times")) %>%
  mutate(q56 = replace(q56, q56 == 2.0, "1 to 2 times")) %>%
  mutate(q56 = replace(q56, q56 == 3.0, "3 to 9 times")) %>%
  mutate(q56 = replace(q56, q56 == 4.0, "10 to 19 times")) %>%
  mutate(q56 = replace(q56, q56 == 5.0, "20 to 39 times")) %>%
  mutate(q56 = replace(q56, q56 == 6.0, "40 or more times")) 
cdc_data2$q56 = as.factor(cdc_data2$q56)
# Rename Column - q56
colnames(cdc_data2)[colnames(cdc_data2) == "q56"] = "drug_Rx_abuse"
head(cdc_data2)

cdc_data2 = cdc_data2 %>% 
  mutate(q57 = replace(q57, q57 == 1.0, "0 times")) %>%
  mutate(q57 = replace(q57, q57 == 2.0, "1 time")) %>%
  mutate(q57 = replace(q57, q57 == 3.0, " > 2 times"))
cdc_data2$q57 = as.factor(cdc_data2$q57)
head(cdc_data2)
# Rename Column - q57
colnames(cdc_data2)[colnames(cdc_data2) == "q57"] = "drug_needle_use"
head(cdc_data2)

# Impute 58
cdc_data2 = cdc_data2 %>% 
  mutate(q58 = replace(q58, q58 == 1.0, "Yes")) %>%
  mutate(q58 = replace(q58, q58 == 2.0, "No"))

cdc_data2$q58 = as.factor(cdc_data2$q58)
head(cdc_data2)
# Rename Column - q58
colnames(cdc_data2)[colnames(cdc_data2) == "q58"] = "drugs_sch"
head(cdc_data2)

# Impute 59
cdc_data2 = cdc_data2 %>% 
  mutate(q59 = replace(q59, q59 == 1.0, "Yes")) %>%
  mutate(q59 = replace(q59, q59 == 2.0, "No"))

cdc_data2$q59 = as.factor(cdc_data2$q59)
head(cdc_data2)
# Rename Column - q59
colnames(cdc_data2)[colnames(cdc_data2) == "q59"] = "sex_ever"
head(cdc_data2)

# Impute Survey answers - q60
cdc_data2 = cdc_data2 %>%
  mutate(q60 = replace(q60, q60 == 1.0, "I have never had sex")) %>%
  mutate(q60 = replace(q60, q60 == 2.0, "11 years or younger")) %>%
  mutate(q60 = replace(q60, q60 == 3.0, "12 years old")) %>%
  mutate(q60 = replace(q60, q60 == 4.0, "13 years old")) %>%
  mutate(q60 = replace(q60, q60 == 5.0, "14 years old")) %>%
  mutate(q60 = replace(q60, q60 == 6.0, "15 years old")) %>%
  mutate(q60 = replace(q60, q60 == 7.0, "16 years old")) %>%
  mutate(q60 = replace(q60, q60 == 8.0, " > 17 years old"))
cdc_data2$q60 = as.factor(cdc_data2$q60)
# Rename Column - q60
colnames(cdc_data2)[colnames(cdc_data2) == "q60"] = "sex_age"
head(cdc_data2)

# Impute Survey answers - q61
cdc_data2 = cdc_data2 %>%
  mutate(q61 = replace(q61, q61 == 1.0, "I have never had sex")) %>%
  mutate(q61 = replace(q61, q61 == 2.0, "1 person")) %>%
  mutate(q61 = replace(q61, q61 == 3.0, "2 people")) %>%
  mutate(q61 = replace(q61, q61 == 4.0, "3 people")) %>%
  mutate(q61 = replace(q61, q61 == 5.0, "4 people")) %>%
  mutate(q61 = replace(q61, q61 == 6.0, "5 people")) %>%
  mutate(q61 = replace(q61, q61 == 7.0, "6 or more people"))
cdc_data2$q61 = as.factor(cdc_data2$q61)
# Rename Column - q61
colnames(cdc_data2)[colnames(cdc_data2) == "q61"] = "sex_partners"
head(cdc_data2)

# Impute Survey answers - q62
cdc_data2 = cdc_data2 %>%
  mutate(q62 = replace(q62, q62 == 1.0, "I have never had sex")) %>%
  mutate(q62 = replace(q62, q62 == 2.0, "I have, but not past 3 mo")) %>%
  mutate(q62 = replace(q62, q62 == 3.0, "1 person")) %>%
  mutate(q62 = replace(q62, q62 == 4.0, "2 people")) %>%
  mutate(q62 = replace(q62, q62 == 5.0, "3 people")) %>%
  mutate(q62 = replace(q62, q62 == 6.0, "4 people")) %>%
  mutate(q62 = replace(q62, q62 == 7.0, "5 people")) %>%
  mutate(q62 = replace(q62, q62 == 8.0, "6 or more people"))
cdc_data2$q62 = as.factor(cdc_data2$q62)
# Rename Column - q62
colnames(cdc_data2)[colnames(cdc_data2) == "q62"] = "sex_partners_3mo"
head(cdc_data2)

# Impute 63
cdc_data2 = cdc_data2 %>% 
  mutate(q63 = replace(q63, q63 == 1.0, "Never had sex")) %>%
  mutate(q63 = replace(q63, q63 == 2.0, "Yes")) %>%
  mutate(q63 = replace(q63, q63 == 3.0, "No"))
cdc_data2$q63 = as.factor(cdc_data2$q63)
head(cdc_data2)
# Rename Column - q63
colnames(cdc_data2)[colnames(cdc_data2) == "q63"] = "sex_drugs"
head(cdc_data2)

# Impute 64
cdc_data2 = cdc_data2 %>% 
  mutate(q64 = replace(q64, q64 == 1.0, "Never had sex")) %>%
  mutate(q64 = replace(q64, q64 == 2.0, "Yes")) %>%
  mutate(q64 = replace(q64, q64 == 3.0, "No"))
cdc_data2$q64 = as.factor(cdc_data2$q64)
head(cdc_data2)
# Rename Column - q64
colnames(cdc_data2)[colnames(cdc_data2) == "q64"] = "sex_condom"
head(cdc_data2)

# Impute 65
cdc_data2 = cdc_data2 %>% 
  mutate(q65 = replace(q65, q65 == 1.0, "Never had sex")) %>%
  mutate(q65 = replace(q65, q65 == 2.0, "No method")) %>%
  mutate(q65 = replace(q65, q65 == 3.0, "Birth control pills")) %>%
  mutate(q65 = replace(q65, q65 == 4.0, "Condoms")) %>%
  mutate(q65 = replace(q65, q65 == 5.0, "IUD")) %>%
  mutate(q65 = replace(q65, q65 == 6.0, "Shot/patch/ring")) %>%
  mutate(q65 = replace(q65, q65 == 7.0, "Withdrawl")) %>%
  mutate(q65 = replace(q65, q65 == 8.0, "Not sure"))
cdc_data2$q65 = as.factor(cdc_data2$q65)
head(cdc_data2)
# Rename Column - q64
colnames(cdc_data2)[colnames(cdc_data2) == "q65"] = "sex_contraception"
head(cdc_data2)

# Impute 66
cdc_data2 = cdc_data2 %>% 
  mutate(q66 = replace(q66, q66 == 1.0, "Never had sex")) %>%
  mutate(q66 = replace(q66, q66 == 2.0, "Females")) %>%
  mutate(q66 = replace(q66, q66 == 3.0, "Males")) %>%
  mutate(q66 = replace(q66, q66 == 4.0, "Both"))
cdc_data2$q66 = as.factor(cdc_data2$q66)
head(cdc_data2)
# Rename Column - q66
colnames(cdc_data2)[colnames(cdc_data2) == "q66"] = "sex_contact"
head(cdc_data2)

# Impute 68
cdc_data2 = cdc_data2 %>% 
  mutate(q68 = replace(q68, q68 == 1.0, "very underweight")) %>%
  mutate(q68 = replace(q68, q68 == 2.0, "slightly underweight")) %>%
  mutate(q68 = replace(q68, q68 == 3.0, "average")) %>%
  mutate(q68 = replace(q68, q68 == 4.0, "slightly overweight")) %>%
  mutate(q68 = replace(q68, q68 == 5.0, "very overweight")) 
cdc_data2$q68 = as.factor(cdc_data2$q68)
head(cdc_data2)
# Rename Column - q68
colnames(cdc_data2)[colnames(cdc_data2) == "q68"] = "weight_percept"
head(cdc_data2)

# Impute 69
cdc_data2 = cdc_data2 %>% 
  mutate(q69 = replace(q69, q69 == 1.0, "Lose weight")) %>%
  mutate(q69 = replace(q69, q69 == 2.0, "Gain Weight")) %>%
  mutate(q69 = replace(q69, q69 == 3.0, "Stay the same")) %>%
  mutate(q69 = replace(q69, q69 == 4.0, "Nothing"))
cdc_data2$q69 = as.factor(cdc_data2$q69)
head(cdc_data2)
# Rename Column - q66
colnames(cdc_data2)[colnames(cdc_data2) == "q69"] = "weight_control"
head(cdc_data2)


############## q 67 
cdc_data2 = cdc_data2 %>% 
  mutate(q67 = replace(q67, q67 == 1.0, "Heterosexual")) %>%
  mutate(q67 = replace(q67, q67 == 2.0, "Gay or Lesbian")) %>%
  mutate(q67 = replace(q67, q67 == 3.0, "Bisexual")) %>%
  mutate(q67 = replace(q67, q67 == 4.0, "Not sure"))
cdc_data2$q67 = as.factor(cdc_data2$q67)
head(cdc_data2)
# Rename Column - q67
colnames(cdc_data2)[colnames(cdc_data2) == "q67"] = "sex_id"
head(cdc_data2)



## Impute q70
cdc_data2 = cdc_data2 %>%
  mutate(q70 = replace(q70, q70 == 1.0, "did not drink fruit juice")) %>%
  mutate(q70 = replace(q70, q70 == 2.0, "1 - 3 times in last week")) %>%
  mutate(q70 = replace(q70, q70 == 3.0, "4 - 6 times in last week")) %>%
  mutate(q70 = replace(q70, q70 == 4.0, "1 time a day")) %>%
  mutate(q70 = replace(q70, q70 == 5.0, "2 times a day")) %>%
  mutate(q70 = replace(q70, q70 == 6.0, "3 times a day")) %>%
  mutate(q70 = replace(q70, q70 == 7.0, "4 or more times a day")) 
cdc_data2$q70 = as.factor(cdc_data2$q70)
# rename q70
colnames(cdc_data2)[colnames(cdc_data2) == "q70"] = "fruit_juice"
head(cdc_data2)

## Impute q71
cdc_data2 = cdc_data2 %>%
  mutate(q71 = replace(q71, q71 == 1.0, "did not eat fruit")) %>%
  mutate(q71 = replace(q71, q71 == 2.0, "1 - 3 times in last week")) %>%
  mutate(q71 = replace(q71, q71 == 3.0, "4 - 6 times in last week")) %>%
  mutate(q71 = replace(q71, q71 == 4.0, "1 time a day")) %>%
  mutate(q71 = replace(q71, q71 == 5.0, "2 times a day")) %>%
  mutate(q71 = replace(q71, q71 == 6.0, "3 times a day")) %>%
  mutate(q71 = replace(q71, q71 == 7.0, "4 or more times a day")) 
cdc_data2$q71 = as.factor(cdc_data2$q71)
# rename q71
colnames(cdc_data2)[colnames(cdc_data2) == "q71"] = "fruit_consumption"
head(cdc_data2)

## Impute q72
cdc_data2 = cdc_data2 %>%
  mutate(q72 = replace(q72, q72 == 1.0, "did not eat salad")) %>%
  mutate(q72 = replace(q72, q72 == 2.0, "1 - 3 times in last week")) %>%
  mutate(q72 = replace(q72, q72 == 3.0, "4 - 6 times in last week")) %>%
  mutate(q72 = replace(q72, q72 == 4.0, "1 time a day")) %>%
  mutate(q72 = replace(q72, q72 == 5.0, "2 times a day")) %>%
  mutate(q72 = replace(q72, q72 == 6.0, "3 times a day")) %>%
  mutate(q72 = replace(q72, q72 == 7.0, "4 or more times a day")) 
cdc_data2$q72 = as.factor(cdc_data2$q72)
# rename q72
colnames(cdc_data2)[colnames(cdc_data2) == "q72"] = "salad_consumption"
head(cdc_data2)

## Impute q73
cdc_data2 = cdc_data2 %>%
  mutate(q73 = replace(q73, q73 == 1.0, "did not eat potatoes")) %>%
  mutate(q73 = replace(q73, q73 == 2.0, "1 - 3 times in last week")) %>%
  mutate(q73 = replace(q73, q73 == 3.0, "4 - 6 times in last week")) %>%
  mutate(q73 = replace(q73, q73 == 4.0, "1 time a day")) %>%
  mutate(q73 = replace(q73, q73 == 5.0, "2 times a day")) %>%
  mutate(q73 = replace(q73, q73 == 6.0, "3 times a day")) %>%
  mutate(q73 = replace(q73, q73 == 7.0, "4 or more times a day")) 
cdc_data2$q73 = as.factor(cdc_data2$q73)
# rename q73
colnames(cdc_data2)[colnames(cdc_data2) == "q73"] = "potato_consumption"
head(cdc_data2)

## Impute q74
cdc_data2 = cdc_data2 %>%
  mutate(q74 = replace(q74, q74 == 1.0, "did not eat salad")) %>%
  mutate(q74 = replace(q74, q74 == 2.0, "1 - 3 times in last week")) %>%
  mutate(q74 = replace(q74, q74 == 3.0, "4 - 6 times in last week")) %>%
  mutate(q74 = replace(q74, q74 == 4.0, "1 time a day")) %>%
  mutate(q74 = replace(q74, q74 == 5.0, "2 times a day")) %>%
  mutate(q74 = replace(q74, q74 == 6.0, "3 times a day")) %>%
  mutate(q74 = replace(q74, q74 == 7.0, "4 or more times a day")) 
cdc_data2$q74 = as.factor(cdc_data2$q74)
# rename q74
colnames(cdc_data2)[colnames(cdc_data2) == "q74"] = "carrot_consumption"
head(cdc_data2)

## Impute q75
cdc_data2 = cdc_data2 %>%
  mutate(q75 = replace(q75, q75 == 1.0, "did not eat other veggies")) %>%
  mutate(q75 = replace(q75, q75 == 2.0, "1 - 3 times in last week")) %>%
  mutate(q75 = replace(q75, q75 == 3.0, "4 - 6 times in last week")) %>%
  mutate(q75 = replace(q75, q75 == 4.0, "1 time a day")) %>%
  mutate(q75 = replace(q75, q75 == 5.0, "2 times a day")) %>%
  mutate(q75 = replace(q75, q75 == 6.0, "3 times a day")) %>%
  mutate(q75 = replace(q75, q75 == 7.0, "4 or more times a day")) 
cdc_data2$q75 = as.factor(cdc_data2$q75)
# rename q75
colnames(cdc_data2)[colnames(cdc_data2) == "q75"] = "veggie_consumption"
head(cdc_data2)

## Impute q76
cdc_data2 = cdc_data2 %>%
  mutate(q76 = replace(q76, q76 == 1.0, "did not drink soda")) %>%
  mutate(q76 = replace(q76, q76 == 2.0, "1 - 3 times in last week")) %>%
  mutate(q76 = replace(q76, q76 == 3.0, "4 - 6 times in last week")) %>%
  mutate(q76 = replace(q76, q76 == 4.0, "1 time a day")) %>%
  mutate(q76 = replace(q76, q76 == 5.0, "2 times a day")) %>%
  mutate(q76 = replace(q76, q76 == 6.0, "3 times a day")) %>%
  mutate(q76 = replace(q76, q76 == 7.0, "4 or more times a day")) 
cdc_data2$q76 = as.factor(cdc_data2$q76)
# rename q76
colnames(cdc_data2)[colnames(cdc_data2) == "q76"] = "soda_consumption"
head(cdc_data2)

## Impute q77
cdc_data2 = cdc_data2 %>%
  mutate(q77 = replace(q77, q77 == 1.0, "did not drink milk")) %>%
  mutate(q77 = replace(q77, q77 == 2.0, "1 - 3 times in last week")) %>%
  mutate(q77 = replace(q77, q77 == 3.0, "4 - 6 times in last week")) %>%
  mutate(q77 = replace(q77, q77 == 4.0, "1 time a day")) %>%
  mutate(q77 = replace(q77, q77 == 5.0, "2 times a day")) %>%
  mutate(q77 = replace(q77, q77 == 6.0, "3 times a day")) %>%
  mutate(q77 = replace(q77, q77 == 7.0, "4 or more times a day")) 
cdc_data2$q77 = as.factor(cdc_data2$q77)
# rename q77
colnames(cdc_data2)[colnames(cdc_data2) == "q77"] = "milk_consumption"
head(cdc_data2)

## Impute q78
cdc_data2 = cdc_data2 %>%
  mutate(q78 = replace(q78, q78 == 1.0, "0 days")) %>%
  mutate(q78 = replace(q78, q78 == 2.0, "1 day")) %>%
  mutate(q78 = replace(q78, q78 == 3.0, "2 days")) %>%
  mutate(q78 = replace(q78, q78 == 4.0, "3 days")) %>%
  mutate(q78 = replace(q78, q78 == 5.0, "4 days")) %>%
  mutate(q78 = replace(q78, q78 == 6.0, "5 days")) %>%
  mutate(q78 = replace(q78, q78 == 7.0, "6 days")) %>%
  mutate(q78 = replace(q78, q78 == 8.0, "7 days"))

cdc_data2$q78 = as.factor(cdc_data2$q78)
# rename q78
colnames(cdc_data2)[colnames(cdc_data2) == "q78"] = "breakfast"
head(cdc_data2)

## Impute q79
cdc_data2 = cdc_data2 %>%
  mutate(q79 = replace(q79, q79 == 1.0, "0 days")) %>%
  mutate(q79 = replace(q79, q79 == 2.0, "1 day")) %>%
  mutate(q79 = replace(q79, q79 == 3.0, "2 days")) %>%
  mutate(q79 = replace(q79, q79 == 4.0, "3 days")) %>%
  mutate(q79 = replace(q79, q79 == 5.0, "4 days")) %>%
  mutate(q79 = replace(q79, q79 == 6.0, "5 days")) %>%
  mutate(q79 = replace(q79, q79 == 7.0, "6 days")) %>%
  mutate(q79 = replace(q79, q79 == 8.0, "7 days"))

cdc_data2$q79 = as.factor(cdc_data2$q79)
# rename q79
colnames(cdc_data2)[colnames(cdc_data2) == "q79"] = "exercise"
head(cdc_data2)

## Impute q80
cdc_data2 = cdc_data2 %>%
  mutate(q80 = replace(q80, q80 == 1.0, "did not watch TV")) %>%
  mutate(q80 = replace(q80, q80 == 2.0, " < 1 hour")) %>%
  mutate(q80 = replace(q80, q80 == 3.0, " 1 hour")) %>%
  mutate(q80 = replace(q80, q80 == 4.0, " 2 hours")) %>%
  mutate(q80 = replace(q80, q80 == 5.0, " 3 hours")) %>%
  mutate(q80 = replace(q80, q80 == 6.0, " 4 hours")) %>%
  mutate(q80 = replace(q80, q80 == 7.0, " >5 hours")) 
cdc_data2$q80 = as.factor(cdc_data2$q80)
# rename q80
colnames(cdc_data2)[colnames(cdc_data2) == "q80"] = "TV_hours"
head(cdc_data2)

## Impute q81
cdc_data2 = cdc_data2 %>%
  mutate(q81 = replace(q81, q81 == 1.0, "did not use computer")) %>%
  mutate(q81 = replace(q81, q81 == 2.0, " < 1 hour")) %>%
  mutate(q81 = replace(q81, q81 == 3.0, " 1 hour")) %>%
  mutate(q81 = replace(q81, q81 == 4.0, " 2 hours")) %>%
  mutate(q81 = replace(q81, q81 == 5.0, " 3 hours")) %>%
  mutate(q81 = replace(q81, q81 == 6.0, " 4 hours")) %>%
  mutate(q81 = replace(q81, q81 == 7.0, " >5 hours")) 
cdc_data2$q81 = as.factor(cdc_data2$q81)
# rename q81
colnames(cdc_data2)[colnames(cdc_data2) == "q81"] = "computer_hours"
head(cdc_data2)

## Impute q82
cdc_data2 = cdc_data2 %>%
  mutate(q82 = replace(q82, q82 == 1.0, "0 days")) %>%
  mutate(q82 = replace(q82, q82 == 2.0, "1 day")) %>%
  mutate(q82 = replace(q82, q82 == 3.0, "2 days")) %>%
  mutate(q82 = replace(q82, q82 == 4.0, "3 days")) %>%
  mutate(q82 = replace(q82, q82 == 5.0, "4 days")) %>%
  mutate(q82 = replace(q82, q82 == 6.0, "5 days")) 
cdc_data2$q82 = as.factor(cdc_data2$q82)
# rename q82
colnames(cdc_data2)[colnames(cdc_data2) == "q82"] = "PE_class"
head(cdc_data2)

# Impute 83
cdc_data2 = cdc_data2 %>%
  mutate(q83 = replace(q83, q83 == 1.0, "0 teams")) %>%
  mutate(q83 = replace(q83, q83 == 2.0, "1 team")) %>%
  mutate(q83 = replace(q83, q83 == 3.0, "2 teams")) %>%
  mutate(q83 = replace(q83, q83 == 4.0, "3 or more teams"))
cdc_data2$q83 = as.factor(cdc_data2$q83)
# rename 83
colnames(cdc_data2)[colnames(cdc_data2) == "q83"] = "sports"
head(cdc_data2)

# Impute 84
cdc_data2 = cdc_data2 %>%
  mutate(q84 = replace(q84, q84 == 1.0, "0 times")) %>%
  mutate(q84 = replace(q84, q84 == 2.0, "1 time")) %>%
  mutate(q84 = replace(q84, q84 == 3.0, "2 times")) %>%
  mutate(q84 = replace(q84, q84 == 4.0, "3 times")) %>%
  mutate(q84 = replace(q84, q84 == 5.0, "4 or more times"))
cdc_data2$q84 = as.factor(cdc_data2$q84)
# rename 84
colnames(cdc_data2)[colnames(cdc_data2) == "q84"] = "concussion_sports"
head(cdc_data2)

# Impute 85
cdc_data2 = cdc_data2 %>% 
  mutate(q85 = replace(q85, q85 == 1.0, "Yes")) %>%
  mutate(q85 = replace(q85, q85 == 2.0, "No")) %>%
  mutate(q85 = replace(q85, q85 == 3.0, "Not sure"))
cdc_data2$q85 = as.factor(cdc_data2$q85)
head(cdc_data2)
# Rename Column - q85
colnames(cdc_data2)[colnames(cdc_data2) == "q85"] = "tested_HIV"
head(cdc_data2)

# Impute 86
cdc_data2 = cdc_data2 %>%
  mutate(q86 = replace(q86, q86 == 1.0, "w/in last 12 mo")) %>%
  mutate(q86 = replace(q86, q86 == 2.0, "between 12 & 24 mo")) %>%
  mutate(q86 = replace(q86, q86 == 3.0, "more than 24 mo")) %>%
  mutate(q86 = replace(q86, q86 == 4.0, "Never")) %>%
  mutate(q86 = replace(q86, q86 == 5.0, "Not sure"))
cdc_data2$q86 = as.factor(cdc_data2$q86)
# rename 86
colnames(cdc_data2)[colnames(cdc_data2) == "q86"] = "dentist"
head(cdc_data2)

# Impute 87
cdc_data2 = cdc_data2 %>% 
  mutate(q87 = replace(q87, q87 == 1.0, "Yes")) %>%
  mutate(q87 = replace(q87, q87 == 2.0, "No")) %>%
  mutate(q87 = replace(q87, q87 == 3.0, "Not sure"))
cdc_data2$q87 = as.factor(cdc_data2$q87)
head(cdc_data2)
# Rename Column - q87
colnames(cdc_data2)[colnames(cdc_data2) == "q87"] = "asthma"
head(cdc_data2)

## Impute q88
cdc_data2 = cdc_data2 %>%
  mutate(q88 = replace(q88, q88 == 1.0, " < 4 hours")) %>%
  mutate(q88 = replace(q88, q88 == 2.0, " 5 hours")) %>%
  mutate(q88 = replace(q88, q88 == 3.0, " 6 hours")) %>%
  mutate(q88 = replace(q88, q88 == 4.0, " 7 hours")) %>%
  mutate(q88 = replace(q88, q88 == 5.0, " 8 hours")) %>%
  mutate(q88 = replace(q88, q88 == 6.0, " 9 hours")) %>%
  mutate(q88 = replace(q88, q88 == 7.0, " > 10 hours")) 
cdc_data2$q88 = as.factor(cdc_data2$q88)
# rename q86
colnames(cdc_data2)[colnames(cdc_data2) == "q88"] = "sleep"
head(cdc_data2)

## Impute q89
cdc_data2 = cdc_data2 %>%
  mutate(q89 = replace(q89, q89 == 1.0, " Mostly As")) %>%
  mutate(q89 = replace(q89, q89 == 2.0, " Mostly Bs")) %>%
  mutate(q89 = replace(q89, q89 == 3.0, " Mostly Cs")) %>%
  mutate(q89 = replace(q89, q89 == 4.0, " Mostly Ds")) %>%
  mutate(q89 = replace(q89, q89 == 5.0, " Mostly Fs")) %>%
  mutate(q89 = replace(q89, q89 == 6.0, " None of these")) %>%
  mutate(q89 = replace(q89, q89 == 7.0, " Not sure")) 
cdc_data2$q89 = as.factor(cdc_data2$q89)
# rename q89
colnames(cdc_data2)[colnames(cdc_data2) == "q89"] = "grade_descript"
head(cdc_data2)

# Impute 90
cdc_data2 = cdc_data2 %>%
  mutate(q90 = replace(q90, q90 == 1.0, "did not drive")) %>%
  mutate(q90 = replace(q90, q90 == 2.0, "0 times")) %>%
  mutate(q90 = replace(q90, q90 == 3.0, "1 time")) %>%
  mutate(q90 = replace(q90, q90 == 4.0, "2 or 3 times")) %>%
  mutate(q90 = replace(q90, q90 == 5.0, "4 or 5 times")) %>%
  mutate(q90 = replace(q90, q90 == 6.0, "> 6 times")) 
cdc_data2$q90 = as.factor(cdc_data2$q90)
# rename 90
colnames(cdc_data2)[colnames(cdc_data2) == "q90"] = "marijuana_drv"
head(cdc_data2)

# Impute Survey answers - q91
cdc_data2 = cdc_data2 %>%
  mutate(q91 = replace(q91, q91 == 1.0, "0 times")) %>%
  mutate(q91 = replace(q91, q91 == 2.0, "1 to 2 times")) %>%
  mutate(q91 = replace(q91, q91 == 3.0, "3 to 9 times")) %>%
  mutate(q91 = replace(q91, q91 == 4.0, "10 to 19 times")) %>%
  mutate(q91 = replace(q91, q91 == 5.0, "20 to 39 times")) %>%
  mutate(q91 = replace(q91, q91 == 6.0, "40 or more times")) 
cdc_data2$q91 = as.factor(cdc_data2$q91)
# Rename Column - q91
colnames(cdc_data2)[colnames(cdc_data2) == "q91"] = "drug_hallucigens"
head(cdc_data2)

## Impute q92
cdc_data2 = cdc_data2 %>%
  mutate(q92 = replace(q92, q92 == 1.0, "did not consume sports drinks")) %>%
  mutate(q92 = replace(q92, q92 == 2.0, "1 - 3 times in last week")) %>%
  mutate(q92 = replace(q92, q92 == 3.0, "4 - 6 times in last week")) %>%
  mutate(q92 = replace(q92, q92 == 4.0, "1 time a day")) %>%
  mutate(q92 = replace(q92, q92 == 5.0, "2 times a day")) %>%
  mutate(q92 = replace(q92, q92 == 6.0, "3 times a day")) %>%
  mutate(q92 = replace(q92, q92 == 7.0, "4 or more times a day")) 
cdc_data2$q92 = as.factor(cdc_data2$q92)
# rename q92
colnames(cdc_data2)[colnames(cdc_data2) == "q92"] = "sportsdrink_consumption"
head(cdc_data2)

## Impute q93
cdc_data2 = cdc_data2 %>%
  mutate(q93 = replace(q93, q93 == 1.0, "did not consume water")) %>%
  mutate(q93 = replace(q93, q93 == 2.0, "1 - 3 times in last week")) %>%
  mutate(q93 = replace(q93, q93 == 3.0, "4 - 6 times in last week")) %>%
  mutate(q93 = replace(q93, q93 == 4.0, "1 time a day")) %>%
  mutate(q93 = replace(q93, q93 == 5.0, "2 times a day")) %>%
  mutate(q93 = replace(q93, q93 == 6.0, "3 times a day")) %>%
  mutate(q93 = replace(q93, q93 == 7.0, "4 or more times a day")) 
cdc_data2$q93 = as.factor(cdc_data2$q93)
# rename q93
colnames(cdc_data2)[colnames(cdc_data2) == "q93"] = "water_consumption"
head(cdc_data2)

# Impute 94
cdc_data2 = cdc_data2 %>% 
  mutate(q94 = replace(q94, q94 == 1.0, "Yes")) %>%
  mutate(q94 = replace(q94, q94 == 2.0, "No")) %>%
  mutate(q94 = replace(q94, q94 == 3.0, "Not sure"))
cdc_data2$q94 = as.factor(cdc_data2$q94)
head(cdc_data2)
# Rename Column - q94
colnames(cdc_data2)[colnames(cdc_data2) == "q94"] = "food_allergies"
head(cdc_data2)

## Impute q95
cdc_data2 = cdc_data2 %>%
  mutate(q95 = replace(q95, q95 == 1.0, "0 days")) %>%
  mutate(q95 = replace(q95, q95 == 2.0, "1 day")) %>%
  mutate(q95 = replace(q95, q95 == 3.0, "2 days")) %>%
  mutate(q95 = replace(q95, q95 == 4.0, "3 days")) %>%
  mutate(q95 = replace(q95, q95 == 5.0, "4 days")) %>%
  mutate(q95 = replace(q95, q95 == 6.0, "5 days")) %>%
  mutate(q95 = replace(q95, q95 == 7.0, "6 days")) %>%
  mutate(q95 = replace(q95, q95 == 8.0, "7 days"))

cdc_data2$q95 = as.factor(cdc_data2$q95)
# rename q95
colnames(cdc_data2)[colnames(cdc_data2) == "q95"] = "tone_muscles"
head(cdc_data2)


## Impute q96
cdc_data2 = cdc_data2 %>%
  mutate(q96 = replace(q96, q96 == 1.0, "did not fake tan")) %>%
  mutate(q96 = replace(q96, q96 == 2.0, "1 - 3 times in last week")) %>%
  mutate(q96 = replace(q96, q96 == 3.0, "4 - 6 times in last week")) %>%
  mutate(q96 = replace(q96, q96 == 4.0, "1 time a day")) %>%
  mutate(q96 = replace(q96, q96 == 5.0, "2 times a day")) %>%
  mutate(q96 = replace(q96, q96 == 6.0, "3 times a day")) %>%
  mutate(q96 = replace(q96, q96 == 7.0, "4 or more times a day")) 
cdc_data2$q96 = as.factor(cdc_data2$q96)
# rename q96
colnames(cdc_data2)[colnames(cdc_data2) == "q96"] = "fake_tan"
head(cdc_data2)

# Impute 97
cdc_data2 = cdc_data2 %>%
  mutate(q97 = replace(q97, q97 == 1.0, "0 times")) %>%
  mutate(q97 = replace(q97, q97 == 2.0, "1 time")) %>%
  mutate(q97 = replace(q97, q97 == 3.0, "2 times")) %>%
  mutate(q97 = replace(q97, q97 == 4.0, "3 times")) %>%
  mutate(q97 = replace(q97, q97 == 5.0, "4 times")) %>%
  mutate(q97 = replace(q97, q97 == 6.0, "> 5 times"))
cdc_data2$q97 = as.factor(cdc_data2$q97)
# rename 97
colnames(cdc_data2)[colnames(cdc_data2) == "q97"] = "sunburn"
head(cdc_data2)

# Impute 98
cdc_data2 = cdc_data2 %>% 
  mutate(q98 = replace(q98, q98 == 1.0, "Yes")) %>%
  mutate(q98 = replace(q98, q98 == 2.0, "No"))

cdc_data2$q98 = as.factor(cdc_data2$q98)
head(cdc_data2)
# Rename Column - q98
colnames(cdc_data2)[colnames(cdc_data2) == "q98"] = "diff_concentration"
head(cdc_data2)

# Impute 99
cdc_data2 = cdc_data2 %>%
  mutate(q99 = replace(q99, q99 == 1.0, "Very well")) %>%
  mutate(q99 = replace(q99, q99 == 2.0, "well")) %>%
  mutate(q99 = replace(q99, q99 == 3.0, "not well")) %>%
  mutate(q99 = replace(q99, q99 == 4.0, "not at all"))
cdc_data2$q99 = as.factor(cdc_data2$q99)
head(cdc_data2)
# Rename Column - q98
colnames(cdc_data2)[colnames(cdc_data2) == "q99"] = "dEnglish"

head(cdc_data2)

##############

write.csv(cdc_data2, "MICE_Impute.csv")
