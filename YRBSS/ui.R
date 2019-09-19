library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title = "YRBSS Data"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Research Summary", tabName = "rs", icon = icon("book-reader")),
            menuItem("Survey Demographics", tabName = "demo", icon = icon("users")),
            menuItem("Youth Violence", tabName = "vio", icon = icon("ambulance")),
            menuItem("Youth Dating Violence", tabName = "dv", icon= icon("heart")),
            menuItem("Youth Bullying", tabName = "bully" , icon = icon("sad-tear")),
            menuItem("Youth Drug Use", tabName = "drug", icon = icon("pills"))
        )),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "rs",
                fluidRow(
                    valueBoxOutput("PerSchool", width = 4),
                    valueBoxOutput("PerSubmit",width = 4),
                    valueBoxOutput("OverallPer",width = 4)
                ),
                    fluidRow(
                        box(width = 12,
                            tags$h1(strong("Youth Risk Behavior Survey")),
                            tags$h2(strong('The Data'),
                            tags$h4(
                            p('The Youth Risk Behavior Survey (YRBS) was developed in the early 1990s to monitor health-risk behaviors that contribute to many issues of 
                                    major public health concern among youth and adults in the United States. These behaviors are often established during childhood and adolescence. 
                                    The YRBS focuses on behaviors that contribute to injury and violence, sexual behaviors, alcohol and drug use, tobacco use, unhealthy dietary behaviors, 
                                    and inadequate physical activity. From 1991 to 2017 the YRBS has collected data from more than 3.8 million high school students across the country. 
                                     The data includes national, state, territorial, tribal, and local school-based surveys of representative samples of 9th through 12th grade students.'))),
                            tags$h2(strong('Sample'),
                            tags$h4(
                                    p('In 2017, all public, Catholic, and other private school students in grades 9 through 12 in the 50 States and the District of Columbia were included 
                                      in the sampling frame. US territories such as Puerto Rico and the Virgin Islands were excluded. Schools were selected systematically with probability 
                                      proportional to enrollment in grades 9 through 12 using a random start resulting in 192 schools being sampled. The response rates were as follows: '),
                                    p('- 142 of the 192 schools participated = 75% school response rate'),
                                    p('- 14,956 of the 18,324 sampled students submitted questionnaires = 81% student response'),
                                    p('- Overall response = 75% * 80% = 60% overall response'))),
                            tags$h2(strong('Sampling Method'),
                            tags$h4(
                                    p('The YRBS uses a three-stage cluster sample design stratified by racial/ethnic concentration and Metropolitain statistical area (MSA) status 
                                    to produce a representative sample of 9th through 12 grade students. A weighting factor was applied to each student record to adjust for 
                                    nonresponse and the oversampling of black and Hispanic students in the sample. The final weights were scaled so the weighted count of students 
                                    was equal to the total sample size and weighted proportions of students in each grade matched population projections for each survey year.'))),
                            tags$h2(strong('Purpose of This Shiny App'),
                            tags$h4(
                                    p('The purpose of this Shiny app is to offer a snapshot of important features of the YRBS that relate to violence, bullying, drug use, and 
                                      suicide through visualization and also offer insight to the distribution of these important features throughout the U.S. youth population'))),
                            tags$h2(strong('Drug Use'),
                            tags$h4(
                                    p("For all students surveyed only 1.79% of students reported one or more uses of illicit drug use in their lifetimes. While Marijuana was 
                                      a part of the survey, the use of marijuana was excluded from this analysis due to the current consideration of marijuana in larger society."),
                                    p("Gender is a significant variable in predicting use of each drug. For girls MDMA, methamphetamines, and heroin are used significantly 
                                      more than boys. Boys used inhalants more than girls."),
                                    p("For those who reported using drugs most did not self report gender or grade. They did however report age, with 12 and 13 year olds 
                                      reporting the highest percentage of use of over 50% each within their populations. High reporting could be due to many factors. There 
                                      is a high likelihood that many students experiment with drugs for the first time at a young age and therefore do not report first time 
                                      usage at older ages. They also might be too young to realize the significance of admitting to drug use."),
                                    p("Students also chose not to self-report race when reporting drug use. However amongst those students who did self-report a higher 
                                      proportion of Native Americans reported using vs never using. Asians have the lowest proportion of usage."))),
                            tags$h3(strong('Drug Use Summary'),
                            tags$h4(
                                    p("Overall student drug use follows the larger patterns of adult drug use. The opioid epidemic is clearly shown amongst students 
                                      as it is in adults. Heroin addiction is also evident amongst students. Finally weight is a small factor in the use of drugs as 
                                      higher weight categories (overweight and obese) report higher usage.")))))),
        tabItem(
            tabName = "demo",
            fluidRow(
                valueBoxOutput("students", width = 3),
                valueBoxOutput("girls",width = 2),
                valueBoxOutput("boys",width = 2),
                valueBoxOutput("mgrade",width = 3),
                valueBoxOutput("mage",width = 2)
            ),
                    fluidRow(
                        box(plotlyOutput("gen_race"),width = 6, title = HTML("Survey Race")),
                        box(plotlyOutput("gen_age"),width = 6, title = HTML("Survey Age")),
                        box(plotlyOutput("kde_h"), width = 6,title = HTML("Survey Height")),
                        box(plotlyOutput("kde_w"),width = 6,title = HTML("Survey Weight"))
                        )),
            tabItem(
                tabName = "vio",
                    fluidRow(
                        box(plotlyOutput("weapons_all"), width = 6, title = HTML("Carried any Weapon in the Past 30 Days"),
                            selectInput(inputId = "featureInput2",label = "Select a Feature:",
                                         choices = featureList, selected = "raceeth")),
                        
                        box(plotlyOutput("weapons_guns"), width = 6, title = HTML("Carried a Gun in the Past 12 Months"),
                            selectInput(inputId = "featureinput2",label = "Select a Feature:",
                                        choices = featureList, selected = "raceeth"))),
                        
                    fluidRow(
                        box(plotlyOutput("weapon_sch"), width = 6, title = HTML("Carried any Weapon to School in the Past 30 Days"),
                            selectInput(inputId = "featureIput2",label = "Select a Feature:",
                                        choices = featureList, selected = "raceeth")), 
                        
                        box(plotlyOutput("weapons_inj"), width = 6, title = HTML("Injured with any Weapon on School Property"),
                            selectInput(inputId = "featureInput2",label = "Select a Feature:",
                                        choices = featureList, selected = "raceeth")))
                        ),
            
            tabItem(
                tabName = "dv",
                    fluidRow(    
                        box(plotlyOutput("dv_rape"), width = 9, title = HTML("Ever Been Forced to have Intercorse")),
                        box(selectizeInput("var_select","Select Variable", fill_options,
                        ), width = 3)),
                
                    fluidRow(
                        box(plotlyOutput("dv_other"), width = 9, title = HTML("Forced to do Other Sexual Things by Anyone, Past 12 months")),
                        box(selectizeInput("var_select2","Select Variable", fill_options,
                        ), width = 3)),
                     
                    fluidRow(
                        box(plotlyOutput("dv_dating1"), width = 9, title = HTML("Forced to do Other Sexual Things by Someone Dating, Past 12 months")),
                        box(selectizeInput("var_select3","Select Variable", fill_options,
                        ), width = 3)),
                    fluidRow(
                        box(plotlyOutput("dv_dating2"), width = 9, title = HTML("Injured by Someone You were Dating")),
                        box(selectizeInput("var_select4","Select Variable", fill_options,
                        ), width = 3))),

            tabItem(
                tabName =  "bully",
                    fluidRow(
                             valueBoxOutput("School", width = 2),
                             valueBoxOutput("School_resp", width = 2),
                             valueBoxOutput("Electronic", width = 2),
                             valueBoxOutput("Elec_resp", width = 2)),
                
                    fluidRow(

                        box(plotlyOutput("bully_elec"), width = 6, title = HTML("Been Electronically Bullied in the Past 12 months"),
                            selectInput(inputId = "featureInput2",label = "Select a Feature:",
                                        choices = featureList, selected = "raceeth")),
                        box(plotlyOutput("bully_sch"), width = 6, title = HTML("Been Bullied on School Property in the Past 12 months"),
                            selectInput(inputId = "featureInput2",label = "Select a Feature:",
                                        choices = featureList, selected = "raceeth")))),
                    


            tabItem(
                tabName = "drug",
                    fluidRow(
                        valueBoxOutput("heroin", width = 2),
                        valueBoxOutput("Inhalants", width = 2),
                        valueBoxOutput("Methamphetamines", width = 3),
                        valueBoxOutput("Opioids", width = 2),
                        valueBoxOutput("MDMA", width = 3)),
                    
                    fluidRow(
                        box(plotlyOutput("gen_drug_use"),width = 12,title = paste("Drug Used by Gender"))
                        ),
                    fluidRow(
                        box(plotOutput("lf_use"),width = 12,title = paste("Times Each Drug Used"))
                        ),
                    fluidRow(
                        box(plotOutput("drug_gen"),width = 6,title = paste("Overall use by Gender")),
                        box(plotOutput("drug_grade"),width = 6,title = paste("Overall use by Grade "))
                        ),
                    fluidRow(
                        box(plotOutput("drug_age"),width = 6,title = paste("Overall Use by Age")),
                        box(plotOutput("drug_eth"),width = 6,title = paste("Overall use by Ethnicity"))
                        ),
                    fluidRow(
                        box(plotOutput("drug_wt"),width = 12,title = paste("Overall use by Weight")))
                )
        ))))

