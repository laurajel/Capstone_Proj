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
                        box(
                            tags$h3(strong("YBRSS Summary")),
                            tags$h5(strong('The Data'),
                                    p('The Youth Risk Behavior Survey (YRBS) was developed in the early 1990s to monitor health risk behaviors 
                                    that contribute to many issues of major public health concern among youth and adults in the United States. 
                                    These behaviors are often established during childhood and adolescence. 
                                    The YRBS focuses on behaviors that contribute to injury and violence, sexual behaviors, alcohol and drug use, 
                                    tobacco use, unhealthy dietary behaviors, and inadequate physical activity. From 1991 to 2017 the YRBS has collected 
                                    data from more than 3.8 million high school students across the country. The data includes national, state, territorial,
                                    tribal, and local school-based surveys of representative samples of 9th through 12 grade students. '),
                            tags$h5('Sample',
                                    p(' In 2017, all public, Catholic, and other private school students in grades 9 through 12 in the 50 States 
                                    and the District of Columbia were included in the sampling frame. US territories such as Puerto Rico and the Virgin 
                                    Islands were excluded. Schools were selected systematically with probability proportional to enrollment in grades 9 
                                    through 12 using a random start resulting in 192 schools being sampled. The response rates were as follows :'),
                                     p('- 142 of the 192 schools participated = 75% school response rate'),
                                      p('- 14,956 of the 18,324 sampled students submitted questionnaires = 81% student response'),
                                       p('- Overall response = 75% * 80% = 60% overall response'),
                            tags$h5('Sampling Method',
                                    p('The YRBS uses a three-stage cluster sample design stratified by racial/ethnic concentration and Metropolitain statistical area (MSA) status to produce a representative 
                                    sample of 9th through 12 grade students. A weighting factor was applied to each student record to adjust for nonresponse and the oversampling 
                                    of black and Hispanic students in the sample. The final weights were scaled so the weighted count of students was equal 
                                    to the total sample size and weighted proportions of students in each grade matched population projections for each survey year.')),
                            tags$h5(strong('Purpose of This Shiny App'),
                                    p('The purpose of this Shiny app is to offer a snapshot of important features of the YRBS that relate to violence, bullying, drug use, and 
                                      suicide through visualization and also offer insight to the distribution of these important features throughout the U.S. youth population')))),
                             br(), tags$header("Key Findings"), br(), tags$header("Violence"), br()
                            ,tags$header("Drug Use"),br(),tags$header("Suicide Risk"))
                    )),
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
                        box(plotlyOutput("weapons_all"), width = 6, title = HTML("Carried any Weapon in the Past 30 Days")),
                        box(plotlyOutput("weapons_guns"), width = 6, title = HTML("Carried a Gun in the Past 12 Months"))
                        ),
                    fluidRow(
                        box(plotlyOutput("weapon_sch"), width = 6, title = HTML("Carried any Weapon to School in the Past 30 Days")), 
                        box(plotlyOutput("weapons_inj"), width = 6, title = HTML("Injured with any Weapon on School Property"))
                        )),
            
            tabItem(
                tabName = "dv",
                    fluidRow(
                        box(plotlyOutput("dv_rape"), width = 6, title = HTML("Ever Been Forced to have Intercorse")),
                        box(plotlyOutput("dv_other"), width = 6, title = HTML("Forced to do Other Sexual Things by Anyone, Past 12 months"))
                       ),
                    fluidRow(
                        box(plotlyOutput("dv_dating1"), width = 6, title = HTML("Forced to do Other Sexual Things by Someone Dating, Past 12 months")),
                        box(plotlyOutput("dv_dating2"), width = 6, title = HTML("Injured by Someone You were Dating"))
                    )),

            tabItem(
                tabName =  "bully",
                    fluidRow(
                        box(plotlyOutput("bully_elec"), width = 6, title = HTML("Been Electronically Bullied in the Past 12 months")),
                        box(plotlyOutput("bully_sch"), width = 6, title = HTML("Been Bullied on School Property in the Past 12 months"))
                    )),

            tabItem(
                tabName = "drug",
                    fluidRow(
                        valueBoxOutput("heroin", width = 2),
                        valueBoxOutput("Inhalants", width = 2),
                        valueBoxOutput("Methamphetamines", width = 3),
                        valueBoxOutput("Opioids", width = 2),
                        valueBoxOutput("MDMA", width = 3)
                    ),
                    
                    fluidRow(
                        box(plotOutput("lf_use"),width = 12,title = paste("Times Each Drug Used"))
                        ),
                    fluidRow(
                        box(plotOutput("gen_drug_use"),width = 12,title = paste("Drug Used by Gender"))
                        ),
                    fluidRow(
                        box(plotOutput("drug_gen"),width = 6,title = paste("Use by Ethnicity")),
                        box(plotOutput("drug_grade"),width = 6,title = paste("Use by Grade "))
                        ),
                    fluidRow(
                        box(plotOutput("drug_age"),width = 6,title = paste("Use by Age")),
                        box(plotOutput("drug_eth"),width = 6,title = paste("Use by Gender"))
                        ),
                    fluidRow(
                        box(plotOutput("drug_wt"),width = 12,title = paste("Use by Weight")))
                )
        ))))

