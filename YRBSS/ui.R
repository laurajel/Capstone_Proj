library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title = "YRBSS Data"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Research Summary", tabName = "rs", icon = icon("book-reader")),
            menuItem("Survey Demographics", tabName = "demo", icon = icon("users")),
            menuItem("Youth Violence", tabName = "vio", icon = icon("ambulance")),
            menuItem("Youth Bullying", tabName = "bully" , icon = icon("sad-tear")),
            menuItem("Youth Drug Use", tabName = "drug", icon = icon("pills"))
        )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "rs",
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
                                      suicide through visualization and also offer insight to the distribution of these important features throughout the U.S. youth population'))
)),
                             br(), tags$header("Key Findings"), br(), tags$header("Violence"), br()
                            ,tags$header("Drug Use"),br(),tags$header("Suicide Risk"))
                    )),
            tabItem(tabName = "demo",
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
                        
            tabItem(tabName = "vio",
                    
                    fluidRow(
                        #box(plotlyOutput("weapons_carry"), width = 6, title = HTML("Weapon Carry")),
                        #box(plotlyOutput("gun_carry"), width = 6, title = HTML("Gun Carry"))
                        ),
                    fluidRow(
                        #box(plotlyOutput("dating_viol"), width = 6, title = HTML 
                        box(
                            title = "Title"
                        )),
                    fluidRow(
                        box(
                            title = "Title"
                        ),
                        box(
                            title = "Title"
                        )),
                    fluidRow(
                        box(
                            width = 12,
                            title = "Title"
                        ))
            ),

            tabItem(tabName =  "bully",
                    fluidRow(
                        box(),
                        box()
                    ))),

            
            tabItem(tabName = "drug",
                    fluidRow(
                        box(
                            #varSelectInput("variable", "Variable:", police_detail),
                            #selectizeInput("views","Select Details to Explore", views),
                            width = 12,
                            #plotlyOutput("view_by"),
                            title = paste("Title")
                        )))
        )))

