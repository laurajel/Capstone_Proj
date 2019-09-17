library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title = "YRBSS Data"),
    dashboardSidebar(
        sidebarMenu(
             menuItem("Research Summary", tabName = "rs", icon = icon("book-reader")),
             menuItem("Survey Demographics", tabName = "demo", icon = icon("users")),
             menuItem("Youth Violence", tabName = "vio", icon = icon("ambulance")),
            menuItem("Youth Drug Use", tabName = "drug", icon = icon("pills"))
        )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "rs",
                    fluidRow(
                        box(tags$header("YBRSS Summary"), br(), tags$header("Key Findings"),
                        br(), tags$header("Violence"), br(),tags$header("Drug Use"),br(),
                        tags$header("Suicide Risk"))
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
                        box(
                            width = 12,
                            #plotlyOutput("view_by"),
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
            tabItem(tabName = "drug",
                    fluidRow(
                        valueBoxOutput("heroin", width = 2),
                        valueBoxOutput("Inhalants", width = 2),
                        valueBoxOutput("Methamphetamines", width = 3),
                        valueBoxOutput("Opioids", width = 2),
                        valueBoxOutput("MDMA", width = 3)
                    ),
                    
                    fluidRow(
                        box(
                            plotOutput("lf_use"),
                            width = 12,
                            title = paste("Times Each Drug Used")
                        )),
                    fluidRow(
                        box(
                            plotOutput("gen_drug_use"),
                            width = 12,
                            title = paste("Drug Used by Gender")
                        )),

                    fluidRow(
                        box(
                            plotOutput("drug_gen"),
                            width = 6,
                            title = paste("Use by Ethnicity")
                        ),
                        box(
                            plotOutput("drug_grade"),
                            width = 6,
                            title = paste("Use by Grade ")
                        )),
                    fluidRow(
                        box(
                            plotOutput("drug_age"),
                            width = 6,
                            title = paste("Use by Age")
                        ),
                        box(
                            plotOutput("drug_eth"),
                            width = 6,
                            title = paste("Use by Gender")
                        )),
                    fluidRow(
                        box(
                            plotOutput("drug_wt"),
                            width = 12,
                            title = paste("Use by Weight")
                        )
                        )
                    
                    
                    
                        )
        ))))

