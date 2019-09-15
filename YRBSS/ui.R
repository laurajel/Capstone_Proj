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
                        box(
                            tags$header("YBRSS Summary"), br(), tags$header("Key Findings"), br(), tags$header("Violence"), br()
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
                        box(
                            #varSelectInput("variable", "Variable:", police_detail),
                            #selectizeInput("views","Select Details to Explore", views),
                            width = 12,
                            #plotlyOutput("view_by"),
                            title = paste("Title")
                        )))
        ))))

