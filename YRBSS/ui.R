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
                        valueBoxOutput("students"),
                        valueBoxOutput("girls"),
                        valueBoxOutput("boys"),
                        valueBoxOutput("mgrade"),
                        valueBoxOutput("mage")
                        
                    ),
                    fluidRow(
                        box(
                            width = 12,
                            title = HTML("Survey Demographics")
                        ),
                        box(
                            title = "Gender"),
                        box(
                            title = "Age"))),
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

