library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title = "YRBSS Data"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Survey Demographics", tabName = "demo", icon = icon("flag")),
            menuItem("Youth Violence", tabName = "vio", icon = icon("balance-scale")),
            menuItem("Youth Drug Use", tabName = "drug", icon = icon("layer-group"))
        )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "demo",
                    fluidRow(
                        valueBoxOutput("students"),
                        valueBoxOutput("girls"),
                        valueBoxOutput("boys")
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

