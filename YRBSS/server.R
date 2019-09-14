library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)


shinyServer(function(input, output){ 
    output$students <- renderValueBox({
         valueBox(
            value = formatC(3243, digits = 1, format = "d"),
            subtitle = "Surveyed Students (Ages 13-18)",
            icon = icon("user-graduate"),
            color = "light-blue"
        )
    })
    output$girls <- renderValueBox({
        valueBox(
            value = formatC(23432, digits = 1, format = "d"),
            subtitle = "Girls in Survey",
            icon = icon("female"),
            color = "maroon"
        )
    })
    output$boys <- renderValueBox({
        valueBox(
            value = formatC(2230, digits = 1, format = "d"),
            subtitle = "Boys in Survey",
            icon = icon("male"),
            color = "blue"
        )
    })
})


