library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)


shinyServer(function(input, output){ 
    #output$map <- renderGvis({
    #    gvisGeoChart(state_race_input <- state_race_input[ state_race_input$Victim_race == input$selected,], locationvar='State.x',
    #                 colorvar= "Relative-Percent",
    #                 hovervar = "State_name",
    #                 options=list(region="US", 
    #                              displayMode="regions", 
    #                              resolution="provinces",
    #                              color = 'red',
    #                              colorAxis = "{minValue: -100,maxValue: 100,  colors: ['#00FF00','#FF0000']}"))
    ##})
    
})


