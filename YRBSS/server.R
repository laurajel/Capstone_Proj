library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)


shinyServer(function(input, output){ 
    output$students <- renderValueBox({
         valueBox(value = total,subtitle = "Surveyed Students (Ages 12-18)",icon = icon("user-graduate"),color = "light-blue")
    })
    output$girls <- renderValueBox({
        valueBox(value = girls,subtitle = "Girls in Survey",icon = icon("female"),color = "maroon")
    })
    output$boys <- renderValueBox({
        valueBox(value = boys,subtitle = "Boys in Survey",icon = icon("male"),color = "blue")
    })
    output$mgrade <- renderValueBox({
        valueBox(value = paste0(median_grade,"th grade"),subtitle = "Meidan Grade of Students",icon = icon("school"),color = "olive")
    })
    output$mage <- renderValueBox({
        valueBox(value = median_age,subtitle = "Median Age of Students",icon = icon("birthday-cake"),color = "orange")
    })
    output$gen_race <- renderPlotly({gender_race_plot})
    output$gen_age <- renderPlotly({age_gender_plot})
    output$kde_h <- renderPlotly({kde_height})
    output$kde_w <- renderPlotly({kde_weight})
    
    
    output$weapons_all <- renderPlotly({wep_all})
    output$weapons_guns <- renderPlotly({wep_guns})
    output$weapon_sch <- renderPlotly({wep_sch})
    output$weapons_inj <- renderPlotly({wep_inj})
    
    output$dv_rape <- renderPlotly({dv_1})
    output$dv_other <- renderPlotly({dv_2})
    output$dv_dating1 <- renderPlotly({dv_3})
    output$dv_dating2 <- renderPlotly({dv_4})
    
    output$bully_elec <- renderPlotly({bully_elec})
    output$bully_sch <- renderPlotly({bully_sch})
})

