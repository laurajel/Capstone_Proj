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
    
    
    observe(input$featureInput2, {
        plot.df <- data.frame(cdc_data2[,input$featureInput2],
                              Feature1 = cdc_data2$weapons_all,
                              Feature2 = cdc_data2$weapons_gun,
                              Feature3 = cdc_data2$weapons_toschool,
                              Feature4 = cdc_data2$inj_weapon,
                              Feature5 = cdc_data2$viol_dating1,
                              Feature6 = cdc_data2$viol_dating2,
                              Feature7 = cdc_data2$viol_dating3,
                              Feature8 = cdc_data2$viol_dating4,
                              Feature9 = cdc_data2$bullied_elec,
                              Feature10 = cdc_data2$bullied_sch,
                              weight = cdc_data2$weight)
        
         colnames(plot.df) <- c("y", "Weapons All", "Weapons Gun", "Weapons To School", "Weapons Gun", "Injured with Weapon", "Forced Intercorse",
                                "Forced Sexual", "Forced Dating", "Dating Injured", "Bullied Electronically", "Bullied at School", "Weight")
    
    
         output$weapons_all <- renderPlotly({ 
             
           #  plot.df %>%
            #     group_by(Feature1, y) %>%
             #    summarize(Count = n()) %>%
              #   filter(Class == "malignant") %>%
               #  plot_ly(x = ~x, y = ~y, z = ~Count, type = "contour") %>%
                # layout(title = "Contour map of number of malignant cases",
                 #       xaxis = list(title = input$featureInput1),
                  #      yaxis = list(title = input$featureInput2))
            plot.df%>%
            ggplot(aes(x = Feature1, weight = weight)) +
            geom_bar(aes(fill = y), position = "fill") +
            coord_flip() +
            labs(y="Proportion", x = "Number of Days Carried Weapon")})
         
    })
    
         output$weapons_guns <- renderPlotly({cdc_data2 %>%
            ggplot(aes(x = weapons_gun, weight = weight)) +
            geom_bar(aes(fill = raceeth), position = "fill") +
            coord_flip() +
            labs(y="Proportion", x = "Number of Days Carried Gun")})
    
        output$weapon_sch <- renderPlotly({cdc_data2 %>%
            ggplot(aes(x = weapons_toschool, weight = weight)) +
            geom_bar(aes(fill = raceeth), position = "fill") +
            coord_flip() +
            labs(y="Proportion", x = "Number of Days Carried Weapon to School")})
    
        output$weapons_inj <- renderPlotly({cdc_data2 %>%
            ggplot(aes(x = inj_weapon, weight = weight)) +
            geom_bar(aes(fill = raceeth), position = "fill") +
            coord_flip() +
            labs(y="Proportion", x = "Number of Days Injured by Weapon at School")})
    
    
 ### dating viol   
        output$dv_rape <- renderPlotly({cdc_data2 %>%
            ggplot(aes(x = viol_dating1, weight = weight)) +
            geom_bar(aes(fill = raceeth), position = "fill") +
            labs(y="Proportion", x = "Forced to have Intercorse")})
    
        output$dv_other <- renderPlotly({cdc_data2 %>%
            ggplot(aes(x = viol_dating2, weight = weight)) +
            geom_bar(aes(fill = raceeth), position = "fill") +
            coord_flip() +
            labs(y="Proportion", x = "Forced to do Other Sexual Things")})
    
        output$dv_dating1 <- renderPlotly({cdc_data2 %>%
            ggplot(aes(x = viol_dating3, weight = weight)) +
            geom_bar(aes(fill = raceeth), position = "fill") +
            coord_flip() +
            labs(y="Proportion", x = "Forced to do Other Sexual Things")})
    
    
         output$dv_dating2 <- renderPlotly({cdc_data2 %>%
            ggplot(aes(x = viol_dating4, weight = weight)) +
            geom_bar(aes(fill = raceeth), position = "fill") +
            coord_flip() +
            labs(y="Proportion", x = "Forced to do Other Sexual Things")
    })
 ### bully   
         output$bully_elec <- renderPlotly( {cdc_data2 %>%
            ggplot(aes(x = bullied_elec, weight = weight)) +
            geom_bar(aes(fill = raceeth), position = "fill") +
            labs(y="Proportion", x = " Bullied ")})
    
         output$bully_sch <- renderPlotly({cdc_data2 %>%
            ggplot(aes(x = bullied_sch, weight = weight)) +
            geom_bar(aes(fill = raceeth), position = "fill") +
            labs(y="Proportion", x = " Bullied")})
   
    
})

