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
         
         

    output$times_used <- renderPlot({times_used})
    output$lf_use <- renderPlot({lf_use_plot})
    output$drug_wt <- renderPlot({drug_wt})
    output$drug_eth <- renderPlot({drug_eth})
    output$drug_grade <- renderPlot({drug_grade})
    output$drug_age <- renderPlot({drug_age})
    output$drug_gen <- renderPlot({drug_gen})

    output$heroin <- renderValueBox({
        valueBox(value = formatC( hero_risk, format = "d", digits = 5),subtitle = "Students Using Heroin (Lifetime)",icon = icon("syringe"),color = "light-blue")
        })
    output$Inhalants <- renderValueBox({
        valueBox(value = formatC( inha_risk, format = "d", digits = 5),subtitle = "Students Using Inhalants (Lifetime)",icon = icon("spray-can"),color = "maroon")
         })
    output$MDMA <- renderValueBox({
        valueBox(value = formatC( mdma_risk, format = "d", digits = 5),subtitle = "Students Using MDMA (Lifetime)",icon = icon("tablets"),color = "blue")
        })
    output$Methamphetamines <- renderValueBox({
        valueBox(value = formatC( meth_risk, format = "d", digits = 5),subtitle = "Students Using Methamphetamines (Lifetime)",icon = icon("bong"),color = "olive")
        })
    output$Opioids<- renderValueBox({
        valueBox(value = formatC( opi_risk, format = "d", digits = 5),subtitle = "Students Using Opioids (Illicit)",icon = icon("pillsf"),color = "orange")
        })
    output$gen_drug_use <- renderPlot({gen_drug_use})

})

