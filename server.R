# load required packages
library(gtools)
library(data.table)
library(ggplot2)
library(plyr)
library(tidyverse) # important to load after plyr
library(shiny)

source("./www/algo_run.R")


shinyServer(function(input, output, session) {
  
  algo_output <- reactiveValues(
    a1 = list(NULL)
  )
  
  # Call to algo_run function when "Run Simul" is pressed
  observeEvent(input$run_simul, {
    
    showNotification(HTML("<h2>Model Running...</h2>"), duration = NULL, type = "warning", id = "model", session = session)
    
    algo_output$data <- algo_run(
      name_disease_1 = input$name_disease_1,
      name_disease_2 = input$name_disease_2,
      name_disease_3 = input$name_disease_3,
      name_disease_4 = input$name_disease_4,
      name_disease_5 = input$name_disease_5,
      
      incidence_dis_1 = input$incidence_dis_1,
      incidence_dis_2 = input$incidence_dis_2,
      incidence_dis_3 = input$incidence_dis_3,
      incidence_dis_4 = input$incidence_dis_4,
      incidence_dis_5 = input$incidence_dis_5,
      
      sensitivity_dis_1 = input$sensitivity_dis_1,
      sensitivity_dis_2 = input$sensitivity_dis_2,
      sensitivity_dis_3 = input$sensitivity_dis_3,
      sensitivity_dis_4 = input$sensitivity_dis_4,
      sensitivity_dis_5 = input$sensitivity_dis_5,
      
      specificity_dis_1 = input$specificity_dis_1,
      specificity_dis_2 = input$specificity_dis_2,
      specificity_dis_3 = input$specificity_dis_3,
      specificity_dis_4 = input$specificity_dis_4,
      specificity_dis_5 = input$specificity_dis_5,
      incidence_dis_other = input$incidence_dis_other
      )
    
    removeNotification(id = "model", session = session)
    showNotification(HTML("<h2>Model successfully run!</h2>"), duration = 3, type = "message")
  }
  )
  
  
  output$best_algo <- renderText({
    req(input$run_simul)
    
    paste0(algo_output$data$algo_1, "<br>", algo_output$data$algo_2, "<br>", algo_output$data$algo_3)
  })
   
  output$plot_cd <- renderPlot({
    req(input$run_simul)
    
    ggplot(algo_output$data$d1, aes(x = names, y = diagnosis, label = round(diagnosis, 2))) +
      geom_bar(stat = "identity") +
      geom_label() + 
      labs(x = "Disease", y = "Correct Diagnosis", fill = "Disease") +
      theme_bw(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      coord_cartesian(ylim = c(0, 1))
  })
  
  output$plot_ppv <- renderPlot({
    req(input$run_simul)
    
    ggplot(algo_output$data$d2, aes(x = names, y = predictive, label = round(predictive, 2))) +
      geom_bar(stat = "identity") +
      geom_label() +
      labs(x = "Disease", y = "Predictive Value", fill = "Disease") +
      theme_bw(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      coord_cartesian(ylim = c(0, 1))
  })
  
  output$plot_algo <- renderPlot({
    req(input$run_simul)
    
    
    ggplot(algo_output$data$d3, aes(x = reorder(Algorithm, -`Correctly Diagnosed`), y = `Correctly Diagnosed Test`, fill = `Test Name`, group = -Position)) +
      geom_bar(position = "stack", stat = "identity") +
      scale_fill_brewer(palette = "Set1") + 
      labs(title = "Algorithms from best to worst", sub = "limited to 30 best algorithms", x = "Algorithm", y = "Proportions of cases correctly diagnosed",
           fill = "Disease") +
      theme_bw(base_size = 13) +
      theme(axis.text.x = element_text(angle = 70, hjust=1), legend.position = "left") +
      coord_cartesian(ylim = c(0, 1))
  })
  
})
