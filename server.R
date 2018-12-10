# load required packages
library(gtools)
library(data.table)
library(ggplot2)
library(plyr)
library(tidyverse) # important to load after plyr
library(shiny)
library(shinyFeedback)

source("./www/algo_run.R")

shinyServer(function(input, output, session) {
  
  # Alert message if the name for diseases 1 or 2 is blank
  observeEvent(input$name_disease_1, {
    feedbackDanger(
      inputId = "name_disease_1",
      condition = input$name_disease_1 == "",
      text = "Required at least two diseases",
      icon = NULL
    )
    })
  
  observeEvent(input$name_disease_2, {
    feedbackDanger(
      inputId = "name_disease_2",
      condition = input$name_disease_2 == "",
      text = "Required",
      icon = NULL
    )
  })
  
  # Warning messages if the prevalence is zero
  observeEvent(input$prevalence_dis_1, {
    feedbackWarning(
      inputId = "prevalence_dis_1",
      condition = input$prevalence_dis_1 == 0,
      text = "Null Prevalence!",
      icon = NULL
    )
  })
  
  observeEvent(input$prevalence_dis_2, {
    feedbackWarning(
      inputId = "prevalence_dis_2",
      condition = input$prevalence_dis_2 == 0,
      text = "Null Prevalence!",
      icon = NULL
    )
  })
  
  observeEvent(input$prevalence_dis_3, {
    feedbackWarning(
      inputId = "prevalence_dis_3",
      condition = input$prevalence_dis_3 == 0,
      text = "Null Prevalence!",
      icon = NULL
    )
  })
  
  observeEvent(input$prevalence_dis_4, {
    feedbackWarning(
      inputId = "prevalence_dis_4",
      condition = input$prevalence_dis_4 == 0,
      text = "Null Prevalence!",
      icon = NULL
    )
  })
  observeEvent(input$prevalence_dis_5, {
    feedbackWarning(
      inputId = "prevalence_dis_5",
      condition = input$prevalence_dis_5 == 0,
      text = "Null Prevalence!",
      icon = NULL
    )
  })
  
  
  algo_output <- reactiveValues(
    a1 = list(NULL)
  )
  
  # Update prevalences based on scenarion values
  observeEvent(input$scenario, {
    
    updateTabsetPanel(session, "panels", selected = "about")
    
    if(input$scenario == 'Custom Scenario'){
      updateTextInput(session, "name_disease_1", value = "")
      updateTextInput(session, "name_disease_2", value = "")
      updateTextInput(session, "name_disease_3", value = "")
      updateTextInput(session, "name_disease_4", value = "")
      updateTextInput(session, "name_disease_5", value = "")
      
      updateNumericInput(session, "prevalence_dis_1", value = 1)
      updateNumericInput(session, "prevalence_dis_2", value = 1)
      updateNumericInput(session, "prevalence_dis_3", value = 1)
      updateNumericInput(session, "prevalence_dis_4", value = 1)
      updateNumericInput(session, "prevalence_dis_5", value = 1)
      
      updateNumericInput(session, "sensitivity_dis_1", value = 100)
      updateNumericInput(session, "sensitivity_dis_2", value = 100)
      updateNumericInput(session, "sensitivity_dis_3", value = 100)
      updateNumericInput(session, "sensitivity_dis_4", value = 100)
      updateNumericInput(session, "sensitivity_dis_5", value = 100)
      
      updateNumericInput(session, "specificity_dis_1", value = 100)
      updateNumericInput(session, "specificity_dis_2", value = 100)
      updateNumericInput(session, "specificity_dis_3", value = 100)
      updateNumericInput(session, "specificity_dis_4", value = 100)
      updateNumericInput(session, "specificity_dis_5", value = 100)
    }
    if(input$scenario == 'South East Asia'){
      updateTextInput(session, "name_disease_1", value = "Malaria")
      updateTextInput(session, "name_disease_2", value = "Dengue")
      updateTextInput(session, "name_disease_3", value = "Scrub")
      updateTextInput(session, "name_disease_4", value = "Typhoid")
      updateTextInput(session, "name_disease_5", value = "Leptospira")
      
      updateNumericInput(session, "sensitivity_dis_1", value = 95)
      updateNumericInput(session, "sensitivity_dis_2", value = 84)
      updateNumericInput(session, "sensitivity_dis_3", value = 73)
      updateNumericInput(session, "sensitivity_dis_4", value = 69)
      updateNumericInput(session, "sensitivity_dis_5", value = 71)
      
      updateNumericInput(session, "specificity_dis_1", value = 95)
      updateNumericInput(session, "specificity_dis_2", value = 94)
      updateNumericInput(session, "specificity_dis_3", value = 97)
      updateNumericInput(session, "specificity_dis_4", value = 90)
      updateNumericInput(session, "specificity_dis_5", value = 65)
      
      updateNumericInput(session, "prevalence_dis_1", value = 2)
      updateNumericInput(session, "prevalence_dis_2", value = 32.1)
      updateNumericInput(session, "prevalence_dis_3", value = 8)
      updateNumericInput(session, "prevalence_dis_4", value = 1.7)
      updateNumericInput(session, "prevalence_dis_5", value = 4.2)
    }
    if(input$scenario == 'South Central Asia'){
      updateTextInput(session, "name_disease_1", value = "Malaria")
      updateTextInput(session, "name_disease_2", value = "Dengue")
      updateTextInput(session, "name_disease_3", value = "Scrub")
      updateTextInput(session, "name_disease_4", value = "Typhoid")
      updateTextInput(session, "name_disease_5", value = "")
      
      updateNumericInput(session, "sensitivity_dis_1", value = 95)
      updateNumericInput(session, "sensitivity_dis_2", value = 84)
      updateNumericInput(session, "sensitivity_dis_3", value = 73)
      updateNumericInput(session, "sensitivity_dis_4", value = 69)
      
      updateNumericInput(session, "specificity_dis_1", value = 95)
      updateNumericInput(session, "specificity_dis_2", value = 94)
      updateNumericInput(session, "specificity_dis_3", value = 97)
      updateNumericInput(session, "specificity_dis_4", value = 90)
      
      updateNumericInput(session, "prevalence_dis_1", value = 5.1)
      updateNumericInput(session, "prevalence_dis_2", value = 36)
      updateNumericInput(session, "prevalence_dis_3", value = 4.9)
      updateNumericInput(session, "prevalence_dis_4", value = 13)
    }
    if(input$scenario == 'East Africa'){
      updateTextInput(session, "name_disease_1", value = "Malaria")
      updateTextInput(session, "name_disease_2", value = "Typhoid")
      updateTextInput(session, "name_disease_3", value = "Leptospira")
      updateTextInput(session, "name_disease_4", value = "")
      updateTextInput(session, "name_disease_5", value = "")
      
      updateNumericInput(session, "sensitivity_dis_1", value = 95)
      updateNumericInput(session, "sensitivity_dis_2", value = 69)
      updateNumericInput(session, "sensitivity_dis_3", value = 71)
      
      updateNumericInput(session, "specificity_dis_1", value = 95)
      updateNumericInput(session, "specificity_dis_2", value = 90)
      updateNumericInput(session, "specificity_dis_3", value = 65)
      
      updateNumericInput(session, "prevalence_dis_1", value = 39.1)
      updateNumericInput(session, "prevalence_dis_2", value = 0.3)
      updateNumericInput(session, "prevalence_dis_3", value = 8.8)
    }
  })
  
  # Call to algo_run function when "Run Simul" is pressed
  observeEvent(input$run_simul, {
    
    updateTabsetPanel(session, "panels", selected = "output")
    
    showNotification(HTML("<h4>Model Running.</h4>"), duration = NULL, type = "warning", id = "model", session = session)
    
    
    algo_output$data <- algo_run(
      name_disease_1 = input$name_disease_1,
      name_disease_2 = input$name_disease_2,
      name_disease_3 = input$name_disease_3,
      name_disease_4 = input$name_disease_4,
      name_disease_5 = input$name_disease_5,
      
      prevalence_dis_1 = input$prevalence_dis_1,
      prevalence_dis_2 = input$prevalence_dis_2,
      prevalence_dis_3 = input$prevalence_dis_3,
      prevalence_dis_4 = input$prevalence_dis_4,
      prevalence_dis_5 = input$prevalence_dis_5,
      
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
      prevalence_dis_other = input$prevalence_dis_other
      )
    
    removeNotification(id = "model", session = session)
    showNotification(HTML("<h4>Model successfully run!</h4>"), duration = 3, type = "message")
  }
  )
  
  
  output$best_algo <- renderText({
    req(input$run_simul)
    
    paste0(icon("thumbs-up"), " ", algo_output$data$algo_1, "<br>", 
           algo_output$data$algo_2, "<br>", 
           algo_output$data$algo_3, "<br>...<br>", 
           icon("thumbs-down"), " ", algo_output$data$algo_n)
  })
  
  output$algo_1 <- renderText({
    req(input$run_simul)
    
    algo_output$data$algo_1
  })
   
  output$plot_cd <- renderPlot({
    req(input$run_simul)
    
    lev <- c(input$name_disease_1, input$name_disease_2, input$name_disease_3, input$name_disease_4, input$name_disease_5)
    lev <- lev[lev != ""]
    
    ggplot(algo_output$data$d1 %>% mutate(names = factor(names, levels = lev)), aes(x = names, y = diagnosis, label = round(diagnosis, 2), fill = names)) +
      geom_bar(stat = "identity") +
      geom_label() + 
      scale_fill_brewer(palette = "Set1") + 
      labs(x = "Disease", y = "Correct Diagnosis", fill = "Disease", subtitle = "Each bar depicts the proportion of each disease \n correctly dignosed by the best algorithm.") +
      theme_bw(base_size = 14) +
      guides(fill = FALSE) +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      coord_cartesian(ylim = c(0, 1))
  })
  
  output$plot_ppv <- renderPlot({
    req(input$run_simul)
    
    lev <- c(input$name_disease_1, input$name_disease_2, input$name_disease_3, input$name_disease_4, input$name_disease_5)
    lev <- lev[lev != ""]
    
    ggplot(algo_output$data$d2 %>% mutate(names = factor(names, levels = lev)), aes(x = names, y = predictive, label = round(predictive, 2), fill = names)) +
      geom_bar(stat = "identity") +
      geom_label() +
      scale_fill_brewer(palette = "Set1") + 
      labs(x = "Disease", y = "Predictive Value", fill = "Disease", subtitle = "Each bar depicts the postive predictive value \n for each disease in the best algorithm.") +
      theme_bw(base_size = 14) +
      guides(fill = FALSE) +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      coord_cartesian(ylim = c(0, 1))
  })
  
  output$plot_algo <- renderPlot({
    req(input$run_simul)
    
    lev <- c(input$name_disease_1, input$name_disease_2, input$name_disease_3, input$name_disease_4, input$name_disease_5)
    lev <- lev[lev != ""]
    
    if(input$max_display == "Show all algos") df <- algo_output$data$d3
    
    nb_disease <- sum(c(input$name_disease_1 != "", input$name_disease_2 != "", input$name_disease_3 != "", 
                        input$name_disease_4 != "", input$name_disease_5 != ""))
    if(input$max_display == "Show top 10") df <- algo_output$data$d3 %>% top_n(n = nb_disease*10, wt = `Correctly Diagnosed`)
    
    print("Selected df:")
    print(df)
      
    ggplot(df %>% mutate(`Test Name` = factor(`Test Name`, levels = lev)), aes(x = reorder(Algorithm, -`Correctly Diagnosed`), y = `Correctly Diagnosed Test`, fill = `Test Name`, group = -Position)) +
      geom_bar(position = "stack", stat = "identity") +
      geom_label(data = df %>% 
                  filter(Position == 1) %>%
                  filter(row_number() %% 4 == 1), 
                aes(y = `Correctly Diagnosed`, label = round(`Correctly Diagnosed`, 2)), fill = 'grey', nudge_y = 0.02) +
      scale_fill_brewer(palette = "Set1") + 
      labs(title = "Algorithms from best to worst", sub = "limited to 30 best algorithms", x = NULL, y = "Proportions of cases correctly diagnosed",
           fill = "Disease", subtitle = "Each columns in figure represents an algorithm. \n The algorithms are arranged from left to right in decreasing order for their correct diagnosis score. \n The stacked bars in each columns represent the tests done in order from bottom to top. \n The length of the each bar represents the contribution of each test to total correct diagnosis.") +
      theme_bw(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position = "left") +
      coord_cartesian(ylim = c(0, 1))
  })
  
})
