# load required packages
library(gtools)
library(data.table)
library(ggplot2)
library(plyr)
library(tidyverse) # important to load after plyr
library(shiny)

source("./www/algo_run.R")
x <- 10000  # number of simulations



shinyServer(function(input, output, session) {
  
  algo_output <- reactiveValues(
    data = data.frame(NULL)
  )
  
  # Number of diseases
  # name_diseases <- reactiveVal(c(input$name_disease_1, input$name_disease_2, input$name_disease_3, input$name_disease_4, input$name_disease_5))
  # r <- reactiveVal(sum(name_diseases() != ""))
  # n <- reactiveVal(r())
  # 
  # inc <- reactiveVal(c(input$incidence_dis_1, input$incidence_dis_2, input$incidence_dis_3, input$incidence_dis_4, input$incidence_dis_5))
  # sn <- reactiveVal(c(input$sensitivity_dis_1, input$sensitivity_dis_2, input$sensitivity_dis_3, input$sensitivity_dis_4, input$sensitivity_dis_5))
  # sp <- reactiveVal(c(input$specificity_dis_1, input$specificity_dis_2, input$specificity_dis_3, input$specificity_dis_4, input$specificity_dis_5))
  # 
  # perm <- reactiveVal(NULL)
  # perm_name <- reactiveVal(NULL)
  # OUTpropCD <- reactiveVal(NULL)
  # OUTsumCD <- reactiveVal(NULL)
  # OUTPPV <- reactiveVal(NULL)
  # OUTNPV <- reactiveVal(NULL)
  # OUTCCD <- reactiveVal(NULL)
  # OUTCD <- reactiveVal(NULL)
  
  
  # Action when "Run Simul" is pressed
  observeEvent(input$run_simul, {
    
    algo_output$data <- algo_run(incidence_dis_1 = input$incidence_dis_1,
                            incidence_dis_2 = input$incidence_dis_2)
    
    # perm(permutations(n(), r(), v = 1:n())) # possible combinations
    # 
    # #data frame of required data
    # perm_name <- perm()
    # perm_name[perm() == 1] <- name_diseases()[1]
    # perm_name[perm() == 2] <- name_diseases()[2]
    # perm_name[perm() == 3] <- name_diseases()[3]
    # perm_name[perm() == 4] <- name_diseases()[4]
    # perm_name[perm() == 5] <- name_diseases()[5]
    # 
    # lp <- nrow(perm())  #length of permutation vector
    
    # OUTpropCD() <- matrix(NA, lp, 5) # proportion of correct diagnosis for each diagnosis
    # OUTsumCD() <- c()                # total correct diagnosis
    # OUTPPV() <- matrix(NA, lp, 5)    # positive predictive value for each disease diagnosis
    # OUTNPV() <- matrix(NA, lp, 5)    # negative predictive value for each disease diagnosis
    # OUTCCD() <- matrix(NA, lp, 5)    # cumulative correct diagnosis after every test for each algorithm 
    # OUTCD() <- matrix(NA, lp, 5)     # correct diagnosis for each test
    
  }
  )
  
   
  output$plot_cd <- renderPlot({
    req(algo_output)
    plot(1, main = paste("There are ", algo_output$data, " permutations"))
    
    
  })
  
  output$plot_ppv <- renderPlot({
    
    plot(1)
    
  })
  
  output$plot_algo <- renderPlot({
    
    plot(1)
    
  })
  
})
