library(shiny)
library(shinyBS)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Diagnostic Algorithm"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("Diseases"),
      p("Add two of five diseases below and, for each disease, provide a name as well as estimates for incidence, sensitivity and specificity. 
        You can also include a non-null incidence for other diseases. Once a minimum of two diseases have been added, you can run the simulation."),
      textInput("name_disease_1", p("Disease #1", em(" —required")), value = "Malaria"),
      conditionalPanel(condition = "input.name_disease_1 != ''",
                       fluidRow(
                         column(width = 4, sliderInput("incidence_dis_1", "Incidence", value = 3, min = 0, max = 60)),
                         column(width = 4, sliderInput("sensitivity_dis_1", "Sensitivity", value = 95, min = 0, max = 100)),
                         column(width = 4, sliderInput("specificty_dis_1", "Specificity", value = 95, min = 0, max = 100))
                         ),
                       hr()
      ),
      conditionalPanel(condition = "input.name_disease_1 != ''",
      textInput("name_disease_2", p("Disease #2", em(" —required")), value = "Dengue")
      ),
      conditionalPanel(condition = "input.name_disease_2 != ''",
                       fluidRow(
                         column(width = 4, sliderInput("incidence_dis_2", "Incidence", value = 7, min = 0, max = 60)),
                         column(width = 4, sliderInput("sensitivity_dis_2", "Sensitivity", value = 84, min = 0, max = 100)),
                         column(width = 4, sliderInput("specificty_dis_2", "Specificity", value = 94, min = 0, max = 100))
                       ),
                       hr()
      ),
      conditionalPanel(condition = "input.name_disease_2 != ''",
                       textInput("name_disease_3", p("Disease #3", em(" —optional, leave blank to ignore")), value = "Scrub")
      ),
      conditionalPanel(condition = "input.name_disease_3 != ''",
                       fluidRow(
                         column(width = 4, sliderInput("incidence_dis_3", "Incidence", value = 4, min = 0, max = 60)),
                         column(width = 4, sliderInput("sensitivity_dis_3", "Sensitivity", value = 73, min = 0, max = 100)),
                         column(width = 4, sliderInput("specificty_dis_3", "Specificity", value = 97, min = 0, max = 100))
                       ),
                       hr()
      ),
      conditionalPanel(condition = "input.name_disease_3 != ''",
                       textInput("name_disease_4", p("Disease #4", em(" —optional, leave blank to ignore")), value = "Typhoid")
      ),
      conditionalPanel(condition = "input.name_disease_4 != ''",
                       fluidRow(
                         column(width = 4, sliderInput("incidence_dis_4", "Incidence", value = 1, min = 0, max = 60)),
                         column(width = 4, sliderInput("sensitivity_dis_4", "Sensitivity", value = 69, min = 0, max = 100)),
                         column(width = 4, sliderInput("specificty_dis_4", "Specificity", value = 90, min = 0, max = 100))
                       ),
                       hr()
      ),
      conditionalPanel(condition = "input.name_disease_4 != ''",
                       textInput("name_disease_5", p("Disease #5", em(" —optional, leave blank to ignore")), value = "Leptospira")
      ),
      conditionalPanel(condition = "input.name_disease_5 != ''",
                       fluidRow(
                         column(width = 4, sliderInput("incidence_dis_5", "Incidence", value = 4, min = 0, max = 60)),
                         column(width = 4, sliderInput("sensitivity_dis_5", "Sensitivity", value = 71, min = 0, max = 100)),
                         column(width = 4, sliderInput("specificty_dis_5", "Specificity", value = 65, min = 0, max = 100))
                       ),
                       hr()
      ),
    
      h3("Other diseases"),
      fluidRow(column(width = 4, sliderInput("incidence_dis_other", "Incidence of other diseases", value = 0, min = 0, max = 60))),
      
      conditionalPanel(condition = "input.name_disease_1 != ''",
                       bsButton("run_simul", "Run Simulation", size = "default", style = "success")
    )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(condition = "input.run_simul > 0",
      h2("Algorithm for Best Possible Diagnosis"),
      p("In order to produce the best possible diagnosis, it is recommended to follow this sequence: "),
      textOutput("best_algo"),
      hr(),
      h2("Proportion of Correctly Diagnosed"),
       plotOutput("plot_cd"),
       p("Each bar depicts the proportion of each disease correctly dignosed by the best algorithm. Each number number represnets for a disease. *(We need to explore a way to link this number to the disease we enter as the first input)"),
      hr(),
      h2("Positive Predictive Value"),
       plotOutput("plot_ppv"),
       p("Each bar depicts the postive predictive value for each disease in the best algorithm. The number denotes for a disease. *(We need to explore a way to link this number to the disease we enter as the first input)"),
      hr(),
      h2("Algorithm"),
       plotOutput("plot_algo", height = "800px"),
       p("Each columns in figure represents an algorithm. The algorithms are arranged from left to right in decreasing order for their correct diagnosis score. The stacked bars in each columns represent 5 tests done in order from bottom to top. The length of the each bar represents the contribution of each test to total correct diagnosis.")
      )
    )
  )
))
