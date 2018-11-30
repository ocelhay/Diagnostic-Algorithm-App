library(shiny)
library(shinyBS)
library(shinyFeedback)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "styles.css",
  
  # Include shinyFeedback
  useShinyFeedback(),
  
  # Application title
  titlePanel("Algorithm for using rapid tests in febrile illness diagnosis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 4,
                 h3("Diseases Inputs"),
                 selectInput("scenario", "Predefined Scenario", width = "60%", selected = NULL,
                             c("South East Asia", "South Central Asia", "East Africa", "none")),
                 
                 conditionalPanel(condition = "input.scenario == 'none'",
                                  p("Add two of five diseases below and, for each disease, provide a name as well as estimates for prevalence, sensitivity and specificity."),
                                  p("Once a minimum of two diseases have been added",  tags$u("and"),  "at least one prevalence is non-null, you can run the simulation.")),
                 
                 conditionalPanel(condition = "input.scenario != 'none'",
                                  p("These are starting point values that can be modified."),
                                  p("We use pooled estimates taken from ", a("Etiology of Severe Febrile Illness in Low- and Middle-Income Countries: A Systematic Review.", href = "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0127962"))
                 ),
                 hr(),
                 textInput("name_disease_1", "Disease #1", width = "50%"),
                 conditionalPanel(condition = "input.name_disease_1 != ''",
                                  fluidRow(
                                    column(width = 4, numericInput("prevalence_dis_1", "Prevalence (%)", value = 3, min = 0, max = 100, width = "90%")),
                                    column(width = 4, sliderInput("sensitivity_dis_1", "Sensitivity (%)", value = 95, min = 0, max = 100)),
                                    column(width = 4, sliderInput("specificity_dis_1", "Specificity (%)", value = 95, min = 0, max = 100))
                                  ),
                                  hr()
                 ),
                 conditionalPanel(condition = "input.name_disease_1 != ''",
                                  textInput("name_disease_2", "Disease #2", width = "50%")
                 ),
                 conditionalPanel(condition = "input.name_disease_2 != ''",
                                  fluidRow(
                                    column(width = 4, numericInput("prevalence_dis_2", "Prevalence (%)", value = 3, min = 0, max = 100)),
                                    column(width = 4, sliderInput("sensitivity_dis_2", "Sensitivity", value = 84, min = 0, max = 100)),
                                    column(width = 4, sliderInput("specificity_dis_2", "Specificity", value = 94, min = 0, max = 100))
                                  ),
                                  hr()
                 ),
                 conditionalPanel(condition = "input.name_disease_2 != ''",
                                  textInput("name_disease_3", "Disease #3 (optional)", width = "50%")
                 ),
                 conditionalPanel(condition = "input.name_disease_3 != ''",
                                  fluidRow(
                                    column(width = 4, numericInput("prevalence_dis_3", "Prevalence (%)", value = 4, min = 0, max = 100)),
                                    column(width = 4, sliderInput("sensitivity_dis_3", "Sensitivity", value = 73, min = 0, max = 100)),
                                    column(width = 4, sliderInput("specificity_dis_3", "Specificity", value = 97, min = 0, max = 100))
                                  ),
                                  hr()
                 ),
                 conditionalPanel(condition = "input.name_disease_3 != ''",
                                  textInput("name_disease_4", "Disease #4 (optional)", width = "50%")
                 ),
                 conditionalPanel(condition = "input.name_disease_4 != ''",
                                  fluidRow(
                                    column(width = 4, numericInput("prevalence_dis_4", "Prevalence (%)", value = 1, min = 0, max = 100)),
                                    column(width = 4, sliderInput("sensitivity_dis_4", "Sensitivity", value = 69, min = 0, max = 100)),
                                    column(width = 4, sliderInput("specificity_dis_4", "Specificity", value = 90, min = 0, max = 100))
                                  ),
                                  hr()
                 ),
                 conditionalPanel(condition = "input.name_disease_4 != ''",
                                  textInput("name_disease_5", "Disease #5 (optional)", width = "50%")
                 ),
                 conditionalPanel(condition = "input.name_disease_5 != ''",
                                  fluidRow(
                                    column(width = 4, numericInput("prevalence_dis_5", "Prevalence (%)", value = 4, min = 0, max = 100)),
                                    column(width = 4, sliderInput("sensitivity_dis_5", "Sensitivity", value = 71, min = 0, max = 100)),
                                    column(width = 4, sliderInput("specificity_dis_5", "Specificity", value = 65, min = 0, max = 100))
                                  ),
                                  hr()
                 ),
                 hr(),
                 strong("Prevalence of other diseases"),
                 p("You can also include a non-null prevalence for other diseases."),
                 fluidRow(column(width = 4, sliderInput("prevalence_dis_other", NULL, value = 0, min = 0, max = 60))),
                 
                 
                 conditionalPanel(condition = "input.name_disease_1 != '' & input.name_disease_2 != '' & (input.prevalence_dis_1 + input.prevalence_dis_2 + input.prevalence_dis_3 + input.prevalence_dis_4 + input.prevalence_dis_5) > 0",
                                  bsButton("run_simul", "Run Simulation", size = "default", style = "success")
                 )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "panels",
                  tabPanel("About", value = "about",
                           br(),
                           h4("TODO: Add FIND logo"),
                           p("This application allows for the prediction of the best diagnostic algorithm based on the correct identification of the diseases."),
                           p("It is especially relevant for the application of rapid diagnostic tests in the diagnosis of Acute Undifferentiated Febrile Illness in the community and primary healthcare facilities of developing countries, where clinical expertise is not available and differentiation of illnesses is dependent on rapid tests’ results. However the algorithm can also be adapted for other conditions which share similar properties in diagnosis."),
                           p(" The application accounts for a maximum of five diseases, takes as inputs the diseases’ prevalence and accuracy of a diagnostic test for each of them. For every algorithm it predicts the correct identification of each illness and positive predictive values. It uses Monte-Carlo simulation approach and runs 10000 simulations for each algorithm."),
                           h4("TODO: Add Credit"),
                           p("Sunil, Sabine, Karell, Olivier, Lisa with emails")
                  ),
                  tabPanel("Output", value = "output",
                           conditionalPanel(condition = "input.run_simul > 0",
                                            h3("Comparisons of Algorithms"),
                                            p("These sequences of algorithms produce the best possible diagnosis: "),
                                            htmlOutput("best_algo"),
                                            br(), br(),
                                            div(class = "center-div",
                                                radioButtons("max_display", label = NULL, choices = c("Show all algos", "Show top 10"), inline = TRUE, width = '400px')
                                            ),
                                            plotOutput("plot_algo", height = "800px"),
                                            hr(),
                                            h3("Details of Best Algorithm"),
                                            htmlOutput("algo_1"),
                                            fluidRow(
                                              column(width = 6,
                                                     h4("Proportion of Correctly Diagnosed"),
                                                     plotOutput("plot_cd")
                                              ),
                                              column(width = 6,
                                                     h4("Positive Predictive Value"),
                                                     plotOutput("plot_ppv")
                                              )
                                            )
                           )
                  )
      )
    )
  )
)
)
