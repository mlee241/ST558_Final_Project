source("shared.R")

# An About page
# * Describe the purpose of the app
# * Briefly discuss the data and its source- providing a link to more information about the data
# * Tell the user the purpose of each tab(page) of the app
# * Include a picture related to the data (for instance, if the data was about the world wildlife fund, you might include a picture of their logo)

shinyUI(dashboardPage(
  # Insurance title
  dashboardHeader(title = "Insurance Premium Project"),
  
  # sidebar menu items/pages
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Data", tabName = "data"),
      menuItem("Data Exploration", tabName = "dataExploration"),
      menuItem("Modeling", tabName = "modeling")
    )
  ), # End of dashboardSidebar
  
  # Dashboard Body 
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              h2("About Page"),
              fluidRow(
                box(
                  h3("The purpose of this application"),
                  ".....The purpose of this application is to run a couple of regression models on insurance premium..... and to make predictions for future medical expenses of individuals that would help medical insurance to make business decisions on charging a premium."
                ),
                box(
                  h3("The data and its source"),
                  "The insurance dataset contains 1338 observations and 7 features.",
                  br(),
                  br(),
                  "More information can be found below:",
                  br(),
                  a("Go to Kaggle for more info about the data",href="https://www.kaggle.com/noordeen/insurance-premium-prediction")
                )
              ),
              fluidRow(
                tabBox(
                  tabPanel("About",
                           "So the purpose of the About page is to:",
                           tags$ul(
                             tags$li("Describe the purpose of the app"),
                             tags$li("Briefly discuss the data and its source"),
                             tags$li("Tell the user the purpose of each page of the app"),
                             tags$li("Include a picture related to the data")
                           )
                  ),
                  tabPanel("Data",
                           "The purpose of the Data page is to:",
                           tags$ul(
                             tags$li("Scroll through the insurance data set"),
                             tags$li("Subset the insurance dataset"),
                             tags$li("Save the possibly subsetted data as a file")
                           )
                  ),
                  tabPanel("Data Exploration",
                           "The purpose of the Data Exploration page is to:",
                           tags$ul(
                             tags$li("Create numerical and graphical summaries. Plots should be downloadable and one of the plots should be interactive"),
                             tags$li("Change the type of plot and type of summary being reported"),
                             tags$li("Change the variables and filter the rows to change the data in the plots/summaries")
                           )
                  ),
                  tabPanel("Modeling",
                           "The purpose of the Modeling page is to:",
                           tags$ul(
                             tags$li("Create three supervised learning models"),
                             tags$li("Explain the three supervised learning models approaches"),
                             tags$li("Model Fitting"),
                             tags$li("Give user a way to use one of the models for prediction")
                           ))
                ),
                box(
                  img(src="insurancepremiumpic.png", width=100, align="center"),
                )
              ),
              
      ),
      tabItem(tabName = "data",
              h2("Insurance Data Page"),
              checkboxInput("dataset","Filter option:"),
              conditionalPanel(
                condition = "input.dataset==true",
                fluidRow(
                  column(3,selectInput("sex", "Variable sex:", levels(insurance_data_update$sex))),
                  column(3,selectInput("smoker", "Variable smoker:", levels(insurance_data_update$smoker))),
                  column(3,selectInput("region", "Variable region:", levels(insurance_data_update$region))),
                  column(3, selectInput("children", "Variable children:", unique(sort(insurance_data_update$children))))
                ),
              ),
              dataTableOutput("data_read"),
              downloadButton("data_saved","Download this data")
      ),
      tabItem(tabName = "dataExploration",
              tabsetPanel(
                # First tab is for graphical summaries plots
                tabPanel("Graphs",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("plotType",
                                         "Select plot",
                                         choices = c("Histogram", "Scatter","Box"), selected = "Histogram"),
                             conditionalPanel(condition = "input.plotType == 'Histogram' ",
                                              selectInput("varhist", "Select a variable below to make a histogram:",
                                                          choices = histogramVarNames,
                                                          selected = "age"),
                                              sliderInput("numberofbins","Select how many bins you want for this histogram?",
                                                          min=1, max=10, step = 1, value = 5),
                                              checkboxInput("colorcode", "Want to color code the histogram?"),
                                              checkboxInput("overlay", "Want to overlay a density plot to this histogram?"),
                                              conditionalPanel(condition="input.overlay == 1",
                                                               sliderInput("alphavalue", "Opacity for the Density plot:",
                                                                           min = 0.1, max= 1, step=0.1, value = 0.5),
                                                               sliderInput("adjustvalue", "Adjustment value for the Density plot:",
                                                                           min = 0.1, max = 1, step = 0.1, value =0.5)
                                                               )
                             ),
                             conditionalPanel(condition = "input.plotType == 'Scatter' ",
                                              selectInput("varxscatter", "Choose a variable for your X-axis:",
                                                          choices = histogramVarNames,
                                                          selected = "age"
                                                          ),
                                              selectInput("varyscatter","Choose a variable for your Y-axis:",
                                                          choices = histogramVarNames,
                                                          selected = "bmi"),
                                              checkboxInput("ScatterColorCode", "Do you want to color code the Scatter Plot by sex?"),
                                              checkboxInput("Scatterline", "Add a line to the plot?:")
                               
                             ),
                             conditionalPanel(condition = "input.plotType == 'Box' ",
                                              selectInput("Varbox", "Select a variable to create a Boxplot:",
                                                          choices = histogramVarNames,
                                                          selected = "age"
                                                          ),
                                              checkboxInput("groupby", "Group the Boxplot by sex?")
                               
                             )
                            ),
                           mainPanel(
                             box(plotlyOutput("graphicalplots"),
                                 width = 12)
                           )
                         )#, # End of sidebar layout
                 ), # End of Tab Panel
                # First tab is for numerical summaries plots
                tabPanel("Numerical Summaries",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("numericVariables","Select a variable for numeric Summary",
                                         choices = histogramVarNames, selected="age"),
                             checkboxInput("groupbysex", "Do you want to have this summary group by sex?")
                           ),
                           mainPanel = (
                             box(tableOutput("NumericalSummary"),width = 7)
                           )
                         ) #End of second sidebar layout
                ) # End of numerical Tab Panel
              ) # End of Tabset Panel
              ),# End of Data Exploration tab
      tabItem(tabName = "modeling",
              tabsetPanel(
                tabPanel("Modeling Info",
                         fluidRow(
                           box(
                             strong("Multiple Linear Regression: "),
                             "Multiple Linear Regression is like a simple linear regression model, but can include more explanatory variables and/or higher order terms.",
                             br(),
                             br(),
                             strong("Benefits: "),
                             "In terms of benefits of using a multiple linear regression, one can find the relationship between the response variable and several explanatory variable.", 
                             br(), 
                             br(),
                             strong("Disadvantages: "),
                             "The disadvantages of using a multiple linear regression is that as more terms are added, it is hard to interpret the model and multicollinearity"
                             ,#width=12,
                             br(),
                             br(),
                             "Example Multiple Linear Regression:",
                             withMathJax(
                               helpText('\\(Y_i= \\beta_0\\ + \\beta_1X_{1i}\\ + \\beta_2X_{2i}\\ + \\beta_3X_{1i}X_{2i}+E_i\\) ')
                             )
                           ),
                           box(
                             strong("Regression Trees: "),
                             "So the purpose of using the tree based method is to split up predictor space into regions. Different predictions for each region. The purpose of the regression tree is to predcit a continuous response meaning for a given region, usually use mean of observations as prediction.",
                             br(),
                             br(),
                             strong("Benefits: "),
                             "In terms of benefits of using a regression tree, it is easy to interpret the regression tree model and works well on both linear and non-linear datasets.", 
                             br(), 
                             br(),
                             strong("Disadvantages:"),
                             "The disadvantages of using a regression tree is that it easily leads to overfitting and does not do so well on small datasets."
                           ),
                           box(
                             strong("Random Forest: "),
                             "So the purpose of using the random forest based method is to create multiple trees from boostrap samples and get average results. The difference between a random forest and bagging is that for random forests, it does not use all the predictors and use a random subset of predictors for each boostrap sample/tree fit.",
                             br(),
                             br(),
                             strong("Benefits: "),
                             "In terms of benefits of using a random forest, it produces an accurate result and works well with non-linear data.", 
                             br(), 
                             br(),
                             strong("Disadvantages:"),
                             "The disadvantages of using a random forest is that it is not easily interpretable and we would need to choose the number of trees."
                           )
                         )
                  #sidebarLayout(
                  #)
                ),
                tabPanel("Modeling Fitting",
                  sidebarLayout(
                    sidebarPanel(
                      sliderInput("proportion", "Proportion of data you want to use: ",
                                  min = 0.1, max = 0.9, value = 0.5, step = 0.1),
                      strong("Note to users: "),
                      "For the models, only age, bmi, and children variables will be used to run the models.",
                      checkboxInput("mlrmodel","Check if you want to display the multiple linear regression model: "),
                      checkboxInput("rtmodel","Check if you want to display the regression tree model: "),
                      #checkboxInput("rfmodel","Check if you want to display the random forestmodel: "),
                      conditionalPanel(
                        condition = "input.mlrmodel == 1",
                        checkboxInput("ageid", "Age"),
                        #checkboxInput("sexid", "Sex"),
                        checkboxInput("bmiid", "BMI"),
                        checkboxInput("childrenid", "Children")#,
                        #checkboxInput("smokerid", "Smoker"),
                        #checkboxInput("regionid", "Region")
                      ),
                      conditionalPanel(
                        condition = "input.rtrmodel == 1",
                        checkboxInput("ageid", "Age"),
                        #checkboxInput("sexid", "Sex"),
                        checkboxInput("bmiid", "BMI"),
                        checkboxInput("childrenid", "Children")#,
                        #checkboxInput("smokerid", "Smoker"),
                        #checkboxInput("regionid", "Region")
                      )
                    ),
                    mainPanel(
                      #dataTableOutput("data_train")
                      box(tableOutput("mlrmodelplot"),width=12),
                      box(
                        strong("Note to users: "),
                        br(),
                        "Lower RMSE indicates Better fit.",
                        br(),
                        "Higher R squared value is better.",
                        width=12
                      ),
                      box(tableOutput("rtrmodelplot"),width=12)
                    )
                  )
                ),
                tabPanel("Prediction",
                  sidebarLayout(
                    sidebarPanel(
                      
                    ),
                    mainPanel(
                      
                    )
                  )
                )
              )
              )
    ) # End of tabItems
  )# End of dashboardBody
  
)) # This will end the shinyUI and the dashboard page
