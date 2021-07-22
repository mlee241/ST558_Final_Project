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
      tabItem(tabName = "modeling")
    ) # End of tabItems
  )# End of dashboardBody
  
)) # This will end the shinyUI and the dashboard page
