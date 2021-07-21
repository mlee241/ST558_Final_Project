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
      tabItem(tabName = "dataExploration"),
      tabItem(tabName = "modeling")
    ) # End of tabItems
  )# End of dashboardBody
  
)) # This will end the shinyUI and the dashboard page
