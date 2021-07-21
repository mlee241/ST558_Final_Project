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
      menuItem("About", tabname = "About"),
      menuItem("Data", tabname = "Data"),
      menuItem("Data Exploration", tabname = "Data Exploration"),
      menuItem("Modeling", tabname = "Modeling")
    ) 
  ), # End of dashboardSidebar
  
  # Dashboard Body 
  dashboardBody(
    tabItem(tabName = "About",
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
    tabItem(tabName = "Data"),
    tabItem(tabName = "Data Exploration"),
    tabItem(tabName = "Modeling")
  )
  
)) # This will end the shinyUI and the dashboard page
