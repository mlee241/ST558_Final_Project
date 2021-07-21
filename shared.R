library(DT)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(caret)
library(dplyr)
library(randomForest)
library(tidyverse)
library(tree)
library(png)

insurance_data <- read.csv("insurance.csv")

# 4 numerical features- age, bmi, children, and expenses
# 3 nominal features - sex, smoker and region

insurance_data_update <- insurance_data %>% dplyr::transmute(
  age = age,
  sex = factor(sex, levels=c("female", "male"), labels = c(0,1)),
  bmi = bmi,
  children = children,
  smoker = factor(smoker, levels=c("no", "yes"), labels = c(0,1)),
  region = factor(region, levels=c("southwest", "southeast","northwest","northeast"), labels = c(0,1,2,3) ),
  expenses = expenses
)

set.seed(123)
