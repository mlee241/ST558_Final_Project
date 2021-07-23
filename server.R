source("shared.R")

shinyServer(function(input,output,session){
  
  #Filtering columns
  getData <- reactive({
     newData <- insurance_data_update #%>% filter(sex == input$sex)#, smoker==input$smoker, region==input$region)
  })
  
  # This data will be used for running the models because if I do not filter, that will be a lot of combinations for the models.
  getmodelData <- reactive({
    newmodelData <- insurance_data_update %>%select(age,bmi,children,expenses)# filter(sex == input$sex)#, smoker==input$smoker, region==input$region)
  })
  
  tableData <- reactive({
    if(input$dataset==TRUE){
      if(input$sex==0){
        getData() %>% filter(sex ==input$sex, smoker==input$smoker, region==input$region,children==input$children)
      } else {
        getData() %>% filter(sex == input$sex, smoker==input$smoker, region==input$region,children==input$children)
      }
    }else{
      getData()
    }
  })
  
  # Reading in the data
  output$data_read = renderDataTable({
    tableData()
  })

  # Option of downloading data
  output$data_saved = downloadHandler(
    filename="insurance.csv",
    content = function(file){
        write.csv(tableData(), file)
    }
  )
  
  # Summaries(Graphical and Numerical)
  
  #Graphical output
  output$graphicalplots = renderPlotly({
    # Histogram
    if(input$plotType == "Histogram"){
      if(input$colorcode & input$overlay){
        histogram_plot = ggplot(data=getData(), aes_string(x=input$varhist))+
          geom_histogram(aes(fill=sex, y=..density..),position = "dodge", color="black", bins = input$numberofbins)+
          geom_density(aes(fill=sex), alpha=input$alphavalue, adjust=input$adjustvalue)+
          scale_fill_manual(values=c("tomato2","green"))
        ggplotly(histogram_plot)
      } else if(input$colorcode & !input$overlay){
        histogram_plot = ggplot(data=getData(), aes_string(x=input$varhist))+
          geom_histogram(aes(fill=sex, y=..density..),position = "dodge", color="black", bins = input$numberofbins)+
          scale_fill_manual(values=c("tomato2","green"))
        ggplotly(histogram_plot)
      } else if(!input$colorcode & input$overlay){
        histogram_plot = ggplot(data=getData(), aes_string(x=input$varhist))+
          geom_histogram(aes(y=..density..), fill="green", color="black", bins = input$numberofbins)+
          geom_density(fill="turquoise1", alpha=input$alphavalue, adjust=input$adjustvalue)
        ggplotly(histogram_plot)
      } else{
        histogram_plot = ggplot(data=getData(), aes_string(x=input$varhist), aes(y=..density..))+
          geom_histogram(aes(y=..density..),fill = "green", color="black", bins = input$numberofbins)
        ggplotly(histogram_plot)
      }
    } else if(input$plotType== "Scatter"){
      # Scatter plot
      if(input$ScatterColorCode & !input$Scatterline){
        scatter_plot = ggplot(data=getData(),aes_string(x=input$varxscatter, y=input$varyscatter))+
          geom_point(aes(color=sex),alpha=0.5)+scale_color_manual(values=c("red","blue"))
        ggplotly(scatter_plot)
      } else if(!input$ScatterColorCode & !input$Scatterline){
        scatter_plot = ggplot(data=getData(),aes_string(x=input$varxscatter, y=input$varyscatter))+
          geom_point(color="blue")
        ggplotly(scatter_plot)
      } else if(input$ScatterColorCode & input$Scatterline){
        scatter_plot = ggplot(data=getData(),aes_string(x=input$varxscatter, y=input$varyscatter))+
          geom_point(aes(color=sex),alpha=0.5)+geom_smooth(aes(group=sex,color=sex),se=TRUE)+scale_color_manual(values=c("red","blue"))
        ggplotly(scatter_plot)
      } else if(!input$ScatterColorCode & input$Scatterline){
        scatter_plot = ggplot(data=getData(),aes_string(x=input$varxscatter, y=input$varyscatter))+
          geom_point(color="green")+geom_smooth(se=TRUE, color="blue")
        ggplotly(scatter_plot)
      }
    } else if(input$plotType=="Box"){
      # Box plot
      if(input$groupby){
        box_plot = ggplot(data=getData(), aes_string(y=input$Varbox))+geom_boxplot(aes(x=sex, color=sex))+scale_color_manual(values=c("red","blue"))
        ggplotly(box_plot)
      } else{
        box_plot = ggplot(data=getData(), aes_string(y=input$Varbox))+geom_boxplot(color="blue")
        ggplotly(box_plot)
      }
    }
    
  })
  
  #Numerical output
  output$NumericalSummary = renderTable({
    if(input$groupbysex){
      group0 = getData() %>% dplyr::select(input$numericVariables,sex) %>% filter(sex==0)
      group0 = group0 %>% dplyr::summarize(
        Min = min(group0[[input$numericVariables]]),
        Max = max(group0[[input$numericVariables]]),
        Mean = round(mean(group0[[input$numericVariables]]),2),
        SD = round(sd(group0[[input$numericVariables]]),2),
        Med = median(group0[[input$numericVariables]]),
        IQR = IQR(group0[[input$numericVariables]])
      )
      
      group0$sex = "Female"
      
      group1 = getData() %>% dplyr::select(input$numericVariables,sex) %>% filter(sex==1)
      group1 = group1 %>% dplyr::summarize(
        Min = min(group1[[input$numericVariables]]),
        Max = max(group1[[input$numericVariables]]),
        Mean = round(mean(group1[[input$numericVariables]]),2),
        SD = round(sd(group1[[input$numericVariables]]),2),
        Med = median(group1[[input$numericVariables]]),
        IQR = IQR(group1[[input$numericVariables]])
      )
      
      group1$sex = "Male"
      
      numerical_summary_table = rbind(group0,group1)
      numerical_summary_table
    } else{
      numerical_summary_table = getData() %>% dplyr::select(input$numericVariables) %>% dplyr::summarize(
        Min = min(getData()[[input$numericVariables]]),
        Max = max(getData()[[input$numericVariables]]),
        Mean = round(mean(getData()[[input$numericVariables]]),2),
        SD = round(sd(getData()[[input$numericVariables]]),2),
        Med = median(getData()[[input$numericVariables]]),
        IQR = IQR(getData()[[input$numericVariables]])
      )
    }
    numerical_summary_table
  })
  
  #Train split data
  trainsplit <- reactive({
    set.seed(123) # For reproducibility
    train = sample(1:nrow(getmodelData()), size=nrow(getmodelData())*input$proportion)
  })
  traindata <- reactive({
    train_data = getmodelData()[trainsplit(), ]
  })
  # Reading in the data
  # output$data_train = renderDataTable({
  #   traindata()
  # })
  testpilot <- reactive({
    test = dplyr::setdiff(1:nrow(getmodelData()), trainsplit())
  })
  testdata <- reactive({
    test_data = getmodelData()[testpilot(), ]
  })
  
  #test = dplyr::setdiff(1:nrow(insurance_data_update), train)
  #test_data = insurance_data_update[test, ]
  
  # Build Multiple linear Regression Model and plot
  # result <- eventReactive(input$generatereport,{
  #   if(is.null(input$ageid)){
  #     return("Did not hit age")
  #   }
  #   
  # })
  output$mlrmodelplot = renderTable({
    set.seed(123)
    if(input$mlrmodel){
      if(input$ageid & !input$bmiid & !input$childrenid){
        # model1 <- lm (expenses~ age, data=traindata())
        fit1 <- train(expenses ~ age, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = 5))
        #summary(model1)$coeffiients
        fit1$results
      } else if(!input$ageid & input$bmiid & !input$childrenid){
        fit1 <- train(expenses ~ bmi, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = 5))
        fit1$results
      } else if(!input$ageid & !input$bmiid & input$childrenid){
        fit1 <- train(expenses ~ children, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = 5))
        fit1$results
      } else if(input$ageid & input$bmiid & !input$childrenid){
        fit1 <- train(expenses ~ age+bmi, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = 5))
        fit1$results
      } else if(input$ageid & !input$bmiid & input$childrenid){
        fit1 <- train(expenses ~ age+children, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = 5))
        fit1$results
      } else if(!input$ageid & input$bmiid & input$childrenid){
        fit1 <- train(expenses ~ bmi+children, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = 5))
        fit1$results
      } else if(input$ageid & input$bmiid & input$childrenid){
        fit1 <- train(expenses ~ age+bmi+children, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = 5))
        fit1$results
      } 
    }
  })
  
  
  # Build Multiple linear Regression Model and plot
  
  output$rtrmodelplot = renderPrint({
    set.seed(123)
    if(input$rtrmodel){
      if(input$ageid & !input$bmiid & !input$childrenid){
        fit1 <- tree(expenses ~age, data=traindata())
        #print(fit1)
        summaryfit1 <- summary(fit1)
        summaryfit1
        #fit1$results
      } else if(!input$ageid & input$bmiid & !input$childrenid){
        fit1 <- tree(expenses ~bmi, data=traindata())
        summaryfit1 <- summary(fit1)
        summaryfit1
      } else if(!input$ageid & !input$bmiid & input$childrenid){
        fit1 <- tree(expenses ~children, data=traindata())
        summaryfit1 <- summary(fit1)
        summaryfit1
      } else if(input$ageid & input$bmiid & !input$childrenid){
        fit1 <- tree(expenses ~age+bmi, data=traindata())
        summaryfit1 <- summary(fit1)
        summaryfit1
      } else if(input$ageid & !input$bmiid & input$childrenid){
        fit1 <- tree(expenses ~age+children, data=traindata())
        summaryfit1 <- summary(fit1)
        summaryfit1
      } else if(!input$ageid & input$bmiid & input$childrenid){
        fit1 <- tree(expenses ~bmi+children, data=traindata())
        summaryfit1 <- summary(fit1)
        summaryfit1
      } else if(input$ageid & input$bmiid & input$childrenid){
        fit1 <- tree(expenses ~age+bmi+children, data=traindata())
        summaryfit1 <- summary(fit1)
        summaryfit1
      } 
    }
  })
  
  #Random Forest
  output$rfmodelplot = renderTable({
    set.seed(123)
    if(input$rfmodel){
      if(input$ageid & !input$bmiid & !input$childrenid){
        # model1 <- lm (expenses~ age, data=traindata())
        fit1<-randomForest(expenses~age, data=traindata(), mtry=ceiling(ncol(traindata()/3)),ntree=100,importance=TRUE)
        # fit1 <- train(expenses ~ age, data = traindata(),
        #               method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
        #                                                                                          number = 3,repeats = 2))
        #summary(model1)$coeffiients
        fit1$results
      } else if(!input$ageid & input$bmiid & !input$childrenid){
        fit1 <- train(expenses ~ bmi, data = traindata(),
                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
                                                                                                 number = 3,repeats = 3))
        fit1$results
      } else if(!input$ageid & !input$bmiid & input$childrenid){
        fit1 <- train(expenses ~ children, data = traindata(),
                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
                                                                                                 number = 3,repeats = 3))
        fit1$results
      } else if(input$ageid & input$bmiid & !input$childrenid){
        fit1 <- train(expenses ~ age+bmi, data = traindata(),
                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
                                                                                                 number = 3,repeats = 3))
        fit1$results
      } else if(input$ageid & !input$bmiid & input$childrenid){
        fit1 <- train(expenses ~ age+children, data = traindata(),
                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeated",
                                                                                                 number = 3,repeats=3))
        fit1$results
      } else if(!input$ageid & input$bmiid & input$childrenid){
        fit1 <- train(expenses ~ bmi+children, data = traindata(),
                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
                                                                                                 number = 3,repeats = 3))
        fit1$results
      } else if(input$ageid & input$bmiid & input$childrenid){
        fit1 <- train(expenses ~ age+bmi+children, data = traindata(),
                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
                                                                                                 number = 3,repeats = 3))
        fit1$results
      } 
    }
  })
  
  

  
  
})