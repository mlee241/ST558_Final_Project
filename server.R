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

  testpilot <- reactive({
    test = dplyr::setdiff(1:nrow(getmodelData()), trainsplit())
  })
  testdata <- reactive({
    test_data = getmodelData()[testpilot(), ]
  })
  
  mlrbutton <- eventReactive(input$generatereport,{
    set.seed(123)
    # Multiple linear regression model
    if(input$mlrmodel){
      if(input$selectvariables == "age"){
        fit1 <- train(expenses ~ age, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = as.numeric(input$numcv)))
        fit1$results
      } else if(input$selectvariables == "bmi"){
        fit1 <- train(expenses ~ bmi, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = as.numeric(input$numcv)))
        fit1$results
      } else if(input$selectvariables == "children"){
        fit1 <- train(expenses ~ children, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = as.numeric(input$numcv)))
        fit1$results
      } else if(input$selectvariables == "age & bmi"){
        fit1 <- train(expenses ~ age+bmi, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = as.numeric(input$numcv)))
        fit1$results
      } else if(input$selectvariables == "age & children"){
        fit1 <- train(expenses ~ age+children, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = as.numeric(input$numcv)))
        fit1$results
      } else if(input$selectvariables == "bmi & children"){
        fit1 <- train(expenses ~ bmi+children, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = as.numeric(input$numcv)))
        fit1$results
      } else if(input$selectvariables == "age & bmi & children"){
        fit1 <- train(expenses ~ age+bmi+children, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = as.numeric(input$numcv)))
        fit1$results
      } 
    }
    
  })
  
  rtbutton <- eventReactive(input$generatereport,{
    set.seed(123)
    #regression tree model
    if(input$rtrmodel){
      if(input$selectvariables == "age"){
        fit2 <- tree(expenses ~age, data=traindata(),split = input$selectionIndex)
        summaryfit2 <- summary(fit2)
        summaryfit2
      } else if(input$selectvariables == "bmi"){
        fit2 <- tree(expenses ~bmi, data=traindata(),split = input$selectionIndex)
        summaryfit2 <- summary(fit2)
        summaryfit2
      } else if(input$selectvariables == "children"){
        fit2 <- tree(expenses ~children, data=traindata(),split = input$selectionIndex)
        summaryfit2 <- summary(fit2)
        summaryfit2
      } else if(input$selectvariables == "age & bmi"){
        fit2 <- tree(expenses ~age+bmi, data=traindata(),split = input$selectionIndex)
        summaryfit2 <- summary(fit2)
        summaryfit2
      } else if(input$selectvariables == "age & children"){
        fit2 <- tree(expenses ~age+children, data=traindata(),split = input$selectionIndex)
        summaryfit2 <- summary(fit2)
        summaryfit2
      } else if(input$selectvariables == "bmi & children"){
        fit2 <- tree(expenses ~bmi+children, data=traindata(),split = input$selectionIndex)
        summaryfit2 <- summary(fit2)
        summaryfit2
      } else if(input$selectvariables == "age & bmi & children"){
        fit2 <- tree(expenses ~age+bmi+children, data=traindata(),split = input$selectionIndex)
        summaryfit2 <- summary(fit2)
        summaryfit2
      } 
    }
  })
  
  rfbutton <- eventReactive(input$generatereport,{
    set.seed(123)
    if(input$rfmodel){
      if(input$selectvariables == "age"){
        print("Select two variables.")
      } else if(input$selectvariables == "bmi"){
        print("Select two variables.")
      } else if(input$selectvariables == "children"){
        print("Select two variables.")
      } else if(input$selectvariables == "age & bmi"){
        fit3 <- train(expenses ~ age+bmi, data = traindata(),
                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
                                                                                                 number = 3,repeats = input$numrepeats))
        fit3$results
      } else if(input$selectvariables == "age & children"){
        fit3 <- train(expenses ~ age+children, data = traindata(),
                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
                                                                                                 number = 3,repeats = input$numrepeats))
        fit3$results
      } else if(input$selectvariables == "bmi & children"){
        fit3 <- train(expenses ~ bmi+children, data = traindata(),
                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
                                                                                                 number = 3,repeats = input$numrepeats))
        fit3$results
      } else if(input$selectvariables == "age & bmi & children"){
        fit3 <- train(expenses ~ age+bmi+children, data = traindata(),
                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
                                                                                                 number = 3,repeats = input$numrepeats))
        fit3$results
      }
    }
  })
  
  mlrtestbutton <- eventReactive(input$generatereport,{
    set.seed(123)
    # Multiple linear regression model
    if(input$mlrmodel){
      if(input$selectvariables == "age"){
        fit1 <- train(expenses ~ age, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = as.numeric(input$numcv)))

        pred1 <- predict(fit1,newdata=testdata())
        t(postResample(pred1, obs = testdata()$expenses))
      } else if(input$selectvariables == "bmi"){
        fit1 <- train(expenses ~ bmi, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = as.numeric(input$numcv)))

        pred1 <- predict(fit1,newdata=testdata())
        t(postResample(pred1, obs = testdata()$expenses))
      } else if(input$selectvariables == "children"){
        fit1 <- train(expenses ~ children, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = as.numeric(input$numcv)))

        pred1 <- predict(fit1,newdata=testdata())
        t(postResample(pred1, obs = testdata()$expenses))
      } else if(input$selectvariables == "age & bmi"){
        fit1 <- train(expenses ~ age+bmi, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = as.numeric(input$numcv)))

        pred1 <- predict(fit1,newdata=testdata())
        t(postResample(pred1, obs = testdata()$expenses))
      } else if(input$selectvariables == "age & children"){
        fit1 <- train(expenses ~ age+children, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = as.numeric(input$numcv)))

        pred1 <- predict(fit1,newdata=testdata())
        t(postResample(pred1, obs = testdata()$expenses))
      } else if(input$selectvariables == "bmi & children"){
        fit1 <- train(expenses ~ bmi+children, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = as.numeric(input$numcv)))

        pred1 <- predict(fit1,newdata=testdata())
        t(postResample(pred1, obs = testdata()$expenses))
      } else if(input$selectvariables == "age & bmi & children"){
        fit1 <- train(expenses ~ age+bmi+children, data = traindata(),
                      method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                                 number = as.numeric(input$numcv)))

        pred1 <- predict(fit1,newdata=testdata())
        t(postResample(pred1, obs = testdata()$expenses))
      } 
    }
  })
  
  rttestbutton <- eventReactive(input$generatereport,{
    set.seed(123)
    #regression tree model
    if(input$rtrmodel){
      if(input$selectvariables == "age"){
        fit2 <- tree(expenses ~age, data=traindata(),split = input$selectionIndex)

        rTree_pred <- predict(fit2, newdata = select(testdata(),
                                                     -expenses))

        rTreepred <- table(rTree_pred, testdata()$expenses)
        rTreepredrecords <- head(data.frame(rTreepred),input$records)
      } else if(input$selectvariables == "bmi"){
        fit2 <- tree(expenses ~bmi, data=traindata(),split = input$selectionIndex)

        rTree_pred <- predict(fit2, newdata = select(testdata(),
                                                     -expenses))

        rTreepred <- table(rTree_pred, testdata()$expenses)
        rTreepredrecords <- head(data.frame(rTreepred),input$records)
      } else if(input$selectvariables == "children"){
        fit2 <- tree(expenses ~children, data=traindata(),split = input$selectionIndex)

        rTree_pred <- predict(fit2, newdata = select(testdata(),
                                                     -expenses))

        rTreepred <- table(rTree_pred, testdata()$expenses)
        rTreepredrecords <- head(data.frame(rTreepred),input$records)
      } else if(input$selectvariables == "age & bmi"){
        fit2 <- tree(expenses ~age+bmi, data=traindata(),split = input$selectionIndex)

        rTree_pred <- predict(fit2, newdata = select(testdata(),
                                                     -expenses))

        rTreepred <- table(rTree_pred, testdata()$expenses)
        rTreepredrecords <- head(data.frame(rTreepred),input$records)
      } else if(input$selectvariables == "age & children"){
        fit2 <- tree(expenses ~age+children, data=traindata(),split = input$selectionIndex)

        rTree_pred <- predict(fit2, newdata = select(testdata(),
                                                     -expenses))

        rTreepred <- table(rTree_pred, testdata()$expenses)
        rTreepredrecords <- head(data.frame(rTreepred),input$records)
      } else if(input$selectvariables == "bmi & children"){
        fit2 <- tree(expenses ~bmi+children, data=traindata(),split = input$selectionIndex)

        rTree_pred <- predict(fit2, newdata = select(testdata(),
                                                     -expenses))

        rTreepred <- table(rTree_pred, testdata()$expenses)
        rTreepredrecords <- head(data.frame(rTreepred),input$records)
      } else if(input$selectvariables == "age & bmi & children"){
        fit2 <- tree(expenses ~age+bmi+children, data=traindata(),split = input$selectionIndex)

        rTree_pred <- predict(fit2, newdata = select(testdata(),
                                                     -expenses))

        rTreepred <- table(rTree_pred, testdata()$expenses)
        rTreepredrecords <- head(data.frame(rTreepred),input$records)
      }

    }
  })
  
  rftestbutton <- eventReactive(input$generatereport,{
    set.seed(123)
    if(input$rfmodel){
      if(input$selectvariables == "age"){
        print("Select two variables.")
      } else if(input$selectvariables == "bmi"){
        print("Select two variables.")
      } else if(input$selectvariables == "children"){
        print("Select two variables.")
      } else if(input$selectvariables == "age & bmi"){
        fit3 <- train(expenses ~ age+bmi, data = traindata(),
                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
                                                                                                 number = 3,repeats = input$numrepeats))

        rforest_pred <- predict(fit3, newdata = select(testdata(),
                                                       -expenses))
        t(postResample(rforest_pred, obs = testdata()$expenses))
      } else if(input$selectvariables == "age & children"){
        fit3 <- train(expenses ~ age+children, data = traindata(),
                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
                                                                                                 number = 3,repeats = input$numrepeats))

        rforest_pred <- predict(fit3, newdata = select(testdata(),
                                                       -expenses))
        t(postResample(rforest_pred, obs = testdata()$expenses))
      } else if(input$selectvariables == "bmi & children"){
        fit3 <- train(expenses ~ bmi+children, data = traindata(),
                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
                                                                                                 number = 3,repeats = input$numrepeats))

        rforest_pred <- predict(fit3, newdata = select(testdata(),
                                                       -expenses))
        t(postResample(rforest_pred, obs = testdata()$expenses))
      } else if(input$selectvariables == "age & bmi & children"){
        fit3 <- train(expenses ~ age+bmi+children, data = traindata(),
                      method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
                                                                                                 number = 3,repeats = input$numrepeats))

        rforest_pred <- predict(fit3, newdata = select(testdata(),
                                                       -expenses))
        t(postResample(rforest_pred, obs = testdata()$expenses))
      }
    }
    
  })
  
    
  output$mlrmodelplot = renderTable({
    mlrbutton()
  })
  
  output$rtrmodelplot = renderPrint({
    rtbutton()
  })
  
  #Random Forest
  output$rfmodelplot = renderTable({
    rfbutton()

  })
  
  output$mlrmodels = renderTable({
    mlrtestbutton()
  })
  output$rtmodels = renderTable({
    rttestbutton()
  })
  output$rfmodels = renderTable({
    rftestbutton()
  })
  
  # This will be used for the prediction
  newdatapred = eventReactive(input$makeprediction, {
    data.frame(
      age =input$ageinput,
      bmi =input$bmiinput,
      children =input$childreninput
    )
  })
  mlrmodel.pred = eventReactive(input$generatereport,{
    fit1 <- train(expenses ~ age+bmi+children, data = traindata(),
                  method = "lm", preProcess = c("center", "scale"), trControl = trainControl(method = "cv",
                                                                                             number = as.numeric(input$numcv)))
  })
  
  rtmodel.pred = eventReactive(input$generatereport,{
    fit2 <- tree(expenses ~age+bmi+children, data=traindata(),split = input$selectionIndex)
  })
  
  rfmodel.pred = eventReactive(input$generatereport,{
    fit3 <- train(expenses ~ age+bmi+children, data = traindata(),
                  method = "rf", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv",
                                                                                             number = 3,repeats = input$numrepeats))
    

  })
  
  prediction = eventReactive(input$makeprediction,{
    if(input$selectingmodel == "Multiple Linear Regression"){
      pred1 <- predict(mlrmodel.pred(),newdata=newdatapred())
    } else if (input$selectingmodel == "Regression tree"){
      pred2 <- predict(rtmodel.pred(), newdata=newdatapred())
    } else if (input$selectingmodel == "Random Forest"){
      pred3 <- predict(rfmodel.pred(), newdata=newdatapred())
    }
  })
  
  output$displayprediction = renderText({
    prediction()
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
})