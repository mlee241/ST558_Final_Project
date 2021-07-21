source("shared.R")

shinyServer(function(input,output,session){
  
  #Filtering columns
  getData <- reactive({
     newData <- insurance_data_update #%>% filter(sex == input$sex)#, smoker==input$smoker, region==input$region)
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
})