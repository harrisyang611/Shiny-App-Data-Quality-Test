# to change the maximum input file size to 30 meg add options(shiny.maxRequestSize = 30*1024^2)  

library(shiny)
library(shinydashboard)
library(dplyr)
library(data.table)
library(openxlsx)

source('eda_fns.R')
source('Helper Function.R')
options(shiny.maxRequestSize = 30*1024^2)

ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput('FileR','Choose File',
                accept = c('text/csv','.csv','text/xlsx','.xlsx','.rda','text/rda')
      ),
      
      selectizeInput('FileF','Which File Format is the Input file',
                     choices = c('csv','xlsx','rda'),
                     options = list(
                       placeholder = 'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      ),
      
      selectizeInput('Proj','Choose an Analysis you want to conduct',
                  choices = c('Data Quality' = 'DQ' , 
                              'Data Overview' = 'DO',
                              'Data Analysis' = 'DA'),
                  options = list(
                    placeholder = 'Please select an option below',
                    onInitialize = I('function() { this.setValue(""); }')
                  )
      )
      
    ),
      
    
    
    mainPanel(
      verbatimTextOutput("text"),
      
      ##Instructions
      uiOutput("Instr"),
      renderText({'<br/>'}),
      ##Factor Intake
      fluidRow(
        column(4, uiOutput("DQF1")),
        column(4, uiOutput("DQF2")),
        column(4, uiOutput("DQF3"))
      ),
      fluidRow(
        column(5,uiOutput("DOF1")),
        column(5,uiOutput("DOF2"))
      ),
      ##DA Method selected
      uiOutput('DAMethodSelect'),
      renderText({'<br/>'}),
      ##DA Method Explanation
      uiOutput('DAMethodExplain'),
      
      renderText({'<br/>'}),
      ##DA input and output Intake
      uiOutput('DAFInputVarNo'),
      uiOutput('DAFbox'),
      uiOutput('DAFOutputVarNo'),
      uiOutput('DAResponse'),
      renderText({'<br/>'}),
      tableOutput('DOtype'),
      renderText({'<br/>'}),
      uiOutput("But"),
      uiOutput("AnyResult"),
      uiOutput('But2')
    )
    
  )
)


server <- function(input,output,session){
  
  
  
#### reactive Section
  
  data_set <- reactive({
    inFile <- input$FileR
    #options(shiny.maxRequestSize = 30*1024^2)
    if(input$FileF == 'csv') { d <- fread(inFile$datapath)}
    if(input$FileF == 'xlsx') { d <- read.xlsx(inFile$datapath) %>% as.data.table()}
    if(input$FileF == 'rda') {d <- get( load(inFile$datapath) ) %>% as.data.table()}
    return(d)
  })
  
  
### Head Shwoing States
  
  output$text <- renderText({
    if( is.null( input$FileR ) ) {'Please Upload File... Note: Only CSV/XLSX/Rda file'}
    else {'Please Choose the Project You Want to Do...'}
  })
  
  
### Introduction / Instruction for both DQ and DO

  output$Instr <- renderUI({
    if (is.null(input$Proj))
      return()

    switch(input$Proj,
           "DQ" = tableOutput('Contents_DQ'),
           "DO" = tableOutput('Contents_DO'),
           "DA" = tableOutput('Contents_DA')
          
    )})
  
#########  Data Quality Section
  output$Contents_DQ <- renderText({
    c( 'If you want to create a Data Quality Report',
       '<br/>',
       'Please Just Click the Download Button',
       '<br/>',
       'Note: All variables from the file are considered as its original type',
       '<br/>',
       'If you want to transform to Numeric/Time/Character format',
       '<br/>',
       'Please type the variable names with comma as separator'
    )
  })
  
  ##  Data Quality Converting Variables Intake
  output$DQF1 <- renderUI({
    if (input$Proj != 'DQ')
      return()
    switch(input$Proj,
           "DQ" =  textInput('DQNum','Numeric')
    )})
  
  output$DQF2 <- renderUI({
    if (input$Proj != 'DQ')
      return()
    switch(input$Proj,
           "DQ" =  textInput('DQDate','Date')
    )})
  
  output$DQF3 <- renderUI({
    if (input$Proj != 'DQ')
      return()
    switch(input$Proj,
           "DQ" =  textInput('DQChar','Character')
    )})
  
  ## Creating DQ Report
  output$Down <- downloadHandler(
    filename = function() {
      x <- input$FileR$name
      x <- gsub('.xlsx|.csv','',x)
      paste(x,'_DQ.html',sep='')
    },
    
    content = function(file) {
      library(rmarkdown)
      x <- input$FileR
      Temp <- GetRmd(x,input$DQNum,input$DQDate,input$DQChar,input$FileF)
      tempReport <- file.path(tempdir(), Temp)
      file.copy(Temp, tempReport, overwrite = TRUE)
      render(Temp, quiet = TRUE,output_file = file)
      file.remove(Temp)
    }
  )
  
  
  
  
  
#########  Data Overview Section
  
  output$Contents_DO <- renderText({
    c( 'If you want to do Analysis for Single Variable',
       '<br/>',
       'Choose the column from your uploaded file',
       '<br/>',
       'We Only provide analysis for numeric variable or character variable',
       'box plot for Numeric and the Word Cloud for character variable'
    )
  })
  
  
  ## Data Overview Variables Intake
  
  output$DOF1 <- renderUI({
    if (input$Proj != 'DO' | is.null(input$FileR))
      return()
    
    switch(input$Proj,
           "DO" = selectizeInput('DOFactor','Select a Variable Name',
                                 choices = names(data_set()),
                                 options = list(
                                   placeholder = 'Please select an option below',
                                   onInitialize = I('function() { this.setValue(""); }')
                                 ))
    )
  })
  
  output$DOF2 <- renderUI({
    if (input$Proj != 'DO' | is.null(input$DOFactor) )
      return()
    
    if(input$Proj == 'DO') {
      data <- data_set()
      c <- input$DOFactor
      datatype <- as.character( class(data[[c]]) )
      
      if(datatype == 'character'){
        textInput('DOBase','The Base of the Char Count')
      }
      
      else if(datatype == 'numeric'){
        selectizeInput('DOBase','Select a Graph You Want to Draw',
                       choices = c('box plot','Histogram'),
                       options = list(
                         placeholder = 'Please select an option below',
                         onInitialize = I('function() { this.setValue(""); }')
                       ))
      }
    }
  })
  
  ##Print the variable type of Variable selected
  
  output$DOtype <- renderText({
    if(input$Proj != 'DO' | is.null(input$DOFactor)) return()
    
    if(input$Proj == 'DO') {
      data <- data_set()
      c <- input$DOFactor
      c('Your selected Variable have type as : ',
        '<br/>',
        class(data[[c]]),
        '<br/>',
        'Now Please Press the Button')}
  })
  
  ## Reactive Chart for Data Overview
  
  my_chart <- reactive({
    if(input$DOButton == 0 )
    {
      return()
    }
    isolate({
      input$DOButton
      data <- data_set()
      c <- input$DOFactor
      datatype <- as.character( class(data[[c]]) )
      
      if( datatype == 'numeric' & input$DOBase == 'box plot'){boxplot(data[[c]])}
      if( datatype == 'numeric' & input$DOBase == 'Histogram'){hist(data[[c]])}
      if( datatype == 'character'){
        if(input$DOBase == ''){
          MyBase <- 100} else {
            MyBase <- as.numeric(input$DOBase)}
        desc.wordcloud(data[[c]] , word.scale = MyBase)
      }
      
    })
  })
  
  ## Plot the chart when click the button
  output$plotOut <- renderPlot({my_chart()})
  
  
  
  
#########  Data Analysis Section
  
  
  
  output$Contents_DA <- renderText({
    c('If you want to do some further analysis',
      '<br/>',
      'Please Add the columns you need'
      )
  })
  
  
  ## Select an analysis to Conduct
  
  output$DAMethodSelect <- renderUI({
    if(input$Proj != 'DA') return()
    
    if(input$Proj == 'DA'){
      selectizeInput('DAMethod','Select an stats method you want to use',
                     choices = c('k-mean clustering','Decision Tree','Linear Regression'),
                     options = list(
                       placeholder = 'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }')
                     ))
    }
  })
  
  ## DA analysis Explaination
  
  output$DAMethodExplain <- renderUI({
    if(input$Proj != 'DA') return()
    
    switch(input$DAMethod,
           'Linear Regression' = tableOutput('LRExplain'),
           'Decision Tree' = tableOutput('DTExplain'),
           'k-mean clustering' = tableOutput('KMExplain')
    )
    })
  
  output$LRExplain <- renderText({
    c(' Linear regression is an approach for modeling the relationship between a scalar dependent variable y and one or more independent variables denoted X.',
      '<br/>',
      '<br/>',
      'Procedures : ',
      '<br/>',
      'Please select how many x you want to choose in the following box',
      '<br/>',
      'then Select Only the numeric variables in the independent variables',
      '<br/>',
      'Check the box "Do you need a dependent variable ?"',
      '<br/>',
      'Select the dependent variable',
      '<br/>',
      'Then Click on the Button to generate the analysis',
      '<br/>',
      'Note : all variables ought to be numeric'
    )
  })
  
  output$DTExplain <- renderText({
    c('Decision Tree analysis is an approach for classcification using Machine Learning, The goal is to create a model that predicts the value of a target variable based on several input variables.',
      '<br/>',
      'It can be used to trace the logic of a set of variables based on one responce variable',
      '<br/>',
      '<br/>',
      'Procedures : ',
      '<br/>',
      'Please select how many logic-used variables you want to choose in the following box',
      '<br/>',
      'Check the box "Do you need a dependent variable ?"',
      '<br/>',
      'Select the dependent variable',
      '<br/>',
      'Then Click on the Button to generate analysis',
      '<br/>',
      'Note : variables can be numeric or character and the dependent variable has to be character',
      '<br/>',
      'We use C5.0 for this Decision tree analysis due to its high readability'
    )
  })
  
  output$KMExplain <- renderText({
    c('k-means clustering aims to partition n observations into k clusters in which each observation belongs to the cluster with the nearest mean, serving as a prototype of the cluster.',
      '<br/>',
      'We generate a chart for the understanding of each clusters',
      '<br/>',
      '<br/>',
      'Procedures : ',
      '<br/>',
      'Please select only 2 in the following numeric input box',
      '<br/>',
      'Do NOT Check the box "Do you need a dependent variable ?"',
      '<br/>',
      'Then Click on the Button to generate analysis',
      '<br/>',
      'Note : variables has to be numeric',
      '<br/>',
      'We use Elbow Method to determine the best k from the sample.'
    )
  })
  
    
    ##################################################################
  
  
  ## How many Variables intake
  
  output$DAFInputVarNo <- renderUI ({
    if (input$Proj != 'DA')
      return()
    
    if(input$Proj == 'DA'){
      numericInput("numInputs", "How many inputs do you want", 2)
    }
  })
  
  
  ## Make the Variables and Index as a box
  
  output$DAFbox <- renderUI({
    if (input$Proj != 'DA') return()
    
    if(input$Proj == 'DA'){
      box(width = 10, title = 'All Independent Variables',
          fluidRow(
            column(5,uiOutput('DAF0')),
            column(3,uiOutput('DAF0Index'))
          )
      )
    }
  })
  
  
  ## Add an Input for the Input Var Number
  ## Add an extra Index for Linear Regression
  
  
  
  observeEvent(input$numInputs, {
    if(input$Proj != 'DA' | is.null(input$FileR)) return()
    
    if(input$Proj == 'DA'){
      
      output$DAF0 = renderUI({
        if(input$Proj != 'DA' | is.null(input$FileR)) return()
        
        if(input$numInputs >= 1){
          input_list <- lapply(1:input$numInputs, function(i) {
            # for each dynamically generated input, give a different name
            inputID <- paste("DAFinput", i, sep = "")
            inputLabel <- paste("Independent Var ", i, sep = "")
            selectizeInput(inputID,inputLabel,
                           choices = names(data_set()),
                           options = list(
                             placeholder = 'Please select an option below',
                             onInitialize = I('function() { this.setValue(""); }')
                           ))
          })
          do.call(tagList, input_list)}
      })
      
      output$DAF0Index = renderUI({
        if(input$Proj != 'DA' | is.null(input$FileR)) return()
        
        if(input$numInputs >= 1 & input$DAMethod == 'Linear Regression'){
          input_list_Index <- lapply(1:input$numInputs, function(i) {
            # for each dynamically generated input index, give a different name
            inputID_Index <- paste("DAFinput_Index", i, sep = "")
            inputLabel_Index <- paste("Power ", i, sep = "")
            selectizeInput(inputID_Index,inputLabel_Index,
                           choices = c(as.character(c(1:5)),'e','log'),
                           options = list(
                             placeholder = '1')
            )
          })
          do.call(tagList, input_list_Index)}
      })
    }
  })
  
  
  ## Check box for whether the response variable is required
  
  output$DAFOutputVarNo <- renderUI ({
    if (input$Proj != 'DA')
      return()
    
    if(input$Proj == 'DA'){
      checkboxInput("WResult", "Do you need a dependent variable ?", FALSE)
    }
  })
  
  ## Add a response variable if one is required
  output$DAResponse <- renderUI({
    if(is.null(input$WResult) | input$Proj != 'DA') {return()}
    
    if(input$Proj == 'DA' & input$WResult == TRUE) {
      
      selectizeInput('DAResult','Select a dependent variable',
                     choices = names(data_set()),
                     options = list(
                       placeholder = 'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }')
                     ))
    }
  })
  

  
  ## Reactive Section For the Intake Variables
  
  my_list <- reactive({
    list <- c()
    i <- 1
    while(i <= input$numInputs) {
      inputName <- paste("DAFinput", i, sep = "")
      list <- c(list,input[[inputName]])
      i <- i + 1
    }
    return(list)
  })
  
  my_index_list <- reactive({
    list <- c()
    i <- 1
    while(i <= input$numInputs) {
      inputName <- paste("DAFinput_Index", i, sep = "")
      list <- c(list,input[[inputName]])
      i <- i + 1
    }
    return(list)
  })
  
  ## Linear Regression Section
  
  my_LReg <- reactive({
    if(input$DAButton == 0) {return()}
    
    isolate({
      input$DAbutton
      data <- data_set()
      list <- my_list()
      index_list <- my_index_list()
      resultfactor<- input$DAResult
      list <- c(list,resultfactor)
      mydt <- data[,list,with = FALSE] %>% as.data.table()
      if(input$DAMethod == 'Linear Regression'){
        LRegressionanalysis(mydt,index_list)
      }
    })
  })
  
  # Linear Regression Print
  output$LReg <- renderPrint({my_LReg()})
  
  
  ## Decision Tree Section
  
  my_DTree <- reactive({
    if(input$DAButton == 0) {return()}
    
    isolate({
      input$DAbutton
      data <- data_set()
      list <- my_list()
      resultfactor<- input$DAResult
      list <- c(list,resultfactor)
      if(input$DAMethod == 'Decision Tree') {
        DTreeanalysis(data,list)
      }
    })
  })
  
  # Summary of DT
  my_DTree_Text <- reactive({
    dt <- my_DTree()
    summary(dt)
  })
  
  # Decision Tree Print
  
  output$DTree <- renderPrint({my_DTree_Text()})
  
  output$But2 <- renderUI({
    if(input$Proj == ''  | is.null(input$FileR) ) return()
    
    switch(input$Proj,
           'DA' = {if (input$DAMethod == 'Decision Tree') {
             downloadButton('DownDT','Downloading the file with DT classification')
           }})
  })
  
  ## Download Decision Tree result
  
  output$DownDT <- downloadHandler(
    filename = function() {
      x <- input$FileR$name
      x <- gsub('.xlsx|.csv|.rda','',x)
      paste(x,'_Decision_Tree.csv',sep='')
    },
    
    content = function(file) {
      x <- data_set()
      list <- my_list()
      resultfactor<- input$DAResult
      list <- c(list,resultfactor)
      Temp <- DTreeanalysis_error(x,list)
      fwrite(Temp,file)
    }
  )
  
  
  ## K-means Clustering
  
  my_chart2 <- reactive({
    if(input$DAButton == 0) {return()}
    
    isolate({
      input$DAbutton
      data <- data_set()
      list <- my_list()
      mydt <- data[,list,with = FALSE] %>% as.data.table()
      if(input$DAMethod == 'k-mean clustering') {
        k <- FindK(mydt)
        x <- KmAnalysis(mydt,k)
        fviz_cluster(x,data = mydt, ellipse.alpha = 0.01,
                     palette = "Dark2")
      }
    })
  })
  
  ## K-means plot
  output$plotOut2 <- renderPlot({my_chart2()})

  
  
  
######### The Button to show the result

  output$But <- renderUI({
    if (is.null(input$Proj))
      return()
    switch(input$Proj,
           "DQ" = downloadButton('Down','Download DQ Report'),
           "DO" = actionButton('DOButton','Generate the Charts'),
           "DA" = actionButton('DAButton','Generate the Analysis')
    )})
  

  ## Generating the Chart for Data Overview and Data analysis
  
  output$AnyResult <- renderUI({
    if (input$Proj == '' | is.null(input$FileR))
      return()
    
    switch(input$Proj,
           'DO' = plotOutput("plotOut"),
           'DA' = {
             if (input$DAMethod == 'k-mean clustering') {plotOutput("plotOut2")
               } else if (input$DAMethod == 'Decision Tree') {
                 verbatimTextOutput("DTree")
               } else if (input$DAMethod == 'Linear Regression') {
                 verbatimTextOutput("LReg")
               }
           }
    )
  })
  

  
  

  
  
  

}

shinyApp(ui = ui,server = server)