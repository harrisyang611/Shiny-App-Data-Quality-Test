library(openxlsx)
library(data.table)
library(dplyr)
library(dtplyr)
library(ggplot2)
library(factoextra)
library(C50)
source('characterize_funs_improved.R')

GetRmd <- function(File,num,dat,char,type){
  ## File must be a string
  Curdate <- Sys.Date()
  Curdate <- format(Curdate,'%B-%d-%Y')
  name <- File$name
  
  from <- File$datapath
  to <- file.path(dirname(from), basename(File$name))
  file.rename(from, to)
  
  num <- strmod(num) 
  dat <- strmod(dat)
  char <- strmod(char)
  
  writeLines(
    c(
      "---",
      paste0("title: '",name,"'"),
      "author: 'IA.DA'",
      paste0("date: '",Curdate,"'"),
      "output: ",
      "  html_document:",
      "    toc: TRUE",
      "    toc_float: TRUE",
      "---",
      # "```{r setup, echo = FALSE, message = FALSE}",
      # "library(openxlsx)",
      # "library(data.table)",
      # "library(dplyr)",
      # "library(dtplyr)",
      # "library(ggplot2)",
      # "source('characterize_funs_improved.R')",
      # "```",
      
      "## General Info",
      "This provides a Data Quality Test on the raw file, summarizing each column in terms of its variable type",
      "<br/><br/>",
      "Remark : The Converted variables will NOT be changed in the raw data, it will only be a change in the report.",
      "<br/><br/>",
      "<br/><br/>",
      " Clarification of Syntax : ",
      "<br/><br/>",
      "Observation Count indicates general info for each variables",
      "<br/><br/>",
      "Number of observations shows the number of rows of each variable", 
      "<br/><br/>",
      "Number of distinct observations shows the number of distinct values in each column",
      "<br/><br/>",
      "Missing values (NA) shows how many NAs for R or NULLs for SQL",
      "<br/><br/>",
      "Blank values shows the number of strings with only spaces, it only works for characters",
      "<br/><br/>",
      

      
      "```{r load,echo = FALSE}",
      
      if(type == 'csv'){
        paste0(" DT <- read.csv( '",to,"' ) %>% as.data.table()")
      },
      
      if(type == 'xlsx'){
        paste0( "DT <- read.xlsx( '",to,"' ) %>% as.data.table()")
      },
      
      if(type == 'rda'){
        paste0( "DT <- get( load( '" ,to, "' ) ) %>% as.data.table()")
      },
      
      if(num==""){"num <- 'NumISNULL'"} else {paste0("num <- " , num)},
      
      if(char==""){"char <- 'CharISNULL'"} else {paste0("char <- " , char)},
      
      if(dat==""){"dat <- 'DatISNULL'"} else {paste0("dat <- " , dat)},
      
      "```",
      
      "## Basic Information of the File",
      " Number of Rows in the File : `r nrow(DT)`",
      "<br/><br/>",
      " Number of Columns in the File : `r ncol(DT)`",
      "<br/><br/>",
      
      
      "```{r characterize, echo = FALSE, results = 'asis'}",
      "if(! (length(dat) == 1 && dat == 'DatISNULL') ){",
      "DT[, dat] <- lapply(DT[, dat, with = FALSE], NumToDate)}",
      
      "if(! (length(char) == 1 && char == 'CharISNULL') ){",
      "DT[, char] <- lapply(DT[, char, with = FALSE], as.character)}",
      
      "if(! (length(num) == 1 && num == 'NumISNULL') ){",
      "DT[, num] <- lapply(DT[, num, with = FALSE], as.numeric)}",
      
      "characterize.data(DT)",
      "```" ),
    'Temp.Rmd'
  )
  return('Temp.Rmd')
}

strmod <- function(mystr){
  if(mystr == '') {return ("")}
  mystr <- as.character(mystr)
  mystr <- strsplit(mystr,',') %>% unlist()
  mystr <- gsub(" ","",mystr)
  
  r <- paste0(paste0("'",mystr,"'"),collapse = ',')
  r <- paste0("c(",r,")")
  return(r)
}

NumToDate <- function(num){
  return(as.Date(num,origin = '1899-12-30'))
}


FindK <- function(dataset,siglevel = 0.05){
  dataset <- as.data.table(dataset)
  k.max <- 15
  wss <- sapply(1:k.max, 
                function(k){kmeans(dataset, k, nstart=50,iter.max = 15 )$tot.withinss})
  tss <- wss[1]
  mss <- wss/tss
  k <- 1
  while (k < k.max){
    if(mss[k] < siglevel){ return (k)}
    else{ k <- k + 1}
  }
}

KmAnalysis <- function(dataset,k){
  dataset <- as.data.table(dataset)
  data_kmeans <- kmeans(dataset, centers = k, nstart = 25)
  return(data_kmeans)
}




DTreeanalysis <- function(dt,lst){
  mydt <- dt %>% as.data.table()
  DTname <- lst
  for(i in DTname){
    if ( class(dt[, get(i) ]) == 'character' ) {
      dt[, i ] <- factor(dt[, get(i) ],levels = unique(dt[, get(i) ]))
    }
  }
  DTf <- DTname[length(DTname)]
  DTname <- DTname[1:length(DTname)-1]
  y <- dt[,get(DTf)]
  
  x <- dt[,..DTname] %>% as.data.frame()
  myC50 <- C5.0(x = x,y = y)
  return(myC50)
}

DTreeanalysis_error <- function(dt,lst){
  mydt <- dt %>% as.data.table()
  DTname <- lst
  for(i in DTname){
    if ( class(dt[, get(i) ]) == 'character' ) {
      dt[, i ] <- factor(dt[, get(i) ],levels = unique(dt[, get(i) ]))
    }
  }
  DTf <- DTname[length(DTname)]
  DTname <- DTname[1:length(DTname)-1]
  y <- dt[,get(DTf)]
  
  x <- dt[,..DTname] %>% as.data.frame()
  myC50 <- C5.0(x = x,y = y)
  
  dt <- dt[, Decision_Tree_Predicted := predict(object = myC50, newdata = x)]
  dt <- dt[, Pred_error := as.numeric(get(DTf)) - as.numeric(Decision_Tree_Predicted)]
  dt <- dt[Pred_error == 0, Error_type := "No error"]
  dt <- dt[Pred_error != 0, Error_type := "Error"]
  return(dt)
}



LRegressionanalysis<- function(dt,parameter){
  DTname <- names(dt)
  DTf <- DTname[length(DTname)]
  DTname <- DTname[1:length(DTname)-1]
  
  i <- 1
  while(i <= length(DTname) ){
    dt[[i]] <- if(parameter[i] == 'e') {exp(dt[[i]])
    } else if (parameter[i] == 'log') {log(dt[[i]])
    } else { dt[[i]] ^ as.numeric(parameter[i]) }
    i <- i + 1
  }
  
  DTx <- paste0(DTname,collapse=' + ')
  DTformula <- paste0( c(DTf,DTx) , collapse = ' ~ ') %>% as.formula()
  fit <- lm(DTformula,data = dt)
  summary(fit)
}



