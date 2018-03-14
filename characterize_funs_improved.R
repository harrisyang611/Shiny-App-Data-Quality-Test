# A set of functions to characterize quality of data set.

library(knitr)
library(ggplot2)
library(stats)
library(base)
library(scales)
library(dplyr)

characterize.data <- function(dt.in, vars.invalid.char.test = NULL, vars.special.char.test = NULL) {
  for (k in (1:ncol(dt.in))) {
    
    if (any(class(dt.in) == "data.table")) { 
      test.var <- (dt.in[, k, with = FALSE])  #this case is when dt.in is a data.table
      test.var <- test.var[[1]] #hack. we need to fix this elegantly
    } else {
      test.var <- dt.in[, k] #in this case, dt.in is a data.frame only.
    }
    
    name.var <- names(dt.in)[k]
    str.header <- sprintf("\n##%s\n", name.var)
    writeLines(str.header)
    test.invalid.char <- name.var %in% vars.invalid.char.test
    test.special.char <- name.var %in% vars.special.char.test
    characterize.variable(test.var, test.invalid.char, test.special.char)
    
  }
}


characterize.variable <- function(var.in, test.invalid.char = FALSE, test.special.char = FALSE) {
  var.type <- class(var.in)
  str.out <- sprintf("\nVariable Class: %s", var.type)
  writeLines(str.out)
  
  if ( ((class(var.in) == "character") && (all(var.in == ""))) | (all(is.na(var.in))) ){
    writeLines("\nAll values are NULL")
    write.general(char.list, var.in, var.type)
  }
  
  else {
    char.list <- switch(var.type, 
                        "character" = characterize.character(var.in, test.invalid.char, test.special.char),
                        "numeric"   = characterize.numeric(var.in), 
                        "factor"    = characterize.factor(var.in), 
                        "Date"      = characterize.date(var.in),
                        "logical"   = characterize.boolean(var.in),
                        "default"   = writeLines("\nI do not know how to characterize this.\n"))
    
    write.general(char.list, var.in, var.type)

    switch(var.type,  "character" = write.character(char.list), 
                       "numeric"   = write.numeric(char.list),
                       "factor"    = write.factor(char.list),
                       "Date"      = write.date(char.list),
                       "logical"   = write.boolean(char.list),
                       "default"   = writeLines("\nI do not know how to characterize this.\n"))
  } 
}

write.general <- function (char.list, var.in, var.type){
  writeLines("\n###Observation Count\n")
  df.gen <- characterize.general(var.in)
  tab.general <- sprintf(paste("\nNumber of observations: %s\n",
                               "\nNumber of distinct observations: %s (%0.3f%%)\n",
                               "\nMissing values (NA): %s (%0.3f%%)\n", sep = ""),
                         format(df.gen$N.Obs, big.mark = ","),
                         format(df.gen$N.Distinct.Obs, big.mark = ","), 100*df.gen$N.Distinct.Obs/df.gen$N.Obs,
                         format(df.gen$N.NAs, big.mark = ","), 100*df.gen$N.NAs/df.gen$N.Obs)
  writeLines(tab.general)
  
  if (var.type %in% c("character", "factor")) {
    tab.blanks <- sprintf("\nBlank values: %s (%0.3f%%)\n", 
                          format(df.gen$N.Blanks, big.mark = ","), 
                          100*df.gen$N.Blanks/df.gen$N.Obs)
    writeLines(tab.blanks)
  }
  
}

write.character <- function(char.list) {
  writeLines("\n###String Length\n") 
  writeLines("\nWe present here summary statistics for the length of each string in this character variable.\n")
  tab.stats   <- kable(char.list[["Stats"]], 
                       digits = 1, 
                       format.args = list(big.mark = ","), 
                       row.names = FALSE)
  writeLines(tab.stats)
  
  writeLines("\n###Frequent values\n")
  tab.freq <- kable(char.list[["Freq"]],
                    row.names = FALSE,
                    format.args = list(big.mark = ","))
  writeLines(tab.freq)
  
 writeLines("\n###Invalid Strings\n")

  if(is.null(char.list[["Invalid"]])){
    writeLines("We didn't check for Invalid String in this column.")
  }
  else if(nrow(char.list[["Invalid"]]) == 0){
    writeLines("There are no invalid Strings.")
  }
  else {
    tab.invalid <- kable(char.list[["Invalid"]],
                         row.names = FALSE,
                         format.args = list(big.mark = ","))
    writeLines(tab.invalid)
  }

  writeLines("\n###Special Characters\n")

  if(is.null(char.list[["Special"]])){
    writeLines("We didn't check for Special Character in this column.")
  }
  else if(nrow(char.list[["Special"]]) == 0){
    writeLines("There are no special character in the Strings.")
  }
  else {
    tab.special <- kable(char.list[["Special"]],
                   row.names = FALSE,
                   format.args = list(big.mark = ","))
    writeLines(tab.special)
 }
}

write.numeric <-  function(char.list) {
  writeLines("\n###Descriptive Statistics\n")  
  tab.stats   <- kable(char.list[["Stats"]], 
                       digits = 3, 
                       format.args = list(big.mark = ","), 
                       row.names = FALSE)
  writeLines(tab.stats)
  
  writeLines("\n###Outliers\n")
  tab.outliers   <- kable(char.list[["Outliers"]], 
                          digits = 4, 
                          format.args = list(big.mark = ","), 
                          row.names = FALSE)
  
  writeLines("\nOutliers are defined in the [traditional (Tukey) sense](https://en.wikipedia.org/wiki/Outlier#Tukey.27s_test). \n")
  writeLines(tab.outliers)
  writeLines("\n")
}

write.factor <- function(char.list) {
  writeLines("\n###Frequent values\n")
  tab.freq <- kable(char.list[["Freq"]], 
                    row.names = FALSE, 
                    format.args = list(big.mark = ","))
  writeLines(tab.freq)
}

write.date <- function(char.list){
  writeLines("\n###Descriptive Statistics\n")  
  # tab.stats   <- kable(char.list[["Stats"]], 
  #                      digits = 3, 
  #                      format.args = list(big.mark = ","), 
  #                      row.names = FALSE)
  # writeLines(tab.stats)
  # 
  # writeLines("\n\n")
  
  tab.freq <- kable(char.list[["Range"]], 
                    row.names = FALSE, 
                    format.args = list(big.mark = ","))
  writeLines(tab.freq)
  
  writeLines("\n####Oldest Dates\n")
  tab.top <- kable(char.list[["Top"]], 
                   row.names = FALSE, 
                   format.args = list(big.mark = ","))
  writeLines(tab.top)
  
  writeLines("\n####Recent Dates\n")
  tab.bottom <- kable(char.list[["Bottom"]], 
                      row.names = FALSE, 
                      format.args = list(big.mark = ","))
  writeLines(tab.bottom)
}

characterize.general <- function(var.in) {
  df.general <- data.frame(N.Obs = length(var.in), 
                           N.Distinct.Obs = length(unique(var.in)),
                           N.NAs  = sum(is.na(var.in)))
  if(class(var.in) %in% c("character", "factor")) {
    df.general$N.Blanks <- sum(grepl("^[:blank:]*$", var.in))
  }
  return(df.general)
}

characterize.character <- function(var.in, test.invalid.char, test.special.char) {
  df.general <- characterize.general(var.in)
  df.stats <- descriptive.stats(nchar(var.in))
  df.freq  <- stats.frequency(var.in)
  
  if (test.invalid.char == TRUE) {
    df.invalid <- invalid.character(var.in)  
  } else {
    df.invalid <- NULL
  }
    
  if(test.special.char == TRUE){
    df.special <- special.character(var.in) 
  } else{
    df.special <- NULL
  }
  
  list.out <- list("General" = df.general, 
                   "Stats" = df.stats, 
                   "Freq"  = df.freq,
                   "Invalid" = df.invalid,
                   "Special" = df.special
                   )
  
  return(list.out)
}

stats.frequency <- function(var.in){
  df.stats <- data.frame(table(var.in))
  if (nrow(df.stats) > 0) {
    colnames(df.stats) <- c("Value", "Frequency")
    df.stats <- df.stats[order(-df.stats$Frequency),]
    if(nrow(df.stats) > 20) {
      df.stats <- rbind(df.stats[1:20, ], 
                        data.frame(Value = "(Others)", 
                                   Frequency = sum(df.stats$Frequency[21:nrow(df.stats)])))
    }
  }
  return(df.stats)
}

descriptive.stats <- function(var.in){
  df.descstats <- data.frame(Min = min(var.in, na.rm = TRUE), 
                             Q10 = quantile(var.in, 1/10, na.rm= TRUE),
                             Q25 = quantile(var.in, 1/4, na.rm= TRUE),
                             Median = median(var.in, na.rm = TRUE),
                             Mean = mean(var.in, na.rm = TRUE),
                             Q75 = quantile(var.in, 3/4, na.rm= TRUE),
                             Q90 = quantile(var.in, 9/10, na.rm= TRUE),
                             Max = max(var.in, na.rm = TRUE), 
                             IQR = IQR(var.in, na.rm = TRUE),
                             Std.Dev = sd(var.in, na.rm = TRUE)
  )
  return(df.descstats)
}

invalid.character<- function(var.in){
  invalid.char <- data.frame(table(var.in))
  invalid.char <- invalid.char[grepl(pattern = "([[:alnum:]])\\1{2}", invalid.char$var.in),]
  names(invalid.char) <-c("Invalid.Names", "Freq")
  return(invalid.char)
}

special.character <- function(var.in){
  special.char <-data.frame(table(var.in))
  punct.pattern <- c('-', '\\.', '!', '\\"', '#', '\\$', '\\%', '\\&', '\'', '\\(', '\\)', '\\*', '\\+', ',', '\\:', '\\/', ';', '<', '\\=', '>', '\\?', '@', '\\[', '\\]', '\\^', '_', '`', '\\{',  '\\}', '~', '[|]', '[\\]')
  punct.pattern <- paste0(punct.pattern, collapse = "|")
  special.char<- special.char[grepl(pattern = punct.pattern, special.char$var.in),]
  names(special.char) <-c("Special.Names", "Freq")
  return(special.char)
}

characterize.numeric <- function(var.in) {
  df.general <- characterize.general(var.in)
  df.stats <- descriptive.stats(var.in)
  #Identify outliers
  thres.up <-  min(max(var.in, na.rm = TRUE),
                   quantile(var.in, 0.75, na.rm = TRUE) + 1.5 * IQR(var.in, na.rm = TRUE), na.rm = TRUE)
  thres.dn <- max(min(var.in, na.rm = TRUE),
                  quantile(var.in, 0.25, na.rm = TRUE) - 1.5 * IQR(var.in, na.rm = TRUE), na.rm = TRUE)
  
  above.thres <- var.in > thres.up
  below.thres <- var.in < thres.dn
  
  idx.outlier <- var.in > thres.up | var.in < thres.dn
  
  Positive.Infs <- is.infinite(var.in > 0)
  Negative.Infs <- is.infinite(var.in < 0)
  
  df.outliers <- data.frame(N.Total.Outliers = sum(idx.outlier, na.rm = TRUE),
                            N.Outliers.Above.Thres = sum(above.thres, na.rm = TRUE),
                            N.Outliers.Below.Thres = sum(below.thres, na.rm = TRUE),
                            N.INFs  = sum(is.infinite(var.in)),
                            N.INFs.Positive = sum(Positive.Infs),
                            N.INFs.Negative = sum(Negative.Infs))
  list.out <- list("General" = df.general, 
                   "Stats" = df.stats, 
                   "Outliers" = df.outliers) 
  
  return(list.out)
}

characterize.factor <- function(var.in) {
  df.general <- characterize.general(var.in)
  df.freq  <- stats.frequency(var.in)
  
  list.out <- list("General" = df.general,
                   "Freq"  = df.freq)
  return(list.out)
  
}

characterize.date <- function(var.in) {
  df.general <- characterize.general(var.in)
  df.ddte <- describe.dates(var.in)
  list.top.bottom <- ft.lt.dates(var.in)
  
  list.out <- list("General" = df.general, "Range"= df.ddte, 
                   "Top" = list.top.bottom[["top.dates"]],
                   "Bottom" = list.top.bottom[["bottom.dates"]])
  return(list.out)
} 

describe.dates <- function(var.in){
  df.descstats <- data.frame(Min = min(var.in, na.rm = TRUE), 
                             Mean = mean(var.in, na.rm = TRUE),
                             Max = max(var.in, na.rm = TRUE), 
                             Std.Dev = sd(var.in, na.rm = TRUE)
  )
  return(df.descstats)
  
}

ft.lt.dates <- function(var.in){
  df.freq <- data.frame(table(var.in))
  df.freq <- df.freq[order(df.freq$var.in),]

  df.head <- head(df.freq,20)
  df.tail <- tail(df.freq,20)
  
  names(df.head) <- c("Top Date", "Freq")
  names(df.tail) <- c("Bottom Date", "Freq")
  
  list.out <- list(top.dates = df.head, bottom.dates = df.tail)
  return(list.out)
}

characterize.boolean <- function(var.in){
  df.general <- characterize.general(var.in)
  # df.stats <- stats.frequency(var.in)
  list.out <- list("General" = df.general)
  return(list.out)
}

