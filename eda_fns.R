library(ggplot2)
library(gridExtra)
library(tm)
library(wordcloud)



sort.factors <- function(factor.in, decreasing = TRUE){
  #This function sorts the levels of factors by occurrance
  df.tab <- data.frame(table(factor.in))
  df.tab <- df.tab[order(df.tab$Freq, decreasing = decreasing), ]
  factor.out <- factor(as.character(factor.in), levels = as.character(df.tab$factor.in))
  return(factor.out)
}

sorted.bar.plot <- function(df.in, sort.field, class.field, top.n = NULL){
  #Sorted bar plot, colors are hardcoded
  df.tab <- data.frame(table(df.in[, sort.field]))
  df.tab <- df.tab[order(df.tab$Freq, decreasing = TRUE), ]
  df.in[, sort.field] <- factor(as.character(df.in[, sort.field]), 
                                levels = as.character(df.tab$Var1))

  if (is.null(top.n)){
    df.plt <- df.in
  } else {
    df.plt <- df.in[df.in[, sort.field] %in% df.tab$Var1[1:top.n], ]
  }
  
  plt.out <- ggplot(data = df.plt, aes_string(x = sort.field, fill = class.field)) +
    geom_bar(stat = "count") +
    coord_flip()
  return(plt.out)
}

sorted.bar.plot.3 <- function(df.in, sort.field, class.field, top.n = NULL){
  plt.out <- sorted.bar.plot(df.in, sort.field, class.field, top.n) 
  plt.out <- plt.out + 
    scale_fill_manual(values = c("red2", "gold1", "royalblue3")) 
  return(plt.out)
}

sorted.bar.plot.5 <- function(df.in, sort.field, class.field, top.n = NULL){
  plt.out <- sorted.bar.plot(df.in, sort.field, class.field, top.n) 
  plt.out <- plt.out + 
    scale_fill_manual(values = c("red2", "orange","gold1", "royalblue2", "royalblue4")) 
  return(plt.out)
}

cross.tab.plot <- function(df.in, cat.x, cat.y, margins = TRUE){
  df.tab <- data.frame(table(df.in[ ,c(cat.x, cat.y)]))
  plt.heatmap <- ggplot(df.tab, aes_string(x = cat.x, y = cat.y)) +
    geom_tile(aes(fill = Freq)) +
    geom_text(aes(fill = Freq, label = Freq)) + 
    scale_fill_gradient(low = "white", high = "red") +
    guides(fill = FALSE)
  
  if (margins) {
    plt.bar.x <- ggplot(df.in) + 
      geom_bar(aes_string(x = cat.x), fill = "red4") 
    plt.bar.y <- ggplot(df.in) +
      geom_bar(aes_string(x = cat.y), fill = "red4") +
      coord_flip() 
    plt.out <- grid.arrange(grobs = list(plt.heatmap, plt.bar.x, plt.bar.y),
                            layout_matrix = matrix(c(2, NA, 1, 3), byrow = TRUE, ncol = 2))
    
  } else{
    plt.out <- plt.heatmap
  }
  return(plt.out)
  
}

time.elapsed.plot.3 <- function(df.in, start.fld, end.fld, y.fld, class.fld, 
                                as.of.date = NULL){
  if (is.null(as.of.date)){
    as.of.date <- max(df.in[, end.fld], na.rm = TRUE)
  }
  idx <- is.na(df.in[, end.fld])
  df.in[idx, end.fld] <- as.of.date
  plt <- ggplot(df.in) + 
    geom_errorbarh(aes_string(y = y.fld, x = start.fld, 
                              xmin = start.fld, xmax = end.fld,
                              colour = class.fld)) +
    scale_y_discrete(labels = NULL) +
    scale_colour_manual(values = c("red2", "gold1", "royalblue3"))
  return(plt)
}

time.elapsed.plot.5 <- function(df.in, start.fld, end.fld, y.fld, class.fld, 
                                as.of.date = NULL){
  if (is.null(as.of.date)){
    as.of.date <- max(df.in[, end.fld], na.rm = TRUE)
  }
  idx <- is.na(df.in[, end.fld])
  df.in[idx, end.fld] <- as.of.date
  plt <- ggplot(df.in) + 
    geom_errorbarh(aes_string(y = y.fld, x = start.fld, 
                              xmin = start.fld, xmax = end.fld,
                              colour = class.fld)) +
    scale_y_discrete(labels = NULL) +
    scale_colour_manual(values = c("red2", "springgreen4","gold1", "royalblue2", "royalblue4"))
  return(plt)
}


desc.wordcloud <- function(str.in, word.scale = 100) {
  corp.in <- VCorpus(VectorSource(str.in), readerControl = list(language = "eng"))
  corp.in <- tm_map(corp.in, stripWhitespace)
  corp.in <- tm_map(corp.in, removePunctuation)
  corp.in <- tm_map(corp.in, content_transformer(tolower))
  corp.in <- tm_map(corp.in, removeWords, stopwords("english"))
  corp.in <- tm_map(corp.in, stemDocument)
  wordcloud(corp.in, min.freq = round(length(str.in)/word.scale))
}

summary.time <- function(num.in) { 
  df.out <- data.frame(Count = sum(!is.na(num.in)),
                       Min = min(num.in, na.rm = TRUE), 
                       Q25 = quantile(num.in, 0.25, na.rm = TRUE), 
                       Median = median(num.in, na.rm = TRUE), 
                       Average = mean(num.in, na.rm = TRUE), 
                       Q75 = quantile(num.in, 0.75, na.rm = TRUE), 
                       Max = max(num.in, na.rm = TRUE), 
                       IQR = IQR(num.in, na.rm = TRUE))
  return(df.out)
}


sorted.heat.map <- function(df.in, sort.field, heat.field, top.n = NULL)
{
  df.dept <- data.frame(table(df.in[, sort.field]))
  df.dept <- df.dept[order(-df.dept$Freq), ]
  
  df.tab <- data.frame(table(df.in[, c(heat.field, sort.field)], 
                             exclude = df.dept$Var1[(top.n+1):nrow(df.dept)]))
  df.tab[,sort.field] <- factor(as.character(df.tab[,sort.field]), 
                                      levels = df.dept$Var1[1:top.n])
  plt <- ggplot(df.tab) + 
    geom_tile(aes_string(x = heat.field, y = sort.field, fill = "Freq")) + 
    geom_text(aes_string(x = heat.field, y = sort.field, label = "Freq"), size = 3)+
    scale_fill_gradient(low = "white", high = "orangered4") 
  return(plt)
}