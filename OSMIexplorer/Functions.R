#load packages
library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)
library(rstanarm)
library(sortable)
library(htmlwidgets)
library(formattable)
library(readr)
library(FactoMineR)
library(missMDA)
library(rstanarm)
library(maps)
library(treemap)
library(d3treeR)
library(htmlwidgets)

#read in data and make column names one word
df2019 <- read_csv("data/OSMI Mental Health in Tech Survey 2019.csv")
column_info <- read_csv("config/Column groupings.csv")
column_info <- column_info %>% mutate_all(as.character)

#process columns
full_names <- colnames(df2019)
colnames(df2019) <- column_info$Short_name
topics <- list()
types <- list()
for (i in 1:nrow(column_info)) {
  col_it <- column_info[i,"Short_name"][[1]]
  topic_it <- column_info[i,"Topic"][[1]]
  type_it <- column_info[i,"Type"][[1]]
  if (type_it %in% names(types)) {
    types[[type_it]] <- c(types[[type_it]],col_it)
  } else {
    types[[type_it]] <- col_it
  }
  if (topic_it %in% names(topics)) {
    topics[[topic_it]] <- c(topics[[topic_it]],col_it)
  } else {
    topics[[topic_it]] <- col_it
  }
}

#create dataframes for analysis
## qualitative
comments <- df2019 %>% select(any_of(types[["Comment"]]))
qualitative <- df2019 %>%
  select(-any_of(types[["Comment"]]))%>% 
  mutate_if(is.logical, as.character)
qualitative[qualitative=="TRUE"] <- "Yes"
qualitative[qualitative=="FALSE"] <- "No"


##quantitative
non_quant_vars <- c(types[["Comment"]], types[["Group"]], types[["Geography_world"]], types[["Geography_US"]])
quantitative <- df2019 %>%
  mutate_all(as.character)

valuesubs <- -1:10
values_in_use <- list()
test <- 0
for (i in 1:nrow(column_info)) {
  colname <- column_info[i,2][[1]]
  
  if (colname %in% non_quant_vars) {next}
  if (colname %in% types[["Binary"]]) {next}
  
  column_data <- quantitative[colname]

  replace_condition_prior <- ifelse(is.na(quantitative[,colname]),F,T)
  replaced_condition <- rep(F,nrow(quantitative[,colname]))

  for (j in 6:ncol(column_info)-1) {
    find <-column_info[[i,j]]
    if (is.na(find)==F) {
      replace <-as.integer(valuesubs[j-4])
      if (colname %in% names(values_in_use)) {
        values_in_use[[colname]] <- unique(c(values_in_use[[colname]],as.numeric(replace)))
      } else {
        values_in_use[[colname]] <- as.numeric(replace)
      }
      replace_condition_with_nas <- as.vector(sapply(column_data[[1]], function(i) str_detect(i,find)))
      replace_condition <- ifelse(is.na(replace_condition_with_nas), F, replace_condition_with_nas)
      print(replace_condition)
      quantitative[replace_condition,colname] <- as.character(replace)
      replaced_condition <- replaced_condition | replace_condition
      test <- test + 1
    }
  }
  if (is.null(values_in_use[[colname]])) {next}
  
  replace <- as.character(mean(values_in_use[[colname]]))
  quantitative[!replaced_condition,colname] <- as.character(replace)
}

quantitative <- quantitative %>% 
  select(-all_of(non_quant_vars)) %>% 
  mutate_all(type.convert)

#charging logic
charting_options <- c("scatterplot", "boxplot", "violin", "heatmap", "bar", "us_map", "global_map")
charting_functions <- list()


charting_functions[["scatterplot"]] <- function(x,y,group=NULL) {
  #build data frame
  df<- cbind(quantitative[,x],quantitative[,y])
  colnames(df) <- c(x,y)
  if (is.null(group) == F) {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nas
  df <- df[complete.cases(df),]
  
  #build ggplot
  gg <- ggplot(df)
  if (is.null(group) == F) {
    gg <- gg + geom_point(aes(x=!!sym(x), y = !!sym(y), color = !!sym(group)))
  } else {
    gg <- gg + geom_point(aes(x=!!sym(x), y = !!sym(y)))
  }
  
  gg <- gg + geom_smooth()
  
  # build plotly and return
  ggplotly(gg) %>% return(.)
}

charting_functions[["boxplot"]] <- function(x,y,group=NULL) {
  #build data frame
  df<- cbind(qualitative[,x],quantitative[,y])
  colnames(df) <- c(x,y)
  if (is.null(group) == F) {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nas
  df <- df[complete.cases(df),]
  
  #build ggplot
  gg <- ggplot(df)
  if (is.null(group) == F) {
    gg <- gg + geom_boxplot(aes(x=!!sym(x), y = !!sym(y))) + facet_wrap(vars(!!sym(group)))
  } else {
    gg <- gg + geom_boxplot(aes(x=!!sym(x), y = !!sym(y)))
  }
  
  # build plotly and return
  ggplotly(gg) %>% return(.)
}

charting_functions[["boxplot"]] <- function(x,y,group=NULL) {
  #build data frame
  df<- cbind(qualitative[,x],quantitative[,y])
  colnames(df) <- c(x,y)
  if (is.null(group) == F) {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nas
  df <- df[complete.cases(df),]
  
  #build ggplot
  gg <- ggplot(df)
  if (is.null(group) == F) {
    gg <- gg + geom_boxplot(aes(x=!!sym(x), y = !!sym(y), fill = !!sym(x))) + facet_wrap(vars(!!sym(group)))
  } else {
    gg <- gg + geom_boxplot(aes(x=!!sym(x), y = !!sym(y)))
  }
  
  # build plotly and return
  ggplotly(gg) %>%
    layout(showlegend = FALSE) %>% 
    return(.)
}

charting_functions[["violin"]] <- function(x,y,group=NULL) {
  #build data frame
  df<- cbind(qualitative[,x],quantitative[,y])
  colnames(df) <- c(x,y)
  if (is.null(group) == F) {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nas
  df <- df[complete.cases(df),]
  
  #build ggplot
  gg <- ggplot(df)
  if (is.null(group) == F) {
    gg <- gg + geom_violin(aes(x=!!sym(x), y = !!sym(y), fill = !!sym(x))) + facet_wrap(vars(!!sym(group)))
  } else {
    gg <- gg + geom_violin(aes(x=!!sym(x), y = !!sym(y)))
  }
  
  # build plotly and return
  ggplotly(gg) %>%
    layout(showlegend = FALSE) %>% 
    return(.)
}

charting_functions[["bar"]] <- function(x,y,group=NULL) {
  #build data frame
  df<- cbind(qualitative[,x],quantitative[,y])
  colnames(df) <- c(x,y)
  if (is.null(group) == F) {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nas
  df <- df[complete.cases(df),]
  
  #build ggplot
  gg <- ggplot(df)
  if (is.null(group) == F) {
    gg <- gg + geom_bar(aes(x=!!sym(x), y = !!sym(y), fill = !!sym(group)))
  } else {
    gg <- gg + geom_bar(aes(x=!!sym(x), y = !!sym(y)))
  }
  
  # build plotly and return
  ggplotly(gg) %>% return(.)
}

charting_functions[["treemap"]] <- function(x,y=NULL,group=NULL) {
  #build data frame
  df<- cbind(qualitative[,x],quantitative[,y])
  colnames(df) <- c(x,y)
  if (is.null(group) == F) {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nasand set count dummy
  df <- df[complete.cases(df),]
  df[,"val"] <- 1
  
  
  #build treemap
  if (is.null(y) == F & is.null(group) == F) {
    df %>% 
      group_by(!!sym(x),!!sym(group)) %>% 
      summarize(mean_res= mean(!!sym(y))) -> summed
    min <- min(summed$mean_res)
    max <- max(summed$mean_res)
    mid <- mean(min,max)
    
    d3tree3(
      treemap(
        df,
        index = c(x,group),
        vSize = "val",
        vColor = y,
        type = "value",
        fun.aggregate = "mean",
        mapping = c(min, mid,max),
      ),rootname = x
    ) 
  } else if (is.null(y) == F) {
    df %>% 
      group_by(!!sym(x)) %>% 
      summarize(mean_res= mean(!!sym(y))) -> summed
    min <- min(summed$mean_res)
    max <- max(summed$mean_res)
    mid <- mean(min,max)
    
    d3tree3(
      tree <- treemap(
        df,
        index = c(x),
        vSize = "val",
        vColor = y,
        type = "value",
        fun.aggregate = "mean",
        mapping = c(min, mid,max),
      ), rootname = x
    )
  } else if (is.null(group) == F) {
    d3tree3(
      tree <- treemap(
        df,
        index = c(x,group),
        vSize = "val",
        type = "index"
      ), rootname = x
    )

  } else  {
    d3tree3(
      tree <- treemap(
        df,
        index = c(x),
        vSize = "val",
        type = "index"
      ), rootname = x
    )
  }
  
  # build interactive treemap and return
    d3tree2(tree, rootname = "world")
}
