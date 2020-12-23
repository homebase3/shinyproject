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
library(leaflet)
library(rgdal)

#read in data and make column names one word
df2019 <- read_csv("data/OSMI Mental Health in Tech Survey 2019.csv")
column_info <- read_csv("config/Column groupings.csv")
column_info <- column_info %>% mutate_all(as.character)
states  <- readOGR(dsn = paste0(getwd(),"/data/states.json"))
world <- readOGR(dsn = paste0(getwd(),"/data/world.json"))


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
  if (is.na(values_in_use[[colname]])) {next}
  
  replace <- as.character(mean(values_in_use[[colname]]))
  quantitative[!replaced_condition,colname] <- as.character(replace)
}

quantitative <- quantitative %>% 
  select(-all_of(non_quant_vars)) %>% 
  mutate_all(type.convert)

#charging logic
charting_options <- c("scatterplot", "boxplot", "violin", "heatmap", "bar", "us_map", "global_map")
charting_functions <- list()

charting_functions[["scatterplot"]] <- function(x,y,group=NA) {
  #build data frame
  df<- cbind(quantitative[,x],quantitative[,y])
  colnames(df) <- c(x,y)
  if (is.na(group) == F) {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nas
  df <- df[complete.cases(df),]
  
  #build ggplot
  gg <- ggplot(df)
  if (is.na(group) == F) {
    gg <- gg + geom_point(aes(x=!!sym(x), y = !!sym(y), color = !!sym(group)))
  } else {
    gg <- gg + geom_point(aes(x=!!sym(x), y = !!sym(y)))
  }
  
  gg <- gg + geom_smooth()
  
  # build plotly and return
  ggplotly(gg) %>% return(.)
}

charting_functions[["boxplot"]] <- function(x,y,group=NA) {
  #build data frame
  df<- cbind(qualitative[,x],quantitative[,y])
  colnames(df) <- c(x,y)
  if (is.na(group) == F) {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nas
  df <- df[complete.cases(df),]
  
  #build ggplot
  gg <- ggplot(df)
  if (is.na(group) == F) {
    gg <- gg + geom_boxplot(aes(x=!!sym(x), y = !!sym(y), fill = !!sym(x))) + facet_wrap(vars(!!sym(group)))
  } else {
    gg <- gg + geom_boxplot(aes(x=!!sym(x), y = !!sym(y)))
  }
  
  # build plotly and return
  ggplotly(gg) %>%
    layout(showlegend = FALSE) %>% 
    return(.)
}

charting_functions[["violin"]] <- function(x,y,group=NA) {
  #build data frame
  df<- cbind(qualitative[,x],quantitative[,y])
  colnames(df) <- c(x,y)
  if (is.na(group) == F) {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nas
  df <- df[complete.cases(df),]
  
  #build ggplot
  gg <- ggplot(df)
  if (is.na(group) == F) {
    gg <- gg + geom_violin(aes(x=!!sym(x), y = !!sym(y), fill = !!sym(x))) + facet_wrap(vars(!!sym(group)))
  } else {
    gg <- gg + geom_violin(aes(x=!!sym(x), y = !!sym(y)))
  }
  
  # build plotly and return
  ggplotly(gg) %>%
    layout(showlegend = FALSE) %>% 
    return(.)
}

charting_functions[["bar"]] <- function(x,y=NA,group=NA) {
  #build data frame
  df<- cbind(qualitative[,x],quantitative[,y])
  colnames(df) <- c(x,y)
  if (is.na(group) == F) {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nas
  df <- df[complete.cases(df),]
  
  
  #build ggplot
  if (is.na(group) == F & is.na(y) == F) {
    gg <- ggplot(df) + geom_bar(aes(x=!!sym(x), y = !!sym(y), fill = !!sym(group))) + coord_flip()
  } else if (is.na(y) == F) {
    gg <- ggplot(df) + geom_bar(aes(x=!!sym(x), y = !!sym(y))) + coord_flip()
  } else if (is.na(group) == F) {
    gg <- swwww43w
  } else {
    gg <- ggplot(df) + geom_bar(aes(x=!!sym(x))) + coord_flip()
  }
  
  # build plotly and return
  ggplotly(gg) %>% return(.)
}

charting_functions[["treemap"]] <- function(x,y=NA,group=NA) {
  #build data frame
  df<- cbind(qualitative[,x],quantitative[,y])
  colnames(df) <- c(x,y)
  if (is.na(group) == F) {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nasand set count dummy
  df <- df[complete.cases(df),]
  df[,"val"] <- 1
  
  
  #build treemap
  if (is.na(y) == F & is.na(group) == F) {
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
  } else if (is.na(y) == F) {
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
  } else if (is.na(group) == F) {
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

charting_functions[["us_map"]] <- function(x,y=NA) {
  
  #build data frame
  df<- cbind(qualitative[,x])
  colnames(df) <- x
  if (is.na(group) == F) {
    df<- cbind(df,quantitative[,y])
    colnames(df)[2] <- y
  }
  #remove nas
  df <- df[complete.cases(df),]
  
  #build leaflet
  if (is.na(y) == F) {
    df %>% 
      group_by(!!sym(x)) %>% 
      summarize(mean_y = mean(!!sym(y))) -> summed
    colnames(summed) <- colnames(df)
    
    states_it <- states
    
    states_it@data %>% 
      left_join(summed, by = c("NAME" = x)) -> states_it@data
    
    mypalette <- colorNumeric(palette="Purples", domain=states_it@data[[y]], na.color="transparent")
    
    mytext <- paste(
      "Country: ", states_it@data$NAME,"<br/>", 
      paste0(y,": "), round(states_it@data[[y]],2), "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    leaflet(states_it) %>% 
      addTiles() %>% 
      setView(lat = 39.8333333, lng = -98.585522,zoom =3) %>% 
      addPolygons(fillColor = ~mypalette(states_it@data[[y]]), stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions( 
                     style = list("font-weight" = "normal", padding = "3px 8px"), 
                     textsize = "13px", 
                     direction = "auto")) %>% 
      addLegend(pal = mypalette, values = states_it@data[[y]],opacity=0.9,position = "bottomleft") %>% 
      return(.)
    
  } else {
    df %>% 
      group_by(!!sym(x)) %>% 
      summarize(count = n()) -> summed
    
    states_it <- states
    
    states_it@data %>% 
      left_join(summed, by = c("NAME" = x)) -> states_it@data
    
    mypalette <- colorNumeric(palette="Purples", domain=states_it@data$count, na.color="transparent")
    
    mytext <- paste(
      "Country: ", states_it@data$NAME,"<br/>", 
      "Count: ", states_it@data$count, "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    leaflet(states_it) %>% 
      addTiles() %>% 
      setView(lat = 39.8333333, lng = -98.585522,zoom =3) %>% 
      addPolygons(fillColor = ~mypalette(states_it@data$count), stroke=FALSE,
                  label = mytext,
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto")) %>% 
      addLegend(pal = mypalette, values = ~count,opacity=0.9,position = "bottomleft") %>% 
      return(.)
  }
}

charting_functions[["world_map"]] <- function(x,y=NA) {
  
  #build data frame
  df<- cbind(qualitative[,x])
  colnames(df) <- x
  if (is.na(group) == F) {
    df<- cbind(df,quantitative[,y])
    colnames(df)[2] <- y
  }
  #remove nas
  df <- df[complete.cases(df),]
  
  #build leaflet
  if (is.na(y) == F) {
    df %>% 
      group_by(!!sym(x)) %>% 
      summarize(mean_y = mean(!!sym(y))) -> summed
    colnames(summed) <- colnames(df)
    
    world_it <- world
    
    world_it@data %>% 
      left_join(summed, by = c("name_sort" = x)) -> world_it@data
    
    mypalette <- colorNumeric(palette="Purples", domain=world_it@data[[y]], na.color="transparent")
    
    mytext <- paste(
      "Country: ", world_it@data$name_sort,"<br/>", 
      paste0(y,": "), round(world_it@data[[y]],2), "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    leaflet(world_it) %>% 
      addTiles() %>% 
      addPolygons(fillColor = ~mypalette(world_it@data[[y]]), stroke=FALSE,
                  label = mytext,
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto")) %>% 
      addLegend(pal = mypalette, values = world_it@data[[y]],opacity=0.9,position = "bottomleft") %>% 
      return(.)
    
  } else {
    df %>% 
      group_by(!!sym(x)) %>% 
      summarize(count = n()) -> summed
    
    world_it <- world
    
    world_it@data %>% 
      left_join(summed, by = c("name_sort" = x)) -> world_it@data
    
    mypalette <- colorNumeric(palette="Purples", domain=world_it@data$count, na.color="transparent")
    
    mytext <- paste(
      "Country: ", world_it@data$name_sort,"<br/>", 
      "Count: ", world_it@data$count, "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    leaflet(world_it) %>% 
      addTiles() %>% 
      addPolygons(fillColor = ~mypalette(states_it@data$count), stroke=FALSE,
                  label = mytext,
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto")) %>% 
      addLegend(pal = mypalette, values = ~count,opacity=0.9,position = "bottomleft") %>% 
      return(.)
  }
}
