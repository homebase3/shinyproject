#variable lookup
lookup_type <- function(var) {
  column_info[column_info$Short_name == var,"Type"] %>% 
    as.character(.) %>% 
    return(.)
}

chart_options <- function(x_var, y_var) {
  tryCatch(
    expr = {
      x_type <- lookup_type(x_var)
      y_type <- lookup_type(y_var)
      if (is.na(y_type)) {
        chart_map[chart_map[[paste0("X_",x_type)]] == 1,1] %>% 
          pull(.) %>% 
          return(.)
      } else {
        chart_map[chart_map[[paste0("X_",x_type)]] == 1 & chart_map[[paste0("Y_",y_type)]] == 1,1] %>% 
          pull(.) %>% 
          return(.)
      }
    },
    error = function(e) {
      return("")
    }
  )
}

  


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
