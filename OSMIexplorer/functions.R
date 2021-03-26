#shiny helpers
lookup_type <- function(var) {
  column_info[column_info$Short_name == var,"Type"] %>% 
    as.character(.) %>% 
    return(.)
}

chart_options <- function(x_var, y_var) {
  tryCatch(
    expr = {
      x_type <- lookup_type(x_var)
      if (y_var == "None") {
        chart_map[chart_map[[paste0("X_",x_type)]] == 1,1] %>% 
          pull(.) %>% 
          return(.)
      } else {
        y_type <- lookup_type(y_var)
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

render_chart <- function(obj, charttype) {
  tryCatch(
    expr = {
      if (charttype %in% c("scatterplot","boxplot","violin","bar")) {
        renderPlotly({obj})
      } else if (charttype == "heatmap") {
        renderD3tree3({obj})
      } else {
        renderLeaflet({obj})
      }
    },
    error = function(e) {
      renderUI("")
    }
  )
}

output_chart <- function(name, charttype) {
  tryCatch(
    expr = {
      if (charttype %in% c("scatterplot","boxplot","violin","bar")) {
        plotlyOutput(name)
      } else if (charttype == "heatmap") {
        d3tree3Output(name)
      } else {
        leafletOutput(name)
      }
    },
    error = function(e) {
      uiOutput("blank")
    }
    
  )

}



#charging logic
charting_options <- c("scatterplot", "boxplot", "violin", "bar")
charting_functions <- list()

charting_functions[["scatterplot"]] <- function(x,y,group="None") {
  #build data frame
  df<- cbind(quantitative[,x],quantitative[,y])
  colnames(df) <- c(x,y)
  if (group != "None") {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nas
  df <- df %>% drop_na(.)
  
  #build ggplot
  if (group != "None") {
    gg <- ggplot(df,aes(x=!!sym(x), y = !!sym(y), color = !!sym(group))) + geom_point() + geom_smooth()
  } else {
    gg <- ggplot(df,aes(x=!!sym(x), y = !!sym(y))) + geom_point() + geom_smooth()
  }

  
  # build plotly and return
  ggplotly(gg) %>% return(.)
}

charting_functions[["boxplot"]] <- function(x,y,group="None") {
  #build data frame
  df<- cbind(qualitative[,x],quantitative[,y])
  colnames(df) <- c(x,y)
  if (group != "None") {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nas
  df <- df %>% drop_na(.)
  
  #build ggplot
  gg <- ggplot(df)
  if (group != "None") {
    gg <- gg + geom_boxplot(aes(x=!!sym(x), y = !!sym(y), fill = !!sym(x))) + facet_wrap(vars(!!sym(group)))
  } else {
    gg <- gg + geom_boxplot(aes(x=!!sym(x), y = !!sym(y)))
  }
  
  # build plotly and return
  ggplotly(gg) %>%
    layout(showlegend = FALSE) %>% 
    return(.)
}

charting_functions[["violin"]] <- function(x,y,group="None") {
  #build data frame
  df<- cbind(qualitative[,x],quantitative[,y])
  colnames(df) <- c(x,y)
  if (group %in% c("None",x,y) == F) {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nas
  df <- df %>% drop_na(.)
  
  #build ggplot
  gg <- ggplot(df)
  if (group != "None") {
    gg <- gg + geom_violin(aes(x=!!sym(x), y = !!sym(y), fill = !!sym(x))) + facet_wrap(vars(!!sym(group)))
  } else {
    gg <- gg + geom_violin(aes(x=!!sym(x), y = !!sym(y)))
  }
  
  # build plotly and return
  ggplotly(gg) %>%
    layout(showlegend = FALSE) %>% 
    return(.)
}

charting_functions[["bar"]] <- function(x,y="None",group="None") {
  #build data frame
  df<- cbind(qualitative[,x])
  
  if (y != "None") {
    df <- cbind(df,  quantitative[,y])
    colnames(df) <- c(x,y)
  } else{
    df <- cbind(df,  quantitative[,x])
    colnames(df)[2]  <- paste0("Mean_",x)
  }
  

  if (group != "None") {
    df<- cbind(df,qualitative[,group])
    colnames(df)[3] <- group 
  }
  
  #remove nas
  df <- df %>% drop_na(.)
  
  
  #build ggplot
  if (group != "None" & y != "None") {
    gg <- ggplot(df) + stat_summary(aes(x=!!sym(x), y = !!sym(y), fill = !!sym(group)),fun = "mean", geom = "bar",position=position_dodge()) + ylab(paste0("Mean: ",y)) + coord_flip()
  } else if (y != "None") {
    gg <- ggplot(df) + stat_summary(aes(x=!!sym(x), y = !!sym(y)),fun = "mean", geom = "bar") + ylab(paste0("Mean: ",y)) + coord_flip()
  } else if (group != "None") {
    gg <- ggplot(df) + geom_bar(aes(x=!!sym(x),  fill = !!sym(group))) + coord_flip()
  } else {
    gg <- ggplot(df) + geom_bar(aes(x=!!sym(x))) + coord_flip()
  }
  
  # build plotly and return
  ggplotly(gg) %>% return(.)
}

charting_functions[["heatmap"]] <- function(x,y="None",group="None") {
  #build data frame
  df<- cbind(qualitative[,x])
  if (y != "None") {
    df <- cbind(df,quantitative[,y])
    colnames(df) <- c(x,y)
  }
  if (group != "None") {
    df<- cbind(df,qualitative[,group])
    colnames(df)[ncol(df)] <- group 
  }
  
  #remove nasand set count dummy
  df <- df %>% drop_na(.)
  df[,"val"] <- 1
  
  
  #build treemap
  if (y != "None" & group != "None") {
    df %>% 
      group_by(!!sym(x),!!sym(group)) %>% 
      summarize(mean_res= mean(!!sym(y))) -> summed
    min <- min(summed$mean_res)
    max <- max(summed$mean_res)
    mid <- mean(min,max)
    
    tm <- treemap(
        df,
        index = c(x,group),
        vSize = "val",
        vColor = y,
        type = "value",
        fun.aggregate = "mean",
        mapping = c(min, mid,max),
        draw = T,
        title.legend = paste0("Mean: ",y),
        format.legend = list(scientific = FALSE, big.mark = " ")
      )
  } else if (y != "None") {
    df %>% 
      group_by(!!sym(x)) %>% 
      summarize(mean_res= mean(!!sym(y))) -> summed
    min <- min(summed$mean_res)
    max <- max(summed$mean_res)
    mid <- mean(min,max)
    
    tm <-treemap(
        df,
        index = c(x),
        vSize = "val",
        vColor = y,
        type = "value",
        fun.aggregate = "mean",
        mapping = c(min, mid,max)
      )
  } else if (group != "None") {
     tm <-  treemap(
        df,
        index = c(x,group),
        vSize = "val",
        type = "index"
     )
  } else  {
    tm <- treemap(
        df,
        index = c(x),
        vSize = "val",
        type = "index"
      )
  }
  d3tree3(tm,rootname = x)
}

charting_functions[["us_map"]] <- function(x,y="None") {
  
  #build data frame
  df<- cbind(qualitative[,x])
  colnames(df) <- x
  if (y != "None") {
    df<- cbind(df,quantitative[,y])
    colnames(df)[2] <- y
  }
  #remove nas
  df <- df %>% drop_na(.)
  
  #build leaflet
  if (y != "None") {
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
      addLegend(pal = mypalette, title = "mean", values = states_it@data[[y]],opacity=0.9,position = "bottomleft") %>% 
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

charting_functions[["world_map"]] <- function(x,y="None") {
  
  #build data frame
  df<- cbind(qualitative[,x])
  colnames(df) <- x
  if (y != "None") {
    df<- cbind(df,quantitative[,y])
    colnames(df)[2] <- y
  }
  #remove nas
  df <- df %>% drop_na(.)
  
  #build leaflet
  if (y != "None") {
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
      addLegend(pal = mypalette, title = "mean", values = world_it@data[[y]],opacity=0.9,position = "bottomleft") %>% 
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
      addPolygons(fillColor = ~mypalette(world_it@data$count), stroke=FALSE,
                  label = mytext,
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto")) %>% 
      addLegend(pal = mypalette, values = ~count,opacity=0.9,position = "bottomleft") %>% 
      return(.)
  }
}


charting_wrapper <-function(type, x, y, group) {
  tryCatch(
    expr = {
      if (type %in% c("us_map","world_map")) {
        return(charting_functions[[type]](x,y))
      } else {
        return(charting_functions[[type]](x,y,group))
      }
    },
    error = function(e) {
      gg <- ggplot(mtcars) + aes(x=cyl, y = wt) + geom_blank() + xlab("") + ylab("")
      return(ggplotly(gg))
    }
  )
}
