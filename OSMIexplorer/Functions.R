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

#read in data and make column names one word
df2019 <- read_csv("data/OSMI Mental Health in Tech Survey 2019.csv")
column_info <- read_csv("Column groupings.csv")
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
    find <- 
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

#remove NAs
test %>% na_rem






y <- "Are you self-employed"
x <- "What is your race?"
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
x <- stan_glm(`*Areyouselfemployed?*` ~ `Whatisyourrace?` , data = df2019, family = binomial(link = "logit"),prior = t_prior, prior_intercept = t_prior,cores = 2, seed = 12345)
