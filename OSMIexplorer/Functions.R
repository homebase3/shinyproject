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

#read in data
df2016 <- read_csv("data/mental-heath-in-tech-2016_20161114.csv")