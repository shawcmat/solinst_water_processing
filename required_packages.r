# required_packages.r

# List of required packages
required_packages <- c(
    "dplyr",
    "lubridate",
    "stringr",
    "ggplot2",
    "shiny",
    "shinythemes",
    "plotly",
    "data.table",
    "fs",
    "remotes",
    "readxl",
    "pracma" 
)

install.packages(required_packages)

library(remotes)

remotes::install_github("thomasp85/shinyFiles")
