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

# Install missing packages
for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
    }
}

# Install shinyFiles package from GitHub
if (!require("shinyFiles", character.only = TRUE)) {
    remotes::install_github("thomasp85/shinyFiles")
}
