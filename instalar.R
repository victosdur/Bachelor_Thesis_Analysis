paquetes <- c("shiny", "bslib", "tidyverse", "readxl", "ggrepel", "highcharter", "jsonlite", "factoextra", "gt", "reldist", "ineq", "scales", "rsconnect")
install.packages(setdiff(paquetes, rownames(installed.packages())))
