paquetes <- c("rmarkdown", "flexdashboard", "shiny", "tidyverse", "readxl", "ggrepel", "highcharter", "jsonlite", "factoextra", "gt", "reldist", "ineq", "scales")
faltantes <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltantes)) install.packages(faltantes)
