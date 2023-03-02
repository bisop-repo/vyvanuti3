#### NACITANIE BALICKOV ####

# Mena balickov
packages <- c("readr", "tidyverse", "survival", "gtsummary", "expss", "plotrix", 
              "gt", "forestmodel", "survminer", "webshot2", "ggstats")

# Nainstalovanie doteraz nenainstalovanych balickov 
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

