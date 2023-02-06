#### NACITANIE BALICKOV ####

# Mena balickov
packages <- c("readr", "tidyverse", "survival", "gtsummary", "expss", "plotrix", 
              "gt", "forestmodel")

# Nainstalovanie doteraz nenainstalovanych balickov 

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Nacitnie balickov
invisible(lapply(packages, library, character.only = TRUE))

# Nastavenie temy pre tabulky z balicku gtsummary
theme_gtsummary_journal("jama")

#### NACITANIE DAT A POPISNA STATISTIKA ####

rm(list = ls())
data <- read_labelled_csv("Feb_delta_omicron.csv")

head(data)
names(data)
str(data)
summary(data)

# vytvorenie identifikátoru unikátnych pacientov 
data <- 
  data %>%
  group_by(ID) %>%
  mutate(first.ID = row_number() == 1L, 
         .before = 1L) %>%
  ungroup()

# tabulka s popisnymi statistikami
sum_tab <- data[, !names(data) %in% "ID"] %>%
  tbl_summary(
    label = first.ID ~ "Number of unique patients",
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     first.ID ~ "{n}"),
    missing_text = "(Missing Observations)",
    percent = c("cell") 
  ) %>% bold_labels()
sum_tab

gt::gtsave(as_gt(sum_tab), file = "sum_tab.png")

#### LOGISTICKY REGRESNY MODEL ####

m1_logreg <- glm(Covidproxy ~ VariantComp + InfPrior + VaccStatus + AgeGr + Sex,
                 family = binomial(link = "logit"), data = data)
summary(m1_logreg)

# vypocet OR(odds ratio)
OR <- exp(coef(m1_logreg))
OR

OR[2]

#### FOREST PLOT ####

#forest_model(m1_logreg)
forest_model(m1_logreg, covariates = "VariantComp")
ggsave("m1_logreg_forest_plot.jpg", units = "px", width = 3840, height = 3840)

#### TABULKY - OR a VE ####

# OR - odds ratio
m1_logreg_OR_plot <- tbl_regression(m1_logreg, exponentiate = T) %>% 
  bold_p()  %>%  bold_labels() %>% 
  add_n(location = "level") #%>% 
  #add_nevent(location = "level")
m1_logreg_OR_plot

gt::gtsave(as_gt(m1_logreg_OR_plot), file = "m1_logreg_OR_plot.png")
