rm(list = ls())

# args <-  commandArgs(trailingOnly=TRUE)
  args <- c("rinput.csv", "DeadByOther")

  packages <- c("readr", "tidyverse", "survival", "gtsummary", "expss", "plotrix", 
              "gt", "forestmodel", "survminer", "webshot2", "ggstats", "wesanderson", 
              "matlib", "scales", "gdata", "gplots", "xtable", "R.utils")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))

data <- read_labelled_csv(args[1])


f <- as.formula(paste("Surv(T1, T2,", args[2],") ~ VaccStatus + Sex + strata(DCCI) "))
m1 <- coxph(f,  data = data)

# vytvorenie identifikátoru unikátnych pacientov 


df <- data.frame(
  beta = coef(m1), # koeficienty coxovho modelu
  beta_CI = confint(m1), # CI ku koeficientom
  HR = exp(coef(m1)), # pomer rizik
  HR_CI   = exp(confint(m1)), # CI k pomeru rizik
  eff = 1 - exp(coef(m1)), # efektivita vakcinacie / imunity ako 1 - HR
  eff_CI = 1 - exp(confint(m1)) # CI k efektivite
)

names(df)[c(2:3, 5:6, 8:9)] <- c("lower", "upper", 
                                 "lower", "upper",
                                 "upper", "lower")
write.table(df, "cox_model_summary.txt")

table <- xtable(df, caption = "Cox model summary")
print(table, file = "cox_model_summary.tex", include.rownames = TRUE)

m1_cox_HR_plot <- tbl_regression(m1, exponentiate = T)# %>% 
forest_plot <- m1_cox_HR_plot %>%
  plot()

forest_plot
ggsave("forest_plot.png", width=4)



