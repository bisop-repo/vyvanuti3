#### VOLBA PARAMETROV SKRIPTU ####
rm(list = ls())
args <-  commandArgs(trailingOnly=TRUE)
# args <- c("zzz.csv", "Infected", "InfPrior + VaccStatus")

# args <- c("Input", "Outcome", "Covariates"), kde: 
# 1. Input: zdrojovej csv soubor 
# 2. Outcome: Infected nebo SeriousCovidProxy nebo LongCovid nebo Hospitalized 
# 3. Covariates: InfPrior + VaccStatus nebo Immunity

#### NACITANIE BALICKOV ####

# Mena balickov
packages <- c("readr", "tidyverse", "survival", "gtsummary", "expss", "plotrix", 
              "gt", "forestmodel", "survminer", "webshot2", "ggstats", "wesanderson", 
              "matlib")

# Nainstalovanie doteraz nenainstalovanych balickov 
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Nacitnie balickov
invisible(lapply(packages, library, character.only = TRUE))

# Nastavenie temy pre tabulky z balicku gtsummary
# theme_gtsummary_journal("jama")

#### NACITANIE DAT A POPISNA STATISTIKA ####

data <- read_labelled_csv(args[1])
# cox.f <- fread("cox_estimation_formulas.txt", stringsAsFactors = FALSE)
cox.f <- data.frame(Outcome = c("Infected", "Infected", "SeriousCovidProxy", "SeriousCovidProxy", 
                                "LongCovid", "LongCovid", "Hospitalized", "Hospitalized"), 
                    Covariates = c(rep(c("InfPrior + VaccStatus", "Immunity"), times = 4)), 
                    Eq = c("Surv(T1, T2, Infected) ~ InfPrior + VaccStatus + AgeGr + Sex", 
                           "Surv(T1, T2, Infected) ~ Immunity + AgeGr + Sex", 
                           "Surv(T1, T2, SeriousCovidProxy) ~ InfPrior + VaccStatus + AgeGr + Sex", 
                           "Surv(T1, T2, SeriousCovidProxy) ~ Immunity + AgeGr + Sex", 
                           "Surv(T1, T2, LongCovid) ~ InfPrior + VaccStatus + AgeGr + Sex", 
                           "Surv(T1, T2, LongCovid) ~ Immunity + AgeGr + Sex", 
                           "Surv(T1, T2, Hospitalized) ~ InfPrior + VaccStatus + AgeGr + Sex", 
                           "Surv(T1, T2, Hospitalized) ~ Immunity + AgeGr + Sex"))

f.input.outcome <- args[2]
f.input.covariates <- args[3]

f.input <- cox.f[cox.f$Outcome == f.input.outcome 
                 & cox.f$Covariates == f.input.covariates, 3] %>% 
  as.character() %>% 
  noquote() %>% 
  as.formula()
# f.input

# head(data)
# names(data)
# str(data)
# summary(data)

#### COXOV MODEL ####
m1_cox <- coxph(f.input,  data = data)

# summary(m1_cox)

#### TREND ####

# Hazard ratios
HR <- exp(coef(m1_cox))

names(HR)

# toto by sa malo dako zautomatizovat ale neviem ako
# zatial si tam proste rucne men dake tie oné :D
names <- names(HR[grep("VaccStatusboost", names(HR))])
names

# series of HR corresponding to a certain source of immunity (subvector of HR)
HR_sub <- HR[names]

# Variance matrix
# V_mat <- m1_cox[["var"]]
V_mat <- vcov(m1_cox)

# corresponding submatrix of V
V_mat_sub <- V_mat[names, names]

# vyextrahujeme t
cas <- as.numeric(gsub("\\D", "", names))
cas <-  ifelse(cas > 1000, cas %>% substring(nchar(cas) - 2, nchar(cas)) %>% 
  as.numeric() %>% - 30, 
  cas + 30)
cas

T_mat <- matrix(diag(HR_sub), ncol = length(HR_sub))

# takova jina matice
W_mat <- t(T_mat) %*% V_mat_sub %*% T_mat

X_mat <- matrix(data = c(rep(1, times = length(HR_sub)), cas), ncol = 2)

# GLS estimate
GLS_est <- inv(t(X_mat) %*% inv(W_mat) %*% X_mat) %*% 
  t(X_mat) %*% inv(W_mat) %*% HR_sub

v <- GLS_est[1]
d <- GLS_est[2]
v
d

# the estimate of the 1 - trend
eff_tau <- NA
for (i in 1 : length(cas)) {
  eff_tau[i] <- 1 - (v + d * cas[i])
}
eff_tau