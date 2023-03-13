#### VOLBA PARAMETROV SKRIPTU ####
rm(list = ls())
args <- c("zzz.csv", "Infected", "InfPrior + VaccStatus")

# args <- c("Input", "Outcome", "Covariates"), kde: 
# 1. Input: zdrojovej csv soubor 
# 2. Outcome: Infected nebo SeriousCovidProxy nebo LongCovid nebo Hospitalized 
# 3. Covariates: InfPrior + VaccStatus nebo Immunity

#### NACITANIE BALICKOV ####

# Mena balickov
packages <- c("readr", "tidyverse", "survival", "gtsummary", "expss", "plotrix", 
              "gt", "forestmodel", "survminer", "webshot2", "ggstats", "wesanderson")

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

data <- read_labelled_csv(args[1])
cox.f <- fread("cox_estimation_formulas.txt", stringsAsFactors = FALSE)
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

#### COXOV MODEL ####
m1_cox <- coxph(f.input,  data = data)

summary(m1_cox)

#### VYTVORENIE TABULKY ####

df <- data.frame(
  beta = coef(m1_cox), # koeficienty coxovho modelu
  HR = exp(coef(m1_cox)), # pomer rizik
  CI   = exp(confint(m1_cox)), # CI k pomeru rizik
  eff = 1 - exp(coef(m1_cox)), # efektivita vakcinacie / imunity ako 1 - HR
  eff_CI = 1 - exp(confint(m1_cox)) # CI k efektivite
)

names(df)[c(3:4, 6:7)] <- c("HR_CI_lower", "HR_CI_upper","eff_CI_upper", "eff_CI_lower")
#### View Nefunguje na serveru v příkazové řádce
#### View(df)

#### GRAF - KRIVKY VYVANUTIA ####

#### GRAF 2 ####
# 2. VaccStatus: -full, -boost, -secboost, -partial

# vyextrahovanie udajov potrebnych pre graf kriviek vyvanutia z tabulky df

cas_full <- dimnames(df)[[1]][grep("VaccStatusfull", dimnames(df)[[1]])] %>% 
  substring(16, 18) %>% as.numeric() %>% + 30
# cas_full

cas_boost <- dimnames(df)[[1]][grep("VaccStatusboost", dimnames(df)[[1]])] %>% 
  substring(17, 19) %>% as.numeric() %>% + 30
# cas_boost

cas_secboost <- dimnames(df)[[1]][grep("VaccStatussecboost", dimnames(df)[[1]])] %>% 
  substring(20, 22) %>% as.numeric() %>% + 30
# cas_secboost

cas_partial <- dimnames(df)[[1]][grep("VaccStatuspartial", dimnames(df)[[1]])] %>% 
  substring(19, 21) %>% as.numeric() %>% + 30
# cas_partial

eff_full <- df$eff[grep("VaccStatusfull", dimnames(df)[[1]])]
eff_boost <- df$eff[grep("VaccStatusboost", dimnames(df)[[1]])]
eff_secboost <- df$eff[grep("VaccStatussecboost", dimnames(df)[[1]])]
eff_partial <- df$eff[grep("VaccStatuspartial", dimnames(df)[[1]])]

eff_CI_lower_full <- df$eff_CI_lower[grep("VaccStatusfull", dimnames(df)[[1]])]
eff_CI_lower_boost <- df$eff_CI_lower[grep("VaccStatusboost", dimnames(df)[[1]])]
eff_CI_lower_secboost <- df$eff_CI_lower[grep("VaccStatussecboost", dimnames(df)[[1]])]
eff_CI_lower_partial <- df$eff_CI_lower[grep("VaccStatuspartial", dimnames(df)[[1]])]

eff_CI_upper_full <- df$eff_CI_upper[grep("VaccStatusfull", dimnames(df)[[1]])]
eff_CI_upper_boost <- df$eff_CI_upper[grep("VaccStatusboost", dimnames(df)[[1]])]
eff_CI_upper_secboost <- df$eff_CI_upper[grep("VaccStatussecboost", dimnames(df)[[1]])]
eff_CI_upper_partial <- df$eff_CI_upper[grep("VaccStatuspartial", dimnames(df)[[1]])]

png(file = "graf_krivky_vyvanutia_VaccStatus.png", width = 800, height = 500)
par(mar = c(3.6, 3.6, 2, 1))
plot(cas_full, eff_full, las = 1, xlim = c(0,1000), ylim = c(0, 1.1), xaxt = "n", 
     xlab = "", ylab = "", type = "n")
axis(1, at=seq(0, 1000, 90), seq(0, 1000, 90))
abline(h = seq(0.2, 1, 0.2), col = "lightgray")

plotCI(cas_full, eff_full, ui = eff_CI_upper_full, li = eff_CI_lower_full, 
       add = T, pch = NA)
plotCI(cas_boost, eff_boost, ui = eff_CI_upper_boost, li = eff_CI_lower_boost, 
       add = T, pch = NA)
plotCI(cas_secboost, eff_secboost, ui = eff_CI_upper_secboost, li = eff_CI_lower_secboost, 
       add = T, pch = NA)
plotCI(cas_partial, eff_partial, ui = eff_CI_upper_partial, li = eff_CI_lower_partial, 
       add = T, pch = NA)

# terrain.colors(5)
plot_color = wes_palette("Zissou1", 4, type = "continuous")
lines(cas_full, eff_full, col = "#3B9AB2", type = "b", pch = 15, lwd = 2)
lines(cas_boost, eff_boost, col = "#9EBE91", type = "b", pch = 16, lwd = 2)
lines(cas_secboost, eff_secboost, col = "#E4B80E", type = "b", pch = 17, lwd = 2)
lines(cas_partial, eff_partial, col = "#F21A00", type = "b", pch = 18, lwd = 2)

mtext("Time from vaccination [days]", side = 1, line = 2.5) 
mtext("Vaccine effectiveness", side = 2, line = 2.5) 

legend("topright", legend = c("Full vaccination", "Booster", "Second booster", "Partial vaccination"), 
       col = plot_color,
       lty = 1, pch = c(15, 16, 17, 18), lwd = 2, bty = "n")

dev.off()

#### GRAF 3 ####
# 3. InfPrior: inf_NA 

cas_NA <- dimnames(df)[[1]][grep("InfPriorinf_NA", dimnames(df)[[1]])] %>% 
  substring(16, 18) %>% as.numeric() %>% + 30 # + 15??
# cas_NA

eff_NA <- df$eff[grep("InfPriorinf_NA", dimnames(df)[[1]])]

eff_CI_lower_NA <- df$eff_CI_lower[grep("InfPriorinf_NA", dimnames(df)[[1]])]

eff_CI_upper_NA <- df$eff_CI_upper[grep("InfPriorinf_NA", dimnames(df)[[1]])]

# graf / krivky vyvanutia 
png(file = "graf_krivky_vyvanutia_InfPriorinf_NA.png", width = 800, height = 500)
par(mar = c(3.6, 3.6, 2, 1))
plot(cas_NA, eff_NA, las = 1, xlim = c(0, 1000), ylim = c(0, 1.1), xaxt = "n", 
     xlab = "", ylab = "", type = "n")
axis(1, at = seq(0, 1000, 90), seq(0, 1000, 90))
abline(h = seq(0.2, 1, 0.2), col = "lightgray")

plotCI(cas_NA, eff_NA, ui = eff_CI_upper_NA, li = eff_CI_lower_NA, 
       add = T, pch = NA)

plot_color = wes_palette("Zissou1", 1, type = "continuous")
lines(cas_NA, eff_NA, col = plot_color, type = "b", pch = 15, lwd = 2)

mtext("Time from infection [days]", side = 1, line = 2.5) 
mtext("Effectiveness", side = 2, line = 2.5) 

legend("topright", legend = c("InfPriorinf_NA"), # ?? 
       col = plot_color,
       lty = 1, pch = 15, lwd = 2, bty = "n")

dev.off()

#### TABULKY - HR a VE ####

# HR - pomer rizik
m1_cox_HR_plot <- tbl_regression(m1_cox, exponentiate = T) %>% 
  bold_p() %>% bold_labels() %>% 
  add_n(location = "level") %>% 
  add_nevent(location = "level")
m1_cox_HR_plot

gt::gtsave(as_gt(m1_cox_HR_plot), file = "m1_cox_HR_plot.png")

# Forest plot - pomer rizik
forest_plot <- m1_cox_HR_plot %>%
  plot()

png(file = "forest_plot.png", width = 700, height = 900)
forest_plot

# dev.copy(device = png, filename = 'forest_plot.png', width = 700, height = 900)
dev.off()

# VE - vakcinacna efektivita

if (f.input.covariates == "InfPrior + VaccStatus") {
  include <- c("InfPrior", "VaccStatus") 
  } else if (f.input.covariates == "Immunity") {
    include <- f.input.covariates
  } else {include <- NA
    }

m1_cox_VE_plot <- tbl_regression(m1_cox, include = include, 
                                 estimate_fun = function(x) {round(1 - exp(x) , 2)}, 
                                 conf.int = F) %>% 
  bold_p() %>% bold_labels() %>% 
  add_n(location = "level") %>%
  add_nevent(location = "level") %>%
  modify_header(estimate ~ "**VE (95% CI)**") %>% 
  modify_footnote(estimate ~ "Vaccine Effectiveness, CI = Confidence Interval", 
                  abbreviation = TRUE)
m1_cox_VE_plot

gt::gtsave(as_gt(m1_cox_VE_plot), file = "m1_cox_VE_plot.png")

# ggforest(m1_cox, data = data, fontsize = 1)
# gsave("m1_cox_risks_log.jpg", units = "px", width = 3840, height = 3840)