#### NACITANIE BALICKOV ####

# Mena balickov
packages <- c("readr", "tidyverse", "survival", "gtsummary", "expss", "plotrix", 
              "gt", "forestmodel", "survminer", "webshot2", "ggstats")

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
data <- read_labelled_csv("zzz.csv")

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

#### COXOV MODEL ####

m1_cox <- coxph(Surv(T1, T2, LongCovid) ~ InfPrior + VaccStatus + AgeGr + Sex, 
                data = data)
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

# vyextrahovanie udajov potrebnych pre graf kriviek vyvanutia z tabulky df
cas_infekce <- dimnames(df)[[1]][grep("InfPrior", dimnames(df)[[1]])] %>% 
  substring(16, 18) %>% as.numeric() %>% + 30
cas_infekce

cas_vakcinace <- dimnames(df)[[1]][grep("VaccStatusfull", dimnames(df)[[1]])] %>% 
  substring(16, 18) %>% as.numeric() %>% + 30
cas_vakcinace

eff_infekce <- df$eff[grep("InfPrior", dimnames(df)[[1]])]
eff_vakcinace <- df$eff[grep("VaccStatusfull", dimnames(df)[[1]])]

eff_CI_lower_infekce <- df$eff_CI_lower[grep("InfPrior", dimnames(df)[[1]])]
eff_CI_lower_vakcinace <- df$eff_CI_lower[grep("VaccStatusfull", dimnames(df)[[1]])]

eff_CI_upper_infekce <- df$eff_CI_upper[grep("InfPrior", dimnames(df)[[1]])]
eff_CI_upper_vakcinace <- df$eff_CI_upper[grep("VaccStatusfull", dimnames(df)[[1]])]

# graf / krivky vyvanutia 

par(mar = c(3.6, 3.6, 2, 1))
plot(cas_infekce, eff_infekce, las = 1, xlim = c(0, 360), ylim = c(0, 1.1), xaxt = "n", 
     xlab = "", ylab = "", type = "n")
axis(1, at=seq(0, 360, 90), seq(0, 360, 90))
abline(h = seq(0.2, 1, 0.2), col = "lightgray")

plotCI(cas_infekce, eff_infekce, ui = eff_CI_upper_infekce, li = eff_CI_lower_infekce, 
       add = T, pch = NA)
plotCI(cas_vakcinace, eff_vakcinace, ui = eff_CI_upper_vakcinace, li = eff_CI_lower_vakcinace, 
       add = T, pch = NA)

lines(c(30, cas_infekce), c(1, eff_infekce), col = "darkblue", type = "b", pch = 15, lwd = 2)
lines(cas_vakcinace, eff_vakcinace, col = "darkred", type = "b", pch = 19, lwd = 2)

mtext("Time from infection/vaccination [days]", side = 1, line = 2.5) 
mtext("Effectiveness of immunity", side = 2, line = 2.5) 
#title("Křivky vyvanutí")

legend("bottomleft", legend = c("Infection", "Full vaccination"), col = c("darkblue", "darkred"), 
       lty = 1, pch = c(15, 19), lwd = 2, bty = "n")

dev.copy(device = png, filename = 'graf_krivky_vyvanutia.png', width = 800, height = 500)
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
forest_plot

dev.copy(device = png, filename = 'forest_plot.png', width = 700, height = 900)
dev.off()

# VE - vakcinacna efektivita
m1_cox_VE_plot <- tbl_regression(m1_cox, include = c("InfPrior", "VaccStatus"), 
                                 estimate_fun = function(x) {round(1 - exp(x), 2)}) %>% 
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
