#### VOLBA PARAMETROV SKRIPTU ####
rm(list = ls())
# args <-  commandArgs(trailingOnly=TRUE)
args <- c("rinput.csv", "SeriousCovidProxy")

# args <- c("Input", "Outcome", "Covariates"), kde: 
# 1. Input: zdrojovej csv soubor 
# 2. Outcome: Infected nebo SeriousCovidProxy nebo LongCovid nebo Hospitalized 
# 3. Covariates: InfPrior + VaccStatus nebo Immunity

#### NACITANIE BALICKOV ####

# Mena balickov
packages <- c("readr", "tidyverse", "survival", "gtsummary", "expss", "plotrix", 
              "gt", "forestmodel", "survminer", "webshot2", "ggstats", "wesanderson", 
              "matlib", "scales", "gdata", "gplots")

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

f <- as.formula(paste("Surv(T1, T2,",args[2],") ~ Immunity + DCCI + AgeGr + Sex "))

#### COXOV MODEL ####
m1_cox <- coxph(f,  data = data)

#### VYTVORENIE TABULKY ####

df <- data.frame(
  beta = coef(m1_cox), # koeficienty coxovho modelu
  beta_CI = confint(m1_cox), # CI ku koeficientom
  HR = exp(coef(m1_cox)), # pomer rizik
  HR_CI   = exp(confint(m1_cox)), # CI k pomeru rizik
  eff = 1 - exp(coef(m1_cox)), # efektivita vakcinacie / imunity ako 1 - HR
  eff_CI = 1 - exp(confint(m1_cox)) # CI k efektivite
)

# names(df)[c(3:4, 6:7)] <- c("HR_CI_lower", "HR_CI_upper","eff_CI_upper", "eff_CI_lower")
names(df)[c(2:3, 5:6, 8:9)] <- c("beta_CI_lower", "beta_CI_upper", 
                                 "HR_CI_lower", "HR_CI_upper",
                                 "eff_CI_upper", "eff_CI_lower")
# df
write.table(df, "cox_model_summary.txt")

#### TREND ####

# Hazard ratios
HR <- exp(coef(m1_cox))

names(HR)

# odstranit levely s XXX+ na konci
odstranit <- c(names(HR)[grep("367+", names(HR))], 
               names(HR)[grep("VaccStatuspartial_062+", names(HR))])

HR <- HR[!names(HR) %in% odstranit]

# Variance matrix
# V_mat <- m1_cox[["var"]]
V_mat <- vcov(m1_cox)
write.table(V_mat, "V_mat.txt")

im_level <- c("Immunityboost", "Immunityfull", "Immunityhybridboost", 
              "Immunityhybridfull", "Immunityinf", "Immunityother",
              "Immunitysecboost", "Immunitysecb45")

eff_tau <- NA
eff_tau_fin <- NA
names_fin <- NA
deltas <- NA
vars_of_deltas <- NA

# names <- names(HR[grep("Immunityother", names(HR))])
for (i in 1 : length(im_level)) {
names <- names(HR[grep(im_level[i], names(HR))])
# names

if (length(names) > 1) {

# series of HR corresponding to a certain source of immunity (subvector of HR)
HR_sub <- HR[names]

# corresponding submatrix of V
V_mat_sub <- V_mat[names, names]

# vyextrahujeme t
cas <- as.numeric(gsub("\\D", "", names))
cas <-  ifelse(cas > 1000, cas %>% substring(nchar(cas) - 2, nchar(cas)) %>% 
  as.numeric() %>% - 30, 
  cas + 30)
# cas

T_mat <- matrix(diag(HR_sub), ncol = length(HR_sub))

# takova jina matice
W_mat <- t(T_mat) %*% V_mat_sub %*% T_mat

X_mat <- matrix(data = c(rep(1, times = length(HR_sub)), cas), ncol = 2)

# GLS estimate
GLS_est <- inv(t(X_mat) %*% inv(W_mat) %*% X_mat) %*% 
  t(X_mat) %*% inv(W_mat) %*% HR_sub

Var_est_mat <- inv(t(X_mat) %*% inv(W_mat) %*% X_mat)

v <- GLS_est[1]
d <- GLS_est[2]
var_of_d <- Var_est_mat[2,2]



# the estimate of the 1 - trend
eff_tau <- NA
for (i in 1 : length(cas)) {
  eff_tau[i] <- 1 - (v + d * cas[i])
}

# eff_tau
names_fin <- append(names_fin, names)
eff_tau_fin <- append(eff_tau_fin, eff_tau)

#convertin to mohths...
deltas <- append(deltas,d * 30.5) 
vars_of_deltas <- append(vars_of_deltas,var_of_d * 30.5 * 30.5)
}
else {
  deltas <- append(deltas,NA) 
  vars_of_deltas <- append(vars_of_deltas,NA)
}

}

immunities <- append(NA,im_level)
est_table <- data.frame(immunities,deltas,vars_of_deltas)[-1,] 
write.table(est_table,'est_table.txt')

df_fin <- data.frame(names_fin, eff_tau_fin)[-1, ] 
write.table(df_fin,'df_fin.txt')


#### COMPARISON OF IMMUNITIES ####
z_score <- NA
z_score_fin <- NA
r_fin <- NA


for (i in 1 : length(im_level)) {
  for (j in 1 : length(im_level)) {
    names_h <- names(HR[grep(im_level[i], names(HR))])
    names_k <- names(HR[grep(im_level[j], names(HR))]) 
    
    names_h_num <- as.numeric(gsub("\\D", "", names_h))
    names_k_num <- as.numeric(gsub("\\D", "", names_k))
    
    sel <- names_h_num[names_h_num %in% names_k_num]
    
    if (!names_h[1] %in% names_k[1]){
      if (length(sel) > 0 & length(sel) < 2){ 
        # pre sel = 1 musime matice brat ako jedno cislo
        names_h <- names_h[names_h_num %in% sel]
        names_k <- names_k[names_k_num %in% sel]
        
        h <- HR[names_h]
        k <- HR[names_k]
        
        V_mat_sub2 <- V_mat[c(names_h, names_k), c(names_h, names_k)]
        
        S_mat <- cbind(h, k)
        
        U_mat <- S_mat %*% V_mat_sub2 %*% t(S_mat)
        
        # n-vector of 1's
        I_n <- rep(1, times = length(h))
        
        #  GLS estimator of rho
        # inverzia z matice 1x1 je prevratena hodnota
        r <- (1 / (I_n %*% (1 / U_mat) %*% I_n)) %*% 
          I_n %*% (1 / U_mat) %*% (h - k)
        
        var_r <- (1 / (I_n %*% (1 / U_mat) %*% I_n)) 
        
        # corresponding z-score
        z_score <- r / sqrt(var_r)

      } else if (length(sel) > 1) {
        names_h <- names_h[names_h_num %in% sel]
        names_k <- names_k[names_k_num %in% sel]
        
        h <- HR[names_h]
        k <- HR[names_k]
        
        V_mat_sub2 <- V_mat[c(names_h, names_k), c(names_h, names_k)]
        
        S_mat <- cbind(matrix(diag(h), ncol = length(h)), 
                       matrix(- diag(k), ncol = length(k)))
 
        U_mat <- S_mat %*% V_mat_sub2 %*% t(S_mat)
        
        # n-vector of 1's
        I_n <- rep(1, times = length(h))
        
        #  GLS estimator of rho
        # inverzia z matice 1x1 je prevratena hodnota
        r <- (1 / (t(I_n) %*% inv(U_mat) %*% I_n)) %*% 
          t(I_n) %*% inv(U_mat) %*% (h - k)
        
        var_r <- (1 / (t(I_n) %*% inv(U_mat) %*% I_n))
        
        # corresponding z-score
        z_score <- r / sqrt(var_r)
      }
    r_fin <- append(r_fin, r)
    z_score_fin <- append(z_score_fin, z_score)
    }
    else
    {
      r_fin <- append(r_fin, NA)
      z_score_fin <- append(z_score_fin, NA)
    }
}
}

r_fin2 <- r_fin[-1]
z_score_fin2 <- z_score_fin[-1]

#r_fin2 <- r_fin[!is.na(r_fin)]
#z_score_fin2 <- z_score_fin[!is.na(z_score_fin)r]

# tady nechápu, proč to tu je
#r_fin2 <- unique(r_fin)
#z_score_fin2 <- unique(z_score_fin)

# toto je brutalne nahodna vec
r_fin3 <- c(NA, r_fin2[1:7], NA, r_fin2[8:14], NA,  r_fin2[15:21], NA,  
            r_fin2[22:28], NA, r_fin2[29:35], NA)
r_fin_mat <- matrix(r_fin3, ncol = 7, byrow = T)
r_fin_mat <- matrix(label_percent()(r_fin_mat), ncol = 7, byrow = T)

z_score_fin3 <- c(NA, z_score_fin2[1:7], NA, z_score_fin2[8:14], NA, 
                  z_score_fin2[15:21], NA, z_score_fin2[22:28], NA, 
                  z_score_fin2[29:35], NA)
z_score_fin_mat <- matrix(z_score_fin3, ncol = 7, byrow = T)

#### HEATMAPA #### 

lowerTriangle(z_score_fin_mat, diag = FALSE, byrow = FALSE) <- NA
lowerTriangle(r_fin_mat, diag = FALSE, byrow = FALSE) <- NA

png(file="heatmap.png")

heatmap.2(z_score_fin_mat, cellnote = r_fin_mat, dendrogram = "none", Rowv = F, 
          Colv = F, notecol="black", 
          trace = "none",
          # key=FALSE, 
          density.info = "none",
          # keysize = 0.25,
          key.title = NULL,
          key.xlab = "Z-score",
          # key.ylab = "",
          margins = c(1, 10),
          srtCol = 270,
          offsetCol = -30, # toto je trochu hruba sila
          labRow = substr(im_level[-6], 9, 20),
          labCol = substr(im_level[-6], 9, 20), 
          # labCol = F,
          col = "terrain.colors")

dev.off()