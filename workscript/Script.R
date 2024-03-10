#### VOLBA PARAMETROV SKRIPTU ####
rm(list = ls())

args <-  commandArgs(trailingOnly=TRUE)
# args <- c("rinput.csv", "SeriousCovidProxy")
# args <- c("LCInf.csv", "LCINF")
# 1. Input: zdrojovej csv soubor 
# 2. Outcome: Infected nebo SeriousCovidProxy nebo LongCovid nebo Hospitalized 
# 3. Covariates: InfPrior + VaccStatus nebo Immunity
#### VOLBA PARAMETROV SKRIPTU ####

#### NACITANIE BALICKOV ####

# Mena balickov
packages <- c("readr", "tidyverse", "survival", "gtsummary", "expss", "plotrix", 
              "gt", "forestmodel", "survminer", "webshot2", "ggstats", "wesanderson", 
              "matlib", "scales", "gdata", "gplots", "xtable")

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

#### COXOV MODEL ####


if (args[2]=="LCINF") {
  m1  <- glm(LongCovid ~ Immunity + DCCI + AgeGr + Sex,
                   family = binomial(link = "logit"), data = data)
  summary(m1)
  
  sumstat <- c("LongCovid","Immunity", "Agegr", "DCCI", "Sex")
} else  {
  f <- as.formula(paste("Surv(T1, T2,",args[2],") ~ Immunity + DCCI + AgeGr + Sex "))
  m1 <- coxph(f,  data = data)
  sumstat <- c(args[2],"Immunity", "Agegr", "Sex")
}


# vytvorenie identifikátoru unikátnych pacientov 
data <- 
  data %>%
  group_by(ID) %>%
  mutate(first.ID = row_number() == 1L, 
         .before = 1L) %>%
  ungroup()



# tabulka s popisnymi statistikami

# TODO: tady potřebuju aby se zobrazovaly jen aktuální proměnné
# odkomentoval jsem něco, nevím, jak to sesouladit


sum_tab <- data[, !names(data) %in% "ID"] %>%
  tbl_summary(
#    label = first.ID ~ "Number of unique patients",
    include = any_of(sumstat),
#    statistic = list(all_categorical() ~ "{n} ({p}%)",
#   first.ID ~ "{n}"),
    missing_text = "(Missing Observations)",
    percent = c("cell")
  ) %>% bold_labels()
sum_tab

gt::gtsave(as_gt(sum_tab), file = "sum_tab.png")



#### VYTVORENIE TABULKY ####

if (args[2]!="LCINF")
{
df <- data.frame(
  beta = coef(m1), # koeficienty coxovho modelu
  beta_CI = confint(m1), # CI ku koeficientom
  HR = exp(coef(m1)), # pomer rizik
  HR_CI   = exp(confint(m1)), # CI k pomeru rizik
  eff = 1 - exp(coef(m1)), # efektivita vakcinacie / imunity ako 1 - HR
  eff_CI = 1 - exp(confint(m1)) # CI k efektivite
)

# names(df)[c(3:4, 6:7)] <- c("HR_CI_lower", "HR_CI_upper","eff_CI_upper", "eff_CI_lower")
names(df)[c(2:3, 5:6, 8:9)] <- c("lower", "upper", 
                                 "lower", "upper",
                                 "upper", "lower")
# df
write.table(df, "cox_model_summary.txt")

table <- xtable(df, caption = "Cox model summary")
print(table, file = "cox_model_summary.tex", include.rownames = TRUE)

# forest plot - pomer rizik

m1_cox_HR_plot <- tbl_regression(m1, exponentiate = T)# %>% 
forest_plot <- m1_cox_HR_plot %>%
  plot()

#png(file = "forest_plot.png", width = 700, height = 900)
forest_plot
ggsave("forest_plot.png", width=4)
# dev.copy(device = png, filename = 'forest_plot.png', width = 700, height = 900)
#dev.off()




# dev.copy(device = png, filename = 'forest_plot.png', width = 700, height = 900)
} else {

  txttable <- summary(m1)

  sink("logreg_model_summary.txt")
  print(txttable)
  sink()
    
  textable <- xtable(summary(m1), caption = "Logistic regression summary")
  print(textable, file = "logreg_model_summary.tex", include.rownames = TRUE)
  

}


#### TREND ####

# Hazard ratios
HR <- exp(coef(m1))

names(HR)

# odstranit levely s XXX+ na konci
# odstranit levely s XXX+ na konci
# MŠ: tohle z nějakého důvodu vyhodilo i Immunityfull_367-427 a Immunityinf_367-427
# odstranit <- c(names(HR)[grep("367+", names(HR))], 
#               names(HR)[grep("VaccStatuspartial_062+", names(HR))])
odstranit <- c(names(HR)[grep("550", names(HR))], 
               names(HR)[grep("VaccStatuspartial_062+", names(HR))])
HR <- HR[!names(HR) %in% odstranit]

# Variance matrix
# V_mat <- m1_cox[["var"]]
V_mat <- vcov(m1)
write.table(V_mat, "V_mat.txt")

# M: tady jsem přidal secb45 a přeházel jsem to
im_level <- c("Immunityfull","Immunityboost",
              "Immunitysecboost", "Immunitysecbnew",
              "Immunityinf",
              "Immunityhybridfull", "Immunityhybridboost"
)

eff_tau <- NA
eff_tau_fin <- NA
names_fin <- NA
deltas <- NA
lower_of_deltas <- NA
upper_of_deltas <- NA

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
    
    #MS
    cas <- cas / 30.5
    
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
    delta <- d 
    deltas <- append(deltas,delta) 
    sd_of_delta <- sqrt(var_of_d) 
    lower_of_deltas <- append(lower_of_deltas,delta - sd_of_delta * 1.96)
    upper_of_deltas <- append(upper_of_deltas,delta + sd_of_delta * 1.96)
  }
  else {
    deltas <- append(deltas,NA) 
    lower_of_deltas <- append(lower_of_deltas,NA)
    upper_of_deltas <- append(upper_of_deltas,NA)
  }
  
}


immunities <- append(NA,im_level)
# TODO tady bych potřeboval přejménovat záhlaví na $\Delta$ (nebo aspoň Delta), Lower, Upper
est_table <- data.frame(immunities,deltas,lower_of_deltas,upper_of_deltas)[-1,] 
write.table(est_table,'est_table.txt')

textable = xtable(est_table, caption="Monthly percentage decrease of effectiveness/protection")
print(textable, file = "est_table.tex", include.rownames = TRUE)


df_fin <- data.frame(names_fin, eff_tau_fin)[-1, ] 
write.table(df_fin,'df_fin.txt')

textable = xtable(df_fin)
print(textable, file = "df_fin.tex", include.rownames = TRUE)


### OU: introducing the comparison perioed
short_t <- 3
short_fin <- NA
short_z_score_fin <- NA

long_t <- 6
long_fin <- NA
long_z_score_fin <- NA


#### COMPARISON OF IMMUNITIES ####
z_score <- NA
z_score_fin <- NA
r_fin <- NA

#OU: starting from zeros instead of 1
for (i in 0 : length(im_level)) {
  for (j in 0 : length(im_level)) {
    
    short_delta_t <- NA
    short_z_score <- NA
    long_delta_t <- NA
    long_z_score <- NA
    
    if(i > 0 & j > 0) {
      r <- NA
      z_score <- NA
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
          
          #M: tady chyběl mínus                
          #        S_mat <- cbind(h, k)
          S_mat <- cbind(h,-k)
          U_mat <- S_mat %*% V_mat_sub2 %*% t(S_mat)
          
          # n-vector of 1's
          I_n <- rep(1, times = length(h))
          
          #  GLS estimator of rho
          # inverzia z matice 1x1 je prevratena hodnota
          # M: tadyto se vykrátí
          # r <- (1 / (I_n %*% (1 / U_mat) %*% I_n)) %*% 
          #  I_n %*% (1 / U_mat) %*% (h - k)
          
          r = h - k;
          
          var_r <- (1 / (I_n %*% (1 / U_mat) %*% I_n)) 
          # corresponding z-score
          z_score <- r / sqrt(var_r)

          t <-  as.numeric(gsub("\\D", "", names_h_num[names_h_num %in% sel]))
          t <-  ifelse(t > 1000, t %>% substring(nchar(t) - 2, nchar(t)) %>% 
                           as.numeric() %>% - 30, t + 30) / 30.5

          if(abs(short_t - t) < 1.1) {
            short_delta_t <- r
            short_z_score = z_score
          }
          if(abs(long_t - t) < 1.1)
          {
            long_delta_t <- r
            long_z_score = z_score
          }
          
          # OU no sense in comparion when there is only a single point
        } 
        else if (length(sel) > 1) {
          
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
          
          #OU srovnani s trendem
          cas <- as.numeric(gsub("\\D", "", names_h))
          cas <-  ifelse(cas > 1000, cas %>% substring(nchar(cas) - 2, nchar(cas)) %>% 
                           as.numeric() %>% - 30, 
                         cas + 30)

          
          cas <- cas / 30.5
          Z_mat <- matrix(data = c(rep(1, times = length(names_h)), cas), ncol = 2)

          GLS_est <- inv(t(Z_mat) %*% inv(U_mat) %*% Z_mat) %*% 
            t(Z_mat) %*% inv(U_mat) %*% (h - k)
          
          Var_est_mat <- inv(t(Z_mat) %*% inv(U_mat) %*% Z_mat)
          
          s <- GLS_est[1]
          a <- GLS_est[2]
          
          short_delta_t <- s + a * short_t
          short_delta_var = t(c(1,short_t)) %*% Var_est_mat %*% c(1,short_t)
          short_z_score = short_delta_t / sqrt(short_delta_var)
        
          if(long_t <= max(cas))
          {
            long_delta_t <- s + a * long_t
            long_delta_var = t(c(1,long_t)) %*% Var_est_mat %*% c(1,long_t)
            long_z_score = long_delta_t / sqrt(long_delta_var)
          }
        }  
        
      }
      r_fin <- append(r_fin, r)
      z_score_fin <- append(z_score_fin, z_score)
      
    } 
    else { # i>1 & j > 1
      # OU
      
      if( j > 0  ) {
        names <- names(HR[grep(im_level[j], names(HR))])
        # names
        
        if (length(names) > 1){
          # OU: here, only code from TREND is coppied
          
          HR_sub <- HR[names]
          V_mat_sub <- V_mat[names, names]
          
          cas <- as.numeric(gsub("\\D", "", names))
          cas <-  ifelse(cas > 1000, cas %>% substring(nchar(cas) - 2, nchar(cas)) %>% 
                           as.numeric() %>% - 30, 
                         cas + 30)
          cas <- cas / 30.5
          
          T_mat <- matrix(diag(HR_sub), ncol = length(HR_sub))
          
          W_mat <- t(T_mat) %*% V_mat_sub %*% T_mat
          
          X_mat <- matrix(data = c(rep(1, times = length(HR_sub)), cas), ncol = 2)
          
          GLS_est <- inv(t(X_mat) %*% inv(W_mat) %*% X_mat) %*% 
            t(X_mat) %*% inv(W_mat) %*% HR_sub
          
          Var_est_mat <- inv(t(X_mat) %*% inv(W_mat) %*% X_mat)
          
          v <- GLS_est[1]
          d <- GLS_est[2]

          short_delta_t <- 1 - (v + d * short_t)
          short_delta_var <- t(c(1,short_t)) %*% Var_est_mat %*% c(1,short_t)
          short_z_score <- short_delta_t / sqrt(short_delta_var)
          if(long_t <= max(cas))
          {
            long_delta_t <- 1-(v + d * long_t)
            long_delta_var = t(c(1,long_t)) %*% Var_est_mat %*% c(1,long_t)
            long_z_score = long_delta_t / sqrt(long_delta_var)
          }
          
        }
      }
    }
    short_fin <- append(short_fin,short_delta_t)
    short_z_score_fin <- append(short_z_score_fin,short_z_score)
    long_fin <- append(long_fin,long_delta_t)
    long_z_score_fin <- append(long_z_score_fin,long_z_score)
  }
}



# M: pak ale tohle musíme udělat jinak
#r_fin <- r_fin[!is.na(r_fin)]
#z_score_fin <- z_score_fin[!is.na(z_score_fin)]
r_fin <- r_fin[-1]
z_score_fin  <- z_score_fin[-1]
#OU
short_fin <- short_fin[-1]
short_z_score_fin <- short_z_score_fin[-1]

long_fin <- long_fin[-1]
long_z_score_fin <- long_z_score_fin[-1]

# M: tohle by ale pak ty všechny NA smrsklo (stejně jako hodnoty, kdyby
# náhodou byly stejné... nebo mi něco nedochází)
#r_fin2 <- unique(r_fin)
#z_score_fin2 <- unique(z_score_fin)
r_fin2 <- r_fin
z_score_fin2 <- z_score_fin


# toto je brutalne nahodna vec
# M: a já jsem to předělal na víc kovariát a nevím, jestli dobře
#r_fin3 <- c(NA, r_fin2[1:6], NA, r_fin2[7:12], NA,  r_fin2[13:18], NA,  
#            r_fin2[19:24], NA, r_fin2[25:30], NA)
#r_fin_mat <- matrix(r_fin3, ncol = 6, byrow = T)
#r_fin_mat <- matrix(label_percent()(r_fin_mat), ncol = 6, byrow = T)

#z_score_fin3 <- c(NA, z_score_fin2[1:6], NA, z_score_fin2[7:12], NA, 
#                  z_score_fin2[13:18], NA, z_score_fin2[19:24], NA, 
#                  z_score_fin2[25:30], NA)
#z_score_fin_mat <- matrix(z_score_fin3, ncol = 6, byrow = T)

# M: tohle jsme ošetřili v cyklu
#r_fin3 <- c(NA, r_fin2[1:7], NA, r_fin2[8:14], NA,  r_fin2[15:21], NA,  
#            r_fin2[22:28], NA, r_fin2[29:35], NA ,r_fin2[36:42], NA)
#r_fin_mat <- matrix(r_fin3, ncol = 7, byrow = T)

#M: ta druhá řádka bůhvíproč tu matici transponovalo
#r_fin_mat <- matrix(r_fin2, ncol = length(im_level), byrow = T)
#r_fin_mat <- matrix(label_percent()(r_fin_mat), ncol = length(im_level), byrow = T)
#M moje náhrada:
r_fin_mat <- matrix(label_percent(accuracy = 0.01)(r_fin2), ncol = length(im_level), byrow = T)

write.table(r_fin_mat,"r_fin_mat.txt")



# TODO sem bych potřeboval nějaké zktratky těch imunit (zkratky dodám)
textable <- xtable(r_fin_mat, caption="Effectiveness/protection differences")
print(textable, file = "r_fin_mat.tex", include.rownames = TRUE)


# M: viz výše
#z_score_fin3 <- c(NA, z_score_fin2[1:7], NA, z_score_fin2[8:14], NA, 
#                  z_score_fin2[15:21], NA, z_score_fin2[22:28], NA, 
#                  z_score_fin2[29:35], NA,z_score_fin2[36:42], NA)
#z_score_fin_mat <- matrix(z_score_fin3, ncol = 7, byrow = T)
z_score_fin_mat <- matrix(z_score_fin2, ncol = length(im_level), byrow = T)

# TODO žéž zde zkratky imunit
write.table(z_score_fin_mat,"z_score_fin_mat.txt")
textable <- xtable(z_score_fin_mat, caption="Z-scores of effectiveness/protection differences")
print(textable, file = "z_score_mat.tex", include.rownames = FALSE)


#### HEATMAPA #### 

lowerTriangle(z_score_fin_mat, diag = FALSE, byrow = FALSE) <- NA
lowerTriangle(r_fin_mat, diag = FALSE, byrow = FALSE) <- NA

#breaks=c(-2.8,-2.58, -1.96, -1.64, 1.64, 1.96, 2.58, 2.8)
#mycol <- colorpanel(n=length(breaks)-1,low="red",mid="lightgrey",high="darkgreen")

#OU
im_level_trend = append("Immunitynone", im_level)

short_fin_mat <- matrix(label_percent(accuracy = 0.01)(short_fin), ncol = length(im_level_trend), byrow = T)
write.table(short_fin_mat,"short_fin_mat.txt")
textable <- xtable(short_fin_mat, caption=paste("Effectiveness/protection differences in ", short_t))
print(textable, file = "short_fin_mat.tex", include.rownames = TRUE)


short_z_score_fin_mat <- matrix(short_z_score_fin, ncol = length(im_level_trend), byrow = T)

write.table(short_z_score_fin_mat,"short_z_score_fin_mat.txt")
textable <- xtable(short_z_score_fin_mat, caption=paste("Z-scores of effectiveness/protection differences in ",short_t))
print(textable, file = "short_z_score_mat.tex", include.rownames = FALSE)

lowerTriangle(short_z_score_fin_mat, diag = FALSE, byrow = FALSE) <- NA
lowerTriangle(short_fin_mat, diag = FALSE, byrow = FALSE) <- NA

long_fin_mat <- matrix(label_percent(accuracy = 0.01)(long_fin), ncol = length(im_level_trend), byrow = T)
write.table(long_fin_mat,"long_fin_mat.txt")
textable <- xtable(long_fin_mat, caption=paste("Effectiveness/protection differences in ", long_t))
print(textable, file = "long_fin_mat.tex", include.rownames = TRUE)


long_z_score_fin_mat <- matrix(long_z_score_fin, ncol = length(im_level_trend), byrow = T)

write.table(long_z_score_fin_mat,"long_z_score_fin_mat.txt")
textable <- xtable(long_z_score_fin_mat, caption=paste("Z-scores of effectiveness/protection differences in ",long_t))
print(textable, file = "long_z_score_mat.tex", include.rownames = FALSE)

lowerTriangle(long_z_score_fin_mat, diag = FALSE, byrow = FALSE) <- NA
lowerTriangle(long_fin_mat, diag = FALSE, byrow = FALSE) <- NA


#we will need heatmap both with and without the color key
for (i in 0 : 1) {

# https://www.biostars.org/p/73644/
breaks=c(-2.8,-2.58, -1.96, -1.64, 1.64, 1.96, 2.58, 2.8)
mycol <- colorpanel(n=length(breaks)-1,low="red",mid="lightgrey",high="darkgreen")
  
  
if(i == 0)
  png(file="heatmapnk.png", height = 325)
else  
  png(file="heatmap.png", height = 325)

# https://www.biostars.org/p/73644/



heatmap.2(z_score_fin_mat, cellnote = r_fin_mat %>% round(0), dendrogram = "none", Rowv = F, 
          Colv = F, notecol="black", 
          trace = "none",
          # notecex = 0.8, # velkost cislicek, default je 1
          # key=FALSE, 
          density.info = "none",
          # keysize = 0.25,
          key.title = "Z-score",
          key.xlab = "critical value",
          margins = c(1, 10),
          srtCol = 270,
          offsetCol = -30, # toto je trochu hruba sila
          labRow = substr(im_level[-length(im_level)], 9, 20),
          labCol = substr(im_level, 9, 20), 
          # labCol = F,
          col = mycol,
          breaks = breaks,
          key = i)

dev.off()

#OU
if(i == 0)
  png(file="heatmapshortnk.png", height = 325)
else  
  png(file="heatmapshort.png", height = 325)


heatmap.2(short_z_score_fin_mat, cellnote = short_fin_mat %>% round(0), dendrogram = "none", Rowv = F, 
          Colv = F, notecol="black", 
          trace = "none",
          # notecex = 0.8, # velkost cislicek
          # key=FALSE, 
          density.info = "none",
          # keysize = 0.25,
          key.title = "Z-score",
          key.xlab = "critical value",
          margins = c(1, 10),
          srtCol = 270,
          offsetCol = -30, # toto je trochu hruba sila
          labRow = substr(im_level_trend[-length(im_level_trend)], 9, 20),
          labCol = substr(im_level_trend, 9, 20), 
          # labCol = F,
          col = mycol,
          breaks = breaks,
          key = i)

dev.off()

if(i == 0)
  png(file="heatmaplongnk.png", height = 325)
else  
  png(file="heatmaplong.png", height = 325)
?heatmap.2

heatmap.2(long_z_score_fin_mat, cellnote = long_fin_mat %>% round(0), dendrogram = "none", Rowv = F, 
          Colv = F, notecol="black", 
          trace = "none",
          # notecex = 0.8, # velkost cislicek
          # key=FALSE, 
          density.info = "none",
          # keysize = 0.25,
          key.title = "Z-score",
          key.xlab = "critical value",
          margins = c(1, 10),
          srtCol = 270,
          offsetCol = -30, # toto je trochu hruba sila
          labRow = substr(im_level_trend[-length(im_level_trend)], 9, 20),
          labCol = substr(im_level_trend, 9, 20), 
          # labCol = F,
          col = mycol,
          breaks = breaks,
          key = i)

dev.off()

}
# we put this here as it takes much time
if(args[2] == "LCINF")
{
  forest_model(m1)
  ggsave("forest_plot.png")
}

save.image("workspace.RData")

