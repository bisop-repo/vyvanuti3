#!/usr/bin/env Rscript
##1. Initiating procedure

#1.1 Loading libraries
library(plyr)
library(stringr)
library(reshape2)
library(xtable)

#1.2 Setting up working directory
setwd("outputs/")

##2. Loading data, preparing files for aggregate results
  model.runs.files = list.files(".",recursive=TRUE,pattern="environment")
  all.models = data.frame()
  for(e in model.runs.files){
    file.to.load = e
    load(file.to.load)
    res$model.path = file.to.load
    res$model.name = strsplit(file.to.load,"/")[[1]][1]
    res$variable = row.names(res)
    all.models = rbind.fill(all.models,res)
    rm(list=ls()[! ls() %in% c("all.models","e","model.runs.files")])
  }
  
setwd("../results/")
  
#2.1 Saving all models db
write.csv(all.models,file="00_All_model_data.csv",row.names=FALSE)
print("Model overview table has been saved")
  
##3. TeX tables
  
#3.1 Reinfections
tex.table.R = all.models[(all.models$model.name == "R0_plus8W"),c("variable","model.hr")]
print(xtable(tex.table.R),include.rownames=FALSE,file = "01_R_model_table_hr.tex", compress = FALSE)
  
#3.2 Infections, Hospitalizations and Deaths
tex.table.IHD = all.models[(all.models$model.name %in% c("D0_plus","H0_plus","I0_plus")),c("variable","model.name","model.hr")]
tex.table.IHD = tex.table.IHD[order(tex.table.IHD$variable),]
tex.table.IHD$model.name = mapvalues(tex.table.IHD$model.name,from=c("D0_plus","H0_plus","I0_plus"),to=c("Deaths","Hospitalizations","Infections"))
  
tex.table.IHD.I = tex.table.IHD[tex.table.IHD$model.name == "Infections",]
tex.table.IHD.I.name = max(tex.table.IHD.I$model.name)
tex.table.IHD.I = tex.table.IHD.I[,c("variable","model.hr")]
names(tex.table.IHD.I)[2] = tex.table.IHD.I.name
  
tex.table.IHD.H = tex.table.IHD[tex.table.IHD$model.name == "Hospitalizations",]
tex.table.IHD.H.name = max(tex.table.IHD.H$model.name)
tex.table.IHD.H = tex.table.IHD.H[,c("variable","model.hr")]
names(tex.table.IHD.H)[2] = tex.table.IHD.H.name
  
tex.table.IHD.D = tex.table.IHD[tex.table.IHD$model.name == "Deaths",]
tex.table.IHD.D.name = max(tex.table.IHD.D$model.name)
tex.table.IHD.D = tex.table.IHD.D[,c("variable","model.hr")]
names(tex.table.IHD.D)[2] = tex.table.IHD.D.name
  
tex.table.IHD = merge(tex.table.IHD.I,tex.table.IHD.H,by="variable",all.x=TRUE)
tex.table.IHD = merge(tex.table.IHD,tex.table.IHD.D,by="variable",all.x=TRUE) 
print(xtable(tex.table.IHD),include.rownames=FALSE,file = "02_IHD_model_table_hr.tex", compress = FALSE)
  
  
##4. Line chart input files
chart.line.data = all.models[,c("variable","eff","eff.high","eff.low","model.mode","model.age.filter","model.path","model.name")]
names(chart.line.data)[3:4] = c("CIU","CIL")
  
##4.1 Reinfections in time
chart2.data = chart.line.data[(grepl("InfPrior",all.models$variable)) & (chart.line.data$model.path == "R0_plus8W/environment.Rdata"),]
chart2.data$variable = ordered(chart2.data$variable, levels = c("InfPrior1", "InfPrior2", "InfPrior3",
                                                                "InfPrior4", "InfPrior5", "InfPrior6",
                                                                "InfPrior7", "InfPrior8", "InfPrior9",
                                                                "InfPriorrest"))
chart2.data = subset(chart2.data, select = -c(model.age.filter,model.mode,model.path,model.name))
names(chart2.data)[1] = "reinfection"
chart2.data = melt(chart2.data,id.vars=c("reinfection"))
chart2.data$variable =  mapvalues(chart2.data$variable,from=c("eff","CIU","CIL"),to=c("M","H","L"))
chart2.data$variable = ordered(chart2.data$variable, levels = c("L", "M", "H"))
  
chart2.data = chart2.data[order(chart2.data$reinfection,chart2.data$variable),]
chart2.data$reinfection = paste(chart2.data$reinfection,chart2.data$variable,sep="_")
chart2.data = chart2.data[,c(1,3)]
chart2.data = t(chart2.data)
  
write.table(chart2.data, file="03_Reinf.csv", na="-100", row.names=FALSE, col.names=FALSE, sep=",")
  
##4.2 Infections, Hospitalizations and Deaths for each of the assesed vaccines
chart1.data = chart.line.data[(grepl("VaccStatus",all.models$variable)) & (chart.line.data$model.age.filter == "Age filter: 0-650") & (chart.line.data$model.mode != "R") &  (chart.line.data$model.mode != "RP") & (chart.line.data$model.name != "I0_plus_select"),]
  
chart1.data$variable = str_replace(chart1.data$variable,"VaccStatus",replacement="")
chart1.data$variable.vaccine = substr(chart1.data$variable,1,1)
chart1.data$variable.vaccine.order = substr(chart1.data$variable,2,20)
chart1.data$variable.vaccine.order = str_replace(chart1.data$variable.vaccine.order,"_",replacement="")
vaccine.order.order = unique(chart1.data$variable.vaccine.order)
chart1.data$variable.vaccine.order = factor(chart1.data$variable.vaccine.order,
                                            ordered=TRUE,levels=vaccine.order.order)

chart1.data$variable.vaccine = mapvalues(chart1.data$variable.vaccine, from=c("A", "M", "J","P"), to=c("Astra Zeneca", "Moderna", "Janssen","Pfizer"))
chart1.data = subset(chart1.data, select = -c(model.age.filter,variable,model.name) )
chart1.data = chart1.data[(chart1.data$variable.vaccine.order != "first1") & (chart1.data$variable.vaccine.order != "first2plus"),]
chart1.data = subset(chart1.data,select = -model.path)
names(chart1.data)[5:6] = c("vaccine","event")
chart1.data$model.mode = str_replace(chart1.data$model.mode,"IP","I")
 
chart1.dataI = chart1.data[chart1.data$model.mode == "I",]
names(chart1.dataI)[1:3] = c("IM","IU","IL")
chart1.dataI = subset(chart1.dataI, select = -c(model.mode) )
  
chart1.dataH = chart1.data[chart1.data$model.mode == "H",]
names(chart1.dataH)[1:3] = c("HM","HU","HL")
chart1.dataH = subset(chart1.dataH, select = -c(model.mode) )
  
chart1.dataD = chart1.data[chart1.data$model.mode == "D",]
names(chart1.dataD)[1:3] = c("DM","DU","DL")
chart1.dataD = subset(chart1.dataD, select = -c(model.mode) )
  
chart1.data = merge(chart1.dataI,chart1.dataH,by=c("vaccine","event"))
chart1.data = merge(chart1.data,chart1.dataD,by=c("vaccine","event"))
  
chart1.data = chart1.data[order(chart1.data$event),]
chart1.data = chart1.data[,c("vaccine","event","IL","IM","IU","HL","HM","HU","DL","DM","DU")]
  
for(v in unique(chart1.data$vaccine)){
  vacdata = chart1.data[chart1.data$vaccine == v,]
  vacdata = subset(vacdata, select = -c(vaccine) )
  write.table(vacdata, file=paste("04_Vacc_",v,".csv",sep=""), na="-100", row.names=FALSE, col.names=TRUE, sep=",")
}