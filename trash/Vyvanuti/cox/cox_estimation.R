#!/usr/bin/env Rscript
##1. Initiating procedure
#1.1 Parsing args (use the commented command below for inputing them wihout console run)
args = commandArgs(trailingOnly=TRUE) #args = c("data/temp/IOmicron.csv","I","temp.txt")
print(args)

#1.2 Loading libraries
  library(survival)
  library(survminer)
  library(ggplot2)
  library(data.table)
  library(vtable)
  library(xtable)

#1.3 Saving various useful data
  systeminfo = Sys.info()
  user = as.character(systeminfo['login'])
  computer = as.character(systeminfo['nodename'])
  time.start = as.character(Sys.time())
  
  ptime = gsub(" ","_",time.start)
  ptime = gsub(":","-",ptime)
  
  print(paste("Hello ",user," of ",computer,", it is ",time.start,".",sep=""))

#1.4 Checking the provided arguments. The following if-else goes through the whole script and stops if requested args are not provided.
  if(length(args) > 1){
    print("Inputs have been provided.")
    data.input = args[1]
    data.input.events = paste(args[1],"events.csv",sep=".")
    f.input.id = args[2]
    cox.f = fread("cox/cox_estimation_formulas.txt",stringsAsFactors = FALSE)
    f.input = noquote(as.character(cox.f[cox.f$ID == f.input.id,2]))

    print(paste("I am using file: ",data.input,sep=""))
    print(paste("...and Cox formula: ",f.input,sep=""))   
    
    if(length(args)>2){
      print("I can see a convertool log provided.")
      convertool.log.location = args[3]
      convertool.log = read.delim(convertool.log.location, header = FALSE, sep = "\t", dec = ".")
      names(convertool.log) = "record"
      convertool.vignette = convertool.log[1:7,]
    } else {
      print("No Convertool log has been provided.")
      convertool.log = NA
      convertool.vignette = NA
    }
    
  ##2. Modeling procedure
    
  #2.1 Data loading and initial processing
  mtab = fread(data.input,stringsAsFactors = FALSE)
  mtab.events = fread(data.input.events,stringsAsFactors = FALSE)
  file.name = strsplit(data.input,"/")[[1]]
  file.name = gsub(".csv","",file.name[length(file.name)])
  
  mtab[, LastVaccTime := as.integer(LastVaccTime)][LastVaccTime == "_none", LastVaccTime := NULL]
  mtab[, InfPriorTime := as.integer(InfPriorTime)][InfPriorTime == "_none", InfPriorTime := NULL]
  
  mtab.events[, LastVaccTime := as.integer(LastVaccTime)][LastVaccTime == "_none", LastVaccTime := NULL]
  mtab.events[, InfPriorTime := as.integer(InfPriorTime)][InfPriorTime == "_none", InfPriorTime := NULL]
  
  #2.2 Preparing output directory
  dir.name = file.name
  dir.name = paste("outputs/",dir.name,sep="")
  paste("The model files will be stored in: ",dir.name,"/",sep="")
  if(file.exists(dir.name)){
    print("The directory already exists, files will be rewritten.")
    } else {
    dir.create(dir.name)
    print("The directory has been created.")
}
 
#2.3. Switching to output directory, saving input file's descriptive stats
  st(mtab,out='csv',file=paste(dir.name,'input_summary.csv',sep="/"))

  custom.dq = list()
  
  VaccStatus_medians = mtab[mtab$T1 == 0,.(N=.N,age_median=median(Age),LastVaccTime_median=median(LastVaccTime),InfPriorTime_median=median(InfPriorTime)),.(VaccStatus)]
  custom.dq[["Median stats for VaccStatus at T1 == 0"]] = VaccStatus_medians
  
#  Immunity_medians = mtab[mtab$T1 == 0,.(N=.N,age_median=median(Age),LastVaccTime_median=median(LastVaccTime),InfPriorTime_median=median(InfPriorTime)),.(Immunity)]
#  custom.dq[["Median stats for Immunity at T1 == 0"]] = Immunity_medians
  
  VaccStatus_medians_events = mtab.events[,.(N=.N,age_median=median(Age),LastVaccTime_median=median(LastVaccTime),InfPriorTime_median=median(InfPriorTime)),.(VaccStatus)]
  custom.dq[["Median stats for VaccStatus at the respective T"]] = VaccStatus_medians_events
  
#  Immunity_medians_events = mtab.events[,.(N=.N,age_median=median(Age),LastVaccTime_median=median(LastVaccTime),InfPriorTime_median=median(InfPriorTime)),.(Immunity)]
#  custom.dq[["Median stats for Immunity at the respective T"]] = Immunity_medians_events
  
  sink(paste(dir.name,"input_summary_custom.txt",sep="/"))
  print(custom.dq)
  sink()  
  gc()

  print("The input file has been loaded, its properties have been saved. Proceeding to Cox model.")
  
  
  
#2.4 Cox model estimation
  f.input = as.formula(f.input)
#  cox = eval(bquote(coxph(.(f.input),data=mtab,cluster=Subject)))
#  cox = eval(bquote(coxph(.(f.input),data=mtab,robust=FALSE,control=coxph.control(iter.max = 100))))
  cox = eval(bquote(coxph(.(f.input),data=mtab)))

  cox.summary = summary(cox) 
  print(cox.summary) # returns output to the console

  ##3. Model outputs generation
  #3.1 Hazard ratio plot
  ggforest(cox, data = data.frame(mtab), fontsize = 1)
  ggsave(paste(dir.name,"cox_risks_log.jpg",sep="/"),units="px",width=3840,height=3840)
  print("Model charts have been generated.")
  
  #3.2 Txt (model summary)
  sink(paste(dir.name,"cox_output.txt",sep="/"))
  print(cox.summary)
  sink()  
  gc()
  print("Model output has been saved (TXT).")

  #3.2 TeX (params and exp(params))
  beta <- coef(cox)
  se   <- sqrt(diag(cox$var))
  p    <- 1 - pchisq((beta/se)^2, 1)
  CI   <- round(confint(cox), 3)
  res <- data.frame(beta, se = se, CI, p)
  names(res)[3:4] = c("low","high")
  res$model.betas = paste(round(res$beta,2)," (",round(res$low,2),", ",round(res$high,2),")",sep="")
  
  res$hr = exp(res$beta)
  res$hr.se = exp(res$se)
  res$hr.low = exp(res$low)
  res$hr.high = exp(res$high)
  res$model.hr = paste(round(res$hr,2)," (",round(res$hr.low,2),", ",round(res$hr.high,2),")",sep="")
  
  res$eff = 1-res$hr
  res$eff.high = 1-res$hr.low
  res$eff.low = 1-res$hr.high
  res$model.effectiveness = paste(round(res$eff,2)," (",round(res$eff.low,2),", ",round(res$eff.high,2),")",sep="")

  modtab = res[,c("model.betas","model.hr","model.effectiveness")]
  names(modtab) = c("Beta","HR","1-HR")
  print(xtable(modtab),include.rownames=TRUE,file = paste(dir.name,"cox_output.tex",sep="/"), compress = FALSE)
  print("Model output has been saved (TeX).")  
  
  #4. Closing sequence
  time.end = as.character(Sys.time())
  model.spent = difftime(time.end,time.start)
  print("The procedure is finished in following time:")
  print(model.spent)
  
  #4.1 Saving model run stats
  run.overview = list()
  run.overview[["user"]] = user
  run.overview[["computer"]] = computer
  run.overview[["arguments"]] = args
  run.overview[["time.start"]] = time.start
  run.overview[["time.end"]] = time.end
  run.overview[["model.spent"]] = model.spent
  run.overview[["model.concordance"]] = cox$concordance['concordance']
  run.overview[["convertool.vignette"]] = convertool.vignette
  
  res$model.concordance = run.overview[["model.concordance"]]
  res$model.ctool.version = run.overview[["convertool.vignette"]][1]
  res$model.age.filter = run.overview[["convertool.vignette"]][4]
  res$model.input.file = run.overview[["convertool.vignette"]][7]
  res$model.input.horizon = run.overview[["convertool.vignette"]][5]
  res$model.mode = f.input.id
  res$model.calculated.at = time.end
  
  write.csv(res,file=paste(dir.name,"model_data.csv",sep="/"),row.names=FALSE)

  sink(paste(dir.name,"model_run_overview.txt",sep="/"))
  print(run.overview)
  sink()

  #4.2 Saving environment image (without the most heavy items)
  rm(mtab,cox)
  save(list=ls(), file=paste(dir.name,"environment.Rdata",sep="/"),compress=TRUE,compression_level=9)
  print("Workspace image and log have been saved.")
  print("https://www.youtube.com/watch?v=g3ENX3aHlqU")
  
  } else {
    stop("Data input has not been provided, goodbye")
    print(data.input)
  }
