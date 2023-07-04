library("dplyr")
library("ggplot2")
library(tidyr)
library(rlist)
library(gtsummary)

# quartals definitons
# signs form quartals and start and end dates
names_of_columns <- c('Q1', 'Q2', 'Q3', 'Q4', 'Q5', 'Q6', 'Q7', 'Q8', 'Q9')
start_of_Q <- c("2021-01-01", "2021-04-01", "2021-07-01", "2021-10-01", "2022-01-01", "2022-04-01", "2022-07-01", "2022-10-01", "2023-01-01")
end_of_Q <- c("2021-03-31", "2021-06-30", "2021-09-30", "2021-12-31", "2022-03-31", "2022-06-30", "2022-09-30", "2022-12-31", "2023-03-31")



# infections
infections.df = read.csv('infections.csv')
#infections.df = read.csv('xaa.csv')
infections.df <- infections.df %>% replace(is.na(.), 'unknown')



#
#graph_01 - variantdiscint
#
fields_for_graph <- unique(infections.df$variantdiscint)

#create dataframe graph.df
graph_01.df <- data.frame(row.names = fields_for_graph)

# fill needed columns of dataframe with zeros
# create zero_column
zero_column <- vector(length(fields_for_graph), mode="integer")

# add the nuber of columns corresponding to number of quartals
for(column in 1:length(names_of_columns)){
  graph_01.df <- cbind(graph_01.df,zero_column)
}
colnames(graph_01.df) <- names_of_columns

#go through all columns and sumarize counts of all cases
for(i in 1:length(names_of_columns)){
    count_of_variants <- ( infections.df[infections.df$infdate >= start_of_Q[i] & infections.df$infdate <= end_of_Q[i],] %>% count(variantdiscint))
    cat("\n \nKvartál:", names_of_columns[i], "\n")
    for(j in 1:nrow(count_of_variants)){
      variant <- count_of_variants[j,1]
      sum_of_variants <- count_of_variants[j,2]
      cat(variant, "pocet: ", sum_of_variants, "\n")
      graph_01.df[variant, names_of_columns[i]] <- sum_of_variants
    }
}


# Redukce pro graf01 - variantdisc

fields_for_graphs <- c("BA12","BA45","Delta","others","unknown")
zero_column <- vector(length(fields_for_graphs), mode="integer")
graph_01a.df <- data.frame(row.names = fields_for_graphs )
for(column in 1:length(names_of_columns)){
  graph_01a.df <- cbind(graph_01a.df,zero_column)
}
colnames(graph_01a.df) <- names_of_columns

my_value <- -1
for(tablerow in fields_for_graphs){
  cat(tablerow, "\n")
  for(tablecolumn in names_of_columns){
    if(tablerow =="others"){
      my_value <- graph_01.df["Alpha",tablecolumn]+graph_01.df["Omicron",tablecolumn]+graph_01.df["Early",tablecolumn]
    }
    else if(tablerow =="BA45"){
      my_value <- graph_01.df["BA45",tablecolumn]+graph_01.df["2023",tablecolumn]
    }
    else{
      my_value <- graph_01.df[tablerow,tablecolumn]
    }
    graph_01a.df[tablerow,tablecolumn] <- my_value
    my_value <- -1
  }
}








#
#graph_02 - variantdiscint
#
fields_for_graph <- unique(infections.df$variantdisc)

#create dataframe graph.df
graph_02.df <- data.frame(row.names = fields_for_graph)

# fill needed columns of dataframe with zeros
# create zero_column
zero_column <- vector(length(fields_for_graph), mode="integer")

# add the nuber of columns corresponding to number of quartals
for(column in 1:length(names_of_columns)){
  graph_02.df <- cbind(graph_02.df,zero_column)
}
colnames(graph_02.df) <- names_of_columns

#go through all columns and sumarize counts of all cases
for(i in 1:length(names_of_columns)){
  count_of_variants <- ( infections.df[infections.df$infdate >= start_of_Q[i] & infections.df$infdate <= end_of_Q[i],] %>% count(variantdisc))
  cat("\n \nKvartál:", names_of_columns[i], "\n")
  for(j in 1:nrow(count_of_variants)){
    variant <- count_of_variants[j,1]
    sum_of_variants <- count_of_variants[j,2]
    cat(variant, "pocet: ", sum_of_variants, "\n")
    graph_02.df[variant, names_of_columns[i]] <- sum_of_variants
  }
}

# Redukce pro graf02 - variantdisc

fields_for_graphs <- c("BA12","BA45","Delta","others","unknown")
zero_column <- vector(length(fields_for_graphs), mode="integer")
graph_02a.df <- data.frame(row.names = fields_for_graphs )
for(column in 1:length(names_of_columns)){
  graph_02a.df <- cbind(graph_02a.df,zero_column)
}
colnames(graph_02a.df) <- names_of_columns

my_value <- -1
for(tablerow in fields_for_graphs){
  cat(tablerow, "\n")
  for(tablecolumn in names_of_columns){
    if(tablerow =="others"){
      my_value <- graph_02.df["Alpha",tablecolumn]+graph_02.df["Omicron",tablecolumn]
    }
    else if(tablerow =="BA45"){
      my_value <- graph_02.df["BA45",tablecolumn]
    }
    else{
      my_value <- graph_02.df[tablerow,tablecolumn]
    }
    graph_02a.df[tablerow,tablecolumn] <- my_value
    my_value <- -1
  }
}



#
# graph_03 - % of discriminated
#







#
# graph_04 - serious_course variantdiscint
#
fields_for_graph <- unique(infections.df$variantdiscint)

#create dataframe graph.df
graph_04.df <- data.frame(row.names = fields_for_graph)

# fill needed columns of dataframe with zeros
# create zero_column
zero_column <- vector(length(fields_for_graph), mode="integer")

# add the nuber of columns corresponding to number of quartals
for(column in 1:length(names_of_columns)){
  graph_04.df <- cbind(graph_04.df,zero_column)
}
colnames(graph_04.df) <- names_of_columns

#go through all columns and sumarize counts of all cases
for(i in 1:length(names_of_columns)){
  count_of_variants <- ( infections.df[infections.df$infdate >= start_of_Q[i] & infections.df$infdate <= end_of_Q[i] & infections.df$serious == 1,] %>% count(variantdiscint))
  cat("\n \nKvartál:", names_of_columns[i], "\n")
  for(j in 1:nrow(count_of_variants)){
    variant <- count_of_variants[j,1]
    sum_of_variants <- count_of_variants[j,2]
    cat(variant, "pocet: ", sum_of_variants, "\n")
    graph_04.df[variant, names_of_columns[i]] <- sum_of_variants
  }
}


# Redukce pro graf04 - variantdisc

fields_for_graphs <- c("BA12","BA45","Delta","others","unknown")
zero_column <- vector(length(fields_for_graphs), mode="integer")
graph_04a.df <- data.frame(row.names = fields_for_graphs )
for(column in 1:length(names_of_columns)){
  graph_04a.df <- cbind(graph_04a.df,zero_column)
}
colnames(graph_04a.df) <- names_of_columns

my_value <- -1
for(tablerow in fields_for_graphs){
  cat(tablerow, "\n")
  for(tablecolumn in names_of_columns){
    if(tablerow =="others"){
      my_value <- graph_04.df["Alpha",tablecolumn]+graph_04.df["Omicron",tablecolumn]+graph_04.df["Early",tablecolumn]
    }
    else if(tablerow =="BA45"){
      my_value <- graph_04.df["BA45",tablecolumn]+graph_04.df["2023",tablecolumn]
    }
    else{
      my_value <- graph_04.df[tablerow,tablecolumn]
    }
    graph_04a.df[tablerow,tablecolumn] <- my_value
    my_value <- -1
  }
}








#
#graph_05 - variantdiscint 
#
fields_for_graph <- unique(infections.df$variantdiscint)

#create dataframe graph.df
graph_05.df <- data.frame(row.names = fields_for_graph)

# fill needed columns of dataframe with zeros
# create zero_column
zero_column <- vector(length(fields_for_graph), mode="integer")

# add the nuber of columns corresponding to number of quartals
for(column in 1:(length(names_of_columns)-1)){
  graph_05.df <- cbind(graph_05.df,zero_column)
}
colnames(graph_05.df) <- names_of_columns[1:(length(names_of_columns)-1)]

#go through all columns and sumarize counts of all cases
for(i in 1:(length(names_of_columns)-1)){
  count_of_variants <- ( infections.df[infections.df$infdate >= start_of_Q[i] & infections.df$infdate <= end_of_Q[i] & infections.df$longcovid == 1,] %>% count(variantdiscint))
  cat("\n \nKvartál:", names_of_columns[i], "\n")
  for(j in 1:nrow(count_of_variants)){
    variant <- count_of_variants[j,1]
    sum_of_variants <- count_of_variants[j,2]
    cat(variant, "pocet: ", sum_of_variants, "\n")
    graph_05.df[variant, names_of_columns[i]] <- sum_of_variants
  }
}


# Redukce pro graf05 - variantdiscint

fields_for_graphs <- c("BA12","BA45","Delta","others","unknown")
zero_column <- vector(length(fields_for_graphs), mode="integer")
graph_05a.df <- data.frame(row.names = fields_for_graphs )
for(column in 1:(length(names_of_columns)-1)){
  graph_05a.df <- cbind(graph_05a.df,zero_column)
}
colnames(graph_05a.df) <- names_of_columns[1:(length(names_of_columns)-1)]

my_value <- -1
for(tablerow in fields_for_graphs){
  cat(tablerow, "\n")
  for(tablecolumn in names_of_columns[1:(length(names_of_columns)-1)]){
    if(tablerow =="others"){
      my_value <- graph_05.df["Alpha",tablecolumn]+graph_05.df["Omicron",tablecolumn]+graph_05.df["Early",tablecolumn]
    }
    else if(tablerow =="BA45"){
      my_value <- graph_05.df["BA45",tablecolumn]+graph_05.df["2023",tablecolumn]
    }
    else{
      my_value <- graph_05.df[tablerow,tablecolumn]
    }
    graph_05a.df[tablerow,tablecolumn] <- my_value
    my_value <- -1
  }
}




#
# VACCINES
#
vaccines.df = read.csv('vaccines.csv')
vaccines.df <- vaccines.df %>% replace(is.na(.), 'unknown')


cross_table <- tbl_cross(vaccines.df, row = 'code', col = 'order')
sink("vaccines_cross_tab.txt")
print(cross_table)
sink()

#
#graph_06 - full
#
fields_for_graph <- unique(vaccines.df$general)

#create dataframe graph.df
graph_06.df <- data.frame(row.names = fields_for_graph)

# fill needed columns of dataframe with zeros
# create zero_column
zero_column <- vector(length(fields_for_graph), mode="integer")

# add the nuber of columns corresponding to number of quartals
for(column in 1:length(names_of_columns)){
  graph_06.df <- cbind(graph_06.df,zero_column)
}
colnames(graph_06.df) <- names_of_columns

#go through all columns and sumarize counts of all cases
for(i in 1:length(names_of_columns)){
  count_of_variants <- ( vaccines.df[vaccines.df$vaccdate >= start_of_Q[i] & vaccines.df$vaccdate <= end_of_Q[i] & vaccines.df$order == "final",] %>% count(general))
  cat("\n \nKvartál:", names_of_columns[i], "\n")
  if(nrow(count_of_variants) != 0){
    for(j in 1:nrow(count_of_variants)){
      variant <- count_of_variants[j,1]
      sum_of_variants <- count_of_variants[j,2]
      cat(variant, "pocet: ", sum_of_variants, "\n")
      graph_06.df[variant, names_of_columns[i]] <- sum_of_variants
    }
  }
}



#
#graph_07 - booster
#
fields_for_graph <- unique(vaccines.df$general)
#create dataframe graph.df
graph_07.df <- data.frame(row.names = fields_for_graph)

# fill needed columns of dataframe with zeros
# create zero_column
zero_column <- vector(length(fields_for_graph), mode="integer")

# add the nuber of columns corresponding to number of quartals
for(column in 1:length(names_of_columns)){
  graph_07.df <- cbind(graph_07.df,zero_column)
}
colnames(graph_07.df) <- names_of_columns

#go through all columns and sumarize counts of all cases
for(i in 1:length(names_of_columns)){
  count_of_variants <- ( vaccines.df[vaccines.df$vaccdate >= start_of_Q[i] & vaccines.df$vaccdate <= end_of_Q[i] & vaccines.df$order == "boost",] %>% count(general))
  cat("\n \nKvartál:", names_of_columns[i], "\n")
  if(nrow(count_of_variants) != 0){
    for(j in 1:nrow(count_of_variants)){
      variant <- count_of_variants[j,1]
      sum_of_variants <- count_of_variants[j,2]
      cat(variant, "pocet: ", sum_of_variants, "\n")
      graph_07.df[variant, names_of_columns[i]] <- sum_of_variants
    }
  }
}


#
#graph_08 - 2nd booster
#
fields_for_graph <- unique(vaccines.df$general)
#create dataframe graph.df
graph_08.df <- data.frame(row.names = fields_for_graph)

# fill needed columns of dataframe with zeros
# create zero_column
zero_column <- vector(length(fields_for_graph), mode="integer")

# add the nuber of columns corresponding to number of quartals
for(column in 1:length(names_of_columns)){
  graph_08.df <- cbind(graph_08.df,zero_column)
}
colnames(graph_08.df) <- names_of_columns

#go through all columns and sumarize counts of all cases
for(i in 1:length(names_of_columns)){
  count_of_variants <- ( vaccines.df[vaccines.df$vaccdate >= start_of_Q[i] & vaccines.df$vaccdate <= end_of_Q[i] & vaccines.df$order == "secboost",] %>% count(general))
  cat("\n \nKvartál:", names_of_columns[i], "\n")
  if(nrow(count_of_variants) != 0){
    for(j in 1:nrow(count_of_variants)){
      variant <- count_of_variants[j,1]
      sum_of_variants <- count_of_variants[j,2]
      cat(variant, "pocet: ", sum_of_variants, "\n")
      graph_08.df[variant, names_of_columns[i]] <- sum_of_variants
    }
  }
}







write.csv(graph_01a.df, "./graph_01a_infections_intervals_disc.csv")
write.csv(graph_02a.df, "./graph_02a_infections_disc.csv")

write.csv(graph_04a.df, "./graph_04a_infections_serious.csv")
write.csv(graph_05a.df, "./graph_05a_infections_longcovid.csv")
write.csv(graph_06.df, "./graph_06_vaccination_full.csv")
write.csv(graph_07.df, "./graph_07_vaccination_booster.csv")
write.csv(graph_08.df, "./graph_08_vaccination_2nd_booster.csv")



# grafy 01


cases <- row.names.data.frame(graph_01a.df)

png(file="./graph_01a_inf_01_with_legend.png",
    width=800, height=720)
ggplot(graph_01a.df, aes(cases, graph_01a.df[ ,1] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1e6))+
  xlab("") + ylab("") + theme(panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(),
#                              legend.position = "none",
                              legend.text = element_text(size=30),
                              legend.key.height= unit(2, 'cm'),
                              legend.key.width= unit(2, 'cm'),
                              axis.line = element_line(colour = "black"), 
                              #                              axis.text.x = element_text(size = 10),
                              axis.text.x = element_blank(),
                              axis.text.y = element_text(size = 15),
                              axis.title.y = element_blank(),
                              axis.title.x = element_blank()
                              )
dev.off()


png(file="./graph_01a_inf_01_with_scale.png",
    width=540, height=400)
ggplot(graph_01a.df, aes(cases, graph_01a.df[ ,1] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_blank(), 
  axis.line = element_line(colour = "black"), 
#                              axis.text.x = element_text(size = 10),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 50),
  axis.title.y = element_blank(),
  axis.title.x = element_blank(),
  plot.margin = margin(1,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()



png(file="./graph_01a_inf_02.png",
    width=400, height=400)
ggplot(graph_01a.df, aes(cases, graph_01a.df[ ,2] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()



#png(file="./graph_01a_inf_01.png",
#    width=400, height=400)
#ggplot(graph_01a.df, aes(cases, graph_01a.df[ ,1] , fill = cases)) + 
#  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1e6))+
#  xlab("") + ylab("") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#  panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_blank(), 
#  axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())+
#  theme(legend.position = "none")
#dev.off()


png(file="./graph_01a_inf_03.png",
    width=400, height=400)
ggplot(graph_01a.df, aes(cases, graph_01a.df[ ,3] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_01a_inf_04.png",
    width=400, height=400)
ggplot(graph_01a.df, aes(cases, graph_01a.df[ ,4] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()
 
png(file="./graph_01a_inf_05.png",
    width=400, height=400)
ggplot(graph_01a.df, aes(cases, graph_01a.df[ ,5] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_01a_inf_06.png",
    width=400, height=400)
ggplot(graph_01a.df, aes(cases, graph_01a.df[ ,6] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_01a_inf_07.png",
    width=400, height=400)
ggplot(graph_01a.df, aes(cases, graph_01a.df[ ,7] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_01a_inf_08.png",
    width=400, height=400)
ggplot(graph_01a.df, aes(cases, graph_01a.df[ ,8] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_01a_inf_09.png",
    width=400, height=400)
ggplot(graph_01a.df, aes(cases, graph_01a.df[ ,9] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()






# grafy_02

cases <- row.names.data.frame(graph_02a.df)


png(file="./graph_02a_inf_01_with_legend.png",
    width=800, height=720)
ggplot(graph_02a.df, aes(cases, graph_02a.df[ ,1] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1e6))+
  xlab("") + ylab("") + theme(panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(),
                              #                              legend.position = "none",
                              legend.text = element_text(size=30),
                              legend.key.height= unit(2, 'cm'),
                              legend.key.width= unit(2, 'cm'),
                              axis.line = element_line(colour = "black"), 
                              #                              axis.text.x = element_text(size = 10),
                              axis.text.x = element_blank(),
                              axis.text.y = element_text(size = 15),
                              axis.title.y = element_blank(),
                              axis.title.x = element_blank()
  )
dev.off()

png(file="./graph_02a_inf_01_with_scale.png",
    width=540, height=400)
ggplot(graph_02a.df, aes(cases, graph_02a.df[ ,1] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 50),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(1,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()



png(file="./graph_02a_inf_02.png",
    width=400, height=400)
ggplot(graph_02a.df, aes(cases, graph_02a.df[ ,2] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()



#png(file="./graph_02a_inf_01.png",
#    width=400, height=400)
#ggplot(graph_02a.df, aes(cases, graph_02a.df[ ,1] , fill = cases)) + 
#  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1e6))+
#  xlab("") + ylab("") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#  panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_blank(), 
#  axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())+
#  theme(legend.position = "none")
#dev.off()


png(file="./graph_02a_inf_03.png",
    width=400, height=400)
ggplot(graph_02a.df, aes(cases, graph_02a.df[ ,3] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_02a_inf_04.png",
    width=400, height=400)
ggplot(graph_02a.df, aes(cases, graph_02a.df[ ,4] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_02a_inf_05.png",
    width=400, height=400)
ggplot(graph_02a.df, aes(cases, graph_02a.df[ ,5] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_02a_inf_06.png",
    width=400, height=400)
ggplot(graph_02a.df, aes(cases, graph_02a.df[ ,6] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_02a_inf_07.png",
    width=400, height=400)
ggplot(graph_02a.df, aes(cases, graph_02a.df[ ,7] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_02a_inf_08.png",
    width=400, height=400)
ggplot(graph_02a.df, aes(cases, graph_02a.df[ ,8] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_02a_inf_09.png",
    width=400, height=400)
ggplot(graph_02a.df, aes(cases, graph_02a.df[ ,9] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()
 





# grafy_04

cases <- row.names.data.frame(graph_04a.df)


png(file="./graph_04a_inf_01_with_legend.png",
    width=800, height=720)
ggplot(graph_04a.df, aes(cases, graph_04a.df[ ,1] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1e6))+
  xlab("") + ylab("") + theme(panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(),
                              #                              legend.position = "none",
                              legend.text = element_text(size=30),
                              legend.key.height= unit(2, 'cm'),
                              legend.key.width= unit(2, 'cm'),
                              axis.line = element_line(colour = "black"), 
                              #                              axis.text.x = element_text(size = 10),
                              axis.text.x = element_blank(),
                              axis.text.y = element_text(size = 15),
                              axis.title.y = element_blank(),
                              axis.title.x = element_blank()
  )
dev.off()

png(file="./graph_04a_inf_01_with_scale.png",
    width=540, height=400)
ggplot(graph_04a.df, aes(cases, graph_04a.df[ ,1] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 50),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(1,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()



png(file="./graph_04a_inf_02.png",
    width=400, height=400)
ggplot(graph_04a.df, aes(cases, graph_04a.df[ ,2] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()



#png(file="./graph_04a_inf_01.png",
#    width=400, height=400)
#ggplot(graph_04a.df, aes(cases, graph_04a.df[ ,1] , fill = cases)) + 
#  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1e6))+
#  xlab("") + ylab("") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#  panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_blank(), 
#  axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())+
#  theme(legend.position = "none")
#dev.off()


png(file="./graph_04a_inf_03.png",
    width=400, height=400)
ggplot(graph_04a.df, aes(cases, graph_04a.df[ ,3] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_04a_inf_04.png",
    width=400, height=400)
ggplot(graph_04a.df, aes(cases, graph_04a.df[ ,4] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_04a_inf_05.png",
    width=400, height=400)
ggplot(graph_04a.df, aes(cases, graph_04a.df[ ,5] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_04a_inf_06.png",
    width=400, height=400)
ggplot(graph_04a.df, aes(cases, graph_04a.df[ ,6] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_04a_inf_07.png",
    width=400, height=400)
ggplot(graph_04a.df, aes(cases, graph_04a.df[ ,7] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_04a_inf_08.png",
    width=400, height=400)
ggplot(graph_04a.df, aes(cases, graph_04a.df[ ,8] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_04a_inf_09.png",
    width=400, height=400)
ggplot(graph_04a.df, aes(cases, graph_04a.df[ ,9] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()











# grafy_05

cases <- row.names.data.frame(graph_05a.df)


png(file="./graph_05a_inf_01_with_legend.png",
    width=800, height=720)
ggplot(graph_05a.df, aes(cases, graph_05a.df[ ,1] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1e6))+
  xlab("") + ylab("") + theme(panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(),
                              #                              legend.position = "none",
                              legend.text = element_text(size=30),
                              legend.key.height= unit(2, 'cm'),
                              legend.key.width= unit(2, 'cm'),
                              axis.line = element_line(colour = "black"), 
                              #                              axis.text.x = element_text(size = 10),
                              axis.text.x = element_blank(),
                              axis.text.y = element_text(size = 15),
                              axis.title.y = element_blank(),
                              axis.title.x = element_blank()
  )
dev.off()

png(file="./graph_05a_inf_01_with_scale.png",
    width=540, height=400)
ggplot(graph_05a.df, aes(cases, graph_05a.df[ ,1] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 50),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(1,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()



png(file="./graph_05a_inf_02.png",
    width=400, height=400)
ggplot(graph_05a.df, aes(cases, graph_05a.df[ ,2] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()



#png(file="./graph_05a_inf_01.png",
#    width=400, height=400)
#ggplot(graph_05a.df, aes(cases, graph_05a.df[ ,1] , fill = cases)) + 
#  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1e6))+
#  xlab("") + ylab("") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#  panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_blank(), 
#  axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())+
#  theme(legend.position = "none")
#dev.off()


png(file="./graph_05a_inf_03.png",
    width=400, height=400)
ggplot(graph_05a.df, aes(cases, graph_05a.df[ ,3] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_05a_inf_04.png",
    width=400, height=400)
ggplot(graph_05a.df, aes(cases, graph_05a.df[ ,4] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_05a_inf_05.png",
    width=400, height=400)
ggplot(graph_05a.df, aes(cases, graph_05a.df[ ,5] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_05a_inf_06.png",
    width=400, height=400)
ggplot(graph_05a.df, aes(cases, graph_05a.df[ ,6] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_05a_inf_07.png",
    width=400, height=400)
ggplot(graph_05a.df, aes(cases, graph_05a.df[ ,7] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

png(file="./graph_05a_inf_08.png",
    width=400, height=400)
ggplot(graph_05a.df, aes(cases, graph_05a.df[ ,8] , fill = cases)) + 
  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
  xlab("") + ylab("") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(.5,0,1,0, "cm"))+
  theme(legend.position = "none")
dev.off()

#png(file="./graph_05a_inf_09.png",
#    width=400, height=400)
#ggplot(graph_05a.df, aes(cases, graph_05a.df[ ,9] , fill = cases)) + 
#  geom_bar(stat="identity", position = "dodge") + scale_y_continuous(expand = c(0,0), trans="log10", limits = c(1,1.2e6))+
#  xlab("") + ylab("") + 
#  theme(panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(),
#        panel.background = element_blank(), 
#        axis.line = element_line(colour = "black"), 
        #                              axis.text.x = element_text(size = 10),
#        axis.line.y = element_blank(),
#        axis.text.x = element_blank(),
#        axis.text.y = element_blank(),
#        axis.title.y = element_blank(),
#        axis.title.x = element_blank(),
#        plot.margin = margin(.5,0,1,0, "cm"))+
#  theme(legend.position = "none")
#dev.off()
















