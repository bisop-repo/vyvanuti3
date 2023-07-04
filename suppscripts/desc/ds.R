# vaccines <- read.csv("vaccines.csv", header = TRUE)

# install.packages("skimr")
# library(skimr)

#summary(vaccines)
#skim(vaccines)

# infections <- read.csv("infections.csv", header = TRUE)
# summary(infections)
# print(sum(!is.na(infections$infdate)))
    

# skim(infections)





inputu= read.csv("../../v3/infvacc/rinput.csv.unprocessed.csv", header = TRUE, sep=",")
summary(inputu)


o1table <- table(inputu$OckovaciLatkaKod1)
write.csv(o1table, file="uol1.csv")
o1table <- table(inputu$OckovaciLatkaKod2)
write.csv(o1table, file="uol2.csv")
o1table <- table(inputu$OckovaciLatkaKod3)
write.csv(o1table, file="uol3.csv")
o1table <- table(inputu$OckovaciLatkaKod4)
write.csv(o1table, file="uol4.csv")

xxxx

 input = read.csv("../../data/data_20230629.csv", header = TRUE, sep=";")
# input = read.csv("../../data/part/datapart.csv", header = TRUE, sep=";")
# summary(input)
# print(sum(input$Umrti==""))

o1table <- table(input$OckovaciLatkaKod1)
write.csv(o1table, file="ol1.csv")
o1table <- table(input$OckovaciLatkaKod2)
write.csv(o1table, file="ol2.csv")
o1table <- table(input$OckovaciLatkaKod3)
write.csv(o1table, file="ol3.csv")
o1table <- table(input$OckovaciLatkaKod4)
write.csv(o1table, file="ol4.csv")
