# vaccines <- read.csv("vaccines.csv", header = TRUE)

# install.packages("skimr")
# library(skimr)

#summary(vaccines)
#skim(vaccines)

# infections <- read.csv("infections.csv", header = TRUE)
# summary(infections)
# print(sum(!is.na(infections$infdate)))
    

# skim(infections)

input = read.csv("data_20230421.csv", header = TRUE, sep=";")
summary(input)
print(sum(input$Umrti==""))


