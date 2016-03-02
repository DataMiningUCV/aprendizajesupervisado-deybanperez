# Installing dependecies in Linux 
# sudo apt-get install libgtk2.0-dev


install = function(pkg){
  # Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

install("RGtk2")
install("MicroStrategyR")
install("party")
install("rpart")
install("rattle")
install("rpart.plot")
install("RColorBrewer")
library("rpart")
library("party")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
mydata = read.csv("minable.csv") #Loading the data


mydata[,1] = NULL
mydata[,1] = NULL
mydata[,14:51] = list(NULL) #Deleting columns without important information




#Splitting data into training and testing sets
sub = sample(nrow(mydata), floor(nrow(mydata) * 0.8)) #Defining indexs
training <- mydata[sub, ] #Creating 
testing <- mydata[-sub, ]

tree = rpart(mIngreso ~ ., dat = training, method = "class", control = rpart.control(minsplit = 5, cp = 0.005, maxdepth = 6))
fancyRpartPlot(tree)

table(predict(tree, newdata = testing,type = "class"), testing$mIngreso)

ptree = prune(tree, cp = tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"])
fancyRpartPlot(ptree)
table(predict(ptree, newdata = testing,type = "class"), testing$mIngreso)
