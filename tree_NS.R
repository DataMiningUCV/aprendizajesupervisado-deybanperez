# Installing dependecies in Linux 
# sudo apt-get install libgtk2.0-dev


install = function(pkg)
{
  # Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}


normalize = function(x)
{
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

install("RGtk2")
install("MicroStrategyR")
install("party")
install("rpart")
install("rattle")
install("rpart.plot")
install("RColorBrewer")
install("FactoMineR")
library("rpart")
library("party")
library("rattle")
library("rpart.plot")
library("RColorBrewer")
library("FactoMineR")


mydata = read.csv("minable.csv") #Loading the data

#Deleting no important and string features
mydata[,"cIdentidad"] = NULL
mydata[,"fNacimiento"] = NULL
mydata[,"jReprobadas"] = NULL
mydata[,"pReside"] = NULL
mydata[,"cDireccion"] = NULL
mydata[,"dHabitacion"] = NULL
mydata[,"oSolicitudes"] = NULL
mydata[,"aEconomica"] = NULL
mydata[,"rating"] = NULL
mydata[,"sugerencias"] = NULL
mydata[,"grOdontologicos"] = NULL
mydata = mydata[-1,]


#Splitting data into training and testing sets
sub = sample(nrow(mydata), floor(nrow(mydata) * 0.8),replace = F)
training <- mydata[sub, ] #Creating 
testing <- mydata[-sub, ]

tree = rpart(mIngreso ~ ., dat = training, method = "class", control = rpart.control(minsplit = 10, cp = 0.001, maxdepth = 4))
fancyRpartPlot(tree)

confusionMatrix = table(testing$mIngreso, predict(tree, newdata = testing,type = "class"))
confusionMatrix

acierto = ((confusionMatrix[1,1] + confusionMatrix[2,2] + confusionMatrix[3,3]) / nrow(testing)) *100
acierto

error = 100 - acierto
error