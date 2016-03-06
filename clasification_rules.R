# Begin
# Installing dependecies in Linux: 
  # sudo apt-get install libgtk2.0-dev, necessary for fancyplot
  #apt-get install r-cran-rjava

#Functions definitions
##########################################
install = function(pkg)
{
  # Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

###########################################
normalize = function(x)
{
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
#########################################
#Packages needed
install("RGtk2")
install("MicroStrategyR")
install("party")
install("rpart")
install("rattle")
install("rpart.plot")
install("RColorBrewer")
install("FactoMineR")
########################################
#Importing libraries
library("rpart")
library("party")
library("rattle")
library("rpart.plot")
library("RColorBrewer")
library("FactoMineR")
##########################################

#Loading the data
mydata = read.csv("minable.csv")
##################################################
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
mydata = mydata[-1,] #Deleting outlier
################################################

#Calculating probabilities for each element into dataset
prob_0 = 1/sum(mydata[,"mIngreso"] == 0);
prob_2 = 1/sum(mydata[,"mIngreso"] == 2);
prob_3 = 1/sum(mydata[,"mIngreso"] == 3);
###################################################
#Allocating space for vector of probabilities
probabilities = seq(1,nrow(mydata),1)
####################################################

aux0 = 0 #Constant for numebr 0
aux2 = 2 #Constant for number 2
aux3= 3 #Constant for number 3
######################################################
#Filling probabilities for each position in data set 
for (i in nrow(mydata))
{
  aux = mydata$mIngreso[i]
  
  if(aux0 == aux)
  {
    probabilities[i] = prob_0
    
  }else if(aux2 == aux)
  {
    probabilities[i] = prob_2
    
  }else if(aux3 == aux)
  {
    probabilities[i] = prob_3
  }
  
}
########################################################
#Splitting data into training and testing sets
sub = sample(nrow(mydata), floor(nrow(mydata) * 0.8), prob = probabilities, replace = F)
training <- mydata[sub, ]
testing <- mydata[-sub, ]
######################################################
#Visializing proportions
sum(training[,"mIngreso"] == 0)
sum(training[,"mIngreso"] == 2)
sum(training[,"mIngreso"] == 3)
#######################################################
training$mIngreso = as.factor(training$mIngreso)
testing$mIngreso = as.factor(testing$mIngreso)
rules = JRip(formula = mIngreso ~ ., data = training)
#######################################################################
#Creating confusion matrix
confusionMatrixClasification = table(testing$mIngreso, predict(rules, newdata = testing,type = "class"))
#Visualizing Confusion matriz
confusionMatrixClasification
##################################################################
#Calculating hit rate
hitRateClasification = ((confusionMatrixClasification[1,1] + confusionMatrixClasification[2,2] + confusionMatrixClasification[3,3]) / nrow(testing)) *100
#Visualizing hit rate
hitRateClasification
####################################################################
#Calculating error rating
errorRateClasification = 100 - hitRateClasification
#Visualizing error rate
errorRateClasification
#################################################################}
#END
