# Begin
# Installing dependecies in Linux: 
  # sudo apt-get install libgtk2.0-dev, necessary for fancyplot
  #apt-get install r-cran-rjava

#Functions definitions
########################################################################
#Funcion para instalar paquetes si no se tienen
install = function(pkg)
{
  # Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

#######################################################################
#Funcion para normalizar una columna de un data frame
normalize = function(x)
{
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

#######################################################################
#Packages needed
install("RGtk2")
install("MicroStrategyR")
install("party")
install("rpart")
install("rattle")
install("rpart.plot")
install("RColorBrewer")
install("FactoMineR")
install("class")
install("RWeka")

#######################################################################
#Importing libraries
library("rpart")
library("party")
library("rattle")
library("rpart.plot")
library("RColorBrewer")
library("FactoMineR")
library("class")
library("RWeka")
#########################################################################
#Loading the data
mydata = read.csv("minable.csv")
#########################################################################
#Deleting no important and string features
mydata[,"cIdentidad"] = NULL
mydata[,"fNacimiento"] = NULL
mydata[,"jReprobadas"] = NULL
mydata[,"cDireccion"] = NULL
mydata[,"dHabitacion"] = NULL
mydata[,"oSolicitudes"] = NULL
mydata[,"aEconomica"] = NULL
mydata[,"rating"] = NULL
mydata[,"sugerencias"] = NULL
mydata[,"eCivil"] = NULL
mydata = mydata[-1,] #Deleting outlier
############################################################################
#Fixing errors in dataset
mydata$pReside[2] = 5
mydata$pReside[26] = 5
mydata$pReside = as.integer(mydata$pReside)
mydata$grOdontologicos = as.integer(mydata$grOdontologicos)
############################################################################
#Calculating probabilities for each element into dataset
prob_0 = 1/sum(mydata[,"mIngreso"] == 0);
prob_2 = 1/sum(mydata[,"mIngreso"] == 2);
prob_3 = 1/sum(mydata[,"mIngreso"] == 3);
############################################################################
#Allocating space for vector of probabilities
probabilities = seq(1,nrow(mydata),1)
############################################################################
aux0 = 0 #Constant for numebr 0
aux2 = 2 #Constant for number 2
aux3= 3 #Constant for number 3
############################################################################
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
##############################################################################
#Splitting data into training and testing sets
set.seed(777)
sub = sample(nrow(mydata), floor(nrow(mydata) * 0.8), prob = probabilities, replace = F)
training <- mydata[sub, ]
testing <- mydata[-sub, ]
##############################################################################
#Visializing proportions for training
sum(training[,"mIngreso"] == 0)
sum(training[,"mIngreso"] == 2)
sum(training[,"mIngreso"] == 3)
#And testing
#Visializing proportions for training
sum(testing[,"mIngreso"] == 0)
sum(testing[,"mIngreso"] == 2)
sum(testing[,"mIngreso"] == 3)
##############################################################################
#Creating decission tree model to predict m Ingreso
tree = rpart(mIngreso ~ ., data = training, method = "class", control = rpart.control(minsplit = 10, cp = 0.001, maxdepth = 3))
#Plotting model
fancyRpartPlot(tree)
##############################################################################
#Creating confusion matrix tree
confusionMatrixTree = table(testing$mIngreso, predict(tree, newdata = testing,type = "class"))
#Visualizing Confusion matriz
confusionMatrixTree
##############################################################################
#Calculating hit rate tree
hitRateTree = ((confusionMatrixTree[1,1] + confusionMatrixTree[2,2] + confusionMatrixTree[3,3]) / nrow(testing)) *100
#Visualizing hit rate tree
hitRateTree
##############################################################################
#Calculating error rating tree
errorRateTree = 100 - hitRateTree
#Visualizing error rate
errorRateTree
##############################################################################
# Beginning k nearest neighbor model
##############################################################################
#Extracting labels for traing and test
trainingLabels = training$mIngreso
testingLabels = testing$mIngreso
##############################################################################
#Deleting rows to be predicted
trainingClasificationRules = training
testingClasificationRules = testing
auxPCA = training
training[,"mIngreso"] = NULL
testing[,"mIngreso"] = NULL
##############################################################################
#Normalizing training and testing sets
training_norm = as.data.frame(lapply(training[1:ncol(training)], normalize))
testing_norm = as.data.frame(lapply(testing[1:ncol(testing)], normalize))
##############################################################################
#Generating knn model
mydata_pred <- knn(train = training_norm, test = testing_norm, cl = trainingLabels, k=14)
##############################################################################
#Creating confusion matrix
confusionMatrixK = table(testingLabels, mydata_pred)
#Visualizing Confusion matriz
confusionMatrixK
#############################################################################
#Calculating hit rate
hitRateK = ((confusionMatrixK[1,1] + confusionMatrixK[2,2] + confusionMatrixK[3,3]) / nrow(testing)) *100
#Visualizing hit rate
hitRateK
##############################################################################
#Calculating error rating
errorRateK = 100 - hitRateK
#Visualizing error rate
errorRateK
##############################################################################
# Beginning clasification rules
###############################################################################
#Transforming column into factor type
trainingClasificationRules$mIngreso = as.factor(trainingClasificationRules$mIngreso)
testingClasificationRules$mIngreso = as.factor(testingClasificationRules$mIngreso)
###############################################################################
#Generating Clasification Rules model
rules = JRip(formula = mIngreso ~ ., data = trainingClasificationRules)
##############################################################################
#Creating confusion matrix
confusionMatrixClasification = table(testingClasificationRules$mIngreso, predict(rules, newdata = testingClasificationRules,type = "class"))
#Visualizing Confusion matriz
confusionMatrixClasification
################################################################################
#Calculating hit rate
hitRateClasification = ((confusionMatrixClasification[1,1] + confusionMatrixClasification[2,2] + confusionMatrixClasification[3,3]) / nrow(testing)) *100
#Visualizing hit rate
hitRateClasification
#################################################################################
#Calculating error rating
errorRateClasification = 100 - hitRateClasification
#Visualizing error rate
errorRateClasification
#################################################################################
#END