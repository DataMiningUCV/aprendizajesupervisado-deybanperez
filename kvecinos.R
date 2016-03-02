install = function(pkg){
  # Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

install("ggvis")
library("ggvis")

mydata = read.csv("minable.csv") #Loading the data

mydata[,1] = NULL
mydata[,1] = NULL
mydata[,14:51] = list(NULL) #Deleting columns without important information




#Splitting data into training and testing sets
sub = sample(nrow(mydata), floor(nrow(mydata) * 0.8)) #Defining indexs
training <- mydata[sub, ] #Creating 
testing <- mydata[-sub, ]

training %>% ggvis(~pAprobado, ~eficiencia, fill = ~mIngreso) %>% layer_points()