mydata = read.csv("minable.csv") #Loading the data

mydata[,1] = NULL
mydata[,15:52] = list(NULL) #Deleting columns without important information

#Splitting data into training and testing sets
sub = sample(nrow(mydata), floor(nrow(mydata) * 0.8)) #Defining indexs
training <- mydata[sub, ] #Creating 
testing <- mydata[-sub, ]

plot(training)

table(training$mIngreso, training$fNacimiento)
table(training$mIngreso, training$sexo)
table(training$mIngreso, training$eCivil)
table(training$mIngreso, training$escuela)
table(training$mIngreso, training$aIngreso)
table(training$mIngreso, training$sCurso)
table(training$mIngreso, training$tGrado)
table(training$mIngreso, training$mInscritas)
table(training$mIngreso, training$pAprobado)
table(training$mIngreso, training$eficiencia)
table(training$mIngreso, training$mAprobadas)
table(training$mIngreso, training$mRetiradas)
table(training$mIngreso, training$mReprobadas)

table(training$mIngreso)
png(file = "decision_tree.png")
outputTree = ctree(mIngreso ~ sexo, data = training)
plot(outputTree)
dev.off()