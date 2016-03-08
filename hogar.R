#begin
#################################################################################
# Seleccionar google_api.R en su sistema de archivos
source(file.choose())
#################################################################################
# Installing required packages
install("curl")
install("xlsx")
#################################################################################
#loading required libraries
library("xlsx")
#################################################################################
#Loading dataset
mydata = read.xlsx("hogares.xlsx", sheetIndex = 1, startRow = 1, endRow = 104, header = T)
#################################################################################
#Preprocessing Dataset
#################################################################################
#Deleting irrelevant column
mydata$Foto = NULL
mydata$Distrito = NULL
mydata$Piso = NULL
#################################################################################
#Removing \n in Column Direction

mydata$Dirección = strsplit(as.character(mydata$Dirección), "\n")

for(i in seq(1:nrow(mydata)))
{
  mydata$Dirección[i] = paste(mydata$Dirección[[i]], collapse = " ")
  
}
##################################################################################
#Converting Direccion Column into factor type
mydata$Dirección = as.character(mydata$Dirección)
##################################################################################
#Adding Distamce column to dataset
mydata$Distance = seq(1:nrow(mydata))
##################################################################################
origen = mydata$Dirección[1]

destino =c("Piazzale Aldo Moro")

# Colocar su API Key 
api_key = "AIzaSyBgTslqWxmNPlZx9-LvmeDrGRnVq8AEXyI"

api_url = get_url(origen, destino, api_key)

datos = get_data(api_url)
