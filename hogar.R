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

destino =c("Piazzale Aldo Moro")
# Colocar su API Key 
api_key = "AIzaSyCJ-6caC04NXNHDjpLq0BV2C9obZINR6bo"

for(i in seq(1:nrow(mydata)))
{
  origen = mydata$Dirección[i]
  api_url = get_url(origen, destino, api_key)
  datos = get_data(api_url)
  retorno = parse_data(datos)
  
  if(retorno$status == "OK")
  {
  
  
    aux1 = strsplit(as.character(retorno$duration$text), " ")
    aux1 = paste(aux1[[1]], collapse = "")
    aux1 = strsplit(as.character(aux1), "min")
    aux1 = strsplit(as.character(aux1), "h")
    
    if (length(aux1[[1]]) == 1)
    {
      mydata$Distance[i] = aux1[[1]][1]
    }
    else
    {
      mydata$Distance[i] = (as.integer(aux1[[1]][1])*60) + as.integer(aux1[[1]][2])
      
    }
  }else
  {
    mydata$Distance[i] = 0;
  }
}

(as.integer(aux1[[1]][1])*60) + as.integer(aux1[[1]][2])

destino =c("Piazzale Aldo Moro")

# Colocar su API Key 
api_key = "AIzaSyCJ-6caC04NXNHDjpLq0BV2C9obZINR6bo"

api_url = get_url(origen, destino, api_key)

datos = get_data(api_url)

a = parse_data(datos)
m = strsplit(a$duration$text, " ")
n = paste(m[[1]], collapse = "")
m = strsplit(n, "min")

m[1]
