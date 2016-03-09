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
#Fixing typos in som Directions
mydata$Dirección[11] = "Galliate"
mydata$Dirección[33] = "Via San Roberto Bellarmino"
mydata$Dirección[61] = "Via di Monte Verde"
##################################################################################
#Adding Distance column to dataset
mydata$Time = 0
##################################################################################
#setting goal position
destino =c("Piazzale Aldo Moro")
##################################################################################
#Setting API key
api_key = "AIzaSyCJ-6caC04NXNHDjpLq0BV2C9obZINR6bo"
#################################################################################
#Cicle to fill time column
for(i in seq(1:nrow(mydata)))
{
  #Setting start position
  origen = mydata$Dirección[i]
  #Making the URL
  api_url = get_url(origen, destino, api_key)
  #Getting the data from API
  datos = get_data(api_url)
  #PArsing Json to Dataframe
  retorno = parse_data(datos)
  #If the response from API is not NULL
  if(retorno$status == "OK")
  {
    #Making split to transform hours to mins
    aux1 = strsplit(as.character(retorno$duration$text), " ")
    aux1 = paste(aux1[[1]], collapse = "")
    aux1 = strsplit(as.character(aux1), "min")
    aux1 = strsplit(as.character(aux1), "h")
    #If is only has minutes
    if (length(aux1[[1]]) == 1)
    {
      mydata$Time[i] = aux1[[1]][1]
    }
    else #If has hours
    {
      mydata$Time[i] = (as.integer(aux1[[1]][1])*60) + as.integer(aux1[[1]][2])
      
    }
  }
}
####################################################################################################################
#Numerizing column Notas, 0 for boys, 1 for girls, 2 for both
mydata$Notas = as.character(mydata$Notas)
mydata$Notas[grepl("(ragazzi)", mydata$Notas)] =  as.character(0)
mydata$Notas[grepl("(ragazze)", mydata$Notas)] =  as.character(1)
mydata$Notas[grepl("(ragazze/i) | (ragazzi/e) | (ragazzi/ragazze) | (ragazze/ragazzi)", mydata$Notas)] =  as.character(2)
mydata$Notas[39] = as.character(3)
mydata$Notas = as.factor(mydata$Notas)
#####################################################################################################################
#Numerizing column Tipo.de.Inmueble, 0 fo Apartments, 1 for mini Apartments, 2 for Monolocale
levels(mydata$Tipo.de.Inmueble)
mydata$Tipo.de.Inmueble = as.character(mydata$Tipo.de.Inmueble)
mydata$Tipo.de.Inmueble[mydata$Tipo.de.Inmueble == "Apartamento"] = as.character(0)
mydata$Tipo.de.Inmueble[mydata$Tipo.de.Inmueble == "Apparrtamento"] = as.character(0)
mydata$Tipo.de.Inmueble[mydata$Tipo.de.Inmueble == "Appartameno"] = as.character(0)
mydata$Tipo.de.Inmueble[mydata$Tipo.de.Inmueble == "Appartamenti"] = as.character(0)
mydata$Tipo.de.Inmueble[mydata$Tipo.de.Inmueble == "Appartamento"] = as.character(0)
mydata$Tipo.de.Inmueble[mydata$Tipo.de.Inmueble == "Mini appartamento"] = as.character(1)
mydata$Tipo.de.Inmueble[mydata$Tipo.de.Inmueble == "Mini\nAppartamento"] = as.character(1)
mydata$Tipo.de.Inmueble[mydata$Tipo.de.Inmueble == "Monolocale"] = as.character(2)
mydata$Tipo.de.Inmueble = as.factor(mydata$Tipo.de.Inmueble)
######################################################################################################################
#Creating otehr feature to avaluate
mydata$Ingresso = 0
mydata$Soggiorno = 0
mydata$Cucina = 0
mydata$Bagno = 0
mydata$Salone = 0
mydata$Disimpegno = 0
mydata$Corridoio = 0
mydata$Internet = 0
mydata$Ripostiglio = 0
mydata$Balcone = 0
mydata$Termo = 0
mydata$Terrazzo = 0
#######################################################################################################################
#Filling new features with description 
#######################################################################################################################
#Filling Ingresso column (entrada)
mydata$Ingresso[grepl("(Ingresso)", mydata$Descripción)] = 1
mydata$Ingresso[grepl("(ingresso)", mydata$Descripción)] = 1
#######################################################################################################################
#Filling Soggiorno column (Sala de estar)
mydata$Soggiorno[grepl("(soggiorno)", mydata$Descripción)] = 1
#######################################################################################################################
#Filling Cucina column (Cocina)
mydata$Cucina[grepl("(cucina)", mydata$Descripción)] = 1
mydata$Cucina[grepl("(cucna)", mydata$Descripción)] = 1
mydata$Cucina[grepl("(cottura)", mydata$Descripción)] = 1
#######################################################################################################################
#Filling Bagno column (Bano)
#######################################################################################################################
# For 1 bagno
mydata$Bagno[grepl("(bagno)", mydata$Descripción)] = 1
mydata$Bagno[grepl("(1 bagno)", mydata$Descripción)] = 1
#For 2 bagni
mydata$Bagno[grepl("(2 bagni)", mydata$Descripción)] = 2
mydata$Bagno[grepl("(2bagni)", mydata$Descripción)] = 2
#For 3 bagni
mydata$Bagno[grepl("(3 bagni)", mydata$Descripción)] = 3
mydata$Bagno[grepl("(3bagni)", mydata$Descripción)] = 3
#For 4 bagni
mydata$Bagno[grepl("(4 bagni)", mydata$Descripción)] = 4
mydata$Bagno[grepl("(4bagni)", mydata$Descripción)] = 4
#######################################################################################################################
#Filling Salone column (Sala de estar)
mydata$Salone[grepl("(salone)", mydata$Descripción)] = 1
mydata$Salone[grepl("(salotto)", mydata$Descripción)] = 1
mydata$Salone[grepl("(sala da pranzo)", mydata$Descripción)] = 1
#######################################################################################################################
#Filling Disimpegno column (Salida de emergencia)
mydata$Disimpegno[grepl("(disimpegno)", mydata$Descripción)] = 1
#######################################################################################################################
#Filling Corridoio column (Corredor)
mydata$Corridoio[grepl("(corridoio)", mydata$Descripción)] = 1
#######################################################################################################################
#Filling internet column (internet)
mydata$Internet[grepl("(internet)", mydata$Descripción)] = 1
#######################################################################################################################
#Filling ripostiglio column (Armario)
mydata$Ripostiglio[grepl("(ripostiglio)", mydata$Descripción)] = 1
#######################################################################################################################
#Filling Balcone column (Balcon)
mydata$Balcone[grepl("(balcone)", mydata$Descripción)] = 1
mydata$Balcone[grepl("(balconcino)", mydata$Descripción)] = 1
#######################################################################################################################
#Filling termo column (calefaccion)
mydata$Termo[grepl("(termo)", mydata$Descripción)] = 1
#######################################################################################################################
#Filling Terrazo column (terraza)
mydata$Terrazzo[grepl("(terrazzo)", mydata$Descripción)] = 1
#######################################################################################################################

grepl("(living)",mydata$Descripción[3])
