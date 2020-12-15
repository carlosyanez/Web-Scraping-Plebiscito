###################
# CARGAR PAQUETES #
###################

library(RSelenium)
library(rvest)
library(tidyverse)
library(sf)
library(stringi)

#######################
# CONECTAR A SELENIUM #
#######################

remDr <- rsDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browser = "firefox"
)

rd <- remDr$client

# Ir a la web del SERVEL
url <- 'http://pv.servelelecciones.cl/'     #Resultados plebiscito (página principal contiene ahora primarias)
#rd$open()
rd$navigate(url)

# Ir a la pag específica de la extracción
rd$findElement(using = "css", 
               value = ".menu_division > li:nth-child(3) > a:nth-child(1)"
)$clickElement()
# body > div.container.body.ng-scope > div:nth-child(1) > div.col-md-3.margen_cero.visible-desktop.visible-tablet > div > ul > li:nth-child(3) > a

#####################################
# Crear listas de regiones y comuna #
#####################################
# Función de ayuda
limpiar_lista <- function(x){
  x %>% 
    str_extract_all(">(.*?)<") %>% 
    data.frame() %>% as_tibble() %>% 
    rename(nombre = 1) %>% 
    filter(nombre != "><",
           !str_detect(nombre, "\\.\\.\\.")) %>% 
    mutate(nombre = str_remove(nombre, ">"),
           nombre = str_remove(nombre, "<")) %>% 
    mutate(inicial = str_sub(nombre, 1, 1)) %>% 
    arrange(inicial)
}

# Objeto que identifica la lista de regiones y comunas
webElemRegion <- rd$findElement(using = "css", 
                                value = "#selRegion")

# Extraer lista y limpiar
lista_regiones <- webElemRegion$getElementAttribute("outerHTML") %>% 
  limpiar_lista()

# Extraer lista de comunas para cada región
reg_com <- tibble()
for (r in seq_along(lista_regiones$inicial)){
  
  # inicial de la region
  ini_reg <- lista_regiones$inicial[r]
  
  Sys.sleep(2)
  
  # insertar inicial en menú desplegable de web
  webElemRegion <- rd$findElement(using = "css", 
                                  value = "#selRegion")
  webElemRegion$sendKeysToElement(list(ini_reg))
  
  Sys.sleep(2)
  
  # Generar lista de comunas
  webElemComuna <- rd$findElement(using = "css", 
                                  value = "#selComunas")
  lista_comunas <- webElemComuna$getElementAttribute("outerHTML") %>% 
    limpiar_lista()
  
  Sys.sleep(2)
  
  com_r <- lista_comunas %>% 
    mutate(reg = lista_regiones$nombre[r])
  
  Sys.sleep(2)
  
  reg_com <- bind_rows(reg_com, com_r)
}

reg_com <- reg_com %>% 
  arrange(nombre)

## ---------------------------------
## GENERAR DATA FRAME CON RESULTADOS
## ---------------------------------

rd$navigate(url)

# Ir a la pag específica de la extracción
rd$findElement(using = "css", 
               value = ".menu_division > li:nth-child(3) > a:nth-child(1)"
)$clickElement()

datos_comuna <- tibble() # data frame que almacenará resultados
for (i in 1:nrow(reg_com)){
  
  # inicial de la comuna
  c <- reg_com[i,]$inicial
  
  Sys.sleep(2)
  
  # insertar inicial en menú desplegable de web
  webElemComuna <- rd$findElement(using = "css", 
                                  value = "#selComunas")
  webElemComuna$sendKeysToElement(list(c))
  
  Sys.sleep(2)
  
  # Votos apruebo
  webElemApruebo <- rd$findElement(using = "css", value = "tr.nivelUno:nth-child(2) > td:nth-child(3) > small:nth-child(1) > span:nth-child(1)")
  apruebo <- webElemApruebo$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()
  
  Sys.sleep(2)
  
  # Votos rechazo
  webElemRechazo <- rd$findElement(using = "css", value = "#basic-table > table > tbody:nth-child(2) > tr:nth-child(5) > td:nth-child(3) > small > span")
  rechazo <- webElemRechazo$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()
  
  Sys.sleep(2)
  
  # Votos nulo
  webElemNulo <- rd$findElement(using = "css", value = "#basic-table > table > tfoot > tr:nth-child(2) > th:nth-child(2) > strong")
  nulo <- webElemNulo$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()  
  
  Sys.sleep(2)
  
  # Votos blanco
  webElemBlanco <- rd$findElement(using = "css", value = "#basic-table > table > tfoot > tr:nth-child(3) > th:nth-child(2) > strong")
  blanco <- webElemBlanco$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()   
  
  # Ingresar valores en data frame
  datos_comuna[i,1] <- reg_com[i,]$reg
  datos_comuna[i,2] <- reg_com[i,]$nombre
  datos_comuna[i,3] <- apruebo
  datos_comuna[i,4] <- rechazo
  datos_comuna[i,5] <- nulo
  datos_comuna[i,6] <- blanco
  
  # Reportar en cada iteración resultados
  datos_comuna %>% 
    slice(i) %>% print()
  
  cbind(nrow(dplyr::distinct(dplyr::select(datos_comuna, `...1`))), nrow(datos_comuna)) %>% print()
  
  Sys.sleep(2)
}

# Ajustar y exportar data frame
datos_comuna <- datos_comuna %>% 
  rename("Region" = 1,
         "Comuna" = 2,
         "Apruebo" = 3,
         "Rechazo" = 4,
         "Nulo" = 5,
         "Blanco" = 6
  ) %>%
  mutate(Region = str_remove(Region, "DE "),
         Region = str_remove(Region, "DEL "),
         Validos = Apruebo+Rechazo,
         Total = Validos+Nulo+Blanco,
         Validos_per = Validos/Total,
         Apruebo_per = (Apruebo/Validos)*100,
         Rechazo_per = (Rechazo/Validos)*100,
         Nulo_per = (Nulo/Total)*100,
         Blanco_per = (Blanco/Total)*100) %>% 
  select(Region, Comuna, Total, Validos, Apruebo, Rechazo, Nulo, Blanco, 
         Validos_per, Apruebo_per, Rechazo_per, Nulo_per, Blanco_per) %>% 
  dplyr::arrange(Region, Comuna) 





## ---------------------------------
## GENERAR DATA FRAME PARTICIPACION
## ---------------------------------

rd$navigate(url)

# Ir a la pag específica de la extracción
rd$findElement(using = "css", 
               value = "li.hidden-xs:nth-child(3)"
)$clickElement()

rd$findElement(using = "css", 
               value = ".menu_division > li:nth-child(3) > a:nth-child(1)"
)$clickElement()


datos_participacion <- tibble() # data frame que almacenará resultados
for (i in 1:nrow(reg_com)){
  
  # inicial de la comuna
  c <- reg_com[i,]$inicial
  
  Sys.sleep(2)
  
  # insertar inicial en menú desplegable de web
  webElemComuna <- rd$findElement(using = "css", 
                                  value = "#selComunas")
  webElemComuna$sendKeysToElement(list(c))
  
  Sys.sleep(1)
  
  # Votos Electores
  webElemElectores <- rd$findElement(using = "css", value = ".table > tfoot:nth-child(4) > tr:nth-child(1) > th:nth-child(3) > strong:nth-child(1)")
  electores <- webElemElectores$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()
  
  Sys.sleep(2)
  
  # Votos Participacion
  webElemParticipacion<- rd$findElement(using = "css", value = ".table > tfoot:nth-child(4) > tr:nth-child(1) > th:nth-child(4) > strong:nth-child(1)")
  participacion <- webElemParticipacion$getElementText()[[1]] %>% 
    str_remove("\\.") %>% 
    as.numeric()
  
  Sys.sleep(2)
  
 
  
  # Ingresar valores en data frame
  datos_participacion[i,1] <- reg_com[i,]$nombre
  datos_participacion[i,2] <- electores
  datos_participacion[i,3] <- participacion
  
  
  # Reportar en cada iteración resultados
  datos_participacion %>% 
    slice(i) %>% print()
  
  cbind(nrow(dplyr::distinct(dplyr::select(datos_participacion, `...1`))), nrow(datos_participacion)) %>% print()
  
  Sys.sleep(2)
}

# Ajustar y exportar data frame
datos_participacion <- datos_participacion %>% 
  rename("Comuna" = 1,
         "Electores" = 2,
         "Participacion" = 3
  ) %>%
  mutate(Participacion_per = Participacion/Electores) %>% 
  dplyr::arrange(Comuna) 

datos_comuna <- datos_comuna %>% left_join(datos_participacion,by="Comuna")


## ---------------------------------
## mapa vectorial de comunas de Chile : Fuente a la Biblioteca del Congreso Nacional de Chile. 
## ---------------------------------



download.file("https://www.bcn.cl/obtienearchivo?id=repositorio/10221/10396/2/Comunas.zip","comunas/comunas.zip")
unzip("comunas/comunas.zip",exdir = "comunas/")

#obtener datos de comunas de df vectorial (util para referencia futura)

comunas_geo <- st_read("comunas/comunas.shp")
st_geometry(comunas_geo) <- NULL
comunas_data <- comunas_geo %>% select(Comuna, Region, Provincia,cod_comuna,codregion) %>%
                                mutate(Comuna_Match=stri_trans_general(str = tolower(Comuna), id = "Latin-ASCII")) 

datos_comuna_export <-datos_comuna %>% mutate(Comuna_Match=stri_trans_general(str = tolower(Comuna), id = "Latin-ASCII"),
                           Comuna_Servel=Comuna) %>% 
                          select(-Comuna,-Region) %>%
                          mutate(Comuna_Match=str_replace_all(Comuna_Match,"trehuaco","treguaco"),                            #special cases
                                 Comuna_Match=str_replace_all(Comuna_Match,"ohiggins","o'higgins"),
                                 Comuna_Match=str_replace_all(Comuna_Match,"\\(ex-navarino\\)","")) %>%                                      
                          left_join(comunas_data,by="Comuna_Match") %>%
                          mutate(Region=str_replace_all(Region,"Región del ",""),
                                 Region=str_replace_all(Region,"Región de ",""),
                                 Region=str_replace_all(Region,"Región Metropolitana de Santiago","Metropolitana")) %>%
                          select(codregion,cod_comuna,Region,Comuna,Total,Validos,Apruebo,Rechazo,Nulo,Blanco,
                                 Validos_per,Apruebo_per,Rechazo_per,Nulo_per,Blanco_per,
                                 Electores,Participacion,Participacion_per,Comuna_Servel) %>%
                          arrange(desc(cod_comuna))  %>%
                          mutate(Comuna=ifelse(Comuna_Servel=="ANTARTICA","Antartica",Comuna),                             #Antartica (not in geo file - not a LGA)
                                 Region=ifelse(Comuna_Servel=="ANTARTICA","Magallanes y Antártica Chilena",Region),
                                 codregion=ifelse(Comuna_Servel=="ANTARTICA",12,codregion),
                                 cod_comuna=ifelse(Comuna_Servel=="ANTARTICA",0,cod_comuna))
  
# Comuna_Servel - se mantiene para comparación con resultados históricos SERVEL 

datos_comuna_export%>% write_excel_csv("datos/DatosPlebiscito.csv")

rd$closeServer();rd$close();remDr$server$stop()

