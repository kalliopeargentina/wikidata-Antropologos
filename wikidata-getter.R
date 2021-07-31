library(WikidataR)
#install.packages("WikidataQueryServiceR")
source('functions.R')
library(WikidataQueryServiceR)
library(openxlsx)
library(tidyverse)
library(stringr)
#install.packages("jsonlite")
#library("jsonlite")
library(RCurl)

#POSIBLE PROYECTO WIKIPEDIA - DATOS DE ANTROPÓLOGOS
#https://www.thedataschool.co.uk/rachel-costa/scraping-wikipedia
#https://www.wikidata.org/wiki/Wikidata:Tools/For_programmers
#https://github.com/TS404/WikidataR
#https://cran.r-project.org/web/packages/WikipediR/index.html

#https://github.com/dahlia/wikidata
#https://www.wikidata.org/w/api.php?action=wbgetentities&sites=enwiki&titles=Franz_Boas
#https://www.wikidata.org/w/api.php?action=wbgetentities&ids=Q49088


#
#nodos <- data.frame(Doubles=double(),
#                          Ints=integer(),
#                          Factors=factor(),
#                          Logicals=logical(),
#                          Date=as.Date(character())
#                          Characters=character(),
#                          stringsAsFactors=FALSE)
#

##BUG: nasc. y muerte tienen que ser fechas
#Parametros
lang <- 'es,en,fr,pt,it,de,ru'
#Lista=TRUE, query=FALSE
query_or_list = TRUE

#Cargar datos ya generados
personas <- read.csv('personas.csv',stringsAsFactors = FALSE)
relaciones <- read.csv('relaciones.csv',stringsAsFactors = FALSE)

if (query_or_list) {
  #BUG: Comportamiento extraño, no devuelve resultados para algunas lineas, en particular los últimos agregados.
 listado_antropologos <- get_from_file_list('antropologos.csv',lang)
 #Limpia ID
 listado_antropologos$item <- gsub('http://www.wikidata.org/entity/', '', listado_antropologos$item)
 #Elimina los que no estan en los lenguajes seleccionados.
 listado_antropologos <- listado_antropologos %>% filter(!grepl("(['Q'][0-9]+)", listado_antropologos$itemLabel))
 
 
 #Limpia País
 listado_antropologos$ciudadania <- gsub('http://www.wikidata.org/entity/', '', listado_antropologos$ciudadania)
 #listado_antropologos %>% rowwise() %>% mutate(ciudadania2 = find_item(ciudadania)[[1]]$label[1])
 
 #write.csv(listado_antropologos,"search_results.csv", row.names = FALSE)

} else {

  ####NO USADO AUN: SET COMPLETO MUY LARGO DE OBTENER######

  listado_antropologos <- query_wikidata(sprintf('
                    select distinct  ?item ?itemLabel ?itemDescription ?pic ?nasc ?muerte where
                    {
                                       ?item wdt:P31 wd:Q5.  # Any instance of a human.
                                       OPTIONAL {?item wdt:P18 ?pic.}
                                       OPTIONAL {?item wdt:P569 ?nasc.}
                                       OPTIONAL {?item wdt:P570 ?muerte.}
                                       {?item wdt:P101 wd:Q23404.} UNION #Area antropología
                                       {?item wdt:P101 wd:Q29051.} UNION #Area Antropología Social
                                       {?item wdt:P101 wd:Q43455.} UNION #Area Etnología
                                       {?item wdt:P101 wd:Q132151.} UNION #Area Etnografía
                                       {?item wdt:P101 wd:Q98074145.} UNION #Area Antropología Social
                                       {?item wdt:P106 wd:Q4773904.} UNION #Profesión Antropologo
                                       {?item wdt:P106 wd:Q96326441.} UNION #Profesion Antropologo mayuscula
                                       {?item wdt:P106 wd:Q12347522.} #Profesión Etnografo
                                       SERVICE wikibase:label { bd:serviceParam wikibase:language "%s" }

                                       }
                                       ORDER BY ASC(?itemLabel)
                    ',lang))

  #Limpia ID
  listado_antropologos$item <- gsub('http://www.wikidata.org/entity/', '', listado_antropologos$item)
  #Elimina los que no estan en los lenguajes seleccionados.
  listado_antropologos <- listado_antropologos %>% filter(!grepl("(['Q'][0-9]+)", listado_antropologos$itemLabel))
  

}


############## Begin: Procesar cada elemento############################################
#for (i in 1:nrow(listado_antropologos)) {
for (i in 1:nrow(listado_antropologos)) {
  selected <- listado_antropologos[i,]


#Ingresa datos a la tabla de personas si ya no existe la entrada para todo el set de datos.
if (!(selected$item %in% personas$id)) {
  persona <- c(id= selected$item,
               nombre = selected$itemLabel,
               descripcion = selected$itemDescription,
               pic = selected$pic,
               nasc = selected$nasc,
               muerte = selected$muerte
               )
  #BUG: nasc y muerte quedan mal fusionadas por lo que se ve.
  personas <- do.call(rbind, list(persona,personas))
}



  #Obtener los datos completos de la persona
  item <- get_item(id = selected$item)
  claims <- item[[1]]$claims


  ## Begin: Relaciones entre personas ##
#TODO: Ver como implementar recursión (hasta ahora solo hay un nivel). Para asegurar el corte, no debería ejecutarse la recursión si ya hay relaciones o si no esta el flag correspondiente


# #TODO: claramente no alcanzan las relaciones explicitas que proponen en wikipedia. Podriamos indagar analizando el texto del articulo con
#https://medium.com/@andreasherman/different-ways-of-doing-relation-extraction-from-text-7362b4c3169e y queries como estos: https://en.wikipedia.org//w/api.php?action=query&format=json&prop=revisions&titles=Alfred_Radcliffe-Brown&utf8=1&rvprop=content

#BUG: no se estan buscando las relaciones de estas relaciones 802, 185 y 1066.
#De esta forma quedan influencias directas no exploradas en el grado 2.
  
res <- get_influences_from_entity(relaciones,personas,item)
relaciones <- res[[1]]
personas <- res[[2]]
  
  ###Discipulos P802  ####
  filtered <- filter_existant_rel(relaciones,
                                   claims$P802[[1]]$datavalue$value$id,
                                   FALSE,'Alumno de',
                                   selected$item)

  if (length(filtered)>0) {
    p802 <-get_item(id = filtered)
    res <- set_relaciones(p802,FALSE,relaciones,'Alumno de',item)
    relaciones <- res[[1]]
    personas <- res[[2]]


    # #Students of students
        for (i in 1:length(p802)) {
          filtered <- filter_existant_rel(relaciones,
                                          p802[[i]]$claims$P802$mainsnak$datavalue$value$id,
                                          FALSE,'Alumno de',
                                          selected$item)
          p802_1 <-get_item(id = p802[[i]]$claims$P802$mainsnak$datavalue$value$id)
        
          
          if (length(p802_1)>0) {
            res <- set_relaciones(p802_1,FALSE,relaciones,'Alumno de',list(p802[[i]]))
            relaciones <- res[[1]]
            personas <- res[[2]]
            res <- get_influences_from_entity(relaciones,personas,p802_1)
            relaciones <- res[[1]]
            personas <- res[[2]]
            }

        }
  }
  ###Estudiantes doctorales P185  ####
  filtered <- filter_existant_rel(relaciones,
                                  claims$P185[[1]]$datavalue$value$id,
                                  FALSE,'Alumno de',
                                  selected$item)

  if (length(filtered)>0) {
    p185 <-get_item(id = filtered)
    res <- set_relaciones(p185,FALSE,relaciones,'Alumno de',item)
    relaciones <- res[[1]]
    personas <- res[[2]]
    #Students of students
    for (i in 1:length(p185)) {
      filtered <- filter_existant_rel(relaciones,
                                      p185[[i]]$claims$P185$mainsnak$datavalue$value$id,
                                      FALSE,'Alumno de',
                                      selected$item)
      p185_1 <-get_item(id = p185[[i]]$claims$P185$mainsnak$datavalue$value$id)

      if (length(p185_1)>0) {
        res <- set_relaciones(p185_1,FALSE,relaciones,'Alumno de',list(p185[[i]]))
        relaciones <- res[[1]]
        personas <- res[[2]]
        res <- get_influences_from_entity(relaciones,personas,p185_1)
        relaciones <- res[[1]]
        personas <- res[[2]]
      }

    }

  }


    ###Discipulo de P1066 ####
  filtered <- filter_existant_rel(relaciones,
                                  claims$P1066[[1]]$datavalue$value$id,
                                  FALSE,'Discipulo de',
                                  selected$item)

  if (length(filtered)>0) {
    p1066 <-get_item(id = filtered)
    res <- set_relaciones(p1066,FALSE,relaciones,'Discipulo de',item)
    relaciones <- res[[1]]
    personas <- res[[2]]
    #Students of students
    for (i in 1:length(p1066)) {
      filtered <- filter_existant_rel(relaciones,
                                      p1066[[i]]$claims$P1066$mainsnak$datavalue$value$id,
                                      FALSE,'Discipulo de',
                                      selected$item)
      p1066_1 <-get_item(id = p1066[[i]]$claims$P1066$mainsnak$datavalue$value$id)

      if (length(p1066_1)>0) {
        res <- set_relaciones(p1066_1,FALSE,relaciones,'Discipulo de',list(p1066[[i]]))
        relaciones <- res[[1]]
        personas <- res[[2]]
        res <- get_influences_from_entity(relaciones,personas,p1066_1)
        relaciones <- res[[1]]
        personas <- res[[2]]
      }

    }
  }

  ## End: Relaciones entre personas ##

}
############## End: Procesar cada elemento############################################

#Guardado de archivos#
write.csv(personas,"personas.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(relaciones,"relaciones.csv", row.names = FALSE, fileEncoding = "UTF-8")

#Prepara Datos para Flourish. Solo las relaciones necesitan cambiarse. La columna id puede ignorarse en los datos de los nodos
select <- select(relaciones, c("Source","Target","Label")) %>% 
  left_join(personas, by = c("Source" = "id"))  %>% 
  select(.,-c(5:9)) %>%
  rename(., from = nombre ) %>%
  left_join(personas, by = c("Target" = "id"))  %>%
  select(.,-c(6:10)) %>%
  rename(., to = nombre ) %>%
  select(.,-c(1:2)) 

write.csv(select,"relaciones-flourish.csv", row.names = FALSE, fileEncoding = "UTF-8")



