get_lang_string <- function(list_find,lang) {
  lang_list <- strsplit(lang, ",")
  string_lang <- ''
  for(i in 1:length(lang_list[[1]])) {
    string_lang <- list_find[lang_list[[1]][i]]
    if (!(is.null(string_lang[[1]]))) {
      break
    }
  }
  return(string_lang[[lang_list[[1]][i]]][["value"]])
}

filter_existant_rel<- function(relaciones,list_ids,inverted,tipo,item_id) {
  res <- list_ids
  for (i in 1:length(list_ids)) {
    tmp_rel <- relaciones
    before <- nrow(tmp_rel)
    if (inverted) {
      tmp_rel <- rbind(tmp_rel,c(item_id,list_ids[[i]],tipo))
    } else {
      tmp_rel <- rbind(tmp_rel,c(list_ids[[i]],item_id,tipo))
    }
    tmp_rel <- tmp_rel%>% distinct()
      if (!(before< nrow(tmp_rel))) {
      res <- res[res %in%  list_ids[[i]]== FALSE] 
    }
    
  }
  
  return(res)
}
get_from_file_list <- function(archivo,lang) {
  entidades <- read.csv(archivo,header = FALSE, stringsAsFactors = FALSE)
  values_str <-''
  for (i in 1:length(entidades$V1)) {
    search_res <- find_item(entidades$V1[i])
    if (length(search_res)>0) {
      values_str <- paste(values_str,'(wd:',search_res[[1]]$id,')',sep="")
    }
  }
  

  listado_antropologos <- query_wikidata(sprintf('
                    select distinct  ?item ?itemLabel ?itemDescription ?pic ?nasc ?muerte ?ciudadania where 
                    {
                                       ?item wdt:P31 wd:Q5.  # Any instance of a human.
                                       OPTIONAL {?item wdt:P18 ?pic.}
                                       OPTIONAL {?item wdt:P569 ?nasc.}
                                       OPTIONAL {?item wdt:P570 ?muerte.}
                                       OPTIONAL {?item wdt:P27 ?ciudadania.}
                                        VALUES (?item) {
                                            %s
                                        }
                                       SERVICE wikibase:label { bd:serviceParam wikibase:language "%s" } 
                                       
                                       }
                                       ORDER BY ASC(?itemLabel)
                    ',values_str,lang))
  
  #Limpia ID
  listado_antropologos$item <- gsub('http://www.wikidata.org/entity/', '', listado_antropologos$item)
  #Elimina los que no estan en los lenguajes seleccionados.
  listado_antropologos <- listado_antropologos %>% filter(!grepl("(['Q'][0-9]+)", listado_antropologos$itemLabel)) 
  #Limpia País
  listado_antropologos$ciudadania <- gsub('http://www.wikidata.org/entity/', '', listado_antropologos$ciudadania)
  #listado_antropologos %>% rowwise() %>% mutate(ciudadania2 = find_item(ciudadania)[[1]]$label[1])
  
  return(listado_antropologos)
}

get_from_list <- function(lista) {
  values_str <-''
  for (i in 1:length(lista[[1]])) {
    search_res <- find_item(trimws(lista[[1]][i]))
    if (length(search_res)>0) {
      values_str <- paste(values_str,'(wd:',search_res[[1]]$id,')',sep="")
    }
  }
  
  listado_antropologos <- query_wikidata(sprintf('
                                                 select distinct  ?item ?itemLabel ?itemDescription ?pic ?nasc ?muerte where 
                                                 {
                                                 ?item wdt:P31 wd:Q5.  # Any instance of a human.
                                                 OPTIONAL {?item wdt:P18 ?pic.}
                                                 OPTIONAL {?item wdt:P569 ?nasc.}
                                                 OPTIONAL {?item wdt:P570 ?muerte.}
                                                 VALUES (?item) {
                                                 %s
                                                 }
                                                 SERVICE wikibase:label { bd:serviceParam wikibase:language "%s" } 
                                                 
                                                 }
                                                 ORDER BY ASC(?itemLabel)
                                                 ',values_str,lang))
  #Limpia ID
  listado_antropologos$item <- gsub('http://www.wikidata.org/entity/', '', listado_antropologos$item)
  #Elimina los que no estan en los lenguajes seleccionados.
  listado_antropologos <- listado_antropologos %>% filter(!grepl("(['Q'][0-9]+)", listado_antropologos$itemLabel)) 
  return(listado_antropologos)
}
set_relaciones <- function(claimList, inverted,relaciones,tipo,item) {
  for(i in 1:length(claimList)) {
    if (!(claimList[[i]]$id %in% personas$id)) {
      discItem <-claimList[[i]]
      if(!is.null(get_lang_string(discItem$descriptions,lang))) {
        desc <- get_lang_string(discItem$descriptions,lang)
      } else {
        desc =''
      }
      if(!is.null(discItem$claims$P18$mainsnak$datavalue$value)) {
        pic <- discItem$claims$P18$mainsnak$datavalue$value
        if (class(pic)== 'data.frame') { pic =''} else {
          pic <- paste('http://commons.wikimedia.org/wiki/Special:FilePath/',pic,sep="")
        }
      } else {
        pic =''
      }
      discipulo <- c(id = discItem$id,
                     nombre = discItem$labels$es$value,
                     descripcion = desc,
                     pic = pic,
                     # nasc = discItem[[1]]$claims$P569$mainsnak$datavalue$value[1],
                     #muerte = discItem[[1]]$claims$P570$mainsnak$datavalue$value[1])
                     nasc = '',
                     muerte='')
      personas <- rbind(discipulo,personas)
    } 
    if (inverted) {
      relaciones <- rbind(relaciones,c(item[[1]]$id,claimList[[i]]$id,tipo))
    } else {
      relaciones <- rbind(relaciones,c(claimList[[i]]$id,item[[1]]$id,tipo))
    }
    
    
  }
  #Borra relaciones redundantes
  relaciones <- relaciones %>% distinct()
  return(list(relaciones,personas))
}

get_influences_from_entity <- function(relaciones, personas, item) { 
  if ((length(item)<1)  || is.null(item[[1]]$sitelinks$enwiki$title)) {
    return(list(relaciones,personas))
  }
  #Relaciones extraidas del texto del artículo#
  url_string <- paste("https://en.wikipedia.org//w/api.php?action=query&format=json&prop=revisions&titles=",URLencode(gsub(' ', '_',item[[1]]$sitelinks$enwiki$title)), "&utf8=1&rvprop=content",sep ='')
  #url_string <- paste("https://en.wikipedia.org//w/api.php?action=query&format=json&prop=revisions&titles=Franz_Boas&utf8=1&rvprop=content",sep ='')
  
  api_request <- getURL(url_string)
  
  
  influenced <- str_extract(gsub("\\\\n\\*","",api_request), regex("(influenced +=)(.*?)(\\\\n)"))
  influences <- str_extract(gsub("\\\\n\\*","",api_request), regex("(influences +=)(.*?)(\\\\n)"))
  
  
  
  #str_view(gsub("\\\\n\\*","",api_request), regex("(influences)(.*?)(\\\\n)"))
  
  influenced <- influenced %>%
    gsub("\\{\\{hlist",'',.) %>%
    gsub("\\|",'',.) %>%
    gsub("\\{\\{.*?\\}\\}",'',.) %>%
    gsub("<(.*?)>",'',.) %>%
    gsub("\\}\\}",'',.) %>%
    gsub("\\\\n",'',.) %>%
    gsub("\\n",'',.)
  
  
  influenced <-  str_extract(influenced,'\\[\\[(.*)\\]\\]') %>%
    gsub("\\{\\{.*\\}\\}",'',.) %>%
    gsub("\\[",'',.) %>%
    gsub("\\]",' ',.)
  
  
  if (!(is.na(influenced)) && str_detect(influenced, "[ \t]{2,}", negate = FALSE)) {
    influenced <- strsplit(influenced,"[ \t]{2,}")
  } else if (!(is.na(influenced))) {
    influenced <- strsplit(influenced,", ")
  }
  influences <- influences %>%
    gsub("\\{\\{hlist",'',.) %>%
    gsub("\\|",'',.) %>%
    gsub("\\{\\{.*?\\}\\}",'',.) %>%
    gsub("<(.*?)>",'',.) %>%
    gsub("\\}\\}",'',.) %>%
    gsub("\\\\n",'',.) %>%
    gsub("\\n",'',.)
  
  influences <-  str_extract(influences,'\\[\\[(.*)\\]\\]') %>%
    gsub("\\{\\{.*\\}\\}",'',.) %>%
    gsub("\\[",'',.) %>%
    gsub("\\]",' ',.)
  
  if (!(is.na(influences))  &&str_detect(influences, "[ \t]{2,}", negate = FALSE)) {
    influences <- strsplit(influences,"[ \t]{2,}")
  } else if (!(is.na(influences)) ) {
    influences <- strsplit(influences,", ")
  }
  
  
  
  ###########################
  if (length(influenced[[1]])>0) {
    ## Obtiene listado
    influenced <- get_from_list(influenced)
  }
  if (length(influenced[[1]])>0) {
    #Inserta
    for (i in 1:length(influenced[[1]])) {
      influencia <- c(id = influenced[[1]][i],
                      nombre = influenced[[2]][i],
                      descripcion = influenced[[3]][i],
                      pic = influenced[[4]][i],
                      # nasc = influenced[[5]][i],
                      #muerte = influenced[[6]][i]])
                      nasc = '',
                      muerte='')
      
      if (!(influenced[[1]][i] %in% personas$id)) {personas <- rbind(influencia,personas)}
      relaciones <- rbind(relaciones,c(selected$item,influenced[[1]][i],"Influenciado por"))
      
    }
  }
  if (length(influences[[1]])>0) {
    influences<- get_from_list(influences)
  }
  if (length(influences[[1]])>0) {
    #Inserta
    for (i in 1:length(influences[[1]])) {
      influencia <- c(id = influences[[1]][i],
                      nombre = influences[[2]][i],
                      descripcion = influences[[3]][i],
                      pic = influences[[4]][i],
                      # nasc = influences[[5]][i],
                      #muerte = influences[[6]][i]])
                      nasc = '',
                      muerte='')
      if (!(influences[[1]][i] %in% personas$id)) {personas <- rbind(influencia,personas)}
      relaciones <- rbind(relaciones,c(selected$item,influences[[1]][i],"Influenciado por"))
      
    }
  }
  
  #Borra relaciones redundantes
  relaciones <- relaciones %>% distinct()
  
  return(list(relaciones, personas))
}