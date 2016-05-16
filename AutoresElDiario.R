library(googlesheets)
library(lubridate)
library(dplyr)
library(XML)
library(rvest)
library(stringr)

# función que recoge el primer elemento de un vector
firstelement <- function(x){x[1]}

# funcion que combierte el vector de autores multiples en una lista para poder analizar cada 
# nombre, y después limpia estos nombres para quedarse con los nombres de pila
nombres_pila_multiples <- function(x){
    split_autores <- strsplit(x, " ")
    nombres_pila <- sapply(split_autores, firstelement)
    nombres_pila
}

autores_eldiario <- function(url, genero){
    eldiario <- read_html("http://eldiario.es")
    firma  <- eldiario %>% html_nodes(".dateline") %>% html_text() 
    firma <- tolower(firma)
    #limpiar datos
    firma <- gsub("\n", "", firma, fixed = TRUE)
    firma <- gsub("\t", "", firma, fixed = TRUE)
    #quitar numeros
    firma <- gsub("[0-9]", "", firma)
    #quitar espacios que sobran
    firma <- str_replace(gsub("\\s+", " ", str_trim(firma)), "B", "b")
    #quitar delegación
    firma <- gsub("\\s\\-\\s*\\w*$", "", firma)
    
    # separar en grupos <- mujeres, hombres y multiples autores
    multiples <- firma[grep(" / ", firma, fixed = TRUE)]
    individuales <- firma[ -grep(" / ", firma, fixed = TRUE)]
    
    # separar multiples autores y analizar si son de un genero o mixtos
    multiples <- strsplit(multiples, " | ")
    # pasar los autores a nombres de pila
    multiples <- lapply(multiples, nombres_pila_multiples)
    split_autores <- strsplit(individuales, " ")
    individuales <- sapply(split_autores, firstelement)
    autores <- append(multiples, individuales)
    # obtener vector con la tabla de resultado por cada autor
    resultado <- sapply(autores, mixto, genero)
    autores <- sapply(autores, firstelement)
    data <- data.frame(autores,resultado)
    data
}

# función que compara la lista de autores recogida y la lista de periodistas mujeres y 
# devuelve una tabla con los casos positivos y negativos
contar_mujeres <- function(autores, mujeres){
    autoras <- autores %in% mujeres #contains()
    table(autoras)
    autoras
}

# analiza la lista de autores y la compara con el vector de mujeres para sabes si es
# "solo hombres", "solo mujeres" o "mixto"
mixto <- function(autores, genero){
    cont_mujer <- 0
    cont_hombre <- 0
    otros <- 0
    for(i in 1:length(autores)){
        if(sum(autores[i] %in% genero$mujer)){ cont_mujer <- cont_mujer+1 }
        else if(sum(autores[i] %in% genero$hombre)){cont_hombre <- cont_hombre+1 }
        else{ otros <- otros+1 }
    }
    if(otros>0){ resultado <- "otros" }
    else if(cont_hombre== length(autores) ){ resultado <- "hombres"}
    else if(cont_mujer== length(autores)){ resultado <- "mujeres"}
    else{ resultado <- "mixto" }
    
    resultado
}

#analiza todas las portadas de elpais durante los días especificados
analizar_fechasD <- function(numero_dias=1){
    #obtener datos de genro del documento de google drive
    gap <- gs_title("analisis_genero_portadas")
    genero <- gap %>% gs_read(ws= "Sheet1")
    datos <- data_frame()
    actual <- Sys.Date()
    url <- "http://eldiario.es"
    autores <- autores_eldiario(url = url, genero = genero)
    
    fila <- table(autores$resultado)
    fila <- data.frame(fecha= actual, hombres= fila["hombres"], mujeres= fila["mujeres"],
                       mixto= fila["mixto"], otros= fila["otros"] )
    if(is.null(datos)){ 
        datos <- fila
        # print(datos)
    }
    else{
        datos <- rbind(datos,fila)
        # print(datos)
    }
    datos
}