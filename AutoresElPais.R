library(googlesheets)
library(lubridate)
library(dplyr)
library(XML)

# función que recoge el primer elemento de un vector
firstelement <- function(x){x[1]}

# funcion que combierte el vector de autores multiples en una lista para poder analizar cada 
# nombre, y después limpia estos nombres para quedarse con los nombres de pila
nombres_pila_multiples <- function(x){
    split_autores <- strsplit(x, " ")
    nombres_pila <- sapply(split_autores, firstelement)
    nombres_pila
}

# función que lee la pagina principal del diario "El Pais" y recoge un vector con todos los nombres
# de los periodias que tienen artículos publicados. Después analiza los autores por género y devuelve una tabla
autores_elpais <- function(url, genero){
    
    #obtener la lista de autores de la portada de elpais 
    html <- htmlTreeParse(url, useInternalNodes =T)
    autores <- xpathSApply(html,"//span[@class='autor']",xmlValue)
    #limpiar datos
    autores <- gsub("\n", "", autores)
    autores <- tolower(autores)
    # separar en grupos <- mujeres, hombres y multiples autores
    multiples <- autores[grep("/", autores)]
    individuales <- autores[ -grep("/", autores)]
    
    # separar multiples autores y analizar si son de un genero o mixtos
    multiples <- strsplit(multiples, " / ")
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
    if(otros>0){ resultado <- "Otros" }
    else if(cont_hombre== length(autores) ){ resultado <- "hombres"}
    else if(cont_mujer== length(autores)){ resultado <- "mujeres"}
    else{ resultado <- "mixto" }
    
    resultado
}

#analiza todas las portadas de elpais durante los días especificados
analizar_fechas <- function(numero_dias){
    #obtener datos de genro del documento de google drive
    gap <- gs_title("analisis_genero_portadas")
    genero <- gap %>% gs_read(ws= "Sheet1")
    datos <- data_frame()
    inicio <- as.Date("2016/04/19")
    for (fecha in 1:numero_dias) {
        actual <- inicio - fecha
        anno <- year(actual)
        mes <- formatC(month(actual), width=2, flag = "0")
        dia <- formatC(day(actual), width=2, flag = "0")
        url <- paste0("http://elpais.com/hemeroteca/elpais/", anno, "/", mes,
                      "/", dia, "/m/portada.html")
        autores <- autores_elpais(url = url, genero = genero)
        fila <- table(autores$resultado)
        fila <- data.frame(fecha= actual, hombres= fila["hombres"], mujeres= fila["mujeres"],
                           mixto= fila["mixto"], otros= fila["otros"] )
        if(is.null(datos)){ 
            datos <- fila
            print(datos)
        }
        else{
            datos <- rbind(datos,fila)
            print(datos)
        }
    }
    datos
}