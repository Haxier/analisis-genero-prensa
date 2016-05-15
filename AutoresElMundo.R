library(rvest)
# función para obtener y analiar los autores de la portada digital de El Mundo http://elmundo.es
autoresElMundo <- function(url, genero){
    elmundo <- read_html(url)
    firma  <- elmundo %>% html_nodes(".mod-author span") %>% html_text()  
    
    autores <- tolower(firma)
    # separar en grupos <- mujeres, hombres y multiples autores
    multiples <- autores[grep(" | ", autores)]
    individuales <- autores[ -grep(" | ", autores)]
    
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