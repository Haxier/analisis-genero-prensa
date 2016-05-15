
autoresElDiario <- function(url, genero){
    eldiario <- read_html("http://eldiario.es")
    firma  <- eldiario %>% html_nodes(".byline a") %>% html_text() 
}