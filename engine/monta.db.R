
monta.db <- function(expressao, tipo, tem.novidade) {

        tipo <- "teste"
                
arquivo.rda <- paste0("../data/", tipo, ".rda")
tem.rda <- file.exists(arquivo)

# if trigger is set, raw process again

if(tem.novidade==1|tem.rda==0) { # them process the data
        arquivos.local <- dir("../data/")
        arquivos.local <- arquivos.local[grep(expressao, arquivos.local)]
        
        dados <- fread(sprintf("gzip -dc %s | tr -d '\\000'",
                               paste0("../data/", arquivos.local[1])),
                       na.strings = "", colClasses = "character")
        
        
        for (i in 2:length(arquivos.local)) {
                dados <- bind_rows(
                        dados, fread(sprintf(
                                "gzip -dc %s | tr -d '\\000'",
                                paste0("../data/", arquivos.local[i])),
                                na.strings = "", colClasses = "character")
                )
        }
        
        
        
        # treating var names
        names(dados) <- gsub("_", ".", tolower(names(dados)))
}        