baixa.arquivos <- function(url, expressao, tipo) {
        
url <- "https://www2.census.gov/geo/docs/reference/codes/files/"

arquivos.server <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE) 
arquivos.server <- getHTMLLinks(arquivos.server)

# arquivos.censo <- paste(strsplit(arquivos.ftp, "\n")[[1]], sep = "")
arquivos.server <- arquivos.server[grep(expressao, arquivos.server)]



tem.novidade <- 0 # start the trigger        

# Verifies if the ftp data is the same that the data that has already been used before.
# If not, set the download trigger and raw process trigger

lista.local <- paste0("../data/lista-", tipo, ".txt")

if (file.exists(lista.local)) {
        data.previous <- readLines(lista.local)
        if (identical(data.previous,arquivos.server)) {
                tem.novidade <- 0
        } else {
                tem.novidade <- 1
                writeLines(arquivos.server, lista.local)
        }
        
} else {
        tem.novidade <- 1
        writeLines(arquivos.server, lista.local)
}



# compares ftp repo to files already downloaded
arquivos.local <- dir("../data/")
arquivos.faltando <- arquivos.server[which(!(arquivos.server %in% arquivos.local))]


# if the lists of files are different, download new ones 

if(length(arquivos.faltando)>0 & tem.novidade ==1) {
        
        arquivo <- character()
        for (i in 1:length(arquivos.faltando)) {
                arquivo <- paste0(url, arquivos.faltando[i])
                download.file(arquivo, 
                              paste0("../data/", arquivos.faltando[i]),
                              method = "curl")
        }
}

}

