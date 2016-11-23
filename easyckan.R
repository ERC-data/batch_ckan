library(ckanr)
  
ckanr_setup(url = 'http://energydata.uct.ac.za', key = readline(prompt = 'Enter your apikey: \n'))

setpath <- function(path){
    if(missing(path)){
        if(exists("path", envir = .GlobalEnv)) {
            print(paste('Your path is set to', get0('path', envir = .GlobalEnv), '. You can choose a different folder with setpath(\'your/path\').'))
        } else {
            path <- readline(prompt = 'Enter path to resource folder: ')
            assign("path", path, envir = .GlobalEnv) 
        }
    } else {
        assign("path", path, envir = .GlobalEnv) 
    } 
}

description <- function(path, filematch, metadata){
    if(isTRUE(metadata)){
        metaformat <- readline('In which format have you saved your metadata file? Choose \'rdsattr\' or \'csv\': ')
        switch(metaformat,
               rdsattr = attr(readRDS(path), 'comment'),
               csv = read.csv(paste(path, 'metadata.csv', sep = '/'), row.names = 'Name', stringsAsFactors = FALSE)[filematch,'Description']
        )}
}

batchdelete <- function(dataset = readline('Enter a valid dataset id: \n')){
    rsrcs <- package_show(dataset, as = 'table')$resources['id']
    for (r in rsrcs$id){
        resource_delete(r)
    }
}

batchcreate <- function(dataset = readline('Enter a valid dataset id: \n'), metadata){
    setpath()
    files <- dir(path)
    match.arg(as.character(metadata), choices = c(TRUE,FALSE))
    for (f in files){
        filepath <- paste(path, f, sep = '/')
        filename <- toupper(gsub("\\..*", "",f))
        dscrptn <- description(path, f, metadata)
        print(dscrptn)
        print(paste('...creating ',filepath))
        resource_create(package_id = dataset, description = dscrptn, name = filename, upload = filepath)
    }
}