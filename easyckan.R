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
    files <- dir(get0('path', envir = .GlobalEnv))
    assign("files", files, envir = .GlobalEnv)
}

description <- function(path, filematch, metadata){
    if(isTRUE(metadata)){
        metaformat <- metadata()
        switch(metaformat,
               rdsattr = attr(readRDS(paste(path, filematch, sep = '/')), 'comment'),
               csv = read.csv(paste(path, 'metadata.csv', sep = '/'), row.names = 'Name', stringsAsFactors = FALSE)[filematch, 'Description']
        )}
}

metadata <- function(){
    setpath()
    if(!is.na(pmatch('metadata', files))){
        return('csv')
    }else{
        print('Could not find a metadata file. Checking for rds files.\n')
        # Check if files are saved as csv or rds files
        if(sum(grepl('rds', files))/length(files) > 0.8){
            return('rdsattr')
        }else{
            write.table(data.frame('Names' = files, 'Description' = rep("", length(files))), paste(path,'metadata.csv',sep='/'), row.names = FALSE, col.names = TRUE)
            print('We have created a metadata.csv file for your dataset. Please add descriptions for your resources.\n')
            break
        }
    }
}

batchcreate <- function(dataset = readline('Enter a valid dataset id: \n'), metadata){
    remove(path, envir = .GlobalEnv)
    setpath()
    match.arg(as.character(metadata), choices = c(TRUE,FALSE))
    files <- files[-match('metadata.csv', files)] #remove metadata file from list of files
    print(files)
    for (f in files){
        filepath <- paste(path, f, sep = '/')
        filename <- gsub("\\..*", "",f)
        dscrptn <- description(path, f, metadata)
        print(paste('...creating ',filename))
        # create resources and check if creation was successful
        if (exists('test')){
            remove(test)
        }
        count <- 1
        while(!exists('test') && count < 5){
            try({
                r <- resource_create(package_id = dataset, description = dscrptn, name = filename, upload = filepath)
                test <- resource_show(r$id)
                print(test)
                count <- count + 1
                Sys.sleep(1)
                })
        }
    }}

batchdelete <- function(dataset = readline('Enter a valid dataset id: \n')){
    rsrcs <- package_show(dataset, as = 'table')$resources['id']
    for (r in rsrcs$id){
        resource_delete(r)
    }
}

private <- function(dataset){
    package_patch(x = list(private='true'), dataset)
    }