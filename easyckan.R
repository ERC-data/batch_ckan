### Use the ckanr package to remotely maintain the Energy Research Data Portal for South Africa with bulk uploads and deletes
### The process for remote bulk uploading of datasets is as follows:
# 1. source file
# 2. run ckanr_setup()
# 3. create new dataset with package_create()
# 4. create / check for metadata file in local resource folder with metadata()
# 5. if new metadata file was created, complete descriptions column in the csv file
# 6. run batchcreate() and hope for SUCCESS!

library(ckanr)
  
ckanr_setup(url = 'http://energydata.uct.ac.za', key = readline(prompt = 'Enter your apikey: \n'))

setpath <- function(path){
    if(missing(path)){
        if(exists("path", envir = .GlobalEnv)) {
            # do nothing
        } else {
            path <- readline(prompt = 'Enter path to resource folder: ')
            assign("path", path, envir = .GlobalEnv) 
        }
    } else {
        assign("path", path, envir = .GlobalEnv) 
    }
    all_files <- dir(get0('path', envir = .GlobalEnv))
    uploads <- all_files[all_files != 'metadata.csv'] #remove metadata file from list of files
    assign("all_files", all_files, envir = .GlobalEnv)
    assign("uploads", uploads, envir = .GlobalEnv)
}

metadata <- function(){
    remove(path, envir = .GlobalEnv)
    setpath()
    if(!is.na(pmatch('metadata', all_files))){
        return('csv')
    }else{
        print('Could not find a metadata file. Checking for rds files.')
        # Check if files are saved as csv or rds files
        if(sum(grepl('rds', uploads))/length(uploads) > 0.8){
            return('rdsattr')
        }else{
            write.csv(data.frame('Names' = uploads, 'Description' = rep("", length(uploads))), paste(path,'metadata.csv',sep='/'), row.names = FALSE)
            print('We have created a metadata.csv file for your dataset. Please add descriptions for your resources.')
            return('new')
        }
    }
}

description <- function(path = path, metadata = TRUE, filematch){
    if(isTRUE(metadata)){
        metaformat <- metadata()
        switch(metaformat,
               rdsattr = attr(readRDS(paste(path, filematch, sep = '/')), 'comment'),
               csv = read.csv(paste(path, 'metadata.csv', sep = '/'), row.names = c('Names'), 
                              stringsAsFactors = FALSE)[filematch, 'Description'],
               new = stop("You must add descriptions to your metadata file to continue.")
        )}
}

batchcreate <- function(dataset = readline('Enter a valid dataset id: \n'), metadata = TRUE){
    remove(path, envir = .GlobalEnv)
    setpath()
    match.arg(as.character(metadata), choices = c(TRUE,FALSE))
    
    # Create a dataframe that contains all the details to upload resources to CKAN
    resources <- data.frame(upload = uploads)
    resources$filepath <- sapply(resources$upload, function(x) paste(path, x, sep = '/'))
    resources$filename <- sapply(resources$upload, function(x) gsub("\\..*", "",x))
    resources$description <- sapply(resources$upload, function(x) description(path, metadata, x))
    
    # Upload the resources to CKAN and save the returned ckan object to an R result object
    result <- sapply(1:NROW(resources), function(r) resource_create(package_id = dataset, description = resources[r,'description'], 
                                       name = resources[r,'filename'], upload = resources[r,'filepath']))
    
    # Check that all resources have uploaded and retry if this is not the case
    for (i in 1:ncol(result)){
        detail <- resources[resources$filename == result['name',i],]
        tryCatch(
            r <- resource_show(result['id',i]),
            error = function(e) resource_create(package_id = dataset, description = detail$description, 
                                                name = detail$filename, upload = detail$filepath),
            finally = print(r)
        )
    }
    return(list(resources, result))}

batchdelete <- function(dataset = readline('Enter a valid dataset id: \n')){
    rsrcs <- package_show(dataset, as = 'table')$resources['id']
    sapply(rsrcs$id, function(x) resource_delete(x))
    }

private <- function(dataset){
    package_patch(x = list(private='true'), dataset)
    }