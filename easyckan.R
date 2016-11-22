library(ckanr)

ckanr_setup(url = 'http://energydata.uct.ac.za', key = readline(prompt = 'Enter your apikey: \n'))

batchdelete <- function(dataset = readline('Enter a valid dataset id: \n')){
  rsrcs <- package_show(dataset, as = 'table')$resources['id']
  for (r in rsrcs$id){
    resource_delete(r)
  }
}

batchcreate <- function(dataset = readline('Enter a valid dataset id: \n')){
  path <- readline(prompt = 'enter path to resource folder: ')
  files <- dir(path)
  for (f in files){
    filepath <- paste(path, f, sep = '/')
    filename <- toupper(gsub("\\..*", "",f))
    dscrptn <- attr(readRDS(filepath), 'comment') ###create a description function?
    print(paste('...creating ',filepath))
    resource_create(package_id = dataset, description = dscrptn, name = filename, upload = filepath)
  }
}