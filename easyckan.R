library(ckanr)

mypackage <- 'tp2-model-outputs'
ckanr_setup(url = 'http://energydata.uct.ac.za', key = readline(prompt = 'enter your apikey: '))

batchdelete <- function(){
  rsrcs <- package_show(mypackage, as = 'table')$resources['id']
  for (r in rsrcs$id){
    resource_delete(r)
  }
}

batchcreate <- function(){
  path <- readline(prompt = 'enter path to resource folder: ')
  files <- dir(path)
  for (f in files){
    filepath <- paste(path, f, sep = '/')
    filename <- toupper(gsub("\\..*", "",f))
    print(paste('...creating ',filepath))
    resource_create(package_id = mypackage, name = filename, upload = filepath)
  }
}