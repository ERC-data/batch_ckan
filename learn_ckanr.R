library(ckanr)

ckanr_setup(url = 'http://energydata.uct.ac.za', key = readline(prompt = 'Enter your apikey: \n'))

package_search('air')
resource_search('format:csv', limit = 10)


res <- resource_show(id = "25131e8c-0a73-4d1a-becf-0cd29bc7fda1", as = "table")
head(fetch(res$url))
x <- fetch(res$url)
x[x$Municipality=='Umdoni',]


package_search("air", as = "table")
resource_search('format:csv', as = "table", limit = 10)

res2 <- resource_show(id = "be62d04a-d3b1-4f40-8993-4ee1826cc6eb", as = "table")
fetch(res2$url)
