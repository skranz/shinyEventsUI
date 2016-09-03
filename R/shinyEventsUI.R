
.onLoad = function(...) {
  addShinyEventsUIRessourcePath()
}

addShinyEventsUIRessourcePath = addShinyRessourcePath = function() {

  www.dir = system.file('www', package='shinyEventsUI')
  # init ressource paths
  shiny::addResourcePath(
    prefix = 'shinyEventsUI',
    directoryPath = www.dir
  )

}

to.style = function(x, sep=";") {
  paste0(names(x),": ", x, sep=sep)
}

nlist = function (...)
{
    li = list(...)
    li.names = names(li)
    names = unlist(as.list(match.call())[-1])
    if (!is.null(li.names)) {
        no.names = li.names == ""
        names(li)[no.names] = names[no.names]
    }
    else {
        names(li) = names
    }
    li
}


remove.null = function(li) {
  ok = !sapply(li, is.null)
  li[ok]
}
