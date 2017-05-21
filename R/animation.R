#' A simple image player for animations
#'
#' you can create svg images from multiple plots with record.svg.plots
#' @export
html.image.animation.player = function(container.id, base.name=NULL, image.files=NULL, extension="svg", images.dir=file.path(getwd(),"images"), loop.mode=c("none","loop","sweep")[1], delay=250, add.container=TRUE) {

  if (is.null(image.files)) {
    image.files =list.files(images.dir, glob2rx(paste0(base.name,"-*.",extension)))
    n = length(image.files)
    # correct order of files
    image.files = paste0(base.name,"-", seq_len(n),".",extension)
  }

  shiny::addResourcePath("images",images.dir)

  js = paste0("
$('#",container.id,"').scianimator({
	'images': [",paste0("'images/",image.files,"'",collapse=", "),"],
  'loopMode': '",loop.mode,"',
  'delay': ",delay,",
});
  ")
  tagList(
    singleton(tags$head(tags$script(src="shinyEventsUI/jquery.scianimator.min.js"))),
    #tags$head(tags$script(src="shinyEventsUI/jquery.scianimator.min.js")),
    if (add.container) div(id=container.id),
    tags$script(HTML(js))
  )
}

record.svg.plots = function(pars,base_name, plot.fun,...,width=4, height=3,image.dir=file.path(getwd(),"images"), pointsize=12, bg="white", use.svglite = TRUE) {
  restore.point("record.svg.plots")


  i = 1
  for (i in seq_along(pars)) {
    par = pars[[i]]
    file = file.path(image.dir,paste0(base_name,"-",i,".svg"))
    if (use.svglite) {
      library(svglite)
      svglite(file = file, width=width, height=height, pointsize=pointsize,bg=bg)
    } else {
      svg(filename = file, width=width, height=height, pointsize=pointsize)
    }
    plot.fun(par,...)
    dev.off()
    cat("\nsaved ", file)
  }
  files = paste0(base_name,"-",seq_along(pars),".svg")
  invisible(files)
}
