examples = function() {
  addShinyRessourcePath()
  app = eventsApp()
  opts = list(
    north=list(resizable=FALSE)
  )
  json.opts =
'
defaults: {
  //spacing_open: 4
},
north: {
  size: "auto",
  resizable: false,
  //spacing_open: 4,
  spacing_closed: 10
},
east: {
  closable: true,
  resizable: true
}
'
style = tags$style(HTML('
.ui-layout-north {
	background:	#FFF;
	border:		none;
	padding:	0px;
	overflow:	auto;
}'))

  app$ui = jqueryLayoutPage(center=div(paste0("Center ",1:100,collapse="\n ")), north=div(style="border: solid;","North"),east="East",json.opts=json.opts, style=style)
  viewApp(app)
}

#' A jqueryLayoutPage
#' @export
jqueryLayoutPage = function(center=NULL, north=NULL,west=NULL, east=NULL, south=NULL, panes=list(center=center, north=north, west=west, east=east, south=south), json.opts="", style=NULL) {
  restore.point("jqueryLayoutPage")


  for (dir in names(panes)) {
    if (!is.null(panes[[dir]]))
      panes[[dir]] = div(class=paste0("ui-layout-",dir),panes[[dir]])
  }

  #json.opts = toJSON(options,auto_unbox = TRUE)

  js = paste0('
$(document).ready(function () {
  $("body").layout({
', json.opts,'
  });
});
')

  tagList(
    singleton(tags$head(tags$script(src="shared/jqueryui/jquery-ui.min.js"))),
    singleton(tags$head(tags$link(href="shared/jqueryui/jquery-ui.min.css", rel="stylesheet"))),
    singleton(tags$head(tags$link(href="shinyEventsUI/layout-default.css", type="text/css", rel="stylesheet"))),
    singleton(tags$head(tags$script(src="shinyEventsUI/jquery.layout.js"))),
    tags$head(tags$script(HTML(js))),
    style,
    panes
  )

}
