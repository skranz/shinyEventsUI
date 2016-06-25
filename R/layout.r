dyn.layout.example = function() {
  app = eventsApp()
  style = tags$style(HTML('
  .ui-layout-pane {
  	background:	#FFF;
  	border:		none;
  	padding:	0px;
  	overflow:	auto;
  }'))

  app$ui = bootstrapPage(
    jqueryLayoutHeader(),
    actionButton("btn","Show Layout"),
    uiOutput("mainUI")
  )
  buttonHandler("btn", function(...) {
    ui = tagList(
      div(id="mydiv",style="height: 400px",
        jqueryLayoutPanes(parent = "#mydiv", js.do.layout = FALSE,
          center=div(paste0("Center ",1:5,collapse="\n ")),
          east=jqueryLayoutPanes(style = style, js.do.layout = FALSE,
              north = div("Title Line",actionButton("addBtn","Add"), uiOutput("titleUI")),
              center = div(paste0("  east center ", 1:100, collapse=" "))
          )
        )
      ),
      tags$script(HTML('
        var mydiv_Layout = $("#mydiv").layout({
          defaults: {
            //spacing_open: 4
          },
          east: {
            closable: true,
            resizable: true
          }
        });
        var mydiv__east__Layout  = mydiv_Layout.panes.east.layout({
          defaults: {
            resizable: false,
            closable: false,
            slideable: false,
            spacing_open: 0
          },
          north: {
            size: "auto"
          }
        });
      '))
    )
    setUI("mainUI",ui)
  })
  buttonHandler("addBtn", function(...) {
    dsetUI("titleUI",p(paste0("more ", 1:20, collapse = " ")))
    resizeLayout("mydiv__east__Layout")
  })
  viewApp(app)
}


examples = function() {
  #addShinyRessourcePath()
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

  app$ui = bootstrapPage(
    jqueryLayoutHeader(),
    jqueryLayoutPanes(
      center=div(paste0("Center ",1:100,collapse="\n ")), north=div(style="border: solid;","North"),east="East",json.opts=json.opts, style=style
    )
  )
  viewApp(app)
}


#' A jqueryLayoutPage
#' @export
jqueryLayoutHeader = function(style=NULL) {
  restore.point("jqueryLayoutHeader")

  tagList(
    singleton(tags$head(tags$script(src="shared/jqueryui/jquery-ui.min.js"))),
    singleton(tags$head(tags$link(href="shared/jqueryui/jquery-ui.min.css", rel="stylesheet"))),
    singleton(tags$head(tags$link(href="shinyEventsUI/layout-default.css", type="text/css", rel="stylesheet"))),
    singleton(tags$head(tags$script(src="shinyEventsUI/jquery.layout.js")))
  )

}


#' A jqueryLayoutPage
#' @export
jqueryLayoutPanes = function(center=NULL, north=NULL,west=NULL, east=NULL, south=NULL, panes=list(center=center, north=north, west=west, east=east, south=south), json.opts="", style=NULL, js.do.layout = TRUE, parent = "body") {
  restore.point("jqueryLayoutPanes")


  for (dir in names(panes)) {
    if (!is.null(panes[[dir]]))
      panes[[dir]] = div(class=paste0("ui-layout-",dir),panes[[dir]])
  }

  if (js.do.layout) {
    var = gsub("#","",parent, fixed=TRUE)
    js = paste0('
      var ',var,'__Layout = $("',parent,'").layout({
        ', json.opts,'
      });
    ')
  } else {
    js = NULL
  }
  tagList(
    style,
    panes,
    tags$script(HTML(js))
  )
}

#' Resize a jqueryLayout. May be useful if inner html has been dynamically changed and we have size="auto"
#' @export
resizeLayout = function(layout.var) {
  js = paste0('
    ',layout.var,'.resizeAll();
  ')
  evalJS(js)
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
    jqueryLayoutHeader(),
    tags$head(tags$script(HTML(js))),
    style,
    panes
  )

}
