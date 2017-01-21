resize.event.example = function() {
  style = tags$style(HTML('
  .ui-layout-pane {
  	background:	#FFF;
  	border:		none;
  	padding:	0px;
  	overflow:	auto;
  }
  .ui-layout-pane {
  	overflow:	hidden;
  }
  '))
  json.opts =
'
defaults: {
  resizable: false,
  closable: false,
  slideable: false,
  spacing_open: 0
},
north: {
  size: "auto",
},
'


  js = "
  // select the target node
  var target = document.querySelector('#myout');

  // create an observer instance
  var observer = new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
          alert('content changed!');
      });
  });

  // configuration of the observer:
  var config = { attributes: true, childList: true, characterData: true }

  // pass in the target node, as well as the observer options
  observer.observe(target, config);

  "

  app = eventsApp()
  app$ui = bootstrapPage(
    jqueryLayoutHeader(),
    jqueryLayoutPanes(id="layout", style=style, json.opts = json.opts,
      north = autoResizeLayoutDiv(id = "north", layout.id="layout",
        h4("Title"),
        uiOutput("myout")
      ),
      center = div(
        actionButton("btn","Fill"),
        {
          txt =paste0("content ", 1:300, collapse=" ")
          p(txt)
        }
      ),
      west=div( actionButton("hideBtn","Hide")),
      east=div( actionButton("showBtn","Show west"))
    )
    #tags$script(HTML(js))
  )
  buttonHandler("hideBtn", function(...) {
    hide.jquery.pane("layout",c("west","center"))
  })
  buttonHandler("showBtn", function(...) {
    show.jquery.pane("layout",c("west","center"))
  })
  buttonHandler("btn", function(...) {
    txt =paste0("content ", 1:sample.int(n=100,size=1), collapse=" ")
    setUI("myout",p(txt))
    txt =paste0("content ", 1:sample.int(n=100,size=1), collapse=" ")
    setUI("myout",p(txt))
  })
  viewApp(app)

  app = eventsApp()
  app$ui = bootstrapPage(
    jqueryLayoutHeader(),
    frozenHeaderPane(id="layout",auto.resize = TRUE,
      head = tagList(
        h4("Title"),
        uiOutput("myout")
      ),
      content = div(
        actionButton("btn","Fill"),
        {
          txt =paste0("content ", 1:300, collapse=" ")
          p(txt)
        }
      )
    )
  )
  buttonHandler("btn", function(...) {
    txt =paste0("content ", 1:sample.int(n=100,size=1), collapse=" ")
    setUI("myout",p(txt))
    txt =paste0("content ", 1:sample.int(n=100,size=1), collapse=" ")
    setUI("myout",p(txt))
  })
  viewApp(app)

}

nested.layout.example = function() {

  app = eventsApp()
  app$ui = bootstrapPage(
    jqueryLayoutHeader(),
    actionButton("btn","Show Layout"),
    uiOutput("mainUI")
  )
  buttonHandler("btn", function(...) {
    ui = tagList(
      div(id="mydiv",style="height: 400px",
        jqueryLayoutPanes(id="mainLayout", parent = "#mydiv",
          center=div(paste0("Center ",1:5,collapse="\n ")),
          east=frozenHeaderPane(id="east",parent.layout = "mainLayout",parent.pane="east",
              head = div("Title Line",actionButton("addBtn","Add"), uiOutput("titleUI")),
              content = div(paste0("  east center ", 1:100, collapse=" "))
          )
        )
      )
    )
    setUI("mainUI",ui)
  })
  buttonHandler("addBtn", function(...) {
    dsetUI("titleUI",p(paste0("more title ", 1:sample.int(n=100,size=1), collapse = " ")))
    #resizeLayout("east")
  })
  viewApp(app)


}

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


frozenHeaderPane = function(id=NULL, header=NULL, content=NULL, parent="body", parent.layout=NULL, parent.pane="center", style = NULL, json.opts=NULL, auto.resize = TRUE) {
  restore.point("frozenHeaderPane")

  if (is.null(id)) {
    id = paste0("frozenHeaderPane" , sample.int(size = 1, n=100000000))
  }
  style = tags$style(HTML(paste0(
    style,'
    #',id,'_north {
  	  overflow:	hidden;
    }
  ')))

  if (is.null(json.opts)) {
    json.opts ="
defaults: {
  resizable: false,
  closable: false,
  slideable: false,
  spacing_open: 0
},
north: {
  size: 'auto'
}
  "
  }

  tagList(
    jqueryLayoutHeader(),
    jqueryLayoutPanes(id=id,parent = parent, parent.layout=parent.layout, parent.pane = parent.pane, style=style, json.opts = json.opts,
      north = autoResizeLayoutDiv(id = paste0(id,"_header"), layout.id=id, auto.resize=auto.resize,
        header
      ),
      center = content
    )
  )
}

#' A jqueryLayoutPage
#' @export
jqueryLayoutHeader = function(style=NULL) {
  restore.point("jqueryLayoutHeader")

  tagList(
    singleton(tags$head(tags$script(src="shared/jqueryui/jquery-ui.min.js"))),
    singleton(tags$head(tags$link(href="shared/jqueryui/jquery-ui.min.css", rel="stylesheet"))),
    singleton(tags$head(tags$link(href="shinyEventsUI/layout-default.css", type="text/css", rel="stylesheet"))),
    singleton(tags$head(tags$script(src="shinyEventsUI/jquery.layout.js"))),
    singleton(tags$head(tags$style("
.ui-layout-pane {
	background:	#FFF;
	border:		none;
	padding:	0px;
	overflow:	auto;
}
    ")))
  )

}

jqueryPaneOptions = function(resizable=TRUE, closable=TRUE, slideable=TRUE, spacing_open=10, spacing_closed=10) {
  nlist(resizable, closable, slideable, spacing_open, spacing_closed)
}

hide.jquery.pane = function(id, dir=c("north","west","east","south","center")[1]) {
  restore.point("hide.jquery.pane")
  js = paste0(id,'LayoutVar.hide("',dir,'");', collapse="\n")
  shinyEvents::evalJS(js);
}


show.jquery.pane = function(id, dir=c("north","west","east","south","center")[1], openPane="true") {
  restore.point("show.jquery.pane")

  js = paste0(id,'LayoutVar.show("',dir,'",',openPane,');',collapse="\n")
  shinyEvents::evalJS(js);
}

#' A jqueryLayoutPage
#' @export
jqueryLayoutPanes = function(id = NULL, center=NULL, north=NULL,west=NULL, east=NULL, south=NULL, panes=list( north=north, west=west,center=center, east=east, south=south), json.opts="", style=NULL, js.do.layout = TRUE, parent = "body", parent.layout = NULL, parent.pane = "center") {
  restore.point("jqueryLayoutPanes")

  if (is.null(id)) {
    id = paste0("jqueryLayout" , sample.int(size = 1, n=100000000))
    if (!is.null(parent.layout)) {
      id = paste0(parent.layout,"_",parent.pane,"_layout")
    }
  }



  for (dir in names(panes)) {
    if (!is.null(panes[[dir]])) {
      class = paste0("ui-layout-",dir)
      pane.id = paste0(id,"_", dir)
      panes[[dir]] = div(class=class, id=pane.id,panes[[dir]])
    }
  }

  if (is.list(json.opts)) {
    json.opts = toJSON(json.opts)
    json.opts = substring(json.opts,2,nchar(json.opts))
  }

  if (js.do.layout) {
    if (is.null(parent.layout)) {
      js = paste0('\nvar ',id,'LayoutVar = $("',parent,'").layout({', json.opts,'});')
    } else {
      js = paste0('\nvar ',id,'LayoutVar = ',parent.layout,'LayoutVar.panes.',parent.pane,'.layout({', json.opts,'});')
    }
  } else {
    js = NULL
  }
  tagList(
    bottomScript(HTML(js)),
    style,
    panes
  )
}

autoResizeLayoutDiv = function(id,  layout.id=NULL,..., layout.var = paste0(layout.id, "LayoutVar"), auto.resize = TRUE) {
  restore.point("autoResizeLayoutDiv")
  if (!auto.resize) {
    return(div(id=id,...))
  }

  js = paste0('
  // create an observer instance
  new MutationObserver(function(mutations) {
    //alert("Resize event!");
    ', layout.var,'.resizeAll();
  }).observe(
    document.querySelector("#',id,'"),
    {attributes: true,  subtree: true, childList: true, characterData: true }
  );
  ')

  tagList(
    div(id=id, ...),
    bottomScript(HTML(js))
  )
}

autoLayoutResizer = function(id,  layout.id=NULL,..., layout.var = paste0(layout.id, "LayoutVar")) {

}

#' Resize a jqueryLayout. May be useful if inner html has been dynamically changed and we have size="auto"
#' @export
resizeLayout = function(id, layout.var=paste0(id,"LayoutVar")) {
  js = paste0('
    ',layout.var,'.resizeAll();
  ')
  evalJS(js)
}

#' A jqueryLayoutPage
#' @export
jqueryLayoutPage = function(id="mainPane",center=NULL, north=NULL,west=NULL, east=NULL, south=NULL, panes=list(center=center, north=north, west=west, east=east, south=south), json.opts="", style=NULL,...) {
  restore.point("jqueryLayoutPage")


  panes = jqueryLayoutPanes(id=id, panes=panes,json.opts=json.opts,style = style)

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
    panes,
    ...
  )

}
