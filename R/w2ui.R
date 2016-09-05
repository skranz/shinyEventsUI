examples.xw2ui = function() {
  restore.point.options(display.restore.point = TRUE)
  library(shinyEventsUI)
  app = eventsApp()
  library(dplyr)
  tabs = xw2tabs(id="myTabs",tabs=data_frame(id=paste0("tab",1:4),caption=paste0("tab",1:4), closable=TRUE,div_id = paste0("tab",1:4,"div")))


  app$ui = bootstrapPage(
    w2header(),
    div(
      tabs,
      div(id = "mainDiv",
        div(id="tab1div", "tab1"),
        div(id="tab2div", "tab2"),
        div(id="tab3div", "tab3"),
        div(id="tab4div", "tab4")
      ),
      actionButton("Hi!")
    )
  )
  clickHandler("myTabs", function(...) {
    args = list(...)
    restore.point("dbczudgvuzfgvzf")
    resizeLayout("myPanes")
    cat("clicked! ",sample(1000,1))
  })

  eventHandler("close", id="myTabs", function(...) {
    args = list(...)
    restore.point("tabClosedEvent")
    cat("closed!")
  })


  viewApp(app)
}



examples.w2ui = function() {
  restore.point.options(display.restore.point = TRUE)
  library(shinyEventsUI)
  app = eventsApp()

  json.opts ="
  defaults: {
    resizable: true,
    closable: false,
    slideable: true,
    spacing_open: 5
  },
  north: {
    size: 'auto',
    resizable: false,
    closable: false,
    slideable: false,
    spacing_open: 0
  },
  east: {
    resizable: true,
    spacing_open: 0,
    spacing_closed: 0,
    size: 0
  }
  "

  id = "mySidebar"
  img = NULL
  nodes = list(
    list(id = "level1", text = "Level 1", img="icon-folder", expanded=TRUE,
      nodes=data.frame(id = paste0("level1_",1:5), text = paste0("1-",1:5), img="icon-page", expanded=TRUE)
    ),
    list(id = "level2", text = "Level 2",img="icon-folder")
  )

  items = "[
  { type: 'check',  id: 'item1', caption: 'Check', img: 'icon-add', checked: true },
  { type: 'break' },
  { type: 'menu',   id: 'item2', caption: 'Drop Down', img: 'icon-folder',
      items: [
          { text: 'Item 1', img: 'icon-page' },
          { text: 'Item 2', img: 'icon-page' },
          { text: 'Item 3', img: 'icon-page' }
      ]
  },
  { type: 'break' },
  { type: 'radio',  id: 'item3',  group: '1', caption: 'Radio 1', img: 'icon-page' },
  { type: 'radio',  id: 'item4',  group: '1', caption: 'Radio 2', img: 'icon-page' },
  { type: 'spacer' },
  { type: 'button',  id: 'item5',  caption: 'Item 5', img: 'icon-save' }
  ]
  "

  items = list(
    list(type="menu",id = "file_menu", caption = "File",
      items=data.frame(
        text = paste0("Item ",1:3), img="icon-page"
      )
    ),
    list(type="break"),
    list(type="check",id ="menu_check",caption="Checkbox", checked=TRUE)
  )

  sidebar = w2sidebar(id="mySidebar", nodes=nodes)
  toolbar = w2toolbar(id="myToolbar", items=items,
    js.on.render="myPanesLayoutVar.resizeAll();")

  library(dplyr)
  tabs = w2tabs(id="myTabs",tabs=data_frame(id=paste0("tab",1:4),caption=paste0("tab",1:4), closable=TRUE, div_id = paste0("tab",1:4,"div")))


  app$ui = jqueryLayoutPage(id="myPanes", json.opts=json.opts,
    w2header(),
    north = div(toolbar,thinHR()),
    west = sidebar,
    center = div(
      tabs,
      div(id = "mainDiv",
        div(id="tab1div", "tab1"),
        div(id="tab2div", "tab2"),
        div(id="tab3div", "tab3"),
        div(id="tab4div", "tab4")
      ),
      p("Hi!")
    )
  )
  clickHandler("myTabs", function(...) {
    args = list(...)
    restore.point("dbczudgvuzfgvzf")
    resizeLayout("myPanes")
    cat("clicked!")
  })


  viewApp(app)
}

set.active.given.tabs = function(active, tabs) {
  if (NROW(tabs)==0) return(NULL)
  if (!is.numeric(active)) return(active)
  if (is.data.frame(tabs)) return(tabs$id[[1]])
  tabs[[1]]$id
}

w2tabs.add = function(id, tabs) {
  callJS("xw2ui.tabs_add",id=id,tabs=tabs)
}



w2tabs = function(id, active=1, tabs=NULL, js.on.render = NULL, add.header=TRUE) {
  restore.point("w2tabs")

  if (is.character(tabs)) {
    json = paste0('{"name": "',id,'", "items": ',tabs,"}")
    tabs = fromJSON(tabs)
    active = set.active.given.tabs(active, tabs)
  } else {
    active = set.active.given.tabs(active, tabs)
    li = list(name=id,active=active,tabs=tabs)
    json = toJSON(li,auto_unbox = TRUE,dataframe = "rows")
  }
  js = paste0('
    $(function () {
      xw2ui.xw2tabs(',json,', true);
      ',js.on.render,'
    });
  ')
  tagList(
    if (add.header) w2header() else NULL,
    singleton(tags$head(tags$script(type="text/javascript",src="shinyEventsUI/eventsUITools.js"))),
    div(id=id),
    tags$script(HTML(js))
  )
}


thinHR = function() {
  hr(style="padding: 0; margin:0; height=1px; color: #888888")
}

#' Header for w2ui widgets
#' @export
w2header = function(...) {
  restore.point("w2uiHeader")
  tagList(
    singleton(tags$head(tags$link(href="shinyEventsUI/w2ui.min.css", type="text/css", rel="stylesheet"))),
    singleton(tags$head(tags$script(type="text/javascript",src="shinyEventsUI/w2ui.min.js"))),
    singleton(tags$head(tags$script(type="text/javascript",src="shinyEventsUI/xw2ui.js")))

  )
}

w2nodes = function(id, text = "", img=c("icon-page","icon-folder")[1], expanded = FALSE, icon=NULL, selected=FALSE, nodes=NULL, as.data.frame = is.null(nodes)) {
  data_frame(id=id, text=text, img=img, expanded=expanded, selected=selected, nodes=nodes)
}

clickHandler = function(id, fun,..., eventId="click") {
  eventHandler(eventId=eventId, id=id, fun=fun,...)
}

w2sidebar = function(id, img=NULL,nodes=NULL,..., width="100%", height="100%", js.on.render="", add.header=TRUE) {
  restore.point("w2Sidebar")


  sidebar = list(name=id,img=img,nodes=nodes)
  json = toJSON(sidebar,auto_unbox = FALSE,dataframe = "rows")
  cat(json)
  eventId = "clickEvent"
  js = paste0('
    $(function () {
      $("#',id,'").w2sidebar(',json,');
      w2ui.',id,'.on("click", function(e) {
        var id = e.target;
        //if (id.constructor === Array) {id = id[0];}
        //alert(JSON.stringify(id));
        Shiny.onInputChange("',eventId,'", {eventId: "',eventId,'", id: id, class: "sidebar_node", sidebarId: "',id,'", nonce: Math.random()});
        //console.log(e);
      });
      ',js.on.render,'
    });
   ')
  tagList(
    if (add.header) w2header() else NULL,
    div(id=id, style=to.style(nlist(height, width))),
    tags$script(HTML(js))
  )
  #callJS(paste0('$("#',id,'mySidebar").w2sidebar', sidebar)

}

w2toolbar = function(id, items=NULL, js.on.render = NULL, add.header=TRUE) {
  restore.point("w2toolbar")


  if (is.character(items)) {
    json = paste0('{"name": "',id,'", "items": ',items,"}")
  } else {
    bar = list(name=id,items=items)
    json = toJSON(bar,auto_unbox = TRUE,dataframe = "rows")
  }
  eventId = "clickEvent"
  js = paste0('
    $(function () {
      $("#',id,'").w2toolbar(',json,');
      w2ui.',id,'.on("click", function(e) {
        var id = e.target;
        //alert(JSON.stringify(id));
        Shiny.onInputChange("',eventId,'", {eventId: "',eventId,'", id: id, class: "toolbar_item", toolbarId: "',id,'", nonce: Math.random()});
      });',
      js.on.render,'
    });
   ')
  tagList(
    if (add.header) w2header() else NULL,
    div(id=id),
    tags$script(HTML(js))
  )
}

