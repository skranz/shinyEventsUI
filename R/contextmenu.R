examples.contextmenu = function() {
  app = eventsApp()

  js = '
 $(function() {
    $.contextMenu({
        selector: ".mydiv",
        callback: function(key, opt) {
          var target = opt.$trigger;
          var node = $.ui.fancytree.getNode(target);
          //alert("Clicked on " + key + " on element " + target.attr("id"));
        },
        items: {
            "edit": {name: "Edit", icon: "fa-edit"},
            "cut": {name: "Cut", icon: "cut"},
            "copy": {name: "Copy", icon: "copy"},
            "paste": {name: "Paste", icon: "paste"},
            "delete": {name: "Delete", icon: "delete"}
        }
    });
  });
  '

  items = list(
    edit = list(name="Edit", icon= "fa-edit"),
    cut = list(name= "Cut", icon= "cut")
  )
  #js = contextMenuJS(items=items, class="mydiv")
  app$ui = tagList(
    contextMenuHeader(),
    div(id="div1", class="mydiv", p("Context Menu 1")),
    div(id="div2", class="mydiv", p("Context Menu 2")),
    contextMenu(id="context", items=items, target.class="mydiv")
  )
  contextMenuHandler("context", function(..., app=getApp(), session=NULL) {
    args = list(...)
    restore.point("context")
    args
    cat("context menu clicked")
  })

  viewApp(app)
}



contextMenuHandler = function(id, fun,..., eventId="contextMenuClick") {
  eventHandler(eventId = eventId,id=id,fun = fun,...)
}

contextMenu = function(id,items,target.id=NULL,target.class=NULL, css.sel=make.css.sel(id=target.id,class=target.class), extra.return="", eventId="contextMenuClick") {
  restore.point("contextMenuJS")
  items.json = toJSON(items,auto_unbox = TRUE)

  js =  paste0('
$(function() {
  $.contextMenu({
    selector: "',css.sel,'",
    callback: function(key, opt) {
      var target = opt.$trigger;
      var node = $.ui.fancytree.getNode(target);
      //alert("Clicked on " + key + " on element " + target.attr("id"));
      Shiny.onInputChange("',eventId,'", {eventId: "',eventId,'", id: "',id,'", key: key, target_data: target.data(), target_id: target.attr("id"), target_class: target.attr("class"), ', extra.return, ' nonce: Math.random()});
    },
    items: \n',items.json,'
  });
});
')
  tags$script(js)
}




#' Header for jqueryContextMenu
#' @export
contextMenuHeader = function(...) {
  restore.point("contextmenuHeader")

  tagList(
    singleton(tags$head(tags$link(href=paste0("shinyEventsUI/jquery.contextMenu.min.css"), type="text/css", rel="stylesheet"))),
    singleton(tags$head(tags$script(type="text/javascript",src="shinyEventsUI/jquery.ui.position.min.js"))),
    singleton(tags$head(tags$script(type="text/javascript",src="shinyEventsUI/jquery.contextMenu.min.js")))
  )
}


#' Header for jqueryContextMenu
#' @export
uiContextMenuHeader = function(...) {
  restore.point("uiContextMenuHeader")
  tagList(
    singleton(tags$head(tags$script(src="shared/jqueryui/jquery-ui.min.js"))),
    singleton(tags$head(tags$link(href="shared/jqueryui/jquery-ui.min.css", rel="stylesheet"))),
    singleton(tags$head(tags$script(type="text/javascript",src="shinyEventsUI/jquery.ui-contextmenu.min.js")))
  )
}




make.css.sel = function(id=NULL, class=NULL, css.sel = NULL) {
  if (!is.null(css.sel)) return(css.sel)
  if (!is.null(id)) return(paste0("#",id))
  if (!is.null(class)) return(paste0(".",class))
  NULL
}
examples.uicontextmenu = function() {
  app = eventsApp()

  js = '
  $("#div1").contextmenu({
    menu: [
        {title: "Copy", cmd: "copy", uiIcon: "ui-icon-copy"},
        {title: "----"},
        {title: "More", children: [
            {title: "Sub 1", cmd: "sub1"},
            {title: "Sub 2", cmd: "sub1"}
            ]}
        ],
    select: function(event, ui) {
        alert("select " + ui.cmd + " on " + ui.target.text());
    }
  });
  $("#div2").contextmenu({
    menu: [
        {title: "Copy", cmd: "copy", uiIcon: "ui-icon-copy"},
        ],
    select: function(event, ui) {
        alert("select " + ui.cmd + " on " + ui.target.text());
    }
  });

  '
  app$ui = tagList(
    uiContextMenuHeader(),
    div(id="div1", p("Context Menu 1")),
    div(id="div2", p("Context Menu 2")),
    tags$script(HTML(js))
  )
  viewApp(app)
}

