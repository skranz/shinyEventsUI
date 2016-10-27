examples.contextmenu = function() {
  app = eventsApp()

  js = '
 $(function() {
    $.contextMenu({
        selector: "#div1",
        callback: function(key, options) {
            var m = "clicked: " + key;
            window.console && console.log(m) || alert(m);
        },
        items: {
            "edit": {name: "Edit", icon: "fa-edit"},
            "cut": {name: "Cut", icon: "cut"},
            "copy": {name: "Copy", icon: "copy"},
            "paste": {name: "Paste", icon: "paste"},
            "delete": {name: "Delete", icon: "delete"}
        }
    });
    $.contextMenu({
        selector: "#div2",
        callback: function(key, options) {
            var m = "clicked: " + key;
            window.console && console.log(m) || alert(m);
        },
        items: {
            "edit Div2": {name: "Edit", icon: "edit"}
        }
    });

  });
  '
  app$ui = tagList(
    contextMenuHeader(),
    div(id="div1", p("Context Menu 1")),
    div(id="div2", p("Context Menu 2")),
    tags$script(HTML(js))
  )
  viewApp(app)
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

