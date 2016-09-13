

examples.fancytree = function() {
  library(rmdtools)
  setwd("D:/libraries/shinyEventsUI/")
  view.html(file = "fancytree.html")

  restore.point.options(display.restore.point = TRUE)
  library(shinyEventsUI)
  app = eventsApp()
  library(dplyr)
  tree = fancytree(theme="win8",id="myTree",source=list(
    list(
      key="games",title="games", expanded=TRUE,
      folder = TRUE,children = data_frame(key=paste0("node",1:4),title=paste0("game",1:4), type="game", gameId = "games234")
    ),
    list(key="prefs",title="prefs")
  ))

  app$ui = jqueryLayoutPage(
    west=div(tree),
    center=div(p("Tree above..."))
  )
  clickHandler("myTree", function(...) {
    args = list(...)
    restore.point("myTree_click")
    cat("clicked! ",sample(1000,1))
  })

  viewApp(app)
}


examples.fancytree = function() {
  library(rmdtools)
  setwd("D:/libraries/shinyEventsUI/")
  view.html(file = "fancytree.html")

  restore.point.options(display.restore.point = TRUE)
  library(shinyEventsUI)
  app = eventsApp()
  library(dplyr)
  tree = fancytree(theme="win8",id="myTree",source=list(
    list(
      key="games",title="games", expanded=TRUE,
      folder = TRUE,children = data_frame(key=paste0("node",1:4),title=paste0("game",1:4), type="game", gameId = "games234")
    ),
    list(key="prefs",title="prefs")
  ))

  app$ui = jqueryLayoutPage(
    west=div(tree),
    center=div(p("Tree above..."))
  )
  clickHandler("myTree", function(...) {
    args = list(...)
    restore.point("myTree_click")
    cat("clicked! ",sample(1000,1))
  })

  viewApp(app)
}




#' Header for fancytree
#' @export
fancytreeHeader = function(...,extensions=NULL, theme="win8") {
  restore.point("fancytreeHeader")
  ext.li = list()
  if ("table" %in% extensions) {
    ext.li = c(ext.li,list(singleton(tags$head(tags$script(type="text/javascript",src="shinyEventsUI/fancytree/jquery.fancytree.table.js")))))
  }
  if ("gridnav" %in% extensions) {
    ext.li = c(ext.li,list(singleton(tags$head(tags$script(type="text/javascript",src="shinyEventsUI/fancytree/jquery.fancytree.gridnav.js")))))
  }
  if ("dnd" %in% extensions) {
    ext.li = c(ext.li,list(singleton(tags$head(tags$script(type="text/javascript",src="shinyEventsUI/fancytree/jquery.fancytree.dnd.js")))))
  }
  tagList(
    singleton(tags$head(tags$script(src="shared/jqueryui/jquery-ui.min.js"))),
    singleton(tags$head(tags$link(href="shared/jqueryui/jquery-ui.min.css", rel="stylesheet"))),
    singleton(tags$head(tags$link(href=paste0("shinyEventsUI/fancytree/skin-",theme,"/ui.fancytree.min.css"), type="text/css", rel="stylesheet"))),
    singleton(tags$head(tags$script(type="text/javascript",src="shinyEventsUI/fancytree/jquery.fancytree.min.js"))),
    ext.li
  )
}

#' fancytree
#' @export
fancytree = function(id, source=NULL,..., theme="win8",add.header=TRUE, extensions="") {
  restore.point("fancytree")
  obj = list(source=source,...)
  json = toJSON(obj,auto_unbox = FALSE,dataframe = "rows")
  js = paste0('
    $(function () {
      $("#',id,'").fancytree(',json,');
      $("#',id,'").on("fancytreeclick", function(e, data){
        var id = data.node.key;
        //alert(id);
        Shiny.onInputChange("click", {eventId: "click", id: "',id,'", class: "fancytree", nodeId: id, data: data.node.data, nonce: Math.random()});
      });
    });
   ')
  tagList(
    if (add.header) fancytreeHeader(theme=theme, extensions=extensions) else NULL,
    div(id=id),
    tags$script(HTML(js))
  )
}

examples.fancytree.table = function() {
  restore.point.options(display.restore.point = TRUE)
  library(shinyEventsUI)
  app = eventsApp()
  library(dplyr)
  js.render = '
    //node, cols
    cols.eq(1).html(\'<input type="text" name="firstname" value="Enter text here">\');
  '
  tree = fancytree.table(num.cols=2,col.width=NULL, col.header = 1:2, js.render=js.render,theme="win8",id="myTree",keyboard=FALSE,tabable=FALSE,source=list(
    list(
      key="games",title="games", expanded=TRUE,
      folder = TRUE,children = data_frame(key=paste0("node",1:4),title=paste0("game",1:4), icon = FALSE, type="game", gameId = "games234")
    ),
    list(key="prefs",title="prefs")
  ))

  app$ui = bootstrapPage(
    div(tree),
    div(p("Tree above..."))
  )
  clickHandler("myTree", function(...) {
    args = list(...)
    restore.point("myTree_click")
    cat("clicked! ",sample(1000,1))
  })

  viewApp(app)
}


fancytree.table = function(id, source=NULL,extensions=c("table","gridnav") ,nodeCol=0,table=list(nodeColumnIdx=nodeCol, indentation=16,checkboxColumnIdx=NULL), gridnav=list(autofocusInput=FALSE, handleCursorKeys=TRUE),..., theme="win8",add.header=TRUE,
num.cols=2, col.width = paste0(round(100/num.cols,2),"%"), col.header = rep("", num.cols)
  ) {
  obj = nlist(source,extensions,table,gridnav,js.render="",...)
  restore.point("fancytree.table")
  json = toJSON(obj,auto_unbox = FALSE,dataframe = "rows")
  json = add.to.json(json,paste0(',
    renderColumns: function(e, data){
        var node = data.node,
            cols = $(node.tr).find(">td");
        ',js.render,'
    }'
  ))

  js = paste0('

  $(function () {
      $("#',id,'").fancytree(',json,');
      $("#',id,'").on("fancytreeclick", function(e, data){
        var id = data.node.key;
        //alert(id);
        Shiny.onInputChange("click", {eventId: "click", id: "',id,'", class: "fancytree_node", nodeId: id, data: data.node.data, nonce: Math.random()});
      });
    });
   ')


  if (!is.null(col.width)) {
    colgroup = paste0('
    <colgroup>
    ',paste0('<col width="',col.width,'"></col>',collapse="\n"),'
    </colgroup>'
    )
  } else {
    colgroup = ""
  }
  table = paste0('<table id="',id,'" style="width=100%">
    ', colgroup,'
    <thead>
        <tr>', paste0("<th>", col.header,"</th>", collapse=" "),'</tr>
    </thead>
    <tbody>
    </tbody>
  </table>
  ')
  tagList(
    if (add.header) fancytreeHeader(theme=theme,extensions=extensions) else NULL,
    HTML(table),
    tags$script(HTML(js))
  )
}

add.to.json = function(json, txt) {
  paste0(str.remove.ends(json,right=1),txt,"}")

}
