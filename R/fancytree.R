

examples.fancytree = function() {
  library(rmdtools)
  setwd("D:/libraries/shinyEventsUI/")

  restore.point.options(display.restore.point = TRUE)
  library(shinyEventsUI)
  app = eventsApp()
  library(dplyr)
  tree = fancytree(theme="win8",id="myTree",source=list(
    list(
      key="games",title="games", expanded=FALSE,
      folder = TRUE,children = data_frame(key=paste0("node",1:4),title=paste0("game",1:4), type="game", gameId = "games234")
    ),
    list(key="prefs",title="prefs", expanded=FALSE)
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
  #view.html(file = "fancytree.html")

  restore.point.options(display.restore.point = TRUE)
  library(shinyEventsUI)
  app = eventsApp()
  library(dplyr)
  tree = fancytree(theme="win8",id="myTree",source=list(
    list(
      key="games",title="games", expanded=FALSE,
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
fancytree = function(id, source=NULL,..., theme="win8",add.header=TRUE, extensions="", auto_unbox=TRUE) {
  restore.point("fancytree")
  obj = list(source=source,...)
  json = toJSON(obj,auto_unbox = auto_unbox,dataframe = "rows")
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


  js.render = paste0('
    //node, cols
    if (node.data.type =="game") {
      cols.eq(1).html(\'<button id="rowButton\'+node.data.gameId+\'" class="gameBtn" type="button">Clickme</button>\');
      cols.eq(2).html(\'<input type="text" name="firstname" value="Enter text here">\');
    }
  ')
  js.render = paste0('
    //node, cols
    if (node.data.type =="game") {
      ',fancytree.table.button(2,"rowButton","node.data.gameId","ClickMe"),'
      ',fancytree.table.textInput(3,"textInput","node.data.gameId","node.data.gameId"),'
      ',fancytree.table.fileInput(4,"fileInput","node.data.gameId"),'
    }
  ')



  tree = fancytree.table(num.cols=4,col.width="*", js.render=js.render,id="myTree",keyboard=FALSE,tabable=FALSE,source=list(
    list(
      key="games",title="games",icon=TRUE, expanded=FALSE,
      folder = FALSE,children = data_frame(key=paste0("node",1:4),title=paste0("game",1:4), icon = FALSE, type="game", gameId = paste0("game",1:4))
    ),
    list(key="prefs",title="prefs")
  ))

  app$ui = bootstrapPage(
    div(tree),
    div(p("Tree above..."))
  )

  classEventHandler("rowButton",stop.propagation = TRUE, event = "click",
    function(...){
      args = list(...)
      data = args$data
      restore.point("rowButton_click")
      cat("clicked button ",sample(1000,1))

    }
  )

  clickHandler("myTree", function(...) {
    args = list(...)
    restore.point("myTree_click")
    cat("clicked! ",sample(1000,1))
  })

  viewApp(app)
}


fancytree.table = function(id, js.render, source=NULL,extensions=c("table","gridnav") ,nodeCol=0, checkboxCol=NULL, checkbox=!is.null(checkboxCol), table=list(nodeColumnIdx=nodeCol, indentation=16,checkboxColumnIdx=checkboxCol), gridnav=list(autofocusInput=FALSE, handleCursorKeys=TRUE),..., theme="win8",add.header=TRUE,
num.cols=2, col.width = paste0(round(100/num.cols,2),"%"), col.header = rep("", num.cols)
  ) {
  obj = nlist(source,extensions,table,gridnav,js.render="",checkbox,...)
  restore.point("fancytree.table")

  col.header = c(col.header,rep("", num.cols))[seq.int(num.cols)]

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
    col.width = rep(col.width, length.out=num.cols)
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

fancytree.table.button = function(col=2, class.id, row.id, label="Clickme", class="xs-small",...) {
  paste0('
    cols.eq(',col-1,').html(\'<button id="', class.id,'_\'+',row.id,'+\'" class="',class.id,' ',class, '" type="button" data-rowid=\'+',row.id,'+\'>',label,'</button>\');
  ')
}

fancytree.table.textInput = function(col=2, class.id, row.id,value=paste0('"',string.value,'"'), class="",string.value="",...) {
  paste0('
    cols.eq(',col-1,').html(\'<input type="text" id="', class.id,'_\'+',row.id,'+\'" name="', class.id,'_\'+',row.id,'+\'"  class="',class.id,' ',class, '" data-rowid="',row.id,'" value="\'+',value,'+\'"/>\');
  ')

}


fancytree.table.fileInput = function(col=2, class.id, row.id, button.label="upload",progress.bar = TRUE,...) {
  restore.point("fancytree.table.fileInput")

  id = paste0(class.id,'_\'+',row.id,'+\'')

  progress.html = if (progress.bar) {
    paste0('<div id="',id,'_progress" class="progress progress-striped active shiny-file-input-progress"><div class="progress-bar"></div></div>')
  } else {
    ""
  }

  paste0('cols.eq(',col-1,').html(\'<div class=\"form-group shiny-input-container\" style="margin-bottom: 0;"><div class=\"input-group\">    <label class=\"input-group-btn\">      <span class=\"btn btn-default btn-file\">', button.label,'<input id=\"',id,'\" name=\"',id,'\" type=\"file\" style=\"display: none;\"/>      </span>    </label>    <input style="" type=\"text\" class=\"form-control\" placeholder=\"No file selected\" readonly=\"readonly\"/>  </div>', progress.html,'</div>\');'
  )
}


add.to.json = function(json, txt) {
  paste0(str.remove.ends(json,right=1),txt,"}")

}

# update all source nodes
fancytree.update.source = function(id, source) {
  restore.point("fancytree.update.source")
  obj = source
  json = toJSON(obj,auto_unbox = TRUE,dataframe = "rows")
  js = paste0('$("#',id,'").fancytree("option","source",',json,');')
  evalJS(js)
}
