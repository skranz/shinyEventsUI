examples.handsontable = function() {
  app = eventsApp()

  dat = data.frame(variants=c("variants↓ params→", "base","highStake"),c("numPlayers",2,""), c("cake",4,10))
  toJSON(as.matrix(dat))
  js.opts = '{
    contextMenu: true,
    allowInsertColumn: true,
    allowDeleteColumn: true,
    allowInsertRow: true,
    fixedColumnsLeft: 2,
    fixedRowsTop: 1,
    cell: [
      {row: 1, col: 1, readOnly: true}
    ]
  }'
  css = '
    #mytab .ht_clone_left td {
      background-color: #eee;
    }
    #mytab .ht_clone_top td {
      background-color: #eee;
    }
  '

  hot = handsontable("mytab", as.matrix(dat),js.opts=js.opts)
  app$ui = tagList(
    handsontableHeader(),
    tags$style(HTML(css)),
    hot,
    HTML("Below Table")
  )
  viewApp(app)
}

#' Header for jqueryContextMenu
#' @export
handsontableHeader = function(...) {
  restore.point("hansontableHeader")
  tagList(
    singleton(tags$head(tags$link(href="shinyEventsUI/handsontable.full.min.css", rel="stylesheet"))),
    singleton(tags$head(tags$script(type="text/javascript",src="shinyEventsUI/handsontable.full.min.js")))
  )
}

handsontable = function(id,data, opts=list(), js.opts=toJSON(opts, auto_unbox=TRUE)) {
  restore.point("handsontable")

  data.json = toJSON(as.matrix(data))
  js = paste0('var ',id,'__handsontable =  new Handsontable(document.getElementById("',id,'"),', js.opts,');
    ', id,'__handsontable.loadData(',data.json,');'
  )
  tagList(
    div(id=id),
    tags$script(HTML(js))
  )
}
