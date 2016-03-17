examples = function() {
  app = eventsApp()
  #viewApp(app)


  app = eventsApp()
  app$ui = freezeHeaderPage(
    freeze.header = TRUE, body.style="",
    header = HTML("I am a header"),
    left.right.div(
      div(style="margin-left: 5%; margin-right:5%;",
        HTML(paste0("content line ", 1:1000,collapse="\n"))
      ),
      div(style="margin-left: 5%; margin-right:5%;",
        HTML(paste0("content line ", 1:10,collapse="\n"))
      )
    )

  )

  app = eventsApp()
  app$ui = hlrPage(
    freeze.header = TRUE, body.style="",
    header = HTML("I am a header"),
    left = div(style="margin-left: 5%; margin-right:5%;",
      HTML(paste0("content line ", 1:200,collapse="\n"))
    ),
    right=div(style="margin-left: 5%; margin-right:5%;",
      HTML(paste0("content line ", 1:10,collapse="\n"))
    )

  )

    viewApp(app)

}

#' A page width a header frozen at the top and scrollbar for the content
#'
#' @export
hlrPage = function(header,left, right,  header.style="max-height: 5em; overflow: auto; margin-left: 5%; margin-right: 5%; padding-bottom: 5px", body.style="overflow: auto;", content.offset=5, freeze.header = TRUE, header.id="pageHeaderDiv", body.id="pageBodyDiv") {
  restore.point("hlrPage")

  if (!freeze.header) {
    header.class=""
    body.class = ""
  } else {
    header.class="FreezePaneHeader"
    left.class="FreezePaneLeft"
    right.class="FreezePaneRight"
  }

  style = tags$style(paste0('
.FreezePaneHeader {
  position: fixed; top:0; left: 0; width: 100%;
}

.FreezePaneLeft {
  position: fixed; top:50px; bottom: 0em; left:0; width:50%;
}
.FreezePaneRight {
  position: fixed; top:50px; bottom: 0em; right:0; width:200px;
}
'))
  script =tags$script(paste0('
function resizeFreezePane() {
  var height = $(".FreezePaneHeader").outerHeight();
  $(".FreezePaneLeft").css("top",height);
  $(".FreezePaneRight").css("top",height);
}
$(window).resize(function() {
  resizeFreezePane();
});
$(window).load( function() {
  resizeFreezePane();
});
$(".FreezePaneRight").resize(function() {
  var width = $(".FreezePaneHeader").outerWidth()-$(".FreezePaneRight").outerWidth();
  $(".FreezePaneLeft").css("width",width);
});
$(".FreezePaneRight").resizable({handles: "w"});
'
  ))
  tagList(
    singleton(tags$head(tags$script(src="shared/jqueryui/jquery-ui.min.js"))),
    singleton(tags$head(tags$link(href="shared/jqueryui/jquery-ui.min.css", rel="stylesheet"))),
    tags$head(script),
    singleton(tags$head(style)),
    div(id=header.id,class=header.class, style=header.style, header),
    div(class=left.class, style=body.style, left),
    div(class=right.class, style="border-left solid #dddddd; border-width: 4px;", right)

  )
}






left.right.resizable = function(left.content, right.content, handle.width="4px", handle.padding="4px", handle.border="solid #dddddd;", overflow.left="scroll") {
  restore.point("left.right.resizable")


  tdleft.style = paste0('padding-right: ',handle.padding,'; vertical-align: top; border-right: ',handle.border,'; border-width: ',handle.width,';  overflow: ', overflow.left,';'
  )
  tdright.style = paste0('
    padding-left: ',handle.padding,'; vertical-align: top;  height: 100%;'
  )

  tagList(
    singleton(tags$head(tags$script(src="shared/jqueryui/jquery-ui.min.js"))),
    singleton(tags$head(tags$link(href="shared/jqueryui/jquery-ui.min.css", rel="stylesheet"))),
    tags$table(style="width: 100%; height: 100%;",
      tags$td(class="resizable-td-left",style=tdleft.style,left.content),
      tags$td(class="resizable-td-right",style=tdright.style,right.content)
    ),
    bottomScript(HTML('$(".resizable-td-left").resizable({handles: "e"});'))
  )
}
