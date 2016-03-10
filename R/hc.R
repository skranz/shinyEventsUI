example = function() {
  setwd("D:/libraries/shinyEventsUI/")
  html = paste0(readLines("test_header.html"), collapse="\n")
  app = eventsApp()
  app$ui = fluidPage(HTML(html))

  app$ui = freezeHeaderPage(
    freeze.header = TRUE,
    header = HTML("I am a header"),
    div(style="margin-left: 5%; margin-right:5%;",
      HTML(paste0("content line ", 1:1000,collapse="\n"))
    )
  )
  viewApp(app)
}

#' A page width a header frozen at the top and scrollbar for the content
#'
#' @export
freezeHeaderPage = function(header, ..., header.style="max-height: 5em; overflow: auto; margin-left: 5%; margin-right: 5%; padding-bottom: 5px", body.style="overflow: auto;", content.offset=5, freeze.header = TRUE, header.id="pageHeaderDiv", body.id="pageBodyDiv") {
  content = list(...)

  if (!freeze.header) {
    header.class=""
    body.class = ""
  } else {
    header.class="FreezePaneHeader"
    body.class="FreezePaneBody"
  }

  style = tags$style(paste0('
.FreezePaneHeader {
  position: fixed; top:0; left: 0; width: 100%;
}

.FreezePaneBody {
  position: fixed; top:50px; bottom: 0em; left:0; width:100%;
}
'))
  script =tags$script(paste0('
function resizeFreezePane() {
  var height = $(".FreezePaneHeader").outerHeight();
  $(".FreezePaneBody").css("top",height);
}
$(window).resize(function() {
  resizeFreezePane();
});
$(window).load( function() {
  resizeFreezePane();
});
'
  ))
  tagList(
    tags$head(script),
    singleton(tags$head(style)),
    div(id=header.id,class=header.class, style=header.style, header),
    div(id=body.id,class=body.class, style=body.style, content)
  )
}


