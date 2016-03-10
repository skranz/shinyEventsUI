example = function() {
  setwd("D:/libraries/shinyEventsUI/")
  html = paste0(readLines("test_header.html"), collapse="\n")
  app = eventsApp()
  app$ui = fluidPage(HTML(html))

  app$ui = freezeHeaderPage(
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
freezeHeaderPage = function(header, ..., header.style="max-height: 5em; overflow: auto; margin-left: 5%; margin-right: 5%; padding-bottom: 5px", content.style="overflow: auto;", content.offset=5) {
  content = list(...)

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
    div(class="FreezePaneHeader", style=header.style, header),
    div(class="FreezePaneBody", style=content.style, content)
  )
}
