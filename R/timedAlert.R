#' show a message in some outputUI for duration of millis milliseconds
timedMessage = function(id,msg="",html=msg,ui=HTML(html), millis=3000, empty.msg = "", empty.ui=HTML(empty.msg), app=getApp()) {
  restore.point("timedMessage")
  try({
    setUI(id, ui)
    dsetUI(id, ui)
  })

  obs.id = paste0("..timedMessage..",id)
  flag.id = paste0("flag", obs.id)
  app[[flag.id]] = FALSE

  # destroy old observer
  if (!is.null(app[[obs.id]])) try(app[[obs.id]]$destroy())

  if (!is.finite(millis)) return()

  app[[obs.id]] = observe({
    if (!isTRUE(app[[flag.id]])) {
      app[[flag.id]] = TRUE
      invalidateLater(millis)
      return()
    }
    try(app[[obs.id]]$destroy())
    try({
      setUI(id, empty.ui)
      dsetUI(id, empty.ui)
    })
  })
}


colored.html = function(txt, color="#cc0000") {
  if (is.null(color)) return(txt)
  paste0("<font color='",color,"'>",txt,"</font>")
}


errorMessage = function(id,msg="",html=colored.html(msg,color="#cc0000"), ui=HTML(html), millis=Inf, empty.msg = "", empty.ui=HTML(empty.msg), app=getApp()) {
  restore.point("errorMessage")
  timedMessage(id=id,msg=msg, html=html, ui=ui, millis=millis, empty.msg="", empty.ui=empty.ui,app=app)
}

uiMessage = function(id,msg="",html=msg, ui=HTML(html), millis=Inf, empty.msg = "", empty.ui=HTML(empty.msg), app=getApp()) {
  restore.point("uiMessage")
  timedMessage(id=id,msg=msg, html=html, ui=ui, millis=millis, empty.msg="", empty.ui=empty.ui,app=app)
}
