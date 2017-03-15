#' show a message in some outputUI for duration of millis milliseconds
timedMessage = function(id,msg="",html=msg,ui=HTML(html), millis=3000, empty.msg = "", empty.ui=HTML(empty.msg), app=getApp()) {
  restore.point("timedMessage")
  setUI(id, ui)
  dsetUI(id, ui)

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
    setUI(id, empty.ui)
    dsetUI(id, empty.ui)
  })
}