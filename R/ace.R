examples.ace = function() {
  library(shinyAce)
  app = eventsApp()

  ace = aceEditorHtml("myAce",value="Hello World!", fontSize=14, fontFamily = '"Lucida Console", Consolas;', height="100%", tabSize=2)
  app$ui = bootstrapPage(
    aceEditorHeader(),
    p("Title"),
    div(style = "height: 700px",    HTML(ace))
    #,tags$script("editor__myAce.resize();")
  )
  viewApp(app)
}

aceEditorHtml <- function(
  outputId, value, mode, theme, vimKeyBinding = FALSE,
  readOnly=FALSE, height="400px",
  fontSize=12, tabSize=4, fontFamily="Arial", debounce=1000, wordWrap=FALSE,
  showLineNumbers = TRUE, highlightActiveLine=TRUE,
  selectionId=NULL, cursorId=NULL, hotkeys=NULL,
  autoComplete=c("disabled", "enabled", "live"),
  autoCompleteList=NULL
){
  editorVar = paste0("editor__",sanitizeId(outputId))
  js <- paste("var ", editorVar," = ace.edit('",outputId,"');",sep="")
  if (!missing(theme)){
    js <- paste(js, "", editorVar,".setTheme('ace/theme/",theme,"');",sep="")
  }
  if (vimKeyBinding){
    js <- paste(js, "", editorVar,".setKeyboardHandler('ace/keyboard/vim');",sep="")
  }
  if (!missing(mode)){
    js <- paste(js, "", editorVar,".getSession().setMode('ace/mode/",mode,"');", sep="")
  }
  if (!missing(value)){
    value = paste(value, collapse="\n")
    js <- paste(js, "", editorVar,".setValue(", shinyAce:::jsQuote(value), ", -1);", sep="")
  }
  if (!showLineNumbers) {
    js <- paste(js, "", editorVar,".renderer.setShowGutter(false);", sep="")
  }
  if (!highlightActiveLine) {
    js <- paste(js, "", editorVar,".setHighlightActiveLine(false);", sep="")
  }

  if (readOnly){
    js <- paste(js, "", editorVar,".setReadOnly(", shinyAce:::jsQuote(readOnly), ");", sep="")
  }
  if (!is.null(fontSize) && !is.na(as.numeric(fontSize))){
    js <- paste(js, "document.getElementById('",outputId,"').style.fontSize='",
                as.numeric(fontSize), "px'; ", sep="")
  }

  if (!is.null(debounce) && !is.na(as.numeric(debounce))){
     # I certainly hope there's a more reasonable way to compare
    # versions with an extra field in them...
    re <- regexpr("^\\d+\\.\\d+(\\.\\d+)?", utils::packageVersion("shiny"))
    shinyVer <- substr(utils::packageVersion("shiny"), 0, attr(re, "match.length"))
    minorVer <- as.integer(substr(utils::packageVersion("shiny"),
      attr(re, "match.length")+2,
      nchar(utils::packageVersion("shiny"))))
    comp <- utils::compareVersion(shinyVer, "0.9.1")
    if (comp < 0 || (comp == 0 && minorVer < 9004)){
      warning(
      "Shiny version 0.9.1.9004 required to use input debouncing in shinyAce.")
    }

    js <- paste(js, "$('#",outputId,"').data('debounce',",debounce,");",
                sep="")
  }

  if (wordWrap){
    js <- paste(js, "", editorVar,".getSession().setUseWrapMode(true);", sep="")
  }

  # https://learn.jquery.com/using-jquery-core/faq/how-do-i-select-an-element-by-an-id-that-has-characters-used-in-css-notation/
  escapedId <- gsub("\\.", "\\\\\\\\.", outputId)
  escapedId <- gsub("\\:", "\\\\\\\\:", escapedId)
  js <- paste(js, "$('#",escapedId,"').data('aceEditor',", editorVar,");", sep="")

  if (!is.null(selectionId)){
    selectJS <- paste("", editorVar,".getSelection().on(\"changeSelection\", function(){
      Shiny.onInputChange(\"",selectionId,
      "\",", editorVar,".getCopyText());})",
      sep="")
    js <- paste(js, selectJS, sep="")
  }

  if (!is.null(cursorId)){
    curJS <- paste("\n", editorVar,".getSelection().on(\"changeCursor\", function(){
      Shiny.onInputChange(\"",cursorId,
      "\",", editorVar,".selection.getCursor() );}\n);",
    sep="")
    js <- paste(js, curJS, sep="")
  }

  for (i in seq_along(hotkeys)) {
    shortcut = hotkeys[[i]]
    if (is.list(shortcut)) {
      shortcut = paste0(names(shortcut),": '", shortcut,"'", collapse=", ")
    } else {
      shortcut = paste0("win: '",shortcut,"',  mac: '",shortcut,"'")
    }

    id = names(hotkeys)[i]
    code = paste0("
    ",editorVar,".commands.addCommand({
        name: '",id,"',
        bindKey: {", shortcut,"},
        exec: function(",editorVar,") {
          Shiny.onInputChange(\"",id,
          "\",{
            editorId : '",outputId,"',
            selection: ", editorVar,".session.getTextRange(",editorVar,".getSelectionRange()),
            cursor : ", editorVar,".selection.getCursor(),
            randNum : Math.random()
          });
        },
        readOnly: true // false if this command should not apply in readOnly mode
    });
    ")
    js = paste0(js, code)
  }

  autoComplete <- match.arg(autoComplete)
  if(autoComplete != "disabled") {
    js <- paste(js, "", editorVar,".setOption('enableBasicAutocompletion', true);", sep="")
  }
  if(autoComplete == "live") {
    js <- paste(js, "", editorVar,".setOption('enableLiveAutocompletion', true);", sep="")
  }
  js <- paste(js, "", editorVar,".setOption('tabSize',",tabSize,");", sep="")

  if (!is.null(height)) {
    style=paste("height:",height)
  } else {
    style = "position: absolute; top: 0; right: 0; bottom: 0;left: 0;"
  }
  if (!missing(fontFamily)) {
    style = paste0(style,"; font-family: ", fontFamily,";")
  }

  as.character(tagList(
    pre(id=outputId, class="shiny-ace",
        style=style,
        `data-autoCompleteList` = autoCompleteList
    ),
    tags$script(type="text/javascript", HTML(js))
  ))
}

aceEditorHeader = function() {
  shinyAce:::initResourcePaths()
  singleton(tags$head(
    tags$script(src = 'shinyAce/ace/ace.js'),
    tags$script(src = 'shinyAce/ace/ext-language_tools.js'),
    tags$script(src = 'shinyAce/shinyAce.js'),
    tags$link(rel = 'stylesheet',
              type = 'text/css',
              href = 'shinyAce/shinyAce.css')
  ))
}

sanitizeId <- function(id){
  gsub("[^[:alnum:]]", "", id)
}
