
examples.filetree = function() {
  restore.point.options(display.restore.point = TRUE)
  library(shinyEventsUI)
  app = eventsApp()

  root.dir = "D:/libraries/shinyEventsUI"
  id = "fileTree"

  tree = fancy.file.tree(id,
    folder.change.handler = function(new.dir,...) {
      args = list(...)
      restore.point("folder.change.handler")
   })

  library(shinyBS)
  app$ui = fluidPage(
    fancytreeHeader(),
    h5("Files"),
    bsButton("mkdirBtn","New Folder",size = "extra-small"),
    bsButton("renameBtn","Rename",size = "extra-small"),
    uiOutput("treeUI")
  )
  setUI("treeUI",tree)

  filetreeRenameButtonHandler("renameBtn",id)
  filetreeMakeDirButtonHandler("mkdirBtn",id)



  filetreeButtonHandler("fileInfoBtn",id,function(...) {
    args = list(...)
    restore.point("fileInfoBtn")
    cat("\nfileInfoBtn")
    df = args$file.df
    df
  })

  viewApp(app)


}

modal.msg = function(msg,title="",...) {
  showModal(modalDialog(HTML(msg)))
}

filetreeRenameButtonHandler = function(id, treeId, msg.fun=modal.msg, listener=NULL) {
  restore.point("filetreeRenameButtonHandler")


  filetreeButtonHandler(id,treeId,function(...) {
    args = list(...)
    restore.point("filetreeRenameButtonHandler.inner")

    file.df = args$file.df
    n = sum(file.df$selected)
    if (n != 1) {
      msg.fun("You have to check exactly 1 file to rename.")
      return()
    }

    sel.file = file.df$name[file.df$selected]
    sel.type = file.df$type[file.df$selected]
    root.dir = args$root.dir
    cur.dir = args$cur.dir


    buttonHandler("filetreeRenameCancelBtn", function(...) {
      removeModal()
    })

    buttonHandler("filetreeRenameOkBtn", function(...) {
      args = list(...)
      new = getInputValue("filetreeRenameTextInput")
      restore.point("filetreeRenameOkBtn")
      if (nchar(new)>0) {
        res = try(file.rename(from=file.path(cur.dir, sel.file),to = file.path(cur.dir, new)))
      }
      refresh.filetree(treeId, root.dir, cur.dir)
      removeModal()
    })



    showModal(modalDialog(size = "s", title = paste0("Rename ",sel.type," ",sel.file),footer = NULL,
      textInput("filetreeRenameTextInput",paste0("Please enter the new ", sel.type," name"),""),
      actionButton("filetreeRenameOkBtn","Ok"),
      actionButton("filetreeRenameCancelBtn","Cancel")
    ))
  })
}


filetreeMakeDirButtonHandler = function(id, treeId, listener=NULL) {
  restore.point("filetreeMakeDirButtonHandler")
  filetreeButtonHandler(id,treeId,function(...) {
    args = list(...)
    restore.point("filetreeMakeDirButtonHandler.inner")
    cat("\nfileInfoBtn")
    root.dir = args$root.dir
    cur.dir = args$cur.dir


    buttonHandler("mkdirCancelBtn", function(...) {
      args = list(...)
      restore.point("mkdirCancelBtnClicked")
      removeModal()
      cat("\nCancel pressed.")
    })

    buttonHandler("mkdirOkBtn", function(...) {
      args = list(...)
      newdir = getInputValue("mkdirTextInput")
      restore.point("mkdirOkBtnClicked")
      if (nchar(newdir)>0) {
        res = try(dir.create(file.path(cur.dir,basename(newdir))))
      }
      refresh.filetree(treeId, root.dir, cur.dir)
      removeModal()


    })



    showModal(modalDialog(title = "New Folder",footer = NULL,
      textInput("mkdirTextInput","Please enter the new folder name",""),
      actionButton("mkdirOkBtn","Ok"),
      actionButton("mkdirCancelBtn","Cancel")
    ))
  })



}

filetreeButtonHandler = function(id, treeId, fun, event="click",stop.propagation=TRUE,eventId=paste0(id,"___",treeId, "___filetreeButton"), ...) {

  restore.point("filefiletreeButtonHandler")

  #eventId = paste0(treeId,"___Button")


  customEventHandler(eventId=eventId,css.locator = paste0("#",id), id=id, event="click",extra.shiny.value.code = paste0("treeInfo: getFileTreeEventJson('",treeId,"')"),stop.propagation=stop.propagation,fun = function(...) {
    args = list(...)
    restore.point("filetreeButtonHandler")
    h = args$treeInfo[[1]]

    df = as.data.frame(do.call(rbind,args$treeInfo)) %>%
      select(-nodeType, -curdir,-rootdir) %>%
      rename(name=itemId,type=itemType) %>%
      filter(!type %in% c("header","upfolder"))
    df$selected = unlist(df$selected)
    args$file.df = df
    args$root.dir = h$rootdir
    args$cur.dir = h$curdir

    do.call(fun,args)
  } )

}

fancy.file.tree = function(id="fileTree", root.dir=getwd(), cur.dir=root.dir, add.checkbox = TRUE, nodeCol=1, checkboxCol=0, file.click.handler=NULL, folder.change.handler=NULL, auto.folder.change=TRUE, ...) {

  restore.point("fancy.file.tree")

  file.nodes = fancy.file.tree.nodes(root.dir, cur.dir)

  js.render = paste0('
    cols.eq(2).text(node.data.size);
    cols.eq(3).html(node.data.mtime);
  ')

  tree = fancytree.table(id=id,col.width=c("*"), num.cols=4,keyboard=FALSE,tabable=FALSE,js.render=js.render,source=file.nodes, nodeCol=nodeCol, checkboxCol=checkboxCol)

  clickHandler("fileTree",fun = function(...) {
    args = list(...)
    data = args$data
    restore.point("filetree.click")
    if (data$itemType=="folder") {
      new.dir = file.path(data$curdir,data$itemId)
      # update tree to new folder
      if (auto.folder.change) {
        source = fancy.file.tree.nodes(data$rootdir, new.dir)
        fancytree.update.source(id, source)
      }
      if (!is.null(folder.change.handler)) {
        folder.change.handler(id=id,new.dir=new.dir,short.folder=data$itemId, from=data$curdir,root.dir=root.dir,data=data, upfolder=FALSE)
      }
    } else if (data$itemType=="upfolder") {
      new.dir = data$itemId
      # update tree to new folder
      if (auto.folder.change) {
        source = fancy.file.tree.nodes(root.dir, new.dir)
        fancytree.update.source(id, source)
      }
      if (!is.null(folder.change.handler)) {
        folder.change.handler(id=id,new.dir=new.dir,short.folder="..", from=data$curdir,root.dir=root.dir, data=data, upfolder=TRUE)
      }

    } else if (data$itemType=="file") {
      if (!is.null(file.click.handler)) {
        file.click.handler(id=id,file=data$itemId, from=cur.dir,data=data)
      }
    }
  })


  tree
}


refresh.filetree = function(id, root.dir, cur.dir) {
  source = fancy.file.tree.nodes(root.dir, cur.dir)
  fancytree.update.source(id, source)


}

fancy.file.tree.nodes = function(root.dir, cur.dir) {
  restore.point("fancy.file.tree.nodes")


  full.files = list.files(cur.dir,include.dirs = TRUE,full.names = TRUE)
  files = list.files(cur.dir,include.dirs = TRUE,full.names = FALSE)
  df = file.info(full.files) %>%
    mutate(file=files) %>%
    arrange(-isdir, file)

  df$Size = file.size.string(df$size)

  df$type = ifelse(df$isdir,"folder","file")
  df$modified = as.character(df$mtime)
  files = df$file


  if (length(files)>0) {
    file.nodes = data_frame(key = paste0("file___",files), title=files, icon=TRUE, folder=df$isdir, expanded=FALSE, nodeType = df$type,itemId=files, itemType=df$type,mtime=df$modified, size=df$Size, curdir=cur.dir, rootdir=root.dir)
  } else {
    file.nodes = NULL
  }


  if (!identical(root.dir, cur.dir)) {
    head.nodes = data_frame(key = paste0("upfolder"), title=paste0(".. (", substring(cur.dir,nchar(root.dir)+2),")"), icon=FALSE, folder=FALSE, expanded=FALSE, nodeType = "upfolder",itemId=dirname(cur.dir), itemType="upfolder",mtime="", size="",curdir=cur.dir, rootdir=root.dir)
    file.nodes = rbind(head.nodes, file.nodes)
  }
  label.nodes = data_frame(key = paste0("header"), title="Name", icon=FALSE, folder=FALSE, expanded=FALSE, nodeType = "header",itemId=cur.dir, itemType="header",mtime="Modified", size="Size",curdir=cur.dir, rootdir=root.dir)

  rbind(label.nodes, file.nodes)

}


file.size.string = function(bytes, zero.val="") {
  size = as.character(bytes)
  size =
    ifelse(bytes ==0, zero.val,
    ifelse(bytes < 1e3, paste0(bytes, " B"),
    ifelse(bytes < 1e6, paste0(round(bytes / 1e3,1), " KB"),
    ifelse(bytes < 1e9, paste0(round(bytes / 1e6,1), " MB"),
    ifelse(bytes < 1e12, paste0(round(bytes / 1e9,1), " GB"),
    paste0(round(bytes / 1e9,1), " TB")
  )))))
  size
}

#' A custom event handler. Need to write correct css.locator
#' @export
filetreeEventHandler = function(eventId, fun, css.locator, event="change", inner.js.code="", shiny.extra.values=NULL, id=NULL,stop.propagation=FALSE,...) {
  restore.point("customEventHandler")

  if (is.null(inner.js.code)) {
    inner.js.code = 'var value = $(this).val();'
  }
  if (is.null(shiny.value.code)) {
    shiny.value.code = paste0('{eventId:"',eventId,'",id: this.id, value: $(this).val(),  data: $(this).data(),nonce: Math.random()}')
  }
  sp = if (stop.propagation) "\ne.stopPropagation();" else ""

  jscript = paste0('
$("body").on("',event,'", "',css.locator,'",function(e) {
  ',inner.js.code,sp,'
  Shiny.onInputChange("',eventId,'", ', shiny.value.code,');
});
')
  eventHandler(eventId=eventId,id=id,fun=fun,...,jscript=jscript)
}

