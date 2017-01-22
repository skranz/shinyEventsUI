
examples.filetree = function() {
  restore.point.options(display.restore.point = TRUE)
  library(shinyEventsUI)
  app = eventsApp()

  root.dir = getwd()
  id = "fileTree"

  tree = fancy.file.tree(id,
    folder.change.handler = function(new.dir,...) {
      args = list(...)
      restore.point("folder.change.handler")
   })

  app$ui = fluidPage(
    h5("Files"),
    uiOutput("treeUI")
  )
  setUI("treeUI",tree)

  viewApp(app)
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
