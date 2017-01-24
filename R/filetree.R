
examples.filetree = function() {
  restore.point.options(display.restore.point = TRUE)
  library(shinyEventsUI)
  app = eventsApp()

  root.dir = "D:/libraries/shinyEventsUI/test"
  setwd(root.dir)
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
    smallButton("mkdirBtn","New Folder",size = "extra-small"),
    smallButton("renameBtn","Rename",size = "extra-small"),
    smallButton("duplicateBtn","Duplicate",size = "extra-small"),
    smallButton("deleteBtn","Delete",size = "extra-small"),
    uiOutput("treeUI"),
    fileInput("uploadBtn","Upload", multiple = TRUE)
  )
  setUI("treeUI",tree)

  filetreeRenameButtonHandler("renameBtn",id)
  filetreeMakeDirButtonHandler("mkdirBtn",id)
  filetreeDuplicateButtonHandler("duplicateBtn",id)
  filetreeDeleteButtonHandler("deleteBtn",id)
  filetreeUploadHandler("uploadBtn",id)

  filetreeButtonHandler("fileInfoBtn",id,function(...) {
    args = list(...)
    restore.point("fileInfoBtn")
    cat("\nfileInfoBtn")
    df = args$file.df
    df
  })

  viewApp(app)


}

fancy.file.tree = function(id="fileTree", root.dir=getwd(), cur.dir=root.dir, add.checkbox = TRUE, nodeCol=1, file.click.handler=NULL, folder.change.handler=NULL, auto.folder.change=TRUE, ...) {

  restore.point("fancy.file.tree")

  file.nodes = fancy.file.tree.nodes(root.dir, cur.dir)

  js.render = paste0('

    if (node.data.itemType === "folder" | node.data.itemType === "file") {
      cols.eq(0).html("<input type=\'checkbox\' class=\'',id,'_filetreeCheckbox\' name=\'',id,'_filetreeCheckbox\' value=\'" + node.data.itemId + "\'>");
    }

    cols.eq(2).html(node.data.size);
    cols.eq(3).html(node.data.mtime);
  ')

  tree = fancytree.table(id=id,col.width=c("*"), num.cols=4,keyboard=FALSE,tabable=FALSE,js.render=js.render,source=file.nodes, nodeCol=nodeCol)

  clickHandler("fileTree",fun = function(...) {
    args = list(...)
    data = args$data
    restore.point("filetree.click")
    if (data$itemType=="folder") {
      new.dir = file.path(data$curdir,data$itemId)
      # update tree to new folder
      if (auto.folder.change) {
        source = fancy.file.tree.nodes(data$rootdir, new.dir)
        set.filetree.state(id, cur.dir=new.dir, selected=NULL)
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
        set.filetree.state(id, cur.dir=new.dir, selected=NULL)
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

  class = paste0(id,"_filetreeCheckbox")
  extra.shiny.value.code=paste0('selected: getCheckBoxValues(".',class,'")')

  classEventHandler(paste0(id,"_filetreeCheckbox"),event = "click", extra.shiny.value.code=extra.shiny.value.code, stop.propagation = TRUE, fun = function(...) {
    args = list(...)
    restore.point("filetreeCheckboxHandler")
    cat("\nfiletree.checkbox clicked")
    set.filetree.state(id, selected=args$selected)

  })


  set.filetree.state(id, cur.dir=cur.dir, root.dir=root.dir, selected=NULL)


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

set.filetree.state = function(treeId,...,app=getApp()) {
  if (is.null(app$.fancy.file.tree.state)) {
    app$.fancy.file.tree.state = list()
  }
  if (is.null(app$.fancy.file.tree.state[[treeId]])) {
    app$.fancy.file.tree.state[[treeId]] = list()
  }

  args = list(...)

  app$.fancy.file.tree.state[[treeId]][names(args)] = args
}

get.filetree.dir = function(treeId, app=getApp()) {
  app$.fancy.file.tree.state[[treeId]][["cur.dir"]]
}

get.filetree.state = function(treeId, app=getApp()) {
  app$.fancy.file.tree.state[[treeId]]
}


modal.msg = function(msg,title="",...) {
  showModal(modalDialog(HTML(msg),title = title))
}


filetreeUploadHandler = function(id, treeId, msg.fun=modal.msg, listener=NULL) {
  restore.point("filetreeUploadHandler")

  ns = NS(treeId)

  changeHandler(id, function(...) {
    args = list(...)
    cur.dir = get.filetree.dir(treeId)
    root.dir = get.filetree.state(treeId)$root.dir
    restore.point("filetreeUploadHandler.inner")
    df = args$value
    res = file.copy(from=df$datapath, to=file.path(cur.dir, df$name),recursive = TRUE,overwrite = TRUE)
    refresh.filetree(treeId, root.dir, cur.dir)


  })
}


filetreeRenameButtonHandler = function(id, treeId, msg.fun=modal.msg, listener=NULL) {
  restore.point("filetreeRenameButtonHandler")

  ns = NS(treeId)

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


    buttonHandler(ns("RenameCancelBtn"), function(...) {
      removeModal()
    })

    buttonHandler(ns("RenameOkBtn"), function(...) {
      args = list(...)
      new = getInputValue(ns("RenameTextInput"))
      restore.point(ns("RenameOkBtn"))
      if (nchar(new)>0) {
        res = try(file.rename(from=file.path(cur.dir, sel.file),to = file.path(cur.dir, new)))
      }
      refresh.filetree(treeId, root.dir, cur.dir)
      removeModal()
    })



    showModal(modalDialog(size = "s", title = paste0("Rename ",sel.type," ",sel.file),footer = NULL,
      textInput(ns("RenameTextInput"),paste0("Please enter the new ", sel.type," name"),""),
      actionButton(ns("RenameOkBtn"),"Ok"),
      actionButton(ns("RenameCancelBtn"),"Cancel")
    ))
  })
}




filetreeDeleteButtonHandler = function(id, treeId, msg.fun=modal.msg, listener=NULL) {
  restore.point("filetreeDeleteButtonHandler")

  ns = NS(treeId)

  filetreeButtonHandler(id,treeId,function(...) {
    args = list(...)
    restore.point("filetreeDeleteButtonHandler.inner")

    file.df = args$file.df
    n = sum(file.df$selected)
    if (n == 0) {
      msg.fun("You have not selected any file.")
      return()
    }

    sel.files = file.df$name[file.df$selected]
    sel.types = file.df$type[file.df$selected]

    root.dir = args$root.dir
    cur.dir = args$cur.dir


    buttonHandler(ns("DeleteCancelBtn"), function(...) {
      removeModal()
    })

    buttonHandler(ns("DeleteOkBtn"), function(...) {
      args = list(...)
      restore.point(ns("DeleteOkBtn"))
      #res = try(file.remove(file.path(cur.dir, sel.files)))
      res = try(unlink(file.path(cur.dir, sel.files), recursive = TRUE))
      if (is(res,"try-error")) {
          msg.fun(as.character(res),title="Deletion failed")
          return()
      }

      refresh.filetree(treeId, root.dir, cur.dir)
      removeModal()
    })



    showModal(modalDialog(size = "s", title = paste0("Delete ",n," files or folders"),footer = NULL,
      if (n==1) {
        p(paste0("Are you sure you want to delete the ", sel.types," '", sel.files,"'?"))
      } else {
        p(paste0("You have selected ", sum(sel.types=="folder"), " folders and ", sum(sel.types=="file")," files. Are you sure you want to delete them?"))
      },
      actionButton(ns("DeleteOkBtn"),"Yes delete"),
      actionButton(ns("DeleteCancelBtn"),"Cancel")
    ))
  })
}



filetreeDuplicateButtonHandler = function(id, treeId, msg.fun=modal.msg, listener=NULL) {
  restore.point("filetreeDuplicateButtonHandler")

  ns = NS(treeId)

  filetreeButtonHandler(id,treeId,function(...) {
    args = list(...)
    restore.point("filetreeDuplicateButtonHandler.inner")

    file.df = args$file.df
    n = sum(file.df$selected)
    if (n != 1) {
      msg.fun("You have to check exactly 1 file or folder to duplicate.")
      return()
    }

    sel.file = file.df$name[file.df$selected]
    sel.type = file.df$type[file.df$selected]
    root.dir = args$root.dir
    cur.dir = args$cur.dir


    buttonHandler(ns("DuplicateCancelBtn"), function(...) {
      removeModal()
    })

    buttonHandler(ns("DuplicateOkBtn"), function(...) {
      args = list(...)
      new = getInputValue(ns("DuplicateTextInput"))
      restore.point(ns("DuplicateOkBtn"))
      if (nchar(new)>0) {
        if (file.exists(file.path(cur.dir, new))) {
          msg.fun(paste0("A file with name '", new, "' already exists."),title="Duplication failed.")
          return()
        }
        if (sel.type == "folder") {


          res = try(dir.create(file.path(cur.dir, new)))
          if (is(res,"try-error")) {
            msg.fun(as.character(res),title="Duplication of directory failed")
            return()
          }
          files = list.files(file.path(cur.dir, sel.file),all.files = TRUE,recursive = FALSE,ignore.case = TRUE,full.names = TRUE,include.dirs = TRUE)
          res = try(file.copy(from=files,to = file.path(cur.dir, new),recursive = TRUE, overwrite = FALSE))

        } else {
          res = try(file.copy(from=file.path(cur.dir, sel.file),to = file.path(cur.dir, new),recursive = TRUE, overwrite = FALSE))
        }
        if (is(res,"try-error")) {
          msg.fun(as.character(res),title="Duplication failed")
          return()
        }

      }
      refresh.filetree(treeId, root.dir, cur.dir)
      removeModal()
    })



    showModal(modalDialog(size = "s", title = paste0("Duplicate ",sel.type," ",sel.file),footer = NULL,
      textInput(ns("DuplicateTextInput"),paste0("Please enter the ", sel.type," name of the duplicate"),paste0("Copy of ", sel.file)),
      actionButton(ns("DuplicateOkBtn"),"Ok"),
      actionButton(ns("DuplicateCancelBtn"),"Cancel")
    ))
  })
}


filetreeMakeDirButtonHandler = function(id, treeId, listener=NULL) {
  restore.point("filetreeMakeDirButtonHandler")
  ns = NS(treeId)
  filetreeButtonHandler(id,treeId,function(...) {
    args = list(...)
    restore.point("filetreeMakeDirButtonHandler.inner")
    cat("\nfileInfoBtn")
    root.dir = args$root.dir
    cur.dir = args$cur.dir


    buttonHandler(ns("mkdirCancelBtn"), function(...) {
      args = list(...)
      restore.point(ns("mkdirCancelBtnClicked"))
      removeModal()
      cat("\nCancel pressed.")
    })

    buttonHandler(ns("mkdirOkBtn"), function(...) {
      args = list(...)
      newdir = getInputValue(ns("mkdirTextInput"))
      restore.point("mkdirOkBtnClicked")
      if (nchar(newdir)>0) {
        res = try(dir.create(file.path(cur.dir,basename(newdir))))
      }
      refresh.filetree(treeId, root.dir, cur.dir)
      removeModal()


    })



    showModal(modalDialog(title = "New Folder",footer = NULL,
      textInput(ns("mkdirTextInput"),"Please enter the new folder name",""),
      actionButton(ns("mkdirOkBtn"),"Ok"),
      actionButton(ns("mkdirCancelBtn"),"Cancel")
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

    df = as_data_frame(do.call(rbind,args$treeInfo))

    df = as_data_frame(lapply(df, unlist)) %>%
      select(-nodeType, -curdir,-rootdir) %>%
      rename(name=itemId,type=itemType) %>%
      filter(!type %in% c("header","upfolder"))

    args$file.df = df
    args$root.dir = h$rootdir
    args$cur.dir = h$curdir

    do.call(fun,args)
  } )

}
