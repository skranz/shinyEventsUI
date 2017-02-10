smallButton = function(id, label,class.add="",class="btn btn-default action-button btn-xs",style="",...) {
  tags$button(id=id, style=style, type="button", class=paste(class,class.add),...,label)
}
