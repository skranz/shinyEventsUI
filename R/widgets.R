smallButton = function(id, label,class.add="",style="",...) {
  HTML(paste0('<button id="',id,'" style="',style,'" type="button" class="btn btn-default action-button btn-xs',class.add,'">',label,'</button>'))
}
