getFileTreeEventJson = function(id) {
  // return file node information
  var tree = $("#"+id).fancytree("getTree");
  var root = tree.rootNode;
  var children = root.children;
  var checked = getCheckBoxValues("."+id+"_filetreeCheckbox");

  var dat = children.map(function(x) {
    var res = x.data;
    res.selected = $.inArray(x.data.itemId, checked) >= 0;
    //x.isSelected();
    return res;
  });


  return(dat);
}


function getCheckBoxValues(sel)
{
  var chkArray = [];

  $(sel+":checked").each(function() {
     chkArray.push($(this).val());
  });
  return(chkArray);
}
