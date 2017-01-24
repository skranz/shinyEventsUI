getFileTreeEventJson = function(id) {
  // return file node information
  var tree = $("#"+id).fancytree("getTree");
  var root = tree.rootNode;
  var children = root.children;
  var dat = children.map(function(x) {
    var res = x.data;
    res.selected = x.isSelected();
    return res;
  });


  return(dat);
}
