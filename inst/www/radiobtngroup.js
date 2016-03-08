$(".asradio-btn-group .btn").click(function() {
  // change value of container div
  // when button is pressed
  // and trigger change event
  var groupid = this.parentNode.id;
  var oldval = $("#"+groupid).val();
  var newval = $(this).val();

  if (oldval !== newval) {
    // update value of container div
    $("#"+groupid)
      .val(newval)
      .trigger("change");
  }
  //alert(val);
});
