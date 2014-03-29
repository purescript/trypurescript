$('#examples').change(function() {
  var name = $('#examples').val();
  if (name) {
    window.location = '/example/' + name;
  }
});
