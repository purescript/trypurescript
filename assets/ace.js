var editor = ace.edit('code');
editor.setTheme('ace/theme/dawn');
editor.renderer.setShowGutter(false);
var session = editor.getSession();
session.setMode('ace/mode/haskell');
session.setValue($('#textarea').val());
session.setUseWrapMode(true);
session.on('change', function(){
  $('#textarea').val(editor.getSession().getValue());
});
if ($('#js')[0]) {
  var js = ace.edit('js');
  js.setTheme('ace/theme/dawn');
  js.renderer.setShowGutter(false);
  js.setReadOnly(true);
  var session = js.getSession();
  session.setUseWrapMode(true);
  if (compiledSuccessfully) {
    session.setMode('ace/mode/javascript');
  }
}
function setHeight() {
  var top = $('#code').offset().top;
  var tot = $(window).height();
  var height = Math.max(tot - top - 50, 200);
  $('#code').height(height + 'px');
  $('#js').height(height + 'px');
}
$(setHeight);
$(window).on('resize', setHeight);
