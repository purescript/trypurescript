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

$('#code').height('250px');
