$.ajaxSetup({
  dataType: "text",
});

export function teardownIFrame() {
  var $ctr = $("iframe#output-iframe");
  $ctr.remove()
}

export function setupIFrame(data, loadCb, failCb) {
  var $ctr = $("#column2");
  var $iframe = $(
    '<iframe sandbox="allow-scripts allow-forms" id="output-iframe" src="frame.html">'
  );
  $ctr.empty().append($iframe);
  var tries = 0;

  var sendSources = setInterval(function () {
    // Stop after 10 seconds
    if (tries >= 100) {
      return clearInterval(sendSources);
    }
    tries++;
    var iframe = $iframe.get(0).contentWindow;
    if (iframe) {
      iframe.postMessage(data, "*");
      loadCb();
    } else {
      failCb();
      console.warn("Frame is not available");
    }
  }, 100);

  window.addEventListener(
    "message",
    function () {
      clearInterval(sendSources);
    },
    { once: true }
  );

  window.addEventListener("message", function (event) {
    if (
      event.data &&
      event.data.githubId
    ) {
      window.location.search = "github=" + event.data.githubId;
    }
    if (
      event.data &&
      event.data.gistId &&
      /^[0-9a-f]+$/.test(event.data.gistId)
    ) {
      window.location.search = "gist=" + event.data.gistId;
    }
  });

  return $iframe;
}

export function copyToClipboard(string, copyCb, failCb) {
  navigator.clipboard.writeText(string).then(
    () => copyCb(),
    () => failCb()
  );
}
