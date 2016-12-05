var console = {
    log: function(s) {
        var text = document.createTextNode(s);

        var div = document.createElement("div");
        div.appendChild(text);

        var cons = document.getElementById("console");
        cons && cons.appendChild(div);
    }
};

window.onerror = function(e) {
    console.log(e);
    return true;
};
