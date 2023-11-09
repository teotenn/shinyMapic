// Print log on the page
window.setInterval(function() {
  var elem = document.getElementById('txt_search_log');
  elem.scrollTop = elem.scrollHeight;
}, 5000);
