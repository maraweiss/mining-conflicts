var ids = [];
var urls = [];
var mapIconClassName = "map_icon";

console.log("ejatlasCrawler: started crawling");
var divs = document.getElementsByClassName(mapIconClassName);
if (divs.length == 0){
  console.log("ejatlasCrawler: Error: unable to find map element. The class names might have been changed by the site owner. You can replace the value of the variable mapIconClassName in crawler.js by a class name that identifies the map icon divs on the webpage. These show up as orange dots on the map.");
}
for (var i = 0; i < divs.length; i++) {
  var classList = divs[i].className.split(/\s+/);
  for (var j = 0; j < classList.length; j++) {
    if (classList[j].includes("id_")) {
      var id = classList[j].substr(3);
      ids.push(id);
    }
  }
}

getUrl = function(id) {
  $.ajax({
    type: "get",
    url: "/info/" + id,
    success: function(n) {
      url = n.match(new RegExp("<a href='" + "(.*)" + "'>"))[1];
      url = url.substr(0, url.indexOf("'>"));
      urls.push(url);
    }
  });
};

function download(content, filename, contentType)
{
  if(!contentType) contentType = 'application/octet-stream';
  var a = document.createElement('a');
  var blob = new Blob([content], {'type':contentType});
  a.href = window.URL.createObjectURL(blob);
  a.download = filename;
  a.click();
}

var i = 0;

function myLoop () {
  setTimeout(function () {
    getUrl(ids[i]);
    i++;
    if (i < ids.length) {
      myLoop();
    }
    else{
      download(urls, "listOfConflicts.data", "txt");
      console.log("ejatlasCrawler: finished crawling");
    }
  }, 1000);
}

myLoop();
