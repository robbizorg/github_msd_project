var gs = require('github-scraper');
var url = '/iteles' // a random username
gs(url, function(err, data) {
  console.log(data); // or what ever you want to do with the data
});