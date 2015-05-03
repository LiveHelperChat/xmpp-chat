/**
 * Main XMPP client server
 * 
 * */
var express = require("express"),
    app = express(),
    md5 = require('MD5'),
    xmppClient = require('./xmppclient'),
    bodyParser = require('body-parser'),
    config = require('./settings'),
    errorHandler = require('errorhandler') ;
 
var hostname = config.listen_address;
var port = config.listen_port;

app.get("/", function (req, res) {
	res.send('ok');
});

// Holds all connected clients
var clients = [];

// JSON Parser
var jsonParser = bodyParser.json()
app.post('/xmpp', jsonParser, function (req, res) {
	  if (!req.body) return res.sendStatus(400)
	  
	  res.send('ok');
	  
	  // Initiate xmpp client
	  var uniqid = md5(req.body.jid+req.body.pass+req.body.host);
	  
	  if (typeof clients[uniqid] === 'undefined') {		  
		  if (config.debug.output == true) {
			  console.log("New client setup");
		  }		  
		  clients[uniqid] = new xmppClient({
			  'client_id':uniqid,
			  'jid':req.body.jid,
			  'pass':req.body.pass,
			  'host':req.body.host,
			  'cb' : function(params){
				  delete clients[params.client_id];
			  }});
	  } else {
		  if (config.debug.output == true) {
			  console.log("Session extend");
		  }
		  clients[uniqid].extendSession();
	  }
})

app.use(errorHandler({
  dumpExceptions: true,
  showStack: true
}));

console.log("LHC xmpp client server listening listening at http://%s:%s", hostname, port);
app.listen(port, hostname);