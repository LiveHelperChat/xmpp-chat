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
    errorHandler = require('errorhandler'),
	exec = require('child_process').exec;

var hostname = config.listen_address;
var port = config.listen_port;

app.get("/", function (req, res) {
	res.send('ok');
});

// Holds all connected clients
var clients = [];



// JSON Parser
var jsonParser = bodyParser.json()

var escapeShell = function(cmd) {
  return '"'+cmd.replace(/(["\s'$`\\])/g,'\\$1')+'"';
};

/**
 * Processes request for setting online visitor as online in xmpp server
 * */
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

/**
 * Register account in XMPP server
 * */
app.post('/xmpp-register', jsonParser, function (req, res) {
	if (!req.body) return res.sendStatus(400)	  
	
	if (typeof req.body.user !== 'undefined' && typeof req.body.host !== 'undefined' && typeof req.body.password !== 'undefined')
	{
		child = exec(config.ejabberdctl + " register " + escapeShell(req.body.user) + " " + escapeShell(req.body.host) + " "+escapeShell(req.body.password), function (error, stdout, stderr) {
			if (config.debug.output == true) {
				console.log('stdout: ' + stdout);
				console.log('stderr: ' + stderr);
			}
			
			if (error !== null) {
				var response = {'error':true,'msg':stderr+stdout};
			} else {
				var response = {'error':false,'msg':stdout};
			}
			
			res.send(JSON.stringify(response));
		});  
	} else {
		var response = {'error':false,'msg':'Invalid arguments'};
		res.send(JSON.stringify(response));
	}
})

/**
 * Deletes user from virtual host
 * */
app.post('/xmpp-unregister', jsonParser, function (req, res) {
	if (!req.body) return res.sendStatus(400)	  
	
	if (typeof req.body.user !== 'undefined' && typeof req.body.host !== 'undefined')
	{
		child = exec(config.ejabberdctl + " unregister " + escapeShell(req.body.user) + " " + escapeShell(req.body.host), function (error, stdout, stderr) {
			if (config.debug.output == true) {
				console.log('stdout: ' + stdout);
				console.log('stderr: ' + stderr);
			}
			
			if (error !== null) {
				var response = {'error':true,'msg':stderr+stdout};
			} else {
				var response = {'error':false,'msg':stdout};
			}
			
			res.send(JSON.stringify(response));
		});  
	} else {
		var response = {'error':false,'msg':'Invalid arguments'};
		res.send(JSON.stringify(response));
	}
})

/**
 * Assigsn account to provided roaster
 * */
app.post('/xmpp-assign-user-to-roaster', jsonParser, function (req, res) {
	if (!req.body) return res.sendStatus(400)	  
	
	if (typeof req.body.user !== 'undefined' && typeof req.body.host !== 'undefined' && typeof req.body.group !== 'undefined' && typeof req.body.grouphost !== 'undefined')
	{	
		child = exec(config.ejabberdctl + " srg_user_add " + escapeShell(req.body.user) + " " + escapeShell(req.body.host) + " " + escapeShell(req.body.group) + " " + escapeShell(req.body.grouphost), function (error, stdout, stderr) {
			if (config.debug.output == true) {
				console.log('stdout: ' + stdout);
				console.log('stderr: ' + stderr);
			}
			
			if (error !== null) {
				var response = {'error':true,'msg':stderr+stdout};
			} else {
				var response = {'error':false,'msg':stdout};
			}
			
			res.send(JSON.stringify(response));
		});  
	} else {
		var response = {'error':false,'msg':'Invalid arguments'};
		res.send(JSON.stringify(response));
	}
})

/**
 * Deletes provided user from roaster
 * */
app.post('/xmpp-delete-user-from-roaster', jsonParser, function (req, res) {
	if (!req.body) return res.sendStatus(400)	  
	
	if (typeof req.body.user !== 'undefined' && typeof req.body.host !== 'undefined' && typeof req.body.group !== 'undefined' && typeof req.body.grouphost !== 'undefined')
	{
		child = exec(config.ejabberdctl + " srg_user_del " + escapeShell(req.body.user) + " " + escapeShell(req.body.host) + " " + escapeShell(req.body.group) + " " + escapeShell(req.body.grouphost), function (error, stdout, stderr) {
			if (config.debug.output == true) {
				console.log('stdout: ' + stdout);
				console.log('stderr: ' + stderr);
			}
			
			if (error !== null) {
			    var response = {'error':true,'msg':stderr+stdout};
			} else {
				var response = {'error':false,'msg':stdout};
			}
			
			res.send(JSON.stringify(response));
		});  
	} else {
		var response = {'error':false,'msg':'Invalid arguments'};
		res.send(JSON.stringify(response));
	}
})

app.use(errorHandler({
  dumpExceptions: true,
  showStack: true
}));

console.log("LHC xmpp client server listening listening at http://%s:%s", hostname, port);
app.listen(port, hostname);