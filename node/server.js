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
		  try {
			  clients[uniqid] = new xmppClient({
			  'client_id':uniqid,
			  'jid':req.body.jid,
			  'pass':req.body.pass,
			  'nick':(typeof req.body.nick !== 'undefined' ? req.body.nick : 'Online visitor'),
			  'status':(typeof req.body.status !== 'undefined' ? req.body.status : '-'),
			  'host':req.body.host,
			  'cb' : function(params){
				  delete clients[params.client_id];
			  }});
			  
		  } catch (err) {
			  if (config.debug.output == true) {
				  console.log("Error during setting a state "+err.message);
			  }							
		  }
		  
	  } else {
		  if (config.debug.output == true) {
			  console.log("Session extend");
		  }
		  
		  clients[uniqid].extendSession({'nick':(typeof req.body.nick !== 'undefined' ? req.body.nick : 'Online visitor'),'status':(typeof req.body.status !== 'undefined' ? req.body.status : '-')});
	  }
})


app.post('/xmpp-send-message', jsonParser, function (req, res) {
	  if (!req.body) return res.sendStatus(400)
	  
	  res.send('ok');
	  
	  // Initiate xmpp client
	  var uniqid = md5(req.body.jid+req.body.pass+req.body.host);
	  
	  if (typeof clients[uniqid] === 'undefined') {		  
		  if (config.debug.output == true) {
			  console.log("Sending message to operator");
		  }		  
		  try {
			 
			  clients[uniqid] = new xmppClient({
			  'client_id':uniqid,
			  'jid':req.body.jid,
			  'pass':req.body.pass,
			  'nick':(typeof req.body.nick !== 'undefined' ? req.body.nick : 'Online visitor'),
			  'status':(typeof req.body.status !== 'undefined' ? req.body.status : ''),
			  'host':req.body.host,			 
			  'cb' : function(params){
				  delete clients[params.client_id];
			  }});
			  
			  // Send a message
			  clients[uniqid].sendMessage(req.body.operator_username,req.body.message);
			  
		  } catch (err) {
			  if (config.debug.output == true) {
				  console.log("Error during setting a state "+err.message);
			  }							
		  }
		  
	  } else {
		  if (config.debug.output == true) {
			  console.log("Session extended now seding a message");
		  }
		  
		  var paramsExtend = {'nick':(typeof req.body.nick !== 'undefined' ? req.body.nick : 'Online visitor')};
		  
		  if (typeof req.body.status !== 'undefined') {
			  paramsExtend.status = req.body.status;
		  }
		  
		  clients[uniqid].extendSession(paramsExtend);		  		
		  clients[uniqid].sendMessage(req.body.operator_username,req.body.message);
	  }
})



/**
 * Register online visitor and addign him to visitors roaster and make online in single request
 * */
app.post('/xmpp-register-online-visitor', jsonParser, function (req, res) {
	if (!req.body) return res.sendStatus(400)	  
	
	if (typeof req.body.user !== 'undefined' && typeof req.body.host !== 'undefined' && typeof req.body.password !== 'undefined')
	{
		child = exec(config.ejabberdctl + " register " + escapeShell(req.body.user) + " " + escapeShell(req.body.host) + " "+escapeShell(req.body.password), function (error, stdout, stderr) {
			if (config.debug.output == true) {
				console.log('stdout: ' + stdout);
				console.log('stderr: ' + stderr);
			}			
			if (/*1==-1 && */error !== null) { // To test and avoid creating new users
				var response = {'error':true,'msg':stderr+stdout};
				res.send(JSON.stringify(response));	
				
			} else {			
				// Assign user to visitors roaster
				child = exec(config.ejabberdctl + " srg_user_add " + escapeShell(req.body.user) + " " + escapeShell(req.body.host) + " " + escapeShell('visitors') + " " + escapeShell(req.body.host), function (error, stdout, stderr) {
					if (config.debug.output == true) {
						console.log('stdout: ' + stdout);
						console.log('stderr: ' + stderr);
					}
					
					if (error !== null) {
						var response = {'error':true,'msg':stderr+stdout};
					} else {						
						var uniqid = md5(req.body.user+'@'+req.body.host+req.body.password+req.body.hostlogin);
												
						try {
							clients[uniqid] = new xmppClient({
							  'client_id':uniqid,
							  'jid':req.body.user+'@'+req.body.host,
							  'pass':req.body.password,
							  'host':req.body.hostlogin,
							  'cb' : function(params){
								  delete clients[params.client_id];
							}});

							var response = {'error':false,'msg':stdout};							
						} catch (err) {
							var response = {'error':true,'msg':err.message};							
						}						
					}
					
					res.send(JSON.stringify(response));					
				});
			}
			
			
		});  
	} else {
		var response = {'error':false,'msg':'Invalid arguments'};
		res.send(JSON.stringify(response));
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

app.post('/xmpp-testing-json', jsonParser, function (req, res) {
	if (!req.body) return res.sendStatus(400)	
	res.send(JSON.stringify({'status':true}));
	console.log(req.body);
})

app.post('/xmpp-testing-body', function (req, res) {	
	res.send(JSON.stringify({'status':true}));
	console.log(req);
})




app.use(errorHandler({
  dumpExceptions: true,
  showStack: true
}));

console.log("LHC xmpp client server listening listening at http://%s:%s", hostname, port);
app.listen(port, hostname);