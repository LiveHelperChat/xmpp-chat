var Client = require('node-xmpp-client'),
config = require('./settings'); 

var XMPPClient = function(params) {

	var _that = this;  
	this.isLogged = true;
	this.inactivtyTimeout = null;
	this.disconnectTimeout = null;
	this.removeCallback = params['cb'];
	this.paramsClient = {'jid':params['jid'],'host':params['host'],'client_id':params['client_id']};

	this.client = new Client({
		jid: params['jid'],
		password: params['pass'],
		host: params['host'],
		reconnect: true
	});

	this.client.on('online', function() {
		if (config.debug.output == true) {
			console.log('Client is online')
		}
		
		_that.onlineHandler();	   
	});

	this.client.on('offline', function () {
		if (config.debug.output == true) {
			console.log('Client is offline')
		}
		_that.isLogged = false;	    
	})

	this.client.on('disconnect', function (e) {
		if (config.debug.output == true) {
			console.log('Client is disconnected')
		}
		_that.isLogged = false;
		clearTimeout(_that.disconnectTimeout);
		
		_that.disconnectTimeout = setTimeout(function(){
			_that.disconnecTimeoutHandler();
		},config.online_timeout_destroy);	  
	})

	this.inactivtyTimeout = setTimeout(function(){
		_that.logout();
	},config.online_timeout);
}

XMPPClient.prototype.disconnecTimeoutHandler = function(){
	if (config.debug.output == true) {
		console.log("Execute request to remove user from ejabberd goes to PHP plugin");
	}
	
	this.removeCallback(this.paramsClient);
	delete this.client;
};

XMPPClient.prototype.logout = function(){
	this.client.end();
	this.isLogged = false;
};

//properties and methods
XMPPClient.prototype.onlineHandler = function() {
	this.client.send("<presence><status>Online visitor</status></presence>");	
	this.isLogged = true;
};

//properties and methods
XMPPClient.prototype.extendSession = function(){

	var _that = this;

	if (this.isLogged == false){
		this.client.connect();
	}

	clearTimeout(this.inactivtyTimeout);
	clearTimeout(this.disconnectTimeout);

	this.inactivtyTimeout = setTimeout(function(){
		_that.logout();
	},config.online_timeout);
};

//node.js module export
module.exports = XMPPClient;