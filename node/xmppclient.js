var Client = require('node-xmpp-client'),
config = require('./settings'),
ltx  = require('node-xmpp-core').ltx;

var XMPPClient = function(params) {

	var _that = this;  
	this.isLogged = false;
	this.inactivtyTimeout = null;
	this.disconnectTimeout = null;
	this.removeCallback = params['cb'];
	this.paramsClient = {'jid':params['jid'],'host':params['host'],'client_id':params['client_id']};
	this.nick = (typeof params['nick'] !== 'undefined' ? params['nick'] : 'Online visitor');
	this.status = (typeof params['status'] !== 'undefined' ? params['status'] : '-');
	
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

	// @todo add some handler error, perhaps post some data to api
	this.client.on('error', function(err) {
		if (config.debug.output == true) {
			console.log('Error here '+err);
		}			   
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
			if (config.debug.output == true) {
				console.log('Inactivity after offline triggered');
			}
			_that.disconnecTimeoutHandler();
		},config.online_timeout_destroy);	  
	})

	this.inactivtyTimeout = setTimeout(function(){
		if (config.debug.output == true) {
			console.log('Inactivity timeout triggered main');
		}
		_that.logout();
	},config.online_timeout);
}

XMPPClient.prototype.disconnecTimeoutHandler = function(){
	if (config.debug.output == true) {
		console.log("Execute request to remove user from ejabberd goes to PHP plugin");
	}
	
	this.removeCallback(this.paramsClient);
	delete this.client;
	
	clearTimeout(this.disconnectTimeout);
	clearTimeout(this.inactivtyTimeout);	
};

XMPPClient.prototype.logout = function(){
	this.client.end();
	this.isLogged = false;
};

XMPPClient.prototype.sendMessage = function(to, message) {	
	
	console.log("Calling send message");
	
	var stanza = new ltx.Element(
	        'message',
	        { to: to, type: 'chat' }
	    ).c('body').t(message);
	    
	if (this.isLogged == true) {	
	    this.client.send(stanza);
	} else { // We are not logged, give 3 seconds to login and try again to send
		var _this = this;
		setTimeout(function() {
			_this.client.send(stanza);
	  },3000);
	}
};

/**
 * Sends presence of online visitor
 * */
XMPPClient.prototype.onlineHandler = function() {		
	var presence = new ltx.Element('presence');
	presence.c('status').t(this.status);
	presence.c('nick',{xmlns : 'http://jabber.org/protocol/nick'}).t(this.nick);	
	this.client.send(presence);		
	this.isLogged = true;
};

//properties and methods
XMPPClient.prototype.extendSession = function(params){

	var needSync = false;
	
	if (typeof params['nick'] !== 'undefined' && this.nick != params['nick']) {
			needSync = true;
			this.nick = params['nick'];
	}
	
	if (typeof params['status'] !== 'undefined' && this.status != params['status']) {
			needSync = true;
			this.status = params['status'];
	}
	
	var _that = this;

	if (this.isLogged == false){
		this.client.connect();
		needSync = false;
	}
	
	// Something changed we need to sync
	if (needSync == true) {
		this.onlineHandler();
	}
	
	clearTimeout(this.inactivtyTimeout);
	clearTimeout(this.disconnectTimeout);

	this.inactivtyTimeout = setTimeout(function(){
		if (config.debug.output == true) {
			console.log('Inactivity timeout triggered');
		}
		_that.logout();
	},config.online_timeout);
};

//node.js module export
module.exports = XMPPClient;