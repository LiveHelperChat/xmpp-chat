$(document).ready(function () {
	connection = new Strophe.Connection(xmppservice.BOSH_SERVICE);
	// We need to give some time to disconnect and connect again
	// Perhaps there is better way, anyone?
	setTimeout(function() {
		connection.connect(xmppservice.USR,xmppservice.PSW,function(status){
			if (status == Strophe.Status.CONNECTING) {
				if (xmppservice.debug == true) {
					console.log('Strophe is connecting.');
				}
		    } else if (status == Strophe.Status.CONNFAIL) {
				if (xmppservice.debug == true) {
					console.log('Strophe failed to connect.');
				}
		    } else if (status == Strophe.Status.DISCONNECTING) {
				if (xmppservice.debug == true) {
					console.log('Strophe is disconnecting.');
				}
		    } else if (status == Strophe.Status.DISCONNECTED) {
				if (xmppservice.debug == true) {
					console.log('Strophe is disconnected.');
				}
		    } else if (status == Strophe.Status.CONNECTED) {
		    	
		    	if (xmppservice.debug == true) {
		    		console.log('Strophe is connected.');
		    	}
		    	
		    	if (xmppservice.use_notification == true)
		    	{
			    	connection.addHandler(function() {
			    		 lhinst.syncusercall();
			    	}, null, 'message', null, null,  null); 
		    	}
		    	
		    	var presence = $pres().c("status").t(xmppservice.status).up();
		    	connection.send(presence); 
		    	
		    	setTimeout(function(){
		    		var presence = $pres().c("status").t(xmppservice.status).up().c('nick',{xmlns : 'http://jabber.org/protocol/nick'}).t(xmppservice.nick);
		    		connection.send(presence); 
		    	},2000);
		    }
		});
	},2000);
});