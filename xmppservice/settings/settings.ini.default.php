<?php 

return array(
    'ahosting' => false,                        // Is it automated hosting enviroment? At the moment keep it false untill automated hosting will be supported
    'subdomain' => '',                          // Under what subdomain it's running
    
    'enabled' => true,                          // Is this enabled in general
    'online_visitors_tracking' => true,         // Should each online visitor get it's own xmpp account?
    'xmpp_host' => '<xmpp_host>',               // E.g xmpp.livehelperchat.com
    
    // Debug settings
    'debug' => false,                            // Write exceptions in cache/default.log use it for debuging purposes

    // Handler, either rpc either node
    'handler' => 'rpc',
    
    /**
     * NodeJS settings if handler is node
     * */
    'node_api_server' => '<node_http_address>', // E.g http://127.0.0.1:4567', not used if RPC is used
    'secret_key' => '<change_me>',              // Secret key, node will accept commands only if this key is provided. It must match in nodejs settings defined secret key.
    'host_login' => 'localhost',                // Host where node server should login as user
    
    /**
     * RPC settings
     * */
    // Host where ejabberd RPC server is running. This should be available only to LHC IP, and not available publicly. By default ejabberd listens on 4560 port
    'rpc_server' => 'http://<ip>:4560',      
    
    // Web socket address, it can be also nginx proxy
    'bosh_service' => 'ws://xmpp.livehelperchat.com:5280/websocket',
    
    // Then operator writes a message we can track that event, should on this event message be synced from back office
    // This gives some real time UX, use it only if you are not using nodejs extensions, otherwise it's no point to have it enabled
    'use_notification' => false,
    
    // Not used at the moment, but may be used in the future
    'prebind_host' => 'http://95.85.55.134:5280/http-prebind/'
);

?>