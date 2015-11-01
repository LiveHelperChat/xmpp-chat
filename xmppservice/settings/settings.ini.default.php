<?php 

return array(
    'ahosting' => false,                        // Is it automated hosting enviroment? At the moment keep it false untill automated hosting will be supported
    'subdomain' => '',                          // Under what subdomain it's running
    
    'enabled' => true,                          // Is this enabled in general
    'online_visitors_tracking' => true,         // Should each online visitor get it's own xmpp account?
    'xmpp_host' => '<xmpp_host>',               // E.g xmpp.livehelperchat.com
    
    // How many seconds append to last activity. Usefull to force xmpp after last ping to be long
    // Or if you are experience ping tracking issues, this extendes operator timeout by this value
    'append_time' => 0,    
    
    // Should last activity be reset if we found that related web operator logged out
    // But there is xmpp timeout shorter then last activity timeout?
    'check_xmpp_activity_on_web_logout' => true,
    
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
    
    'rpc_username' => '<your_account_name>',    // E.g admin
    
    'rpc_password' => '<your_account_password>',// E.g password
    
    'rpc_account_host' => '<rpc_account_hostname>',// E.g xmpp.example.com        

    // Web socket address, it can be also nginx proxy
    // If you are using nginx proxy. Config line could look like 
    // 'ws://'.$_SERVER['HTTP_HOST'].'/websocket'
    // Nginx config example you can find in doc folder
    'bosh_service' => 'ws://xmpp.example.com:5280/websocket', // ws://xmpp.livehelperchat.com:5280/websocket

    // Then operator writes a message we can track that event, should on this event message be synced from back office
    // This gives some real time UX, use it only if you are not using nodejs extensions, otherwise it's no point to have it enabled
    'use_notification' => false,

    // Not used at the moment, but may be used in the future
    'prebind_host' => 'http://95.85.55.134:5280/http-prebind/',

    // Should we create XMPP users when lhc user is created
    'create_xmpp_username_by_lhc_username' => false,

    // On what attribute based XMPP user should be created
    // username or email supported
    'type_for_xmpp_username' => 'username',
    
    // Should new accounts automatically receive all new chat requests
    'xmpp_send_messages' => true
);

?>