<?php 

return array(
    'ahosting' => false,                        // Is it automated hosting enviroment? At the moment keep it false untill automated hosting will be supported
    'subdomain' => '',                          // Under what subdomain it's running
    'secret_key' => '<change_me>',              // Secret key, node will accept commands only if this key is provided. It must match in nodejs settings defined secret key.
    'enabled' => true,                          // Is this enabled in general
    'online_visitors_tracking' => true,         // Should each online visitor get it's own xmpp account?
    'xmpp_host' => '<xmpp_host>',               // E.g xmpp.livehelperchat.com
    'node_api_server' => '<node_http_address>', // E.g http://127.0.0.1:4567',
    'host_login' => 'localhost',                // Host where node server should login as user
    'debug' => false,                            // Write exceptions in cache/default.log use it for debuging purposes
);

?>