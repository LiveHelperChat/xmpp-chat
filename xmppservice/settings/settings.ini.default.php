<?php 

return array(
    'ahosting' => false,                        // Is it automated hosting enviroment? At the moment keep it false untill automated hosting will be supported
    'online_visitors_tracking' => true,         // Should each online visitor get it's own xmpp account?
    'xmpp_host' => '<xmpp_host>',               // E.g xmpp.livehelperchat.com
    'node_api_server' => '<node_http_address>', // E.g http://127.0.0.1:4567',
    'host_login' => 'localhost',                // Host where node server should login as user
    'debug' => true,                            // Write exceptions in cache/default.log use it for debuging purposes
);

?>