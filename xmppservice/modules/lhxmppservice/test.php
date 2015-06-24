<?php 

http://95.85.55.134:4560

$rpc = new \GameNet\Jabber\RpcClient([
    'server' => 'http://95.85.55.134:4560',
    'host' => 'xmpp2.livehelperchat.com',
    'debug' => false,
]);


print_r($rpc->deleteSharedRosterGroup('testrpc2'));


echo "Asdasd";
exit;

?>