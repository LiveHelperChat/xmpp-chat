<?php 



$rpc = new \GameNet\Jabber\RpcClient([
    'server' => 'http://95.85.55.134:4560',
    'host' => 'xmpp2.livehelperchat.com',
    'debug' => false,
]);

$rpc->sendMessageChat('visitor.11.test1-new@xmpp.livehelperchat.com','remdex.test1-new@xmpp.livehelperchat.com','Labadiena kaip sekasi');

exit;

?>