<?php 

/* This is what messages looks like
$params = array(		
		'body' => 'PArasiau vartotojui zinute',
		'sender' => 'remdex2',
		'receiver' => 'visitor.6034.chat',
		'server' => 'xmpp.livehelperchat.com'
		'type' => 'chat' | 'groupchat' | 'presence'
); */

erLhcoreClassLog::write(print_r($_POST,true));


if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['enabled'] == false) {
    erLhcoreClassModule::redirect('/');
    exit ;
}

$params = array (
		'body' => isset($_POST['body']) ? $_POST['body'] : '',
		'sender' => isset($_POST['sender']) ? $_POST['sender'] : '',
		'receiver' => isset($_POST['receiver']) ? $_POST['receiver'] : '',
		'server' => isset($_POST['server']) ? $_POST['server'] : '',
		'type' => isset($_POST['type']) ? $_POST['type'] : '',
		'status' => isset($_POST['status']) ? $_POST['status'] : ''
);

try {
    if (!empty($params['sender']) && !empty($params['receiver']) && !empty($params['server']) && !empty($params['type'])) {
    	erLhcoreClassExtensionXmppserviceHandler::handleMessageFromOperator($params);
    }
} catch (Exception $e) {   
    if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) { 
        erLhcoreClassLog::write(print_r($e,true));
    }
}
exit;
?>
