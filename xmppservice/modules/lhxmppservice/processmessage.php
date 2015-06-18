<?php 

/* This is what messages looks like
$params = array(		
		'body' => 'PArasiau vartotojui zinute',
		'sender' => 'remdex2',
		'receiver' => 'visitor.6034.chat',
		'server' => 'xmpp.livehelperchat.com'
); */

//erLhcoreClassLog::write(print_r($_POST,true));


if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['enabled'] == false) {
    erLhcoreClassModule::redirect('/');
    exit ;
}

$params = array (
		'body' => isset($_POST['body']) ? $_POST['body'] : '',
		'sender' => isset($_POST['sender']) ? $_POST['sender'] : '',
		'receiver' => isset($_POST['receiver']) ? $_POST['receiver'] : '',
		'server' => isset($_POST['server']) ? $_POST['server'] : ''
);

try {
    if (!empty($params['body']) && !empty($params['sender']) && !empty($params['receiver']) && !empty($params['server'])) {
    	erLhcoreClassExtensionXmppserviceHandler::handleMessageFromOperator($params);
    }
} catch (Exception $e) {   
    if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) { 
        erLhcoreClassLog::write(print_r($e,true));
    }
}
exit;
?>