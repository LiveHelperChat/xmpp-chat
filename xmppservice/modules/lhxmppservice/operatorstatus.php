<?php 

if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['enabled'] == false) {
    exit;
}

$content = file_get_contents("php://input");
erLhcoreClassExtensionXmppserviceHandler::handleOperatorPing($content);
exit;

?>