<?php 

$content = file_get_contents("php://input");
erLhcoreClassExtensionXmppserviceHandler::handleOperatorPing($content);
exit;

?>