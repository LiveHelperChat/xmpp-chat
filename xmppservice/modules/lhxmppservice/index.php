<?php

if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['enabled'] == false) {
    erLhcoreClassModule::redirect('/');
    exit ;
}

$tpl = erLhcoreClassTemplate::getInstance( 'lhxmppservice/index.tpl.php');
$Result['content'] = $tpl->fetch();
$Result['path'] = array(array('title' => erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/module','XMPP Service')));

erLhcoreClassChatEventDispatcher::getInstance()->dispatch('xmppservice.index_path',array('result' => & $Result));

?>