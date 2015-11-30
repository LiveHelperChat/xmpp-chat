<?php 

if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['enabled'] == false) {
    erLhcoreClassModule::redirect('/');
    exit ;
}

$tpl = erLhcoreClassTemplate::getInstance('lhxmppservice/newxmppaccount.tpl.php');

$xmppaccount = new erLhcoreClassModelXMPPAccount();

if (ezcInputForm::hasPostData()) {
        
    $Errors = erLhcoreClassXMPPServiceAccountValidator::validateXMPPAccount($xmppaccount);

    if (count($Errors) == 0) {
        try {
            erLhcoreClassXMPPServiceAccountValidator::publishXMPPAccount($xmppaccount);
                       
            erLhcoreClassModule::redirect('xmppservice/operators');
            exit ;
            
        } catch (Exception $e) {
            $tpl->set('errors',array($e->getMessage()));
        }

    } else {
        $tpl->set('errors',$Errors);
    }       
}

$tpl->setArray(array(
        'xmppaccount' => $xmppaccount,
));

$Result['content'] = $tpl->fetch();

$Result['path'] = array(
    array (
        'url' =>erLhcoreClassDesign::baseurl('xmppservice/operators'), 
        'title' => erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/module','XMPP Accounts')        
    ),
    array(
        'title' => erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/module', 'New XMPP account')
    )
);

erLhcoreClassChatEventDispatcher::getInstance()->dispatch('xmppservice.newxmppaccount_path',array('result' => & $Result));

?>