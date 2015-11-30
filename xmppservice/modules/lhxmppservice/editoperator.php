<?php 

if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['enabled'] == false) {
    erLhcoreClassModule::redirect('/');
    exit ;
}

$tpl = erLhcoreClassTemplate::getInstance('lhxmppservice/editoperator.tpl.php');

$xmppaccount =  erLhcoreClassModelXMPPAccount::fetch($Params['user_parameters']['id']);

if (ezcInputForm::hasPostData()) {
        
    if (isset($_POST['Cancel_action'])) {
        erLhcoreClassModule::redirect('xmppservice/operators');
        exit ;
    }
    
    $Errors = erLhcoreClassXMPPServiceAccountValidator::validateXMPPAccount($xmppaccount);

    if (count($Errors) == 0) {
        try {
            erLhcoreClassXMPPServiceAccountValidator::publishXMPPAccount($xmppaccount);
                       
            erLhcoreClassModule::redirect('xmppservice/operators');
            exit;
            
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
    array (       
        'title' => erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/editoperator', 'Edit operator')
    )
);

erLhcoreClassChatEventDispatcher::getInstance()->dispatch('xmppservice.editoperator_path',array('result' => & $Result));

?>