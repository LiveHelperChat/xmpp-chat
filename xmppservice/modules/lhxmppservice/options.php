<?php

if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['enabled'] == false) {
    erLhcoreClassModule::redirect('/');
    exit ;
}


$tpl = erLhcoreClassTemplate::getInstance('lhxmppservice/options.tpl.php');

$xmppOptions = erLhcoreClassModelChatConfig::fetch('xmppservice_options');
$data = (array)$xmppOptions->data;

if ( isset($_POST['StoreOptions']) ) {

    $definition = array(
        'TrackOnline' => new ezcInputFormDefinitionElement(
            ezcInputFormDefinitionElement::OPTIONAL, 'boolean'
        ),
        'XMPPEnabled' => new ezcInputFormDefinitionElement(
            ezcInputFormDefinitionElement::OPTIONAL, 'boolean'
        )
    );
      
    $form = new ezcInputForm( INPUT_POST, $definition );
    $Errors = array();
        
    if ( $form->hasValidData( 'TrackOnline' ) && $form->TrackOnline == true ) {
        $data['track_online'] = 1;
    } else {
        $data['track_online'] = 0;
    }
        
    if ( $form->hasValidData( 'XMPPEnabled' ) && $form->XMPPEnabled == true ) {
        $data['xmpp_enabled'] = 1;
    } else {
        $data['xmpp_enabled'] = 0;
    }
    
    $xmppOptions->explain = '';
    $xmppOptions->type = 0;
    $xmppOptions->hidden = 1;
    $xmppOptions->identifier = 'xmppservice_options';
    $xmppOptions->value = serialize($data);
    $xmppOptions->saveThis();
    
    $tpl->set('updated','done');
}

$tpl->set('xmpp_options',$data);

$Result['content'] = $tpl->fetch();

$Result['path'] = array(
    array(
        'url' => erLhcoreClassDesign::baseurl('xmppservice/index'),
        'title' => erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/module', 'XMPP Service')
    ),
    array(
        'url' => erLhcoreClassDesign::baseurl('xmppservice/operators'),
        'title' => erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/module', 'XMPP Options')
    )
);

erLhcoreClassChatEventDispatcher::getInstance()->dispatch('xmppservice.options_path',array('result' => & $Result));

?>