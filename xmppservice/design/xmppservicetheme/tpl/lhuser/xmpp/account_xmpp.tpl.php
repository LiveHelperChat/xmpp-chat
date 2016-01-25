<?php 

$xmppServiceUserAccount = erLhcoreClassModelXMPPAccount::findOne(array('filter' => array(
    'user_id' => $user->id,
    'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_OPERATOR
)));

?>
<div class="panel panel-default">
  <div class="panel-heading">XMPP information</div>
  <div class="panel-body">
    <?php if ($xmppServiceUserAccount !== false) : ?>
    <ul class="mb0 ml0">
        <li><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/editoperator','Username');?>: <?php echo $xmppServiceUserAccount->username?></li>
        <li><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/editoperator','Server');?>: <?php echo erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['xmpp_host']?></li>
        <li><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/editoperator','Port');?>: <?php echo erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['xmpp_port']?></li>
    </ul>
    <?php else : ?>
        <p><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/editoperator','No associated account was found. Please create one.');?></p>
    <?php endif;?>
  </div>
</div>