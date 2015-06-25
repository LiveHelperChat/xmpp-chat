<?php if (isset($Result['chat']) && is_numeric($Result['chat']->id)) : 
$xmppServiceExtension = erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice');
if ($xmppServiceExtension->settings['enabled'] == true && ($xmppAccount = $xmppServiceExtension->getXMPPAccountByChat($Result['chat'])) !== false) : 
$paramsOnline = erLhcoreClassExtensionXmppserviceHandler::getNickAndStatusByChat($Result['chat']); ?>
<script>var xmppservice = {debug:<?php echo $xmppServiceExtension->settings['debug'] == true ? 'true' : 'false' ?>,use_notification:<?php echo $xmppServiceExtension->settings['use_notification'] == true ? 'true' : 'false' ?>,nick:<?php echo json_encode($paramsOnline['nick'])?>,status:<?php echo json_encode($paramsOnline['status'])?>,BOSH_SERVICE : '<?php echo $xmppServiceExtension->settings['bosh_service']?>', USR : <?php echo json_encode($xmppAccount->username)?>,PSW : <?php echo json_encode($xmppAccount->password)?>}</script>
<script type="text/javascript" language="javascript" src="<?php echo erLhcoreClassDesign::designJS('js/strope.xmppservice.js;js/xmppservice.js');?>"></script>
<?php  endif; endif;?>