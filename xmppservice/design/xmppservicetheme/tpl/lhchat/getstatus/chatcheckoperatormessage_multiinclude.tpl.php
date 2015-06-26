<?php 
if (isset($xmppAccount)) : 
$xmppServiceExtension = erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice');
if ($xmppServiceExtension->settings['enabled'] == true && $xmppServiceExtension->settings['online_visitors_tracking'] == true ) : 
$paramsOnline = erLhcoreClassExtensionXmppserviceHandler::getNickAndStatusByOnlineVisitor($visitor); ?>
var xmppservice = {debug:<?php echo $xmppServiceExtension->settings['debug'] == true ? 'true' : 'false' ?>,use_notification:<?php echo $xmppServiceExtension->settings['use_notification'] == true ? 'true' : 'false' ?>,nick:<?php echo json_encode($paramsOnline['nick'])?>,status:<?php echo json_encode($paramsOnline['status'])?>,BOSH_SERVICE : '<?php echo $xmppServiceExtension->settings['bosh_service']?>', USR : <?php echo json_encode($xmppAccount->username)?>,PSW : <?php echo json_encode($xmppAccount->password)?>};
var th = document.getElementsByTagName('head')[0];
var s = document.createElement('script');
s.setAttribute('type','text/javascript');
s.setAttribute('src','<?php echo erLhcoreClassModelChatConfig::fetch('explicit_http_mode')->current_value?>//<?php echo $_SERVER['HTTP_HOST']?><?php echo erLhcoreClassDesign::designJS('js/strope.xmppservice.js;js/xmppservice_online.js');?>');
th.appendChild(s);	

<?php endif;endif;?>