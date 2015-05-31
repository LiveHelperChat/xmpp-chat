<?php include(erLhcoreClassDesign::designtpl('lhxmppservice/xmppservice_tab_enabled_pre.tpl.php')); ?>
<?php if (erLhcoreClassUser::instance()->hasAccessTo('lhxmppservice','use_admin') && $xmppservice_tab_enabled_pre == true) : ?>
<li><a href="<?php echo erLhcoreClassDesign::baseurl('xmppservice/index')?>">XMPP</a></li>
<?php endif;?>