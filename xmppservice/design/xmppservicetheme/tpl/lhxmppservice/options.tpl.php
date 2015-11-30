<h1 class="attr-header"><?php include(erLhcoreClassDesign::designtpl('lhxmppservice/parts/text/options_xmpp_options.tpl.php')); ?></h1>

<form action="" method="post">

    <?php include(erLhcoreClassDesign::designtpl('lhkernel/csfr_token.tpl.php'));?>
    
    <?php if (isset($updated) && $updated == 'done') : $msg = erTranslationClassLhTranslation::getInstance()->getTranslation('chat/onlineusers','Settings updated'); ?>
    	<?php include(erLhcoreClassDesign::designtpl('lhkernel/alert_success.tpl.php'));?>
    <?php endif; ?>

    <div class="form-group">
        <label><input type="checkbox" value="on" name="XMPPEnabled" <?php isset($xmpp_options['xmpp_enabled']) && ($xmpp_options['xmpp_enabled'] == true) ? print 'checked="checked"' : ''?> /> <?php include(erLhcoreClassDesign::designtpl('lhxmppservice/parts/text/options_xmpp_enabled.tpl.php')); ?></label><br/>
        <label><input type="checkbox" value="on" name="TrackOnline" <?php isset($xmpp_options['track_online']) && ($xmpp_options['track_online'] == true) ? print 'checked="checked"' : ''?> /> <?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/options','Track online visitors');?></label>
    </div>
    
    <input type="submit" class="btn btn-default" name="StoreOptions" value="<?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('system/buttons','Save'); ?>" />

</form>
