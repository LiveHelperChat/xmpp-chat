<div class="form-group">
    <label><?php include(erLhcoreClassDesign::designtpl('lhxmppservice/parts/text/form_xmpp_username.tpl.php')); ?></label>
        
    <div class="input-group">
        <input type="text" maxlength="250" autocomplete="new-password" class="form-control" name="username" <?php if ($xmppaccount->id > 0) : ?>disabled="disabled"<?php endif;?> value="<?php echo htmlspecialchars($xmppaccount->username_plain_edit);?>" />
        <div class="input-group-addon"><?php if (($ahSubdomain = erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['subdomain']) != '')  : ?>.<?php echo $ahSubdomain?><?php endif;?>@<?php echo erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['xmpp_host']?></div>
    </div>    
</div>

<div class="form-group">
    <label><?php include(erLhcoreClassDesign::designtpl('lhxmppservice/parts/text/form_xmpp_password.tpl.php')); ?></label>
    <input type="password" maxlength="250" autocomplete="new-password" class="form-control" name="password"  value="" />
</div>

<div class="form-group">
    <label><input type="checkbox" name="sendmessage" value="on" <?php $xmppaccount->sendmessage == 1 ? print 'checked="checked"' : ''?> /> <?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/form','Send message to this operator of comes chat request to one of his departments');?></label>
</div>

<div class="form-group">
    <label><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/form','Account type, you never can change account type');?></label>
    <select class="form-control" name="accountType" disabled="disabled">
        <option value="0" <?php $xmppaccount->type == 0 ? print 'selected="selected"' : ''?>><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/form','Operator');?></option>
        <option value="1" <?php $xmppaccount->type == 1 ? print 'selected="selected"' : ''?>><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/form','Visitor');?></option>
        <option value="2" <?php $xmppaccount->type == 2 ? print 'selected="selected"' : ''?>><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/form','Chat');?></option>
    </select>
</div>

<div class="form-group">
    <label><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/form','Operator to whom belongs account')?></label>
    <?php if ($xmppaccount->type == 0) : ?>
    <?php echo erLhcoreClassRenderHelper::renderCombobox(array(
            'input_name'     => 'user_id',
    		'optional_field' => erTranslationClassLhTranslation::getInstance()->getTranslation('chat/lists/search_panel','Select user'),
            'selected_id'    => $xmppaccount->user_id,
            'css_class'      => 'form-control',
            'list_function'  => 'erLhcoreClassXMPPServiceAccountValidator::getLHCUsers',
            'list_function_params'  => array('user_id' => $xmppaccount->user_id),
    )); ?>
    <?php else : ?>
    <i><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/form','This user type can not have assigned operator')?></i>
    <?php endif;?>
</div>


