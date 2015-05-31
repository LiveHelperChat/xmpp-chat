<div class="form-group">
    <label><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/form','XMPP Username');?></label>
        
    <div class="input-group">
        <input type="text" class="form-control" name="username" <?php if ($xmppaccount->id > 0) : ?>disabled="disabled"<?php endif;?> value="<?php echo htmlspecialchars($xmppaccount->username_plain);?>" />
        <div class="input-group-addon">@<?php echo erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['xmpp_host']?></div>
    </div>    
</div>

<div class="form-group">
    <label><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/form','XMPP Password, enter only if you are creating new user. In most cases system does not need it.');?></label>
    <input type="text" class="form-control" name="password"  value="" />
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
    <?php echo erLhcoreClassRenderHelper::renderCombobox( array (
            'input_name'     => 'user_id',
    		'optional_field' => erTranslationClassLhTranslation::getInstance()->getTranslation('chat/lists/search_panel','Select user'),
            'selected_id'    => $xmppaccount->user_id,
            'css_class'      => 'form-control',
            'list_function'  => 'erLhcoreClassModelUser::getUserList'
    )); ?> 
    <?php else : ?>
    <i><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/form','This user type can not have assigned operator')?></i>
    <?php endif;?>
</div>


