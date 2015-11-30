<h1><?php include(erLhcoreClassDesign::designtpl('lhxmppservice/parts/text/editoperator_edit_xmpp_account.tpl.php')); ?></h1>

<?php if (isset($errors)) : ?>
	<?php include(erLhcoreClassDesign::designtpl('lhkernel/validation_error.tpl.php'));?>
<?php endif; ?>

<form action="<?php echo erLhcoreClassDesign::baseurl('xmppservice/editoperator')?>/<?php echo $xmppaccount->id?>" method="post">

	<?php include(erLhcoreClassDesign::designtpl('lhxmppservice/xmppaccount/form.tpl.php'));?>
	
    <div class="btn-group" role="group" aria-label="...">
		<input <?php if ($xmppaccount->type != 0) : ?>title="<?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/editoperator','You cannot edit this user type');?>" disabled="disabled"<?php endif;?> type="submit" class="btn btn-default" name="Save_action" value="<?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('system/buttons','Save');?>"/>
		<input type="submit" class="btn btn-default" name="Cancel_action" value="<?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('system/buttons','Cancel');?>"/>
	</div>
	
</form>
