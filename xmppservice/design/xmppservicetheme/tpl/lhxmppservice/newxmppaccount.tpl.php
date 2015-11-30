<h1><?php include(erLhcoreClassDesign::designtpl('lhxmppservice/parts/text/newxmppaccount_new_xmpp_account.tpl.php')); ?></h1>

<?php if (isset($errors)) : ?>
	<?php include(erLhcoreClassDesign::designtpl('lhkernel/validation_error.tpl.php'));?>
<?php endif; ?>

<form action="<?php echo erLhcoreClassDesign::baseurl('xmppservice/newxmppaccount')?>" method="post">

	<?php include(erLhcoreClassDesign::designtpl('lhxmppservice/xmppaccount/form.tpl.php'));?>
	
    <div class="btn-group" role="group" aria-label="...">
		<input type="submit" class="btn btn-default" name="Save_departament" value="<?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('system/buttons','Save');?>"/>
		<input type="submit" class="btn btn-default" name="Cancel_departament" value="<?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('system/buttons','Cancel');?>"/>
	</div>
	
</form>
