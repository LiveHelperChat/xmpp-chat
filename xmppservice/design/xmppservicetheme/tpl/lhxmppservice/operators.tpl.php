<h1 class="attr-header"><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/operators','XMPP Accounts');?></h1>

<?php include(erLhcoreClassDesign::designtpl('lhxmppservice/search_panel.tpl.php')); ?>

<?php if ($pages->items_total > 0) { ?>

<form action="<?php echo $input->form_action,$inputAppend?>" method="post">

<?php include(erLhcoreClassDesign::designtpl('lhkernel/csfr_token.tpl.php'));?>

<table cellpadding="0" cellspacing="0" class="table" width="100%">
<thead>
    <tr>   
        <th width="1%">ID</th>
        <th><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/operators','Username');?></th>
        <th><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/operators','Last activity');?></th>
        <th><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/operators','Created');?></th>
        <th><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/operators','Operator/Chat/Online visitor');?></th>
        <th width="1%"></th>
    </tr>
</thead>
    <?php foreach ($items as $item) : ?>
    <tr>
        <td><?php echo $item->id?></td>
        <td><?php echo $item->username?></td>
        <td><?php if ($item->lactivity > 0) : ?><?php echo $item->lactivity_front;?><?php else : ?>-<?php endif;?></td>     
        <td><?php if ($item->ctime > 0) : ?><?php echo $item->ctime_front;?><?php else : ?>-<?php endif;?></td>        	
        <td>
        <?php if ($item->type == erLhcoreClassModelXMPPAccount::USER_TYPE_OPERATOR) : ?>
            <?php echo $item->user?>
        <?php elseif ($item->type == erLhcoreClassModelXMPPAccount::USER_TYPE_VISITOR) : ?>
            <?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/operators','Online visitor');?> #<?php echo $item->user_id?>
        <?php elseif ($item->type == erLhcoreClassModelXMPPAccount::USER_TYPE_CHAT) : ?>
            <?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/operators','Chat');?> #<?php echo $item->user_id?>
        <?php endif;?>
        </td>
        <td nowrap>
          <div class="btn-group" role="group" aria-label="..." style="width:60px;">
            <a class="btn btn-default btn-xs icon-pencil" href="<?php echo erLhcoreClassDesign::baseurl('xmppservice/editoperator')?>/<?php echo $item->id?>" ></a>
            <a class="btn btn-danger btn-xs icon-cancel-squared csfr-required" onclick="return confirm('<?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('kernel/messages','Are you sure?');?>')" href="<?php echo erLhcoreClassDesign::baseurl('xmppservice/deleteoperator')?>/<?php echo $item->id?>" ></a>
          </div>
        </td>
    </tr>
    <?php endforeach; ?>
</table>

<?php include(erLhcoreClassDesign::designtpl('lhkernel/secure_links.tpl.php')); ?>

<?php if (isset($pages)) : ?>
    <?php include(erLhcoreClassDesign::designtpl('lhkernel/paginator.tpl.php')); ?>
<?php endif;?>

</form>

<?php } else { ?>
<br/>
<p><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('chat/activechats','Empty...');?></p>
<?php } ?>

<div><a class="btn btn-default" href="<?php echo erLhcoreClassDesign::baseurl('xmppservice/newxmppaccount')?>"><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/operators','New xmpp account');?></a></div>