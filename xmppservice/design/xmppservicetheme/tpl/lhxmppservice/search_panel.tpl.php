<form action="<?php echo $input->form_action?>" method="get" name="SearchFormRight">
	<input type="hidden" name="doSearch" value="1">
	<div class="row">
		<div class="col-md-3">
			<div class="form-group">
				<label><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/module','Username');?></label> <input type="text" class="form-control" name="username" value="<?php echo htmlspecialchars($input->username)?>" />
			</div>
		</div>
		<div class="col-md-3">
			<div class="form-group">
				<label><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/module','Account type');?></label>
				<select name="type" class="form-control">
				    <option value="" <?php echo htmlspecialchars($input->type === '') ? 'selected="selected"' : ''?>><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/module','Any');?></option>
				    <option value="0" <?php echo htmlspecialchars($input->type === 0) ? 'selected="selected"' : ''?>><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/module','Operators');?></option>
				    <option value="1" <?php echo htmlspecialchars($input->type === 1) ? 'selected="selected"' : ''?>><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/module','Online visitors');?></option>
				    <option value="2" <?php echo htmlspecialchars($input->type === 2) ? 'selected="selected"' : ''?>><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/module','Chats');?></option>
				</select>
			</div>
		</div>
		<div class="col-md-6">
			<div class="form-group">
				<label><?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('chat/lists/search_panel','Date range from to');?></label>
				<div class="row">
					<div class="col-xs-6">
						<input type="text" class="form-control" name="timefrom" id="id_timefrom" placeholder="E.g <?php echo date('Y-m-d',time()-7*24*3600)?>" value="<?php echo htmlspecialchars($input->timefrom)?>" />
					</div>
					<div class="col-xs-6">
						<input type="text" class="form-control" name="timeto" id="id_timeto" placeholder="E.g <?php echo date('Y-m-d')?>" value="<?php echo htmlspecialchars($input->timeto)?>" />
					</div>
				</div>
			</div>
		</div>
	</div>

	<div class="btn-group" role="group" aria-label="...">
		<input type="submit" name="doSearch" class="btn btn-default" value="<?php echo erTranslationClassLhTranslation::getInstance()->getTranslation('chat/lists/search_panel','Search');?>" />
	</div>
</form>

<script>
$(function() {
	$('#id_timefrom,#id_timeto').fdatepicker({
		format: 'yyyy-mm-dd'
	});
});
</script>