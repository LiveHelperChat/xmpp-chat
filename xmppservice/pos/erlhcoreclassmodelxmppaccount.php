<?php

$def = new ezcPersistentObjectDefinition();
$def->table = "lhc_xmpp_service_account";
$def->class = "erLhcoreClassModelXMPPAccount";

$def->idProperty = new ezcPersistentObjectIdProperty();
$def->idProperty->columnName = 'id';
$def->idProperty->propertyName = 'id';
$def->idProperty->generator = new ezcPersistentGeneratorDefinition(  'ezcPersistentNativeGenerator' );

$def->properties['username'] = new ezcPersistentObjectProperty();
$def->properties['username']->columnName   = 'username';
$def->properties['username']->propertyName = 'username';
$def->properties['username']->propertyType = ezcPersistentObjectProperty::PHP_TYPE_STRING;

$def->properties['password'] = new ezcPersistentObjectProperty();
$def->properties['password']->columnName   = 'password';
$def->properties['password']->propertyName = 'password';
$def->properties['password']->propertyType = ezcPersistentObjectProperty::PHP_TYPE_STRING;

$def->properties['user_id'] = new ezcPersistentObjectProperty();
$def->properties['user_id']->columnName   = 'user_id';
$def->properties['user_id']->propertyName = 'user_id';
$def->properties['user_id']->propertyType = ezcPersistentObjectProperty::PHP_TYPE_INT;

$def->properties['type'] = new ezcPersistentObjectProperty();
$def->properties['type']->columnName   = 'type';
$def->properties['type']->propertyName = 'type';
$def->properties['type']->propertyType = ezcPersistentObjectProperty::PHP_TYPE_INT;

$def->properties['ctime'] = new ezcPersistentObjectProperty();
$def->properties['ctime']->columnName   = 'ctime';
$def->properties['ctime']->propertyName = 'ctime';
$def->properties['ctime']->propertyType = ezcPersistentObjectProperty::PHP_TYPE_INT;

$def->properties['lactivity'] = new ezcPersistentObjectProperty();
$def->properties['lactivity']->columnName   = 'lactivity';
$def->properties['lactivity']->propertyName = 'lactivity';
$def->properties['lactivity']->propertyType = ezcPersistentObjectProperty::PHP_TYPE_INT;

$def->properties['sendmessage'] = new ezcPersistentObjectProperty();
$def->properties['sendmessage']->columnName   = 'sendmessage';
$def->properties['sendmessage']->propertyName = 'sendmessage';
$def->properties['sendmessage']->propertyType = ezcPersistentObjectProperty::PHP_TYPE_INT;

return $def;

?>