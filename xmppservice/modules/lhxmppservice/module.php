<?php

$Module = array( "name" => "LHC XMPP module",
				 'variable_params' => true );

$ViewList = array();

/**
 * Callback handlers
 * */
$ViewList['operatorstatus'] = array(
    'params' => array(),
    'uparams' => array()
);

$ViewList['processmessage'] = array(
    'params' => array(),
    'uparams' => array()
);

/**
 * General user cases
 * */
$ViewList['operators'] = array(
    'params' => array(),
    'uparams' => array('username','type','timefrom','timeto'),
    'functions' => array('use_admin'),
);

$ViewList['editoperator'] = array(
    'params' => array('id'),
    'uparams' => array(),
    'functions' => array('use_admin'),
);

$ViewList['deleteoperator'] = array(
    'params' => array('id'),
    'uparams' => array('csfr'),
    'functions' => array('use_admin'),
);

$ViewList['index'] = array(
    'params' => array(),
    'uparams' => array(),
    'functions' => array('use_admin'),
);

$ViewList['newxmppaccount'] = array(
    'params' => array(),
    'uparams' => array(),
    'functions' => array('use_admin'),
);

$FunctionList['use_admin'] = array('explain' => 'Allow operator to use XMPP module');