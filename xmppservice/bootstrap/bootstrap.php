<?php

class erLhcoreClassExtensionXmppservice
{
    public function __construct()
    {}
    
    public function run()
    {
        $this->registerAutoload();
        
        $dispatcher = erLhcoreClassChatEventDispatcher::getInstance();
        
        $dispatcher->listen('onlineuser.created', array(
            $this,
            'onlineUserCreated'
        ));
        
        $dispatcher->listen('onlineuser.pageview_logged', array(
            $this,
            'onlineUserPageViewLogged'
        ));
        
        $dispatcher->listen('chat.addmsguser', array(
            $this,
            'addMessageUser'
        ));
        
        $dispatcher->listen('chat.chat_started', array(
            $this,
            'chatStarted'
        ));
        
        $dispatcher->listen('xmppservice.removeaccount', array(
            $this,
            'deleteXMPPUser'
        ));
        
        $dispatcher->listen('chat.close', array(
            $this,
            'deleteChatUser'
        ));
        
        $dispatcher->listen('chat.delete', array(
            $this,
            'deleteChatUser'
        ));
    }
    
    public function deleteXMPPUser($params) {    
        erLhcoreClassExtensionXmppserviceHandler::deleteXMPPUser(array(
            'xmpp_account' => $params['account'],
            'xmpp_host' => $this->settings['xmpp_host'],
            'host_login' => $this->settings['host_login'],
            'node_api_server' => $this->settings['node_api_server'],          
        ));
    }
    
    public function deleteChatUser($params) {
        $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array('filter' => array('type' => erLhcoreClassModelXMPPAccount::USER_TYPE_CHAT, 'user_id' => $params['chat']->id)));
        
        if ($xmppAccount !== false) {
            $xmppAccount->removeThis();
        }
    }
    
    public function sendMessageToOperatorAsUserByChat($params)
    {
        $chat = $params['chat'];
        
        $xmppAccount = false;
        
        if ($chat->online_user_id > 0) {
            $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array('filter' => array('type' => erLhcoreClassModelXMPPAccount::USER_TYPE_VISITOR, 'user_id' => $chat->online_user_id)));
        }
        
        if ($xmppAccount === false) {
            $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array('filter' => array('type' => erLhcoreClassModelXMPPAccount::USER_TYPE_CHAT, 'user_id' => $chat->id)));
        }
        
        if ($xmppAccount === false) {
            $xmppAccount = $this->registerXMPPAccountByChat(array('chat' => $chat));
        }
        
        if ($xmppAccount !== false && $chat->user_id > 0)
        {
            // Forward this information to NodeJS server
            erLhcoreClassExtensionXmppserviceHandler::sendMessageByVisitorDirect(array(
                'xmpp_account' => $xmppAccount,
                'xmpp_account_operator' => $params['xmpp_account_operator'],                
                'host_login' => $this->settings['host_login'],
                'node_api_server' => $this->settings['node_api_server'],            
                'msg' => $params['msg'],
                'chat' => $chat
            ));
        }        
    }

    public function addMessageUser($params)
    {
        $chat = $params['chat'];
        
        $xmppAccount = false;
        
        if ($chat->online_user_id > 0) {
            $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array('filter' => array('type' => erLhcoreClassModelXMPPAccount::USER_TYPE_VISITOR, 'user_id' => $chat->online_user_id)));
        }

        if ($xmppAccount === false) {
            $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array('filter' => array('type' => erLhcoreClassModelXMPPAccount::USER_TYPE_CHAT, 'user_id' => $chat->id)));
        }

        if ($xmppAccount === false) {
            $xmppAccount = $this->registerXMPPAccountByChat(array('chat' => $chat));
        }
        
        // We could not determine a recipient
        if ($xmppAccount !== false && $chat->user_id > 0)        
        {
            // Forward this information to NodeJS server
            erLhcoreClassExtensionXmppserviceHandler::sendMessageByVisitor(array(
                'xmpp_account' => $xmppAccount,
                'host_login' => $this->settings['host_login'],
                'node_api_server' => $this->settings['node_api_server'],
                'chat' => $chat,
                'msg' => $params['msg'],
            ));
        } else {           
            $params['xmpp_account'] = $xmppAccount;
            $this->sendMessageToAllDepartmentOperators($params);
        }
    }

    public function sendMessageToAllDepartmentOperators($params)
    {
        // Determine what operators should receive this message
        $db = ezcDbInstance::get();
        $stmt = $db->prepare("SELECT user_id FROM lh_userdep WHERE dep_id = 0 OR dep_id = :dep_id AND lh_userdep.last_activity > :last_activity");
        $stmt->bindValue(':dep_id',$params['chat']->dep_id,PDO::PARAM_INT);
        $stmt->bindValue(':last_activity',(time()-(int)erLhcoreClassModelChatConfig::fetch('sync_sound_settings')->data['online_timeout']),PDO::PARAM_INT);
        $stmt->execute();
        $usersId = $stmt->fetchAll(PDO::FETCH_COLUMN);
            
        if (!empty($usersId)) {
            $accountsXMPP = erLhcoreClassModelXMPPAccount::getList(array('filter' => array('type' => erLhcoreClassModelXMPPAccount::USER_TYPE_OPERATOR, 'sendmessage' => 1),'filterin' => array('user_id' => $usersId)));
        
            foreach ($accountsXMPP as $xmppAccountOpetrator) {
        
                if ($params['msg'] === false) {
                    $params['msg'] = new stdClass();
                    $params['msg']->msg = 'New chat request from visitor. Reply to accept a chat';
                }
        
                erLhcoreClassExtensionXmppserviceHandler::sendMessageStartChat(array(
                    'xmpp_account' => $params['xmpp_account'],
                    'xmpp_account_operator' => $xmppAccountOpetrator,
                    'host_login' => $this->settings['host_login'],
                    'node_api_server' => $this->settings['node_api_server'],
                    'chat' => $params['chat'],
                    'msg' => $params['msg'],
                ));
            }
        }
    }

    public function registerXMPPAccountByChat($params)
    {
        $chat = $params['chat'];
        
        // Create DB record for XMPP user
        $xmppAccount = new erLhcoreClassModelXMPPAccount();
        $xmppAccount->lactivity = $xmppAccount->ctime = time();
        $xmppAccount->username = 'visitor.'.$chat->id.'.chat@'.$this->settings['xmpp_host'];
        $xmppAccount->password = substr(md5(microtime().rand(0,100).$chat->hash),0,20);
        $xmppAccount->user_id = $chat->id;
        $xmppAccount->type = erLhcoreClassModelXMPPAccount::USER_TYPE_CHAT;
        $xmppAccount->saveThis();
        
        // Forward this information to NodeJS server
        erLhcoreClassExtensionXmppserviceHandler::newChat(array(
            'xmpp_account' => $xmppAccount,
            'xmpp_host' => $this->settings['xmpp_host'],
            'chat' => $chat,
            'host_login' => $this->settings['host_login'],
            'node_api_server' => $this->settings['node_api_server'],
        ));
        
        return $xmppAccount;
    }
    
    public function registerOperator(erLhcoreClassModelXMPPAccount $xmppAccount)
    {
        erLhcoreClassExtensionXmppserviceHandler::registerOperator(array(
            'xmpp_account' => $xmppAccount,
            'xmpp_host' => $this->settings['xmpp_host'],
            'host_login' => $this->settings['host_login'],
            'node_api_server' => $this->settings['node_api_server'],
        ));
    }

    /**
     * @desc Change operator password if entered
     * 
     * @param erLhcoreClassModelXMPPAccount $xmppAccount
     */
    public function changeOperatorPassword(erLhcoreClassModelXMPPAccount $xmppAccount)
    {
        erLhcoreClassExtensionXmppserviceHandler::changeOperatorPassword(array(
            'xmpp_account' => $xmppAccount,
            'xmpp_host' => $this->settings['xmpp_host'],
            'host_login' => $this->settings['host_login'],
            'node_api_server' => $this->settings['node_api_server'],
        ));
    }
    
    /**
     * Executes then chat is started. Workflow
     * 1. Check perhaps chat online user already have an xmpp account
     * 2. If account not found create an account based on chat data. Third type of xmpp users
     * 3. First we select all online operators based on department
     * 4. Then we check does any of selectred operators has xmpp account. If they do have send initial message to them.
     * 5. First operator who answers a user is assigned as chat owner.
     * 6. if chat already has operator operator message is ignore
     * */
    public function chatStarted($params)
    {               
        $xmppAccount = false;
        
        if ($params['chat']->online_user_id > 0) {
           $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array('filter' => array('type' => erLhcoreClassModelXMPPAccount::USER_TYPE_VISITOR, 'user_id' => $params['chat']->online_user_id)));            
        }
        
        // Online user is not assigned or xmpp account does not exists
        // So create an account based on chat
        if ($xmppAccount === false) {            
            $xmppAccount = $this->registerXMPPAccountByChat(array('chat' => $params['chat']));           
        }

        $params['xmpp_account'] = $xmppAccount;
        
        $this->sendMessageToAllDepartmentOperators($params);
    }

    /**
     * @desc get's callend then online user is created
     * 
     * @param array $params
     * */
    public function onlineUserCreated($params)
    {    	
        if ($this->settings['online_visitors_tracking'] == true) {
        	// Create DB record for XMPP user
        	$xmppAccount = new erLhcoreClassModelXMPPAccount();
        	$xmppAccount->lactivity = $xmppAccount->ctime = time();
        	$xmppAccount->username = 'visitor.'.$params['ou']->id.'@'.$this->settings['xmpp_host'];
        	$xmppAccount->password = substr(md5(microtime().rand(0,100).$params['ou']->vid),0,20);
        	$xmppAccount->user_id = $params['ou']->id;
        	$xmppAccount->type = erLhcoreClassModelXMPPAccount::USER_TYPE_VISITOR;
        	$xmppAccount->saveThis();
        	
        	// Forward this information to NodeJS server
        	erLhcoreClassExtensionXmppserviceHandler::newOnlineVisitor(array(
        			'xmpp_account' => $xmppAccount,
        	        'xmpp_host' => $this->settings['xmpp_host'],
        			'ou' => $params['ou'],
        			'host_login' => $this->settings['host_login'],
        			'node_api_server' => $this->settings['node_api_server'],
        	));
        }
    }
    
    /**
     * @desc get's callend then online user does a pageview
     * 
     * @param unknown $params
     */
    public function onlineUserPageViewLogged($params) {
        if ($this->settings['online_visitors_tracking'] == true) {
        	if (($xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array('filter' => array('type' => erLhcoreClassModelXMPPAccount::USER_TYPE_VISITOR, 'user_id' => $params['ou']->id)))) !== false)
        	{
    	    		// Forward this information to NodeJS server
    	    		erLhcoreClassExtensionXmppserviceHandler::onlineUserPageViewLogged(array(
    	    				'xmpp_account' => $xmppAccount,
    	    				'ou' => $params['ou'],
    	    				'host_login' => $this->settings['host_login'],
    	    				'node_api_server' => $this->settings['node_api_server'],
    	    		));
        	} 
        }
    }
    
    public function __get($var) {
    	switch ($var) {
    		    
    		case 'settings':
    			$this->settings = include ('extension/xmppservice/settings/settings.ini.php');
    			if ($this->settings['ahosting'] == true) {
    				$autoamtedHostingSettings = erLhcoreClassInstance::getInstance()->getCustomFieldsData(1);
    				//$this->settings['is_enabled'] = isset($autoamtedHostingSettings['clicktocall_supported']) && $autoamtedHostingSettings['clicktocall_supported'] == 1;
    				//$this->settings['buttonid'] = isset($autoamtedHostingSettings['clicktocall_buttonid']) ? $autoamtedHostingSettings['clicktocall_buttonid'] : '';
    				//$this->settings['customfields'] = isset($autoamtedHostingSettings['clicktocall_customfields']) ? $autoamtedHostingSettings['clicktocall_customfields'] : '';
    			}
    			return $this->settings;
    			break;    
    
    		default:
    			;
    			break;
    	}
    }
    
    public function registerAutoload()
    {
        spl_autoload_register(array($this, 'autoload'), true, false);
    }
    
    public static function getSession()
    {
        if ( !isset( self::$persistentSession ) )
        {
            self::$persistentSession = new ezcPersistentSession(
                ezcDbInstance::get(),
                new ezcPersistentCodeManager( './extension/xmppservice/pos' )
            );
        }
        
        return self::$persistentSession;
    }
    
    public function autoload($className)
    {
        $classesArray = array(
            'erLhcoreClassModelXMPPAccount' => 'extension/xmppservice/classes/erlhcoreclassmodelxmppaccount.php',
            'erLhcoreClassXMPPServiceAccountValidator' => 'extension/xmppservice/classes/erlhcoreclassxmppaccountvalidator.php',
            'erLhcoreClassExtensionXmppserviceHandler' => 'extension/xmppservice/classes/erlhcoreclassxmppservicehandler.php',
        );
        
        if (key_exists($className, $classesArray)) {
            include_once $classesArray[$className];
        }       
    }
    
    private static $persistentSession;
}


