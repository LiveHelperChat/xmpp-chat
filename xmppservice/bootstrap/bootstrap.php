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
        
        $dispatcher->listen('instance.registered.created', array(
            $this,
            'instanceCreated'
        ));

        $dispatcher->listen('instance.destroyed', array(
            $this,
            'instanceDestroyed'
        ));

        $dispatcher->listen('instance.extensions_structure', array(
            $this,
            'checkStructure'
        ));

        $dispatcher->listen('chat.messages_added_passive', array(
            $this,
            'passiveMessage'
        ));

        $dispatcher->listen('user.after_logout', array(
            $this,
            'afterLogout'
        ));

        $dispatcher->listen('user.user_created', array(
            $this,
            'userCreated'
        ));

        $dispatcher->listen('user.deleted', array(
            $this,
            'userDeleted'
        ));

        $dispatcher->listen('chat.nick_changed', array(
            $this,
            'nickChanged'
        ));
        $dispatcher->listen('group_chat.web_add_msg_admin', array(
            $this,
            'groupChatMsg'
        ));
        $dispatcher->listen('group_chat.new_group_chat_room', array(
            $this,
            'newGroupChatRoom'
        ));
        $dispatcher->listen('group_chat.remove_group_chat', array(
            $this,
            'removeGroupChat'
        ));
        $dispatcher->listen('group_chat.new_group_chat_member', array(
            $this,
            'newGroupChatMember'
        ));
    }

    /**
     * Used only in automated hosting enviroment
     */
    public function instanceCreated($params)
    {
        try {
            // Set subdomain manual, so we avoid calling in cronjob
            $this->instanceManual = $params['instance'];
            
            // Just do table updates
            erLhcoreClassUpdate::doTablesUpdate(json_decode(file_get_contents('extension/xmppservice/doc/structure.json'), true));
            
            erLhcoreClassExtensionXmppserviceHandler::registerInstanceRoasters(array(
                'subdomain' => str_replace('.', '-', $params['instance']->address),
                'xmpp_host' => $this->settings['xmpp_host'],
                'node_api_server' => $this->settings['node_api_server'],
                'handler' => $this->settings['handler'],
                'rpc_server' => $this->settings['rpc_server'],
                'rpc_username' => $this->settings['rpc_username'],
                'rpc_password' => $this->settings['rpc_password'],
                'rpc_account_host' => $this->settings['rpc_account_host']
            ));
            
            if ($this->settings['create_xmpp_username_by_lhc_username'] == true) {

                $userData = new erLhcoreClassModelUser();
                $userData->username = $params['instance']->email;
                $userData->email = $params['instance']->email;
                $userData->id = 1;                            
                
                $this->userCreated(array(
                    'userData' => $userData,
                    'subdomain' => str_replace('.', '-', $params['instance']->address),
                    'password' => $params['password']
                ));
            }
        } catch (Exception $e) {
            erLhcoreClassLog::write(print_r($e, true));
        }
    }

    /**
     * Checks automated hosting structure
     * 
     * This part is executed once in manager is run this cronjob.
     * php cron.php -s site_admin -e instance -c cron/extensions_update
     * 
     * */
    public function checkStructure()
    {
        // Just do table updates
        erLhcoreClassUpdate::doTablesUpdate(json_decode(file_get_contents('extension/xmppservice/doc/structure.json'), true));
        
        // Shared rosters for standalone enviroment have to be created manually
        if ($this->settings['ahosting'] == true) {
            erLhcoreClassExtensionXmppserviceHandler::checkSharedRoasters(array(
                'subdomain' => $this->settings['subdomain'],
                'xmpp_host' => $this->settings['xmpp_host'],
                'node_api_server' => $this->settings['node_api_server'],
                'handler' => $this->settings['handler'],
                'rpc_server' => $this->settings['rpc_server'],
                'rpc_username' => $this->settings['rpc_username'],
                'rpc_password' => $this->settings['rpc_password'],
                'rpc_account_host' => $this->settings['rpc_account_host']
            ));
        }
    }

    /**
     * Used then instance is destroyed
     */
    public function instanceDestroyed($params)
    {
        try {
            // Remove shared roasters
            erLhcoreClassExtensionXmppserviceHandler::instanceDestroyed(array(
                'subdomain' => str_replace('.', '-', $params['instance']->address),
                'xmpp_host' => $this->settings['xmpp_host'],
                'node_api_server' => $this->settings['node_api_server'],
                'handler' => $this->settings['handler'],
                'rpc_server' => $this->settings['rpc_server'],
                'rpc_username' => $this->settings['rpc_username'],
                'rpc_password' => $this->settings['rpc_password'],
                'rpc_account_host' => $this->settings['rpc_account_host']
            ));
            
            // Remove users
            foreach (erLhcoreClassModelXMPPAccount::getList(array(
                'limit' => 1000000
            )) as $user) {
                $user->removeThis();
            }
            
        } catch (Exception $e) {
            erLhcoreClassLog::write(print_r($e, true));
        }
    }

    /**
     * Deletes related XMPP account on user removement if enabled
     */
    public function userDeleted($params)
    {
        if ($this->settings['delete_xmpp_by_user_removement'] == true) {
            foreach (erLhcoreClassModelXMPPAccount::getList(array(
                'limit' => 1000000,
                'filter' => array(
                    'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_OPERATOR,
                    'user_id' =>  $params['userData']->id
                ),
            )) as $user) {
                $user->removeThis();
            }
        }
    }
    
    public function deleteXMPPUser($params)
    {
        erLhcoreClassExtensionXmppserviceHandler::deleteXMPPUser(array(
            'xmpp_account' => $params['account'],
            'xmpp_host' => $this->settings['xmpp_host'],
            'host_login' => $this->settings['host_login'],
            'node_api_server' => $this->settings['node_api_server'],
            'handler' => $this->settings['handler'],
            'rpc_server' => $this->settings['rpc_server'],
            'rpc_username' => $this->settings['rpc_username'],
            'rpc_password' => $this->settings['rpc_password'],
            'rpc_account_host' => $this->settings['rpc_account_host']
        ));
    }

    public function deleteChatUser($params)
    {
        $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array(
            'filter' => array(
                'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_CHAT,
                'user_id' => $params['chat']->id
            )
        ));
        
        if ($xmppAccount !== false) {
            $xmppAccount->removeThis();
        }
    }

    /**
     * This extension can overide web account logout action
     * 
     * overrides default logout action and set last activity if required
     * 
     * @desc last activity is set if
     * 1. Module is enabled
     * 2. Override last activity is active
     * 3. We have found related XMPP user and it was active in the last time() - default timeout - extended timeout
     * */
    public function afterLogout($params)
    {
        if ($this->settings['enabled'] == true && $this->settings['check_xmpp_activity_on_web_logout'] == true && erLhcoreClassModelXMPPAccount::getCount(array('filtergt' => array('lactivity' => time() - (int)erLhcoreClassModelChatConfig::fetch('sync_sound_settings')->data['online_timeout'] - (int)$this->settings['append_time']),'filter' => array('user_id' => (int)$params['user']->getUserID())))) {
            $db = ezcDbInstance::get();
            $stmt = $db->prepare('UPDATE lh_userdep SET last_activity = :last_activity WHERE user_id = :user_id');
            $stmt->bindValue( ':last_activity',time()+$this->settings['append_time']);
            $stmt->bindValue( ':user_id',(int)$params['user']->getUserID());
            $stmt->execute();
        }
    }
    
    public function getXMPPAccountByChat($chat)
    {
        $xmppAccount = false;
        
        if ($chat->online_user_id > 0) {
            $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array(
                'filter' => array(
                    'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_VISITOR,
                    'user_id' => $chat->online_user_id
                )
            ));
        }
        
        if ($xmppAccount === false) {
            $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array(
                'filter' => array(
                    'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_CHAT,
                    'user_id' => $chat->id
                )
            ));
        }
        
        return $xmppAccount;
    }
    
    public function sendMessageToOperatorAsUserByChat($params)
    {
        if ($this->settings['enabled'] == true && isset($params['chat']) && isset($params['msg'])) {
            $chat = $params['chat'];
            
            $xmppAccount = false;
            
            if ($chat->online_user_id > 0) {
                $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array(
                    'filter' => array(
                        'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_VISITOR,
                        'user_id' => $chat->online_user_id
                    )
                ));
            }
            
            if ($xmppAccount === false) {
                $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array(
                    'filter' => array(
                        'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_CHAT,
                        'user_id' => $chat->id
                    )
                ));
            }
            
            if ($xmppAccount === false) {
                $xmppAccount = $this->registerXMPPAccountByChat(array(
                    'chat' => $chat
                ));
            }
            
            if ($xmppAccount !== false && $chat->user_id > 0) {
                // Forward this information to NodeJS server
                erLhcoreClassExtensionXmppserviceHandler::sendMessageByVisitorDirect(array(
                    'xmpp_account' => $xmppAccount,
                    'xmpp_account_operator' => $params['xmpp_account_operator'],
                    'host_login' => $this->settings['host_login'],
                    'node_api_server' => $this->settings['node_api_server'],
                    'msg' => $params['msg'],
                    'chat' => $chat,
                    'handler' => $this->settings['handler'],
                    'rpc_server' => $this->settings['rpc_server'],
                    'rpc_username' => $this->settings['rpc_username'],
                    'rpc_password' => $this->settings['rpc_password'],
                    'rpc_account_host' => $this->settings['rpc_account_host'],
                    'xmpp_host' => $this->settings['xmpp_host'],
                ));
            }
        }
    }

    public function passiveMessage($params)
    {
        if ($this->settings['enabled'] == true) {
            
            $onlineOptions = erLhcoreClassModelChatConfig::fetch('xmppservice_options')->data;
            
            if (isset($onlineOptions['xmpp_enabled']) && $onlineOptions['xmpp_enabled'] == true) {
                // Send message if XMPP account was active in the past 300 seconds
                $xmppOperator = erLhcoreClassModelXMPPAccount::findOne(array(
                    'filtergt' => array(
                        'lactivity' => time() - 300
                    ),
                    'filter' => array(
                        'user_id' => $params['chat']->user_id,
                        'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_OPERATOR
                    )
                ));
                
                if ($xmppOperator !== false) {
                    $params['xmpp_account_operator'] = $xmppOperator;
                    $params['msg'] = erLhcoreClassBBCode::parseForMail($params['msg']->msg);
                    $this->sendMessageToOperatorAsUserByChat($params);
                }
            }
        }
    }

    /**
     * Executed then user sends a message to operator. We just forward message to xmpp username
     * */
    public function addMessageUser($params)
    {
        if ($this->settings['enabled'] == true) {
            
            $onlineOptions = erLhcoreClassModelChatConfig::fetch('xmppservice_options')->data;
            
            if (isset($onlineOptions['xmpp_enabled']) && $onlineOptions['xmpp_enabled'] == true) {
            
                $chat = $params['chat'];
                
                $xmppAccount = false;
                
                if ($chat->online_user_id > 0) {
                    $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array(
                        'filter' => array(
                            'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_VISITOR,
                            'user_id' => $chat->online_user_id
                        )
                    ));
                }
                
                if ($xmppAccount === false) {
                    $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array(
                        'filter' => array(
                            'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_CHAT,
                            'user_id' => $chat->id
                        )
                    ));
                }
                
                if ($xmppAccount === false) {
                    $xmppAccount = $this->registerXMPPAccountByChat(array(
                        'chat' => $chat
                    ));
                }
                                
                // We could not determine a recipient
                if ($xmppAccount !== false && $chat->user_id > 0) {
                    // Forward this information to NodeJS server
                    erLhcoreClassExtensionXmppserviceHandler::sendMessageByVisitor(array(
                        'xmpp_account' => $xmppAccount,
                        'host_login' => $this->settings['host_login'],
                        'node_api_server' => $this->settings['node_api_server'],
                        'chat' => $chat,
                        'msg' => $params['msg'],
                        'handler' => $this->settings['handler'],
                        'rpc_server' => $this->settings['rpc_server'],
                        'rpc_username' => $this->settings['rpc_username'],
                        'rpc_password' => $this->settings['rpc_password'],
                        'rpc_account_host' => $this->settings['rpc_account_host'],
                        'xmpp_host' => $this->settings['xmpp_host'],
                    ));
                } else {
                    $params['xmpp_account'] = $xmppAccount;
                    $this->sendMessageToAllDepartmentOperators($params);
                }
            }
        }
    }
    
    /**
     * Nick changed handling so we change nick in XMPP client.
     * 
     * */
    public function nickChanged($params)
    {
        if ($this->settings['enabled'] == true) {
        
            $onlineOptions = erLhcoreClassModelChatConfig::fetch('xmppservice_options')->data;
        
            if (isset($onlineOptions['xmpp_enabled']) && $onlineOptions['xmpp_enabled'] == true) {

                $chat = $params['chat'];

                $xmppAccount = false;

                if ($chat->online_user_id > 0) {
                    $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array(
                        'filter' => array(
                            'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_VISITOR,
                            'user_id' => $chat->online_user_id
                        )
                    ));
                }
        
                if ($xmppAccount === false) {
                    $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array(
                        'filter' => array(
                            'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_CHAT,
                            'user_id' => $chat->id
                        )
                    ));
                }
        
                if ($xmppAccount === false) {
                    $xmppAccount = $this->registerXMPPAccountByChat(array(
                        'chat' => $chat
                    ));
                }
                
                // Change user nick to proper one
                $rpc = new \GameNet\Jabber\RpcClient(array(
                    'server' => $this->settings['rpc_server'],
                    'host' =>  $this->settings['xmpp_host'],
                    'account_host' => $this->settings['rpc_account_host'],
                    'username' => $this->settings['rpc_username'],
                    'password' => $this->settings['rpc_password']
                ));
                
                $paramsOnlineUser = erLhcoreClassExtensionXmppserviceHandler::getNickAndStatusByChat($chat);
                
                $rpc->setNick($xmppAccount->username_plain, $paramsOnlineUser['nick'], $paramsOnlineUser['status']);
            }
        }        
    }
    
    /**
     * New Operator Group Chat created
     *
     * */
    public function newGroupChatRoom($params)
    {
        if ($this->settings['enabled'] == true) {
            $chat = $params['chat'];
            if ($chat !== false) {
                $host = $this->settings['xmpp_host'];
                $room = strtolower($chat->name);
                $userData = erLhcoreClassModelUser::fetch($chat->user_id);
                if ($this->roomExists($room) == false) {
                    // Create new MUC room
                    $rpc = new \GameNet\Jabber\RpcClient(array(
                        'server' => $this->settings['rpc_server'],
                        'host' =>  $this->settings['xmpp_host'],
                        'account_host' => $this->settings['rpc_account_host'],
                        'username' => $this->settings['rpc_username'],
                        'password' => $this->settings['rpc_password']
                    ));
                    $rpc->createRoom($room);
                }
                $rpc = new \GameNet\Jabber\RpcClient(array(
                    'server' => $this->settings['rpc_server'],
                    'host' =>  $this->settings['xmpp_host'],
                    'account_host' => $this->settings['rpc_account_host'],
                    'username' => $this->settings['rpc_username'],
                    'password' => $this->settings['rpc_password']
                ));
                $rpc->setRoomOption($room, "allow_subscription", "true"); 
            }
        }
    }

    public function newGroupChatMember($params) {
        if ($this->settings['enabled'] == true) {
            $member = $params['member'];
            $chat = erLhcoreClassModelGroupChat::findOne(array('filter' => array('id' => $member->group_id)));
            if ($chat !== false) {
                $room = strtolower($chat->name);
                $userData = erLhcoreClassModelUser::fetch($member->user_id);
                $userName = strtolower($userData->name . '.' . $userData->surname . '.operator');
                $host = $this->settings['xmpp_host'];

                $xmppOperator = erLhcoreClassModelXMPPAccount::findOne(array(
                    'filter' => array(
                        'user_id' => $member->user_id,
                        'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_OPERATOR
                    )
                ));

                // If there is NOT an XMPP account then just subscribe else invite and subscribe
                if ($xmppOperator === false) {
                    $rpc = new \GameNet\Jabber\RpcClient(array(
                        'server' => $this->settings['rpc_server'],
                        'host' =>  $host,
                        'account_host' => $this->settings['rpc_account_host'],
                        'username' => $this->settings['rpc_username'],
                        'password' => $this->settings['rpc_password']
                    ));
                    $rpc->subscribeToRoom($userName . '@' . $host, $userName, $room);
                } else {
                    $rpc = new \GameNet\Jabber\RpcClient(array(
                        'server' => $this->settings['rpc_server'],
                        'host' =>  $this->settings['xmpp_host'],
                        'account_host' => $this->settings['rpc_account_host'],
                        'username' => $this->settings['rpc_username'],
                        'password' => $this->settings['rpc_password']
                    ));
                    $rpc->inviteToRoom($room, "", "invite for private chat", array($xmppOperator->username));

                    $rpc = new \GameNet\Jabber\RpcClient(array(
                        'server' => $this->settings['rpc_server'],
                        'host' =>  $host,
                        'account_host' => $this->settings['rpc_account_host'],
                        'username' => $this->settings['rpc_username'],
                        'password' => $this->settings['rpc_password']
                    ));
                    $rpc->subscribeToRoom($xmppOperator->username, $userName, $room);
                }
            }
        }
    }

    public function removeGroupChat($params)
    {
        if ($this->settings['enabled'] == true) {
            $chat = $params['chat'];
            if ($chat !== false) {
                $room = strtolower($chat->name);
                if ($this->roomExists($room) == true) {
                    // Destroy the MUC room
                    $rpc = new \GameNet\Jabber\RpcClient(array(
                        'server' => $this->settings['rpc_server'],
                        'host' =>  $this->settings['xmpp_host'],
                        'account_host' => $this->settings['rpc_account_host'],
                        'username' => $this->settings['rpc_username'],
                        'password' => $this->settings['rpc_password']
                    ));
                    $rpc->deleteRoom($room);
                }
            }
        }
    }

    public function roomExists($room) {
        $rpc = new \GameNet\Jabber\RpcClient(array(
            'server' => $this->settings['rpc_server'],
            'host' =>  $this->settings['xmpp_host'],
            'account_host' => $this->settings['rpc_account_host'],
            'username' => $this->settings['rpc_username'],
            'password' => $this->settings['rpc_password']
        ));
        $rooms = $rpc->getOnlineRooms();
        if (in_array($room . '@conference.' . $this->settings['xmpp_host'], $rooms)) {
            return true;
        } else {
            return false;
        } 
    }

    public function groupChatMsg($params) {
        if ($this->settings['enabled'] == true) {
            $msg = $params['msg'];
            $chat = $params['chat'];
            $room = strtolower($chat->name) . '@conference.' . $this->settings['xmpp_host'];
            $sender = str_replace(' ', '.', $msg->name_support . '.operator@' .  $this->settings['xmpp_host']);
            $rpc = new \GameNet\Jabber\RpcClient(array(
                'server' => $this->settings['rpc_server'],
                'host' =>  $this->settings['xmpp_host'],
                'account_host' => $this->settings['rpc_account_host'],
                'username' => $this->settings['rpc_username'],
                'password' => $this->settings['rpc_password']
            ));
            $rpc->sendMessageChat($sender, $room, $msg->msg, "", 'groupchat');
        }

    }

    public function sendMessageToAllDepartmentOperators($params)
    {
        if ($this->settings['enabled'] == true) {
            // Determine what operators should receive this message
            $db = ezcDbInstance::get();
            $stmt = $db->prepare("SELECT user_id FROM lh_userdep WHERE dep_id = 0 OR dep_id = :dep_id AND lh_userdep.last_activity > :last_activity");
            $stmt->bindValue(':dep_id', $params['chat']->dep_id, PDO::PARAM_INT);
            $stmt->bindValue(':last_activity', (time() - (int) erLhcoreClassModelChatConfig::fetch('sync_sound_settings')->data['online_timeout']), PDO::PARAM_INT);
            $stmt->execute();
            $usersId = $stmt->fetchAll(PDO::FETCH_COLUMN);
            
            if (! empty($usersId)) {
                $accountsXMPP = erLhcoreClassModelXMPPAccount::getList(array(
                    'filter' => array(
                        'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_OPERATOR,
                        'sendmessage' => 1
                    ),
                    'filterin' => array(
                        'user_id' => $usersId
                    )
                ));
                
                // Change user nick to proper one
                $rpc = new \GameNet\Jabber\RpcClient(array(
                    'server' => $this->settings['rpc_server'],
                    'host' =>  $this->settings['xmpp_host'],
                    'account_host' => $this->settings['rpc_account_host'],
                    'username' => $this->settings['rpc_username'],
                    'password' => $this->settings['rpc_password']
                ));
                
                $paramsOnlineUser = erLhcoreClassExtensionXmppserviceHandler::getNickAndStatusByChat($params['chat']);
                
                // Nick
                $rpc->setNick($params['xmpp_account']->username_plain, $paramsOnlineUser['nick'], $paramsOnlineUser['status']);
                
                // Send messages
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
                        'handler' => $this->settings['handler'],
                        'rpc_server' => $this->settings['rpc_server'],
                        'rpc_username' => $this->settings['rpc_username'],
                        'rpc_password' => $this->settings['rpc_password'],
                        'rpc_account_host' => $this->settings['rpc_account_host'],
                        'xmpp_host' => $this->settings['xmpp_host'],
                    ));
                }
            }
        }
    }

    public function registerXMPPAccountByChat($params)
    {
        if ($this->settings['enabled'] == true) {
            $chat = $params['chat'];
            
            // Append automated hosting subdomain if required
            $subdomainUser = '';
            if ($this->settings['subdomain'] != '') {
                $subdomainUser = '.' . $this->settings['subdomain'];
            }
            
            // Create DB record for XMPP user
            $xmppAccount = new erLhcoreClassModelXMPPAccount();
            $xmppAccount->lactivity = $xmppAccount->ctime = time();
            $xmppAccount->username = 'visitor.' . $chat->id . '.chat' . $subdomainUser . '@' . $this->settings['xmpp_host'];
            $xmppAccount->password = substr(md5(microtime() . rand(0, 100) . $chat->hash), 0, 20);
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
                'handler' => $this->settings['handler'],
                'rpc_server' => $this->settings['rpc_server'],
                'rpc_username' => $this->settings['rpc_username'],
                'rpc_password' => $this->settings['rpc_password'],
                'rpc_account_host' => $this->settings['rpc_account_host']
            ));
            
            erLhcoreClassChatEventDispatcher::getInstance()->dispatch('xmppservice.chat_account_created', array(
                'xmppaccount' => $xmppAccount,
                'chat' => $chat
            ));
            
            return $xmppAccount;
        }
    }

    public function registerOperator(erLhcoreClassModelXMPPAccount $xmppAccount)
    {
        if ($this->settings['enabled'] == true) {
            erLhcoreClassExtensionXmppserviceHandler::registerOperator(array(
                'xmpp_account' => $xmppAccount,
                'xmpp_host' => $this->settings['xmpp_host'],
                'host_login' => $this->settings['host_login'],
                'node_api_server' => $this->settings['node_api_server'],
                'handler' => $this->settings['handler'],
                'rpc_server' => $this->settings['rpc_server'],
                'rpc_username' => $this->settings['rpc_username'],
                'rpc_password' => $this->settings['rpc_password'],
                'rpc_account_host' => $this->settings['rpc_account_host']
            ));
        }
    }

    /**
     * Change operator password if entered
     *
     * @param erLhcoreClassModelXMPPAccount $xmppAccount            
     */
    public function changeOperatorPassword(erLhcoreClassModelXMPPAccount $xmppAccount)
    {
        if ($this->settings['enabled'] == true) {
            erLhcoreClassExtensionXmppserviceHandler::changeOperatorPassword(array(
                'xmpp_account' => $xmppAccount,
                'xmpp_host' => $this->settings['xmpp_host'],
                'host_login' => $this->settings['host_login'],
                'node_api_server' => $this->settings['node_api_server'],
                'handler' => $this->settings['handler'],
                'rpc_server' => $this->settings['rpc_server'],
                'rpc_username' => $this->settings['rpc_username'],
                'rpc_password' => $this->settings['rpc_password'],
                'rpc_account_host' => $this->settings['rpc_account_host']
            ));
        }
    }

    /**
     * Executes then chat is started.
     * Workflow
     * 1. Check perhaps chat online user already have an xmpp account
     * 2. If account not found create an account based on chat data. Third type of xmpp users
     * 3. First we select all online operators based on department
     * 4. Then we check does any of selectred operators has xmpp account. If they do have send initial message to them.
     * 5. First operator who answers a user is assigned as chat owner.
     * 6. if chat already has operator operator message is ignore
     */
    public function chatStarted($params)
    {
        if ($this->settings['enabled'] == true) {
            
            $onlineOptions = erLhcoreClassModelChatConfig::fetch('xmppservice_options')->data;
            
            if (isset($onlineOptions['xmpp_enabled']) && $onlineOptions['xmpp_enabled'] == true) {
            
                $xmppAccount = false;
                
                if ($params['chat']->online_user_id > 0) {
                    $xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array(
                        'filter' => array(
                            'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_VISITOR,
                            'user_id' => $params['chat']->online_user_id
                        )
                    ));
                }
                
                // Online user is not assigned or xmpp account does not exists
                // So create an account based on chat
                if ($xmppAccount === false) {
                    $xmppAccount = $this->registerXMPPAccountByChat(array(
                        'chat' => $params['chat']
                    ));
                }
                
                $params['xmpp_account'] = $xmppAccount;
                
                $this->sendMessageToAllDepartmentOperators($params);
            }
        }
    }

    /**
     * get's callend then online user is created
     *
     * @param array $params            
     *
     */
    public function onlineUserCreated($params)
    {
        if ($this->settings['enabled'] == true && $this->settings['online_visitors_tracking'] == true) {
            
            $onlineOptions = erLhcoreClassModelChatConfig::fetch('xmppservice_options')->data;
            
            if (isset($onlineOptions['track_online']) && $onlineOptions['track_online'] == true) {
                // Append automated hosting subdomain if required
                $subdomainUser = '';
                if ($this->settings['subdomain'] != '') {
                    $subdomainUser = '.' . $this->settings['subdomain'];
                }
                
                // Create DB record for XMPP user
                $xmppAccount = new erLhcoreClassModelXMPPAccount();
                $xmppAccount->lactivity = $xmppAccount->ctime = time();
                $xmppAccount->username = 'visitor.' . $params['ou']->id . $subdomainUser . '@' . $this->settings['xmpp_host'];
                $xmppAccount->password = substr(md5(microtime() . rand(0, 100) . $params['ou']->vid), 0, 20);
                $xmppAccount->user_id = $params['ou']->id;
                $xmppAccount->type = erLhcoreClassModelXMPPAccount::USER_TYPE_VISITOR;
                $xmppAccount->saveThis();
                            
                if ($this->settings['handler'] == 'rpc' && is_object($params['tpl']) && $this->settings['online_visitors_tracking'] == true) {
                    $params['tpl']->set('xmppAccount', $xmppAccount);
                }
                
                // Forward this information to NodeJS server
                erLhcoreClassExtensionXmppserviceHandler::newOnlineVisitor(array(
                    'xmpp_account' => $xmppAccount,
                    'xmpp_host' => $this->settings['xmpp_host'],
                    'ou' => $params['ou'],
                    'host_login' => $this->settings['host_login'],
                    'node_api_server' => $this->settings['node_api_server'],
                    'handler' => $this->settings['handler'],
                    'rpc_server' => $this->settings['rpc_server'],
                    'rpc_username' => $this->settings['rpc_username'],
                    'rpc_password' => $this->settings['rpc_password'],
                    'rpc_account_host' => $this->settings['rpc_account_host']
                ));
                
                erLhcoreClassChatEventDispatcher::getInstance()->dispatch('xmppservice.online_account_created', array(
                    'xmppaccount' => $xmppAccount,
                    'ou' => $params['ou']
                ));
            }
        }
    }

    /**
     * get's callend then online user does a pageview
     *
     * @param array $params            
     */
    public function onlineUserPageViewLogged($params)
    {
        if ($this->settings['enabled'] == true && $this->settings['online_visitors_tracking'] == true) {

            $onlineOptions = erLhcoreClassModelChatConfig::fetch('xmppservice_options')->data;
            
            if (isset($onlineOptions['track_online']) && $onlineOptions['track_online'] == true) {
                           
                if (($xmppAccount = erLhcoreClassModelXMPPAccount::findOne(array(
                    'filter' => array(
                        'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_VISITOR,
                        'user_id' => $params['ou']->id
                    )
                ))) !== false) {
                                  
                    if ($this->settings['handler'] == 'rpc' && is_object($params['tpl']) && $this->settings['online_visitors_tracking'] == true) {
    
                        /**
                         * In the future then websockets will support attatch method this could be used
                         * */
                        /* $xmppAccount->attach_data = erLhcoreClassExtensionXmppserviceHandler::prebindSession(array(
                            'username' => $xmppAccount->username,
                            'password' => $xmppAccount->password,
                            'host' => $this->settings['prebind_host'] . $xmppAccount->username_plain,
                        )); */
                        
                        $params['tpl']->set('xmppAccount', $xmppAccount); 
                    }
              
                    // Forward this information to NodeJS server
                    erLhcoreClassExtensionXmppserviceHandler::onlineUserPageViewLogged(array(
                        'xmpp_account' => $xmppAccount,
                        'ou' => $params['ou'],
                        'host_login' => $this->settings['host_login'],
                        'node_api_server' => $this->settings['node_api_server'],
                        'handler' => $this->settings['handler'],
                        'xmpp_host' => $this->settings['xmpp_host'],
                        'rpc_server' => $this->settings['rpc_server'],
                        'rpc_username' => $this->settings['rpc_username'],
                        'rpc_password' => $this->settings['rpc_password'],
                        'rpc_account_host' => $this->settings['rpc_account_host']                        
                    ));
                }
            }
        }
    }

    /**
     * Executed when new account is created.
     * */
    public function userCreated($params)
    {
        if ($this->settings['create_xmpp_username_by_lhc_username'] == true) {

            $xmppAccount = new erLhcoreClassModelXMPPAccount();

            // "username" or "email" Supported
            if ($this->settings['type_for_xmpp_username'] == 'username') {
                $xmppAccount->username = $params['userData']->username;
            } elseif ($this->settings['type_for_xmpp_username'] == 'email') {
                list($xmppAccount->username) = explode('@', $params['userData']->email);
            }

            // Clear username
            $xmppAccount->username = preg_replace("/[^a-zA-Z0-9]/", "", $xmppAccount->username);

            // Set operator username if it's empty
            if ($xmppAccount->username == '') {
                $xmppAccount->username = 'operator.' . $params['userData']->id;
            }

            // Format valid username
            $subdomain = isset($params['subdomain']) ? $params['subdomain'] : erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['subdomain'];
            $xmppAccount->username = $xmppAccount->username . ($subdomain != '' ? '.'.$subdomain : '') . '@' . erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['xmpp_host'];

            // Other attributes
            $xmppAccount->ctime = time();
            $xmppAccount->password = $params['password'];
            $xmppAccount->user_id = $params['userData']->id;
            $xmppAccount->sendmessage = (int)$this->settings['xmpp_send_messages'];
            
            // XMPP account
            erLhcoreClassXMPPServiceAccountValidator::publishXMPPAccount($xmppAccount);
        }
    }

    public function __get($var)
    {
        switch ($var) {
                        
            case 'settings':
                $this->settings = include ('extension/xmppservice/settings/settings.ini.php');
                if ($this->settings['ahosting'] == true) {
                    $this->settings['subdomain'] = str_replace('.', '-',$this->instanceManual !== false ? $this->instanceManual->address :  erLhcoreClassInstance::getInstance()->address);
                    $this->settings['enabled'] = $this->instanceManual !== false ? $this->instanceManual->full_xmpp_chat_supported == 1 : erLhcoreClassInstance::getInstance()->full_xmpp_chat_supported == 1;
                    $this->settings['online_visitors_tracking'] = $this->instanceManual !== false ? $this->instanceManual->full_xmpp_visitors_tracking == 1 : erLhcoreClassInstance::getInstance()->full_xmpp_visitors_tracking == 1;
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
       
        spl_autoload_register(array(
            $this,
            'autoload'
        ), true, false);
    }

    public static function getSession()
    {
        if (! isset(self::$persistentSession)) {
            self::$persistentSession = new ezcPersistentSession(ezcDbInstance::get(), new ezcPersistentCodeManager('./extension/xmppservice/pos'));
        }
        
        return self::$persistentSession;
    }

    public function autoload($className)
    {
        $classesArray = array(
            'erLhcoreClassModelXMPPAccount' => 'extension/xmppservice/classes/erlhcoreclassmodelxmppaccount.php',
            'erLhcoreClassXMPPServiceAccountValidator' => 'extension/xmppservice/classes/erlhcoreclassxmppaccountvalidator.php',
            'erLhcoreClassExtensionXmppserviceHandler' => 'extension/xmppservice/classes/erlhcoreclassxmppservicehandler.php',
            'GameNet\Jabber\RpcClient'                 => 'extension/xmppservice/classes/php-jabber-rpc/GameNet/Jabber/RpcClient.php',
            'GameNet\Jabber\Mixins\UserTrait'          => 'extension/xmppservice/classes/php-jabber-rpc/GameNet/Jabber/Mixins/UserTrait.php',
            'GameNet\Jabber\Mixins\RosterTrait'        => 'extension/xmppservice/classes/php-jabber-rpc/GameNet/Jabber/Mixins/RosterTrait.php',
            'GameNet\Jabber\Mixins\RoomTrait'          => 'extension/xmppservice/classes/php-jabber-rpc/GameNet/Jabber/Mixins/RoomTrait.php',
            'GameNet\Jabber\Mixins\GroupTrait'         => 'extension/xmppservice/classes/php-jabber-rpc/GameNet/Jabber/Mixins/GroupTrait.php'
        );
        
        if (key_exists($className, $classesArray)) {
            include_once $classesArray[$className];
        }
    }

    private $instanceManual = false;
    
    private static $persistentSession;
}
