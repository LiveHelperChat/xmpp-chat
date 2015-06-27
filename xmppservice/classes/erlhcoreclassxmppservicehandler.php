<?php


class erLhcoreClassExtensionXmppserviceHandler
{

    /**
     * $userPlain remdex2@xmpp.livehelperchat.com/16103000591431105595929303
     *
     * @param string $userPlain            
     */
    public static function parseXMPPUser($userPlain)
    {
        $parts = explode('/', $userPlain);
        
        list ($user, $server) = explode('@', $parts[0]);
        
        if ($user != '' && $server != '') {
            return array(
                'user' => $user,
                'server' => $server,
                'xmppuser' => $parts[0]
            );
        } else {
            throw new Exception('Could not parse user - ' . $userPlain);
        }
    }

    /**
     * Deletes old XMPP accounts based on lactivity field value.
     *
     * Deletes only chat and visitors accounts, NOT operators.
     * Old account is considered if from last login has passed more than 1 day
     *
     * Are called in these methods
     *
     * @see erLhcoreClassExtensionXmppserviceHandler::newChat()
     * @see erLhcoreClassExtensionXmppserviceHandler::newOnlineVisitor()
     */
    public static function cleanupOldXMPPAccounts()
    {
        $oldAccounts = erLhcoreClassModelXMPPAccount::getList(array(
            'filterin' => array(
                'type' => array(
                    erLhcoreClassModelXMPPAccount::USER_TYPE_CHAT,
                    erLhcoreClassModelXMPPAccount::USER_TYPE_VISITOR
                )
            ),
            'filterlt' => array(
                'lactivity' => time() - (24 * 3600)
            )
        ));
        foreach ($oldAccounts as $xmppAccount) {
            $xmppAccount->removeThis();
        }
    }

    /**
     * Sends request as JSON content and returns response in plain text
     *
     * @param
     *            url to request
     *            
     * @param array $data
     *            which later is encoded in json_encode
     *            
     * @param bool $asJson            
     *
     *
     */
    public static function sendRequest($url, $data, $asJson = true)
    {
        // Append secret key
        $data['secret_key'] = erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['secret_key'];
        
        $data_string = json_encode($data);
        
        $ch = curl_init($url);
        curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "POST");
        curl_setopt($ch, CURLOPT_POSTFIELDS, $data_string);
        curl_setopt($ch, CURLOPT_TIMEOUT, 5);
        curl_setopt($ch, CURLOPT_CONNECTTIMEOUT, 5);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);
        curl_setopt($ch, CURLOPT_SSL_VERIFYHOST, false);
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Content-Type: application/json',
            'Content-Length: ' . strlen($data_string)
        ));
        
        $response = curl_exec($ch);
        
        if ($response === false) {
            throw new Exception('Empty response');
        }
        
        if ($asJson == true) {
                 
            $jsonObject = json_decode($response, true);
            
            if ($jsonObject === false) {
                throw new Exception('Could not decode JSON, response - ' . $response);
            }
            
            if ($jsonObject === null) {
                throw new Exception('Could not decode JSON, response - ' . $response);
            }
                        
            return $jsonObject;
        }
        
        return $response;
    }

    /**
     * Updates last activity of LHC internal user
     *
     * @param int $userId            
     *
     * @param string $lastActivity            
     */
    public static function updateActivityByUserId($userId, $lastActivity = false)
    {
        if ($lastActivity === false) {
            $lastActivity = time();
        }
        
        $db = ezcDbInstance::get();
        $stmt = $db->prepare('UPDATE lh_userdep SET last_activity = :last_activity WHERE user_id = :user_id');
        $stmt->bindValue(':last_activity', $lastActivity, PDO::PARAM_INT);
        $stmt->bindValue(':user_id', $userId, PDO::PARAM_INT);
        $stmt->execute();
        
        // Update last activity field value in XMPP accounts table
        if ($lastActivity !== false && $lastActivity > 0) {
            $stmt = $db->prepare('UPDATE lhc_xmpp_service_account SET lactivity = :lactivity WHERE user_id = :user_id');
            $stmt->bindValue(':lactivity', $lastActivity, PDO::PARAM_INT);
            $stmt->bindValue(':user_id', $userId, PDO::PARAM_INT);
            $stmt->execute();
        }
    }

    /**
     * Updates XMPP account activity by account ID
     */
    public static function updateActivityByXMPPAccountId($id)
    {
        $db = ezcDbInstance::get();
        $stmt = $db->prepare('UPDATE lhc_xmpp_service_account SET lactivity = :lactivity WHERE id = :id');
        $stmt->bindValue(':lactivity', time(), PDO::PARAM_INT);
        $stmt->bindValue(':id', $id, PDO::PARAM_INT);
        $stmt->execute();
    }

    /**
     *
     * @param XMPP username
     *            
     * @return int related lhc user id
     *        
     */
    public static function getUserIDByXMPPUsername($xmppUsername)
    {
        $db = ezcDbInstance::get();
        $stmt = $db->prepare('SELECT user_id FROM lhc_xmpp_service_account WHERE username = :username');
        $stmt->bindValue(':username', $xmppUsername, PDO::PARAM_STR);
        $stmt->execute();
        return $stmt->fetch(PDO::FETCH_COLUMN);
    }

    /**
     * @desc delete xmpp user operator/visitor/chat based
     *
     * @param unknown $params
     */
    public static function deleteXMPPUser($params)
    {
        $xmppAccount = $params['xmpp_account'];
        
        $userParts = explode('@', $xmppAccount->username);
     
        // Append automated hosting subdomain if required
        $subdomainUser = erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['subdomain'];
        if ($subdomainUser != '') {
            $subdomainUser = '.' . $subdomainUser;
        }
        
        // Delete from shared roaster first
        $data = array(
            "user" => $userParts[0],
            "host" => $params['xmpp_host'],
            "grouphost" => $params['xmpp_host'],
            "group" => $xmppAccount->type == erLhcoreClassModelXMPPAccount::USER_TYPE_OPERATOR ? 'operators'.$subdomainUser : 'visitors'.$subdomainUser
        );
        
        try {
            
            if ($params['handler'] == 'rpc') {
                            
                $rpc = new \GameNet\Jabber\RpcClient([
                    'server' => $params['rpc_server'],
                    'host' => $params['xmpp_host']
                ]);
            
                $rpc->deleteUserSharedRosterGroup($data['user'],  $data['group']);
                
            } else {
            
                $response = self::sendRequest($params['node_api_server'] . '/xmpp-delete-user-from-roaster', $data);
                
                if ($response['error'] == true) {
                    throw new Exception($response['msg']);
                }
            }
            
        } catch (Exception $e) {
            
            if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                erLhcoreClassLog::write(print_r($e, true));
            }
            
            throw new Exception('Could not delete user from roaster!');
        }
        
        // Delete user
        $data = array(
            "user" => $userParts[0],
            "host" => $params['xmpp_host']
        );
        
        try {
            
            if ($params['handler'] == 'rpc') {
                
                $rpc = new \GameNet\Jabber\RpcClient([
                    'server' => $params['rpc_server'],
                    'host' => $params['xmpp_host']
                ]);
                
                $rpc->unregisterUser($data['user']);
                
            } else {
                $response = self::sendRequest($params['node_api_server'] . '/xmpp-unregister', $data);
                
                if ($response['error'] == true) {
                    throw new Exception($response['msg']);
                }
            }
            
        } catch (Exception $e) {
            
            if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                erLhcoreClassLog::write(print_r($e, true));
            }
            
            throw new Exception('Could not delete user!');
        }
    }

    /**
     * @desc Used then visitor writes a message but chat is not accepted and we send message to all connected resposible department operators from this user with next his message.
     * 
     * @todo add RPC support
     * 
     * */
    public static function sendMessageByVisitorDirect($params = array())
    {
        $paramsOnlineUser = self::getNickAndStatusByChat($params['chat']);
        
        $data = array(
            "jid" => $params['xmpp_account']->username,
            "pass" => $params['xmpp_account']->password,
            "host" => $params['host_login'],
            "operator_username" => $params['xmpp_account_operator']->username,
            "message" => $params['msg'],
            "nick" => $paramsOnlineUser['nick'],
            "status" => $paramsOnlineUser['status']
        );
        
        try {
            
            if ($params['handler'] == 'rpc') {
            
                $rpc = new \GameNet\Jabber\RpcClient([
                    'server' => $params['rpc_server'],
                    'host' => $params['xmpp_host']
                ]);
            
                $rpc->sendMessageChat($data['jid'], $data['operator_username'], $data['message']);
            
            } else {
                $response = self::sendRequest($params['node_api_server'] . '/xmpp-send-message', $data, false);
                
                if ($response != 'ok') {
                    throw new Exception('ok as response not received');
                }
            }
        } catch (Exception $e) {
            
            if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                erLhcoreClassLog::write(print_r($e, true));
            }
        }
    }

    /**
     * Used then chat is accepted by one of operators and visitor writes a message
     * 
     * */
    public static function sendMessageByVisitor($params = array())
    {
        $xmppAccount = $params['xmpp_account'];
        
        $page = '';
        
        if ($params['chat']->online_user !== false) {
            $page = $params['chat']->online_user->current_page;
        }
        
        $paramsOnlineUser = self::getNickAndStatusByChat($params['chat'], $page);
        
        if ($params['chat']->user_id > 0) {
            
            $xmppAccountOperator = erLhcoreClassModelXMPPAccount::findOne(array(
                'filter' => array (
                    'type' => erLhcoreClassModelXMPPAccount::USER_TYPE_OPERATOR,
                    'user_id' => $params['chat']->user_id
                )
            ));
            
            if ($xmppAccountOperator !== false) {
                $data = array(
                    "jid" => $xmppAccount->username,
                    "pass" => $xmppAccount->password,
                    "host" => $params['host_login'],
                    "operator_username" => $xmppAccountOperator->username,
                    "message" => $params['msg']->msg,
                    "nick" => $paramsOnlineUser['nick'],
                    "status" => $paramsOnlineUser['status']
                );
                
                try {

                    if ($params['handler'] == 'rpc') {
                    
                        $rpc = new \GameNet\Jabber\RpcClient([
                            'server' => $params['rpc_server'],
                            'host' => $params['xmpp_host']
                        ]);
                    
                        $rpc->sendMessageChat($data['jid'], $data['operator_username'], $data['message']);
                    
                    } else {
                        $response = self::sendRequest($params['node_api_server'] . '/xmpp-send-message', $data, false);
                        
                        if ($response != 'ok') {
                            throw new Exception('ok as response not received');
                        }
                    }
                } catch (Exception $e) {
                    if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                        erLhcoreClassLog::write(print_r($e, true));
                    }
                }
                
                self::updateActivityByXMPPAccountId($xmppAccount->id);
            }
        }
    }

    /**
     * Used then chat is started and we send messages to all online operators
     * 
     * @todo remake using rpc method
     * 
     * */
    public static function sendMessageStartChat($params = array())
    {
        $paramsOnlineUser = self::getNickAndStatusByChat($params['chat']);
        
        $data = array(
            "jid" => $params['xmpp_account']->username,
            "pass" => $params['xmpp_account']->password,
            "host" => $params['host_login'],
            "operator_username" => $params['xmpp_account_operator']->username,
            "message" => $params['msg']->msg,
            "nick" => $paramsOnlineUser['nick'],
            "status" => $paramsOnlineUser['status']
        );
        
        try {
            
            if ($params['handler'] == 'rpc') {
                
                $rpc = new \GameNet\Jabber\RpcClient([
                    'server' => $params['rpc_server'],
                    'host' => $params['xmpp_host']
                ]);
                
                $rpc->sendMessageChat($data['jid'], $data['operator_username'], $data['message']);
                
            } else {
            
                $response = self::sendRequest($params['node_api_server'] . '/xmpp-send-message', $data, false);
                
                if ($response != 'ok') {
                    throw new Exception('ok as response not received');
                }
            }
        } catch (Exception $e) {
            if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                erLhcoreClassLog::write(print_r($e, true));
            }
        }
    }

    /**
     * Sumarizes attributes if it's chat
     */
    public static function getNickAndStatusByChat(erLhcoreClassModelChat $chat, $page = '')
    {
        $paramsReturn = array();
        $paramsOnlineUser = array();
        
        $paramsOnlineUser[] = (string) $chat->department;
        
        if ($chat->country_code != '') {
            $paramsOnlineUser[] = $chat->country_code . ($chat->city != '' ? ' (' . $chat->city . ')' : '');
        }
        
        if ($page == '' && $chat->referrer != '') {
            $paramsOnlineUser[] = $chat->referrer;
        } elseif ($page != '') {
            $paramsOnlineUser[] = $page;
        }
        
        if ($chat->user_tz_identifier != '') {
            $paramsOnlineUser[] = 'Time zone: ' . $chat->user_tz_identifier . ' (' . $chat->user_tz_identifier_time . ')';
        }
        
        if ($chat->status == erLhcoreClassModelChat::STATUS_ACTIVE_CHAT) {
            $paramsOnlineUser[] = 'Active chat';
        } elseif ($chat->status == erLhcoreClassModelChat::STATUS_PENDING_CHAT) {
            $paramsOnlineUser[] = 'Pending chat';
        }
        
        $paramsReturn['status'] = implode("\n| ", $paramsOnlineUser);
        
        $paramsReturn['nick'] = $chat->nick . ' #' . $chat->id;
        
        return $paramsReturn;
    }

    /**
     * Sumarizes online visitor attribtues
     *
     * @param erLhcoreClassModelChatOnlineUser $onlineUser            
     *
     * @return array
     */
    public static function getNickAndStatusByOnlineVisitor(erLhcoreClassModelChatOnlineUser $onlineUser)
    {
        $paramsReturn = array();
        
        $paramsOnlineUser = array();
        
        if ($onlineUser->chat_id > 0 && $onlineUser->chat !== false) {
            return self::getNickAndStatusByChat($onlineUser->chat, $onlineUser->current_page);
        }
        
        if ($onlineUser->user_country_code != '') {
            $paramsOnlineUser[] = $onlineUser->user_country_code . ($onlineUser->city != '' ? ' (' . $onlineUser->city . ')' : '');
        }
        
        if ($onlineUser->current_page != '') {
            $paramsOnlineUser[] = $onlineUser->current_page;
        }
        
        if ($onlineUser->visitor_tz != '') {
            $paramsOnlineUser[] = 'Time zone: ' . $onlineUser->visitor_tz . ' (' . $onlineUser->visitor_tz_time . ')';
        }
        
        if ($onlineUser->lastactivity_ago != '') {
            $paramsOnlineUser[] = 'Last activity ago: ' . $onlineUser->lastactivity_ago;
        }
        
        $paramsOnlineUser[] = 'Visits - ' . $onlineUser->total_visits;
        
        $paramsReturn['status'] = implode("\n| ", $paramsOnlineUser);
        
        $paramsReturn['nick'] = "Online visitor";
        
        return $paramsReturn;
    }

    /**
     * @desc
     * 
     * Prebind xmpp sessions, it's faster this way. At the moment it's not used because websocket does not supports attach method
     * 
     * */
    public static function prebindSession($params)
    {
        $ch = curl_init($params['host']);
        curl_setopt($ch, CURLOPT_TIMEOUT, 5);
        curl_setopt($ch, CURLOPT_CONNECTTIMEOUT, 5); 
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);
        curl_setopt($ch, CURLOPT_SSL_VERIFYHOST, false);
        curl_setopt($ch, CURLOPT_USERPWD, $params['username'].':'.$params['password']);
        $response = curl_exec($ch);
        
        if ($response !== false && $response != ''){
            
            list($JID, $SID, $RID) = explode("\n", $response);
            
            return array (
                'jid' => $JID,
                'sid' => $SID,
                'rid' => $RID,
            );
        }
        
        return false;
    }
    
    
    /**
     * called then online visityor does a pageview
     *
     */
    public static function onlineUserPageViewLogged($params = array())
    {
        $xmppAccount = $params['xmpp_account'];
        
        // We execute only if nodejs handler is used, otherwise visitor connects himself
        if ($params['handler'] != 'rpc') {
        
            $paramsOnlineUser = self::getNickAndStatusByOnlineVisitor($params['ou']);
            
            $data = array(
                "jid" => $xmppAccount->username,
                "pass" => $xmppAccount->password,
                "host" => $params['host_login'],
                "nick" => $paramsOnlineUser['nick'],
                "status" => $paramsOnlineUser['status']
            );
            
            try {
                $response = self::sendRequest($params['node_api_server'] . '/xmpp', $data, false);
                
                if ($response != 'ok') {
                    throw new Exception('ok as response not received');
                }
                
            } catch (Exception $e) {
                if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                    erLhcoreClassLog::write(print_r($e, true));
                }
            }
        }
        
        self::updateActivityByXMPPAccountId($xmppAccount->id);
    }

    /**
     * Called then new chat is initiated
     *
     * register chat request as XMPP user and sends message to all online operators responsible to department
     *
     * @param array $params    
     *         
     * @throws Exception
     * 
     */
    public static function newChat($params = array())
    {
        $xmppAccount = $params['xmpp_account'];
        
        $userParts = explode('@', $xmppAccount->username);
        
        $paramsChat = self::getNickAndStatusByChat($params['chat']);
        
        // Append automated hosting subdomain if required
        $subdomainUser = erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['subdomain'];
        if ($subdomainUser != '') {
            $subdomainUser = '.' . $subdomainUser;
        }
        
        $data = array (
            "user" => $userParts[0],
            "host" => $params['xmpp_host'],
            "password" => $xmppAccount->password,
            "hostlogin" => $params['host_login'],
            "nick" => $paramsChat['nick'],
            "status" => $paramsChat['status'],
            'group' => 'visitors' . $subdomainUser
        );
        
        try {
            
            if ($params['handler'] == 'rpc') {
                
                $rpc = new \GameNet\Jabber\RpcClient([
                    'server' => $params['rpc_server'],
                    'host' => $params['xmpp_host']
                ]);
                
                $rpc->createUser($data['user'], $data['password']);
                
                $rpc->addUserToSharedRosterGroup($data['user'], $data['group']);
                
            } else {   
                         
                $response = self::sendRequest($params['node_api_server'] . '/xmpp-register-online-visitor', $data);
                
                if ($response['error'] == true) {
                    throw new Exception($response['msg']);
                }
            }
            
        } catch (Exception $e) {
            if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                erLhcoreClassLog::write(print_r($e, true));
            }
        }
        
        // Cleanup is made then new account is created
        self::cleanupOldXMPPAccounts();
    }

    /**
     * Changes password for operator
     *
     * @param array $params            
     *
     * @throws Exception
     */
    public static function changeOperatorPassword($params = array())
    {
        $data = array(
            "user" => $params['xmpp_account']->username_plain,
            "host" => $params['xmpp_host'],
            "password" => $params['xmpp_account']->password
        );
        
        try {
            
            if ($params['handler'] == 'rpc') {
            
                $rpc = new \GameNet\Jabber\RpcClient([
                    'server' => $params['rpc_server'],
                    'host' => $params['xmpp_host']
                ]);
            
                $rpc->changePassword($data['user'],  $data['password']);
         
            } else {     
                       
                $response = self::sendRequest($params['node_api_server'] . '/xmpp-change-password', $data);
                         
                if ($response['error'] == true) {
                    throw new Exception($response['msg']);
                }
            }
        } catch (Exception $e) {
            if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                erLhcoreClassLog::write(print_r($e, true));
            }
            
            throw new Exception('Could not change operator password in XMPP server!');
        }
    }

    /**
     * registers operator and assigns to shared operators roaster
     * 
     * @param array $params
     */
    public static function registerOperator($params = array())
    {
        try {                        
            if ($params['handler'] == 'rpc') {
                
                $rpc = new \GameNet\Jabber\RpcClient([
                    'server' => $params['rpc_server'],
                    'host' => $params['xmpp_host']
                ]);
                
                $rpc->createUser($params['xmpp_account']->username_plain,  $params['xmpp_account']->password);
                
            } else {
                
                $data = array(
                    "user" => $params['xmpp_account']->username_plain,
                    "host" => $params['xmpp_host'],
                    "password" => $params['xmpp_account']->password
                );
                
                $response = self::sendRequest($params['node_api_server'] . '/xmpp-register', $data);
                
                if ($response['error'] == true) {
                    throw new Exception($response['msg']);
                }
            }
            
        } catch (Exception $e) {
            if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                erLhcoreClassLog::write(print_r($e, true));
            }
            
            throw new Exception('Could not register operator in XMPP server!');
        }
        
        // Append automated hosting subdomain if required
        $subdomainUser = erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['subdomain'];
        if ($subdomainUser != '') {
            $subdomainUser = '.' . $subdomainUser;
        }
        
        try {
            
            if ($params['handler'] == 'rpc') {

                $rpc = new \GameNet\Jabber\RpcClient([
                    'server' => $params['rpc_server'],
                    'host' => $params['xmpp_host']
                ]);
                
                $rpc->addUserToSharedRosterGroup(
                    $params['xmpp_account']->username_plain,  
                    'operators' . $subdomainUser
                );                
                
            } else {
                
                // Assign user to operators roaster
                $data = array(
                    "user" => $params['xmpp_account']->username_plain,
                    "host" => $params['xmpp_host'],
                    "group" => 'operators' . $subdomainUser,
                    "grouphost" => $params['xmpp_host']
                );
                
                $response = self::sendRequest($params['node_api_server'] . '/xmpp-assign-user-to-roaster', $data);
                
                if ($response['error'] == true) {
                    throw new Exception($response['msg']);
                }
            }
            
        } catch (Exception $e) {
            if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                erLhcoreClassLog::write(print_r($e, true));
            }
            
            throw new Exception('Could not assign operator to operators shared roaster!');
        }
    }

    /**
     * Is executed then new online visitor account is created
     *
     * @todo test
     *
     * @param array $params            
     */
    public static function newOnlineVisitor($params = array())
    {
        $xmppAccount = $params['xmpp_account'];
        
        $userParts = explode('@', $xmppAccount->username);
        
        $paramsOnlineUser = self::getNickAndStatusByOnlineVisitor($params['ou']);
        
        // Append automated hosting subdomain if required
        $subdomainUser = erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['subdomain'];
        
        if ($subdomainUser != '') {
            $subdomainUser = '.' . $subdomainUser;
        }
        
        if ($params['handler'] == 'rpc') {
            
            try {
                
                $rpc = new \GameNet\Jabber\RpcClient([
                    'server' => $params['rpc_server'],
                    'host' => $params['xmpp_host']
                ]);
                
                $rpc->createUser($userParts[0],  $xmppAccount->password);
                
                $rpc->addUserToSharedRosterGroup(
                    $userParts[0],
                    'visitors' . $subdomainUser
                );
                          
            } catch (Exception $e) {
                if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                    erLhcoreClassLog::write(print_r($e, true));
                }
            }
            
        } else {
            $data = array(
                "user" => $userParts[0],
                "host" => $params['xmpp_host'],
                "password" => $xmppAccount->password,
                "hostlogin" => $params['host_login'],
                "nick" => $paramsOnlineUser['nick'],
                "status" => $paramsOnlineUser['status'],
                "group" => 'visitors' . $subdomainUser
            );
            
            try {
                $response = self::sendRequest($params['node_api_server'] . '/xmpp-register-online-visitor', $data);
                
                if ($response['error'] == true) {
                    throw new Exception($response['msg']);
                }
            } catch (Exception $e) {
                if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                    erLhcoreClassLog::write(print_r($e, true));
                }
            }
        }
        
        // Cleanup is made then new account is created
        self::cleanupOldXMPPAccounts();
    }
    
    /**
     * Checks that instance of shared roaster existed
     * 
     */
    public static function checkSharedRoasters($params)
    {
        // Delete visitors shared roaster
        $data[] = array(
            "group" => "visitors.".$params['subdomain'],
            "host" => $params['xmpp_host'],
        );
        
        $data[] = array(
            "group" => "operators.".$params['subdomain'],
            "host" => $params['xmpp_host'],
        );
        
        // First register visitors shared roaster
        $dataRegister["visitors.".$params['subdomain']] = array(
            "group" => "visitors.".$params['subdomain'],
            "host" => $params['xmpp_host'],
            "name" => "Visitors",
            "desc" => "Visitors",
            "display" => '\"\"',
            "display_array" => []
        );
        
        // Register operators shared roaster
        $dataRegister["operators.".$params['subdomain']] = array(
            "group" => "operators.".$params['subdomain'],
            "host" => $params['xmpp_host'],
            "name" => "Operators",
            "desc" => "Operators",
            "display" => '\"operators.'.$params['subdomain'].'\\\nvisitors.'.$params['subdomain'].'\"',
            "display_array" => ['operators.'.$params['subdomain'],'visitors.'.$params['subdomain']]
        );
        
        if ($params['handler'] == 'rpc') {
            
            try {
                $rpc = new \GameNet\Jabber\RpcClient([
                    'server' => $params['rpc_server'],
                    'host' => $params['xmpp_host']
                ]);
                
                foreach ($data as $groupData) {                
                    $infoSharedRoaster = $rpc->getInfoSharedRosterGroup($groupData['group']);
                    
                    if (empty($infoSharedRoaster)) {
                        $rpc->createSharedRosterGroup(
                            $dataRegister[$groupData['group']]['group'], 
                            $dataRegister[$groupData['group']]['name'], 
                            $dataRegister[$groupData['group']]['desc'],
                            $dataRegister[$groupData['group']]['display_array']);                                        
                    }
                }
                
            } catch (Exception $e) {
                if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                    erLhcoreClassLog::write(print_r($e, true));
                }
                        
                throw $e;
            }
            
        } else {
            // Iterates through groups and checks des it exists, if not creates
            foreach ($data as $groupData)
            {            
                try {
                    $response = self::sendRequest($params['node_api_server'] . '/xmpp-does-shared-roaster-exists', $groupData);
            
                    if ($response['error'] == true) {                    
                         try {
                            $response = self::sendRequest($params['node_api_server'] . '/xmpp-setup-instance-roasters', $dataRegister[$groupData['group']]);
                        
                            if ($response['error'] == true) {
                                throw new Exception($response['msg']);
                            }
                            
                        } catch (Exception $e) {
                            throw $e;
                        }                    
                    }                
                    
                } catch (Exception $e) {
                    if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                        erLhcoreClassLog::write(print_r($e, true));
                    }
                            
                    throw $e;
                }
            }
        }
    }
    
    
    /**
     * Get's called then instance is destroyed
     * 
     * */
    public static function instanceDestroyed($params)
    {
        // Delete visitors shared roaster
        $data[] = array(
            "group" => "visitors.".$params['subdomain'],
            "host" => $params['xmpp_host'],
        );
        
        $data[] = array(
            "group" => "operators.".$params['subdomain'],
            "host" => $params['xmpp_host'],
        );
        
        foreach ($data as $groupData)
        {
            if ($params['handler'] == 'rpc') {
                try {
                    $rpc = new \GameNet\Jabber\RpcClient([
                        'server' => $params['rpc_server'],
                        'host' => $params['xmpp_host']
                    ]);
    
                    foreach ($data as $groupData) {
                        $rpc->deleteSharedRosterGroup($groupData['group']);
                    }
                                        
                } catch (Exception $e) {  
                    
                    if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                        erLhcoreClassLog::write(print_r($e, true));
                    }
                    
                    // To terminate instance termination
                    throw $e;                
                }
                
            } else {
                try {
                    $response = self::sendRequest($params['node_api_server'] . '/xmpp-delete-instance-roasters', $groupData);
                
                    if ($response['error'] == true) {
                        throw new Exception($response['msg']);
                    }
                
                } catch (Exception $e) {
                    if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                        erLhcoreClassLog::write(print_r($e, true));
                    }
                    
                    // To terminate instance termination
                    throw $e;                
                }
            }
        }   
    }
        
    /**
     * Creates required shared roasters
     * 
     * @todo add support for RPC
     * 
     * */
    public static function registerInstanceRoasters($params) {
        
        // First register visitors shared roaster
        $data[] = array(
              "group" => "visitors.".$params['subdomain'],
              "host" => $params['xmpp_host'],
              "name" => "Visitors",
              "desc" => "Visitors",
              "display" => '\"\"'
        );
        
        // Register operators shared roaster
        $data[] = array(
            "group" => "operators.".$params['subdomain'],
            "host" => $params['xmpp_host'],
            "name" => "Operators",
            "desc" => "Operators",
            "display" => '\"operators.'.$params['subdomain'].'\\\nvisitors.'.$params['subdomain'].'\"'
        );
        
        foreach ($data as $groupData)
        {
            try {
                $response = self::sendRequest($params['node_api_server'] . '/xmpp-setup-instance-roasters', $groupData);
            
                if ($response['error'] == true) {
                    throw new Exception($response['msg']);
                }
                
            } catch (Exception $e) {
                if (erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['debug'] == true) {
                    erLhcoreClassLog::write(print_r($e, true));
                }
            }
        }
    }
    
    /**
     * @desc Executed in erLhcoreClassExtensionXmppserviceHandler::handleMessageFromOperator send message to user from operator.
     * if provided message is a command to operator is send command response
     *
     * @param erLhcoreClassModelChat $chat            
     *
     * @param erLhcoreClassModelXMPPAccount $xmppUser            
     *
     * @param string $body            
     *
     * @throws Exception
     */
    public static function sendMessageToChat(erLhcoreClassModelChat $chat, erLhcoreClassModelXMPPAccount $xmppUser, $body)
    {
        $db = ezcDbInstance::get();
        $db->beginTransaction();
        
        try {
            
            $user = $xmppUser->user;
            
            $messageUserId = $user->id;

            // Predefine
            $statusCommand = array (
                'processed' => false,
                'process_status' => '',
            );
            
            if (strpos(trim($body), '!') === 0) {
                
                $statusCommand = erLhcoreClassChatCommand::processCommand(array('no_ui_update' => true, 'msg' => $body, 'chat' => & $chat));
                if ($statusCommand['processed'] === true) {
                    $messageUserId = -1; // Message was processed set as internal message
                    $body = '[b]'.$userData->name_support.'[/b]: '.$body .' '. ($statusCommand['process_status'] != '' ? '|| '.$statusCommand['process_status'] : '');
                }               
            }
   
            $msg = new erLhcoreClassModelmsg();
            $msg->msg = $body;
            $msg->chat_id = $chat->id;
            $msg->user_id = $messageUserId;
            $msg->time = time();
            $msg->name_support = $user->name_support;
            
            if ($messageUserId > 0 && $chat->chat_locale != '' && $chat->chat_locale_to != '') {
                erLhcoreClassTranslate::translateChatMsgOperator($chat, $msg);
            }
            
            erLhcoreClassChat::getSession()->save($msg);
            
            // Set last message ID
            if ($chat->last_msg_id < $msg->id) {
                
                $userChange = '';
                
                // Assign operator if chat does not have one
                if ($chat->user_id == 0) {
                    $userChange = ',user_id = :user_id';
                }
                
                $stmt = $db->prepare("UPDATE lh_chat SET status = :status, user_status = :user_status, last_msg_id = :last_msg_id{$userChange} WHERE id = :id");
                $stmt->bindValue(':id', $chat->id, PDO::PARAM_INT);
                $stmt->bindValue(':last_msg_id', $msg->id, PDO::PARAM_INT);
                
                $changeStatus = false;
                
                if ($user->invisible_mode == 0) {
                    if ($chat->status == erLhcoreClassModelChat::STATUS_PENDING_CHAT) {
                        $chat->status = erLhcoreClassModelChat::STATUS_ACTIVE_CHAT;
                        $changeStatus = true;
                    }
                }
                
                if ($chat->user_status == erLhcoreClassModelChat::USER_STATUS_CLOSED_CHAT) {
                    $chat->user_status = erLhcoreClassModelChat::USER_STATUS_PENDING_REOPEN;
                    
                    if ( ($onlineuser = $chat->online_user) !== false) {
                        $onlineuser->reopen_chat = 1;
                        $onlineuser->saveThis();
                    }                    
                }
                
                $stmt->bindValue(':user_status', $chat->user_status, PDO::PARAM_INT);
                $stmt->bindValue(':status', $chat->status, PDO::PARAM_INT);
                
                if ($userChange != '') {
                    $stmt->bindValue(':user_id', $msg->user_id, PDO::PARAM_INT);
                }
                
                $stmt->execute();
            }
              
            // If chat status changes update statistic
            if ($changeStatus == true) {
                
                if ($chat->department !== false) {
                    erLhcoreClassChat::updateDepartmentStats($chat->department);
                }
                
                erLhcoreClassChat::updateActiveChats($chat->user_id);
            }
            
            $db->commit();
            
            // Inform operator about command status
            if ($statusCommand['processed'] == true) {
                $xmppService = erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice');
                $xmppService->sendMessageToOperatorAsUserByChat(array(
                    'xmpp_account_operator' => $xmppUser,
                    'chat' => $chat,
                    'msg' => '[[System Assistant]] ' . $statusCommand['process_status']
                ));
            }
            
            // For nodejs plugin
            erLhcoreClassChatEventDispatcher::getInstance()->dispatch('chat.desktop_client_admin_msg', array(
                'msg' => & $msg,
                'chat' => & $chat
            ));
            
            // For general listeners
            erLhcoreClassChatEventDispatcher::getInstance()->dispatch('chat.web_add_msg_admin', array(
                'msg' => & $msg,
                'chat' => & $chat
            ));

        } catch (Exception $e) {
            $db->rollback();
            throw $e;
        }
    }

    /**
     * handles messages from operator.
     * Workflow sounds like that
     * 1. Check that we can determine an operator
     * 2. Check to what messages was send, to chat or online visitor
     * 3. If message is send to online visitor check perphaps visitor has active chat
     * 4. If active chat found, send a message
     * 5. If active chat not found send message as proactive invitation
     *
     * @param array $params            
     *
     */
    public static function handleMessageFromOperator($params)
    {
        try {
            $parts = explode('.', $params['receiver']);
            
            if (isset($parts[1])) {
                
                $xmppUserLogin = $params['sender'] . '@' . $params['server'];
                
                $xmppUser = erLhcoreClassModelXMPPAccount::findOne(array(
                    'filter' => array(
                        'username' => $xmppUserLogin
                    )
                ));
                
                if ($xmppUser !== false) {
                    
                    // It's message to online visitor
                    if (isset($parts[2]) && $parts[2] == 'chat') {
                        
                        $chat = erLhcoreClassModelChat::fetch($parts[1]);
                        $user = $xmppUser->user;
                        
                        // Messages to chat is only send if chat is not accepted or sender is chat owner
                        if ($chat->user_id == $user->id || $chat->user_id == 0) {
                            self::sendMessageToChat($chat, $xmppUser, $params['body']);
                        } else {
                            $xmppService = erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice');
                            $xmppService->sendMessageToOperatorAsUserByChat(array(
                                'xmpp_account_operator' => $xmppUser,
                                'chat' => $chat,
                                'msg' => '[[System Assistant]] Chat was already accepted by [' . $chat->user . '], your messages are now ignored'
                            ));
                        }
                    } else {
                        
                        $visitorId = $parts[1];
                        $visitor = erLhcoreClassModelChatOnlineUser::fetch($visitorId);
                        
                        // We do not have any active chat
                        if ($visitor->chat === false || ! in_array($visitor->chat->status, array(
                            erLhcoreClassModelChat::STATUS_ACTIVE_CHAT,
                            erLhcoreClassModelChat::STATUS_PENDING_CHAT
                        ))) {
                            $visitor->operator_message = $params['body'];
                            $visitor->message_seen = 0;
                            $visitor->invitation_id = - 1;
                            $visitor->operator_user_id = $xmppUser->user_id;
                            $visitor->saveThis();
                            
                            // We have active chat
                        } else {
                            
                            self::sendMessageToChat($visitor->chat, $xmppUser, $params['body']);
                        }
                    }
                } else {
                    throw new Exception('Could not find a operator');
                }
            } else {
                throw new Exception('Could not extract visitor ID');
            }
        } catch (Exception $e) {
            throw $e;
        }
    }

    /**
     * Handlers requests like
     * 
     * May 08 23:02:11 [Warning] [default] [default] {"action":"ping","user":"remdex2@xmpp.livehelperchat.com/25304460891431118632139491"}
     * May 08 23:02:14 [Warning] [default] [default] {"action":"disconnect","user":"remdex2","server":"xmpp.livehelperchat.com"}
     * May 08 23:21:52 [Warning] [default] [default] {"action":"connect","user":"remdex2","server":"xmpp.livehelperchat.com"}
     */
    public static function handleOperatorPing($jsonContent)
    {
        $params = json_decode($jsonContent, true);
        
        // If ping just update last action
        if ($params['action'] == 'ping') {
            
            // Parse user parts
            $userParts = self::parseXMPPUser($params['user']);
            
            // Fetches user id by xmpp username
            $userId = self::getUserIDByXMPPUsername($userParts['xmppuser']);
            
            // Updates last activity
            if (is_numeric($userId)) {
                self::updateActivityByUserId($userId);
            } else {
                throw new Exception("Could not find LHC user by user - " . $userParts['xmppuser']);
            }
        } elseif ($params['action'] == 'disconnect' || $params['action'] == 'connect') {
            
            // Fetches user id by xmpp username
            $userId = self::getUserIDByXMPPUsername($params['user'] . '@' . $params['server']);
            
            // Updates last activity to zero
            if (is_numeric($userId)) {
                self::updateActivityByUserId($userId, $params['action'] == 'connect' ? time() : 0);
            } else {
                throw new Exception("Could not find LHC user by user - " . $params['user'] . '@' . $params['server']);
            }
        }
        
        return true;
    }
}
