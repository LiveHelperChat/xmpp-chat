<?php

class erLhcoreClassModelXMPPAccount
{

    public function getState()
    {
        return array(
            'id' => $this->id,
            'username' => $this->username,
            'password' => $this->password,
            'user_id' => $this->user_id,
            'type' => $this->type,
            'ctime' => $this->ctime,
            'lactivity' => $this->lactivity,
            'sendmessage' => $this->sendmessage
        );
    }

    public function setState(array $properties)
    {
        foreach ($properties as $key => $val) {
            $this->$key = $val;
        }
    }

    public static function findOne($paramsSearch = array())
    {
        $paramsSearch['limit'] = 1;
        $list = self::getList($paramsSearch);
        if (! empty($list)) {
            reset($list);
            return current($list);
        }
        
        return false;
    }

    public static function fetch($id)
    {
        return erLhcoreClassExtensionXmppservice::getSession()->load('erLhcoreClassModelXMPPAccount', (int) $id);
    }

    public function __toString()
    {
        return $this->username;
    }

    public function removeThis()
    {        
        erLhcoreClassChatEventDispatcher::getInstance()->dispatch('xmppservice.removeaccount',array('account' => & $this));
        erLhcoreClassExtensionXmppservice::getSession()->delete($this);
    }
    
    function str_lreplace($search, $replace, $subject)
    {
        $pos = strrpos($subject, $search);
    
        if($pos !== false)
        {
            $subject = substr_replace($subject, $replace, $pos, strlen($search));
        }
    
        return $subject;
    }
    
    public function __get($var)
    {
        switch ($var) {
            
            case 'ctime_front':
                $this->ctime_front = date('Ymd') == date('Ymd', $this->ctime) ? date(erLhcoreClassModule::$dateHourFormat, $this->ctime) : date(erLhcoreClassModule::$dateDateHourFormat, $this->ctime);
                return $this->ctime_front;
                break;
            
            case 'lactivity_front':
                $this->lactivity_front = date('Ymd') == date('Ymd', $this->lactivity) ? date(erLhcoreClassModule::$dateHourFormat, $this->lactivity) : date(erLhcoreClassModule::$dateDateHourFormat, $this->lactivity);
                return $this->lactivity_front;
                break;
            
            case 'user':
                $this->user = false;
                if ($this->user_id > 0) {
                    try {
                        $this->user = erLhcoreClassModelUser::fetch($this->user_id, true);
                    } catch (Exception $e) {
                        $this->user = false;
                    }
                }
                return $this->user;
                break;
            
            case 'username_plain':
                list($this->username_plain) = explode('@', $this->username);                              
                return $this->username_plain;
                break;
            
            case 'username_plain_edit':
                list($this->username_plain_edit) = explode('@', $this->username); 
                $subdomain = erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['subdomain'];
                
                if ($subdomain != '') {
                    $this->username_plain_edit = $this->str_lreplace('.'.$subdomain, '', $this->username_plain_edit);
                }
                
                return $this->username_plain_edit;
                break;
            
            default:
                ;
                break;
        }
    }

    public function saveThis()
    {
        erLhcoreClassExtensionXmppservice::getSession()->saveOrUpdate($this);
    }

    public static function getCount($params = array())
    {
        $session = erLhcoreClassExtensionXmppservice::getSession();
        $q = $session->database->createSelectQuery();
        $q->select("COUNT(id)")->from("lhc_xmpp_service_account");
        
        $conditions = array();
        
        if (isset($params['filter']) && count($params['filter']) > 0) {
            foreach ($params['filter'] as $field => $fieldValue) {
                $conditions[] = $q->expr->eq($field, $q->bindValue($fieldValue));
            }
        }
        
        if (isset($params['filterlike']) && count($params['filterlike']) > 0) {
            foreach ($params['filterlike'] as $field => $fieldValue) {
                $conditions[] = $q->expr->like($field, $q->bindValue($fieldValue . '%'));
            }
        }
        
        if (isset($params['filterin']) && count($params['filterin']) > 0) {
            foreach ($params['filterin'] as $field => $fieldValue) {
                $conditions[] = $q->expr->in($field, $fieldValue);
            }
        }
        
        if (isset($params['filterlt']) && count($params['filterlt']) > 0) {
            foreach ($params['filterlt'] as $field => $fieldValue) {
                $conditions[] = $q->expr->lt($field, $q->bindValue($fieldValue));
            }
        }
        
        if (isset($params['filtergt']) && count($params['filtergt']) > 0) {
            foreach ($params['filtergt'] as $field => $fieldValue) {
                $conditions[] = $q->expr->gt($field, $q->bindValue($fieldValue));
            }
        }
        
        if (isset($params['filterlte']) && count($params['filterlte']) > 0) {
            foreach ($params['filterlte'] as $field => $fieldValue) {
                $conditions[] = $q->expr->lte($field, $q->bindValue($fieldValue));
            }
        }
        
        if (isset($params['filtergte']) && count($params['filtergte']) > 0) {
            foreach ($params['filtergte'] as $field => $fieldValue) {
                $conditions[] = $q->expr->gte($field, $q->bindValue($fieldValue));
            }
        }
        
        if (count($conditions) > 0) {
            $q->where($conditions);
        }
        
        $stmt = $q->prepare();
        $stmt->execute();
        $result = $stmt->fetchColumn();
        
        return $result;
    }

    public static function getList($paramsSearch = array())
    {
        $paramsDefault = array(
            'limit' => 32,
            'offset' => 0
        );
        
        $params = array_merge($paramsDefault, $paramsSearch);
        
        $session = erLhcoreClassExtensionXmppservice::getSession();
        $q = $session->createFindQuery('erLhcoreClassModelXMPPAccount');
        
        $conditions = array();
        
        if (isset($params['filter']) && count($params['filter']) > 0) {
            foreach ($params['filter'] as $field => $fieldValue) {
                $conditions[] = $q->expr->eq($field, $q->bindValue($fieldValue));
            }
        }
        
        if (isset($params['filterlike']) && count($params['filterlike']) > 0) {
            foreach ($params['filterlike'] as $field => $fieldValue) {
                $conditions[] = $q->expr->like($field, $q->bindValue($fieldValue . '%'));
            }
        }
        
        if (isset($params['filterin']) && count($params['filterin']) > 0) {
            foreach ($params['filterin'] as $field => $fieldValue) {
                $conditions[] = $q->expr->in($field, $fieldValue);
            }
        }
        
        if (isset($params['filterlt']) && count($params['filterlt']) > 0) {
            foreach ($params['filterlt'] as $field => $fieldValue) {
                $conditions[] = $q->expr->lt($field, $q->bindValue($fieldValue));
            }
        }
        
        if (isset($params['filtergt']) && count($params['filtergt']) > 0) {
            foreach ($params['filtergt'] as $field => $fieldValue) {
                $conditions[] = $q->expr->gt($field, $q->bindValue($fieldValue));
            }
        }
        
        if (isset($params['filterlte']) && count($params['filterlte']) > 0) {
            foreach ($params['filterlte'] as $field => $fieldValue) {
                $conditions[] = $q->expr->lte($field, $q->bindValue($fieldValue));
            }
        }
        
        if (isset($params['filtergte']) && count($params['filtergte']) > 0) {
            foreach ($params['filtergte'] as $field => $fieldValue) {
                $conditions[] = $q->expr->gte($field, $q->bindValue($fieldValue));
            }
        }
        
        if (count($conditions) > 0) {
            $q->where($conditions);
        }
        
        $q->limit($params['limit'], $params['offset']);
        
        $q->orderBy(isset($params['sort']) ? $params['sort'] : 'id DESC');
        
        $objects = $session->find($q);
        
        return $objects;
    }

    const USER_TYPE_OPERATOR = 0;

    const USER_TYPE_VISITOR = 1;

    const USER_TYPE_CHAT = 2;

    public $id = null;

    public $username = '';

    public $password = '';

    public $ctime = 0;
    
    public $lactivity = 0;

    public $user_id = 0;
    
    // Send message to this operator if chat request comes to this
    public $sendmessage = 0;

    public $type = self::USER_TYPE_OPERATOR;
}

?>