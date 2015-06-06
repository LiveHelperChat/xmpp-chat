<?php

class erLhcoreClassXMPPServiceAccountValidator
{    
    public static function validateXMPPAccount(erLhcoreClassModelXMPPAccount & $xmppAccount)
    {
            $definition = array(
                'username' => new ezcInputFormDefinitionElement(
                    ezcInputFormDefinitionElement::OPTIONAL, 'unsafe_raw'
                ),
                'password' => new ezcInputFormDefinitionElement(
                    ezcInputFormDefinitionElement::OPTIONAL, 'unsafe_raw'
                ),
                'user_id' => new ezcInputFormDefinitionElement(
                    ezcInputFormDefinitionElement::OPTIONAL, 'int'
                ),
                'sendmessage' => new ezcInputFormDefinitionElement(
                    ezcInputFormDefinitionElement::OPTIONAL, 'boolean'
                )
            );
        
            $form = new ezcInputForm( INPUT_POST, $definition );
            $Errors = array();

            // Username is available only for new accounts
            if ($xmppAccount->id == 0) {
                if ( !$form->hasValidData( 'username' ) || $form->username == '' )
                {
                    $Errors[] =  erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/operatorvalidator','Please enter a username');
                } else {
                                      
                    if (strpos($form->username, 'visitor') === false) {
                        if (preg_match('/[^a-z_0-9]/i', $form->username)) {
                             $Errors[] = erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/operatorvalidator','Not allowed characters detected');
                        } elseif ($form->username != 'admin') {
                            $subdomain = erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['subdomain'];
                            $xmppAccount->username = $form->username . ($subdomain != '' ? '.'.$subdomain : '') . '@' . erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->settings['xmpp_host'];
                        } else {
                            $Errors[] = erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/operatorvalidator','Admin is reserved username and can not be used');
                        }
                    } else {
                        $Errors[] = erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/operatorvalidator','Username cannot start with visitor keyword');
                    }
                }
            }
                        
            if ($xmppAccount->id == 0) {
                if ( !$form->hasValidData( 'password' ) || $form->password == '' )
                {
                    $Errors[] =  erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/operatorvalidator','Please enter a password');
                } else {
                    $xmppAccount->password = $form->password;
                }
            } else {
                if ( $form->hasValidData( 'password' ) && $form->password != '' ) {
                    $xmppAccount->password = $form->password;
                }
            }

            if ( $form->hasValidData( 'user_id' ))
            {
                $xmppAccount->user_id = $form->user_id;
            } else {
                $Errors[] =  erTranslationClassLhTranslation::getInstance()->getTranslation('xmppservice/operatorvalidator','Please enter a user id');
            }

            if ( $form->hasValidData( 'sendmessage' ) && $form->sendmessage == true)
            {
                $xmppAccount->sendmessage = 1;
            } else {
                $xmppAccount->sendmessage = 0;
            }

            return $Errors;        
    }    
    
    /**
     * At the moment it just stores an account. In the future there will be a call to create an account in xmpp server using NodeJS extension
     * @param erLhcoreClassModelXMPPAccount $xmppAccount
     */
    public static function publishXMPPAccount(erLhcoreClassModelXMPPAccount & $xmppAccount)
    {
        if ($xmppAccount->id == 0) {
            erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->registerOperator($xmppAccount);  
        } elseif ($xmppAccount->password != '') {
            erLhcoreClassModule::getExtensionInstance('erLhcoreClassExtensionXmppservice')->changeOperatorPassword($xmppAccount);
        }
        
        // We do not store operators passwords because we do not need them
        if ($xmppAccount->type == erLhcoreClassModelXMPPAccount::USER_TYPE_OPERATOR) {
            $xmppAccount->password = '';
        }
        
        $xmppAccount->saveThis();        
    }
}


