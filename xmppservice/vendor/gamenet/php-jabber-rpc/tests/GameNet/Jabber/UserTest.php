<?php

/**
 * Class UserTest
 *
 * @package tests\GameNet\Jabber
 * @copyright Copyright (с) 2015, GameNet. All rights reserved.
 * @author Vadim Sabirov <vadim.sabirov@syncopate.ru>
 * @version 1.0
 */
class UserTest extends  PHPUnit_Framework_TestCase
{
    private $mock;

    public function setUp()
    {
        $this->mock = $this->getMockBuilder('\GameNet\Jabber\RpcClient')
            ->disableOriginalConstructor()
            ->setMethods(['sendRequest'])
            ->getMock();
    }

    public function testCreateUser()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('register'));

        $this->mock->createUser('user', 'password');
    }

    public function testCheckAccount()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('check_account'));

        $this->mock->checkAccount('user');
    }

    public function testChangePassword()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('change_password'));

        $this->mock->changePassword('user', 'password');
    }

    public function testSetNickname()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('set_nickname'));

        $this->mock->setNickname('user', 'nickname');
    }

    public function testLetLastActivity()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('get_last'));

        $this->mock->getLastActivity('user');
    }

    public function testSendMessageChat()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('send_message_chat'));

        $this->mock->sendMessageChat('from', 'to', 'body');
    }

    public function testUnregisterUser()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('unregister'));

        $this->mock->unregisterUser('user');
    }

    public function testSetStatus()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest');

        $this->mock->setStatus('user', 'show', 'status', 'priority');
    }

    public function testUserSessionsInfo()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('user_sessions_info'));

        $this->mock->userSessionsInfo('user');
    }

    public function testGetVCard()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('get_vcard'));

        $this->mock->getVCard('user', 'name');
    }

    public function testGetVCardExtra()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('get_vcard2'));

        $this->mock->getVCard('user', 'extra name');
    }

    public function testSetVCard()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('set_vcard'));

        $this->mock->setVCard('user', 'name', 'value');
    }

    public function testSetVCardExtra()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('set_vcard2'));

        $this->mock->setVCard('user', 'extra name', 'value');
    }

    public function testBanAccount()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('ban_account'));

        $this->mock->banAccount('user', 'reason');
    }

    public function testSetGroupForUserRoster()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest');

        $this->mock->setGroupForUserRoster('user', 'contact', ['group']);
    }

    public function testSendStanza()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest');

        $this->mock->sendStanzaC2S('user', 'stanza');
    }

    public function testAddUserToGroup()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest');

        $this->mock->addUserToGroup('user', 'contact', 'group');
    }

    public function testDeleteUserFromGroup()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest');

        $this->mock->deleteUserFromGroup('user', 'contact', 'group');
    }
}
 