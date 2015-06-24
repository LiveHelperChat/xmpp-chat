<?php

/**
 * Class RosterTest
 *
 * @package tests\GameNet\Jabber
 * @copyright Copyright (с) 2015, GameNet. All rights reserved.
 * @author Vadim Sabirov <vadim.sabirov@syncopate.ru>
 * @version 1.0
 */
class RosterTest extends  PHPUnit_Framework_TestCase
{
    private $mock;

    public function setUp()
    {
        $this->mock = $this->getMockBuilder('\GameNet\Jabber\RpcClient')
            ->disableOriginalConstructor()
            ->setMethods(['sendRequest'])
            ->getMock();
    }

    public function testGetRoster()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('get_roster'));

        $this->mock->getRoster('user');
    }

    public function testAddRosterItem()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('add_rosteritem'));

        $this->mock->addRosterItem('user', 'contact', 'nickname');
    }

    public function testRemoveRosterItem()
    {
        $this->mock->expects($this->once())
            ->method('sendRequest')
            ->with($this->equalTo('delete_rosteritem'));

        $this->mock->deleteRosterItem('user', 'contact');
    }
}
 