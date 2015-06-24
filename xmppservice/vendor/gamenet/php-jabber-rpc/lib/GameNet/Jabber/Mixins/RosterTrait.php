<?php
/**
 * The MIT License
 *
 * Copyright (c) 2014, GameNet
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * PHP version 5.4
 *
 * @package GameNet\Jabber
 * @copyright 2014, GameNet
 * @author Vadim Sabirov <vadim.sabirov@syncopate.ru>
 * @license MIT http://opensource.org/licenses/MIT
 */
namespace GameNet\Jabber\Mixins;

/**
 * Class RosterTrait
 *
 * @package   GameNet\Jabber
 * @copyright Copyright (с) 2014, GameNet. All rights reserved.
 * @author    Vadim Sabirov <vadim.sabirov@syncopate.ru>
 * @version   1.0
 */
trait RosterTrait
{
    abstract protected function sendRequest($command, array $params);

    /**
     * Retrieve the roster for a given user. Returns a list of the contacts in a user roster. It also returns the state
     * of the contact subscription. Subscription can be either "none", "from", "to", "both". Pending can be "in", "out"
     * or "none".
     *
     * @param string $user
     *
     * @return array [['jid', 'nick', 'subscription', 'ask', 'group'], [], ...]
     */
    public function getRoster($user)
    {
        $response = $this->sendRequest(
            'get_roster',
            [
                'user' => $user,
                'host' => $this->host,
            ]
        );

        if (!isset($response['contacts']) || empty($response['contacts'])) {
            return [];
        }

        $rosterContacts = [];
        foreach ($response['contacts'] as $item) {
            $contact = [];
            foreach ($item['contact'] as $data) {
                foreach ($data as $key => $value) {
                    $contact[$key] = $value;
                }
            }
            $rosterContacts[] = $contact;
        }

        return $rosterContacts;
    }

    /**
     * Add an item to a user's roster (supports ODBC).
     *
     * Subs is the state of the roster item subscription. It can be either `both`, `to`, `from` or `none`. `None` means
     * that presence packets are not send between parties. `Both` means that presence packets are send in both direction.
     * `To` means that the user see the presence of the given nickname. `From` means that the nickname specified sees the
     * user presence.
     *
     * Do not forget that roster items should be kept symmetric: when adding a roster item for a user, you have to do
     * the symmetric roster item addition.
     *
     * @param string $user
     * @param string $contact
     * @param string $nickname
     * @param string $group
     * @param string $subs Available: none, from, to or both
     */
    public function addRosterItem($user, $contact, $nickname, $group = '', $subs = 'both')
    {
        $this->sendRequest(
            'add_rosteritem',
            [
                'localuser'   => $user,
                'localserver' => $this->host,
                'user'        => $contact,
                'server'      => $this->host,
                'nick'        => $nickname,
                'group'       => $group,
                'subs'        => $subs,
            ]
        );
    }

    /**
     * Delete an item from a user's roster (supports ODBC).
     *
     * Do not forget that roster items should be kept symmetric: when removing a roster item for a user, you have to do
     * the symmetric roster item removal. This mechanism bypass the standard roster approval addition mechanism and
     * should only be used for server administration or server integration purpose.
     *
     * @param string $localUser The roster owner.
     * @param string $localUser The roster owner.
     * @param string $user Roster user item which would be deleted.
     */
    public function deleteRosterItem($localUser, $user)
    {
        $this->sendRequest(
            'delete_rosteritem',
            [
                'localuser'   => $localUser,
                'localserver' => $this->host,
                'user'        => $user,
                'server'      => $this->host,
            ]
        );
    }
} 