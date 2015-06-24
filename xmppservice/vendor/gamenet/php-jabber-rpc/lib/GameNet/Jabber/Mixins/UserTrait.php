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
 * Class UserTrait
 *
 * @category  GGS
 * @package   GameNet\Jabber
 * @copyright Copyright (с) 2014, GameNet. All rights reserved.
 * @author    Vadim Sabirov <vadim.sabirov@syncopate.ru>
 * @version   1.0
 */
trait UserTrait
{
    abstract protected function sendRequest($command, array $params);

    /**
     * Create an ejabberd user account.
     *
     * @param string $user
     * @param string $password
     *
     * @throws \RuntimeException
     */
    public function createUser($user, $password)
    {
        $response = $this->sendRequest(
            'register',
            [
                'host'     => $this->host,
                'user'     => $user,
                'password' => $password
            ]
        );

        if ($response['res'] != 0) {
            throw new \RuntimeException('Unable create user');
        }
    }

    /**
     * Check if ejabberd user account already exists.
     *
     * @param string $user
     *
     * @return bool
     */
    public function checkAccount($user)
    {
        $response = $this->sendRequest(
            'check_account',
            [
                'user' => $user,
                'host' => $this->host
            ]
        );

        return $response['res'] == 0;
    }

    /**
     * Change the password on behalf of the given user.
     *
     * @param string $user
     * @param string $password
     */
    public function changePassword($user, $password)
    {
        $this->sendRequest(
            'change_password',
            [
                'host'    => $this->host,
                'user'    => $user,
                'newpass' => $password
            ]
        );
    }

    /**
     * Define user nickname.
     *
     * The nickname is set/updated in the user Vcard. Other informations are unchanged.
     *
     * @param string $user
     * @param string $nickname
     */
    public function setNickname($user, $nickname)
    {
        $this->sendRequest(
            'set_nickname',
            [
                'host'     => $this->host,
                'user'     => $user,
                'nickname' => $nickname
            ]
        );
    }

    /**
     * Get last activity information
     *
     * Timestamp is the seconds since 1970-01-01 00:00:00 UTC, for example: date +%s
     *
     * @param string $user
     *
     * @return string
     */
    public function getLastActivity($user)
    {
        $response = $this->sendRequest(
            'get_last',
            [
                'host' => $this->host,
                'user' => $user,
            ]
        );

        return $response['last_activity'];
    }

    /**
     * Send a chat message to a local or remote bare of full JID
     *
     * @param string $fromJid
     * @param string $toJid
     * @param string $message
     */
    public function sendMessageChat($fromJid, $toJid, $message)
    {
        $this->sendRequest(
            'send_message_chat',
            [
                'from' => $fromJid,
                'to'   => $toJid,
                'body' => $message
            ]
        );
    }

    /**
     * Unregister an ejabberd user account. This mechanism should only be used for server administration or server
     * integration purpose.
     *
     * @param string $user
     */
    public function unregisterUser($user)
    {
        $this->sendRequest(
            'unregister',
            [
                'host' => $this->host,
                'user' => $user,
            ]
        );
    }

    /**
     * Set user status with message
     *
     * @param string $user
     * @param string $show Valid values are: away, chat, dnd, xa
     * @param string $status Text message
     * @param int $priority The value MUST be an integer between -128 and +127
     */
    public function setStatus($user, $show, $status, $priority)
    {
        $priority = (string) $priority;
        $stanza = "
            <presence>
                <show>$show</show>
                <status>$status</status>
                <priority>$priority</priority>
            </presence>";

        $this->sendStanzaC2S($user, $stanza);
    }

    /**
     * Get information about all sessions of a user
     *
     * @param string $user
     *
     * @return array [['connection', 'ip', 'port', 'priority', 'node', 'uptime', 'status', 'resource', 'statustext'], [], ...]
     */
    public function userSessionsInfo($user)
    {
        $response = $this->sendRequest(
            'user_sessions_info',
            [
                'host' => $this->host,
                'user' => $user,
            ]
        );

        if (!isset($response['sessions_info']) || empty($response['sessions_info'])) {
            return [];
        }

        $sessions = [];
        foreach ($response['sessions_info'] as $info) {
            $session = [];
            foreach ($info['session'] as $data) {
                foreach ($data as $key => $value) {
                    $session[$key] = $value;
                }
            }
            $sessions[] = $session;
        }

        return $sessions;
    }

    /**
     * Get content from a vCard field
     *
     * @param string $user
     * @param string $name
     *
     * @return string
     */
    public function getVCard($user, $name)
    {
        if (strstr($name, ' ')) {
            $command = 'get_vcard2';
            list($name, $subname) = explode(' ', $name);

            $params = [
                'host' => $this->host,
                'user' => $user,
                'name' => $name,
                'subname' => $subname,
            ];
        } else {
            $command = 'get_vcard';
            $params = [
                'host' => $this->host,
                'user' => $user,
                'name' => $name,
            ];
        }

        try {
            $response = $this->sendRequest($command, $params);
        } catch (\RuntimeException $e) {
            return '';
        }

        return $response['content'];
    }

    /**
     * Set content in a vCard field
     *
     * @param string $user
     * @param string $name
     * @param string $value
     */
    public function setVCard($user, $name, $value)
    {
        if (strstr($name, ' ')) {
            $command = 'set_vcard2';
            list($name, $subname) = explode(' ', $name);

            $params = [
                'host' => $this->host,
                'user' => $user,
                'name' => $name,
                'subname' => $subname,
                'content' => $value,
            ];
        } else {
            $command = 'set_vcard';
            $params = [
                'host' => $this->host,
                'user' => $user,
                'name' => $name,
                'content' => $value,
            ];
        }

        $this->sendRequest($command, $params);
    }

    /**
     * Ban an account: kick sessions and set random password
     *
     * @param string $user
     * @param string $reason
     */
    public function banAccount($user, $reason)
    {
        $this->sendRequest(
            'ban_account',
            [
                'host' => $this->host,
                'user' => $user,
                'reason' => $reason,
            ]
        );
    }

    /**
     * Send a stanza as if sent from a c2s session
     *
     * @param string $user
     * @param string $stanza XML string
     */
    public function sendStanzaC2S($user, $stanza)
    {
        $sessions = $this->userSessionsInfo($user);
        foreach ($sessions as $session) {
            $stanza = str_replace('{resource}', $session['resource'], $stanza);
            $this->sendRequest(
                'send_stanza_c2s',
                [
                    'host' => $this->host,
                    'user' => $user,
                    'resource' => $session['resource'],
                    'stanza' => $stanza,
                ]
            );
        }
    }

    /**
     * Set groups for contact in roster user owner
     *
     * @deprecated
     * @param string $user
     * @param string $contact
     * @param array $groups
     */
    public function setGroupForUserRoster($user, $contact, array $groups)
    {
        $id = uniqid();
        $userJid = "$user@$this->host";
        $contactJid = "$contact@$this->host";
        $groupList = '';
        foreach ($groups as $group) {
            $groupList .= "<group>$group</group>";
        }

        $stanza = "
            <iq from=\"$userJid/{resource}\" type=\"set\" id=\"$id\">
                <query xmlns=\"jabber:iq:roster\">
                    <item jid=\"$contactJid\">$groupList</item>
                </query>
            </iq>";

        $this->sendStanzaC2S($user, $stanza);
    }

    /**
     * Add jid to a group in a user's roster (supports ODBC)
     *
     * WARNING!
     * This method uses commands that are not available in the basic version eJabberd
     * These commands are available in the branch version https://github.com/gamenet/ejabberd
     * See request to add features - https://github.com/gamenet/ejabberd/pull/11
     *
     * @param $user
     * @param $contact
     * @param $group
     */
    public function addUserToGroup($user, $contact, $group)
    {
        $this->sendRequest(
            'add_jid_to_group',
            [
                'localserver' => $this->host,
                'localuser'   => $user,
                'jid'         => "$contact@$this->host",
                'group'       => $group,
            ]
        );
    }

    /**
     * Delete a jid from a user's roster group (supports ODBC)
     *
     * WARNING!
     * This method uses commands that are not available in the basic version eJabberd
     * These commands are available in the branch version https://github.com/gamenet/ejabberd
     * See request to add features - https://github.com/gamenet/ejabberd/pull/11
     *
     *
     * @param $user
     * @param $contact
     * @param $group
     */
    public function deleteUserFromGroup($user, $contact, $group)
    {
        $this->sendRequest(
            'delete_jid_from_group',
            [
                'localserver'  => $this->host,
                'localuser'    => $user,
                'jid'          => "$contact@$this->host",
                'group'        => $group,
                'deleteroster' => "false",
            ]
        );
    }
}