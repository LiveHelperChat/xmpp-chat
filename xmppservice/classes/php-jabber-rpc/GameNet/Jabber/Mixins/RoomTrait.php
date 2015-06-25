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
 * Class RoomTrait
 *
 * @package   GameNet\Jabber
 * @copyright Copyright (с) 2014, GameNet. All rights reserved.
 * @author    Vadim Sabirov <vadim.sabirov@syncopate.ru>
 * @version   1.0
 */
trait RoomTrait
{
    abstract protected function sendRequest($command, array $params);

    /**
     * Create a MUC room name@service in host.
     *
     * @param string $name
     */
    public function createRoom($name)
    {
        $this->sendRequest(
            'create_room',
            [
                'name'    => $name,
                'service' => 'conference.' . $this->host,
                'host'    => $this->host,
            ]
        );
    }

    /**
     * Send a direct invitation to several destinations.
     *
     * Password and Message can also be: none. Users JIDs are separated with `:`
     *
     * @param string $name The room name.
     * @param string $password The room password.
     * @param string $reason The invitation text which be sent to users.
     * @param array  $users The users JIDs invited to room.
     */
    public function inviteToRoom($name, $password, $reason, array $users)
    {
        $this->sendRequest(
            'send_direct_invitation',
            [
                'room'     => $name . '@conference.' . $this->host,
                'password' => $password,
                'reason'   => $reason,
                'users'    => join(':', $users),
            ]
        );
    }

    /**
     * Delete a MUC room.
     *
     * @param string $name The room name.
     */
    public function deleteRoom($name)
    {
        $this->sendRequest(
            'destroy_room',
            [
                'name'    => $name,
                'service' => 'conference.' . $this->host,
                'host'    => $this->host,
            ]
        );
    }

    /**
     * List existing rooms.
     *
     * @return array Return array like ['room1@conference.j.test.dev', 'room2@conference.j.test.dev', ...]
     */
    public function getOnlineRooms()
    {
        $rooms = $this->sendRequest(
            'muc_online_rooms',
            ['host' => $this->host]
        );

        if (!isset($rooms['rooms']) || empty($rooms['rooms'])) {
            return [];
        }

        $roomList = [];
        foreach ($rooms['rooms'] as $item) {
            $roomList[] = $item['room'];
        }

        return $roomList;
    }

    /**
     * Change an option in a MUC room.
     *
     * @param string $name The room name
     * @param string $option Valid values:
     *                       title (string)
     *                       password (string)
     *                       password_protected (bool)
     *                       anonymous (bool)
     *                       max_users (int)
     *                       allow_change_subj (bool)
     *                       allow_query_users (bool)
     *                       allow_private_messages (bool)
     *                       public (bool)
     *                       public_list (bool)
     *                       persistent (bool)
     *                       moderated (bool)
     *                       members_by_default (bool)
     *                       members_only (bool)
     *                       allow_user_invites (bool)
     *                       logging (bool)
     * @param string $value
     */
    public function setRoomOption($name, $option, $value)
    {
        $value = !is_bool($value) ? $value : ($value ? 'true' : 'false');

        $this->sendRequest(
            'change_room_option',
            [
                'name'    => $name,
                'service' => 'conference.' . $this->host,
                'option'  => $option,
                'value'   => (string) $value,
            ]
        );
    }

    /**
     * Change an affiliation in a MUC room.
     *
     * @param string $name The room name.
     * @param string $userJid
     * @param string $affiliation Valid values: outcast, none, member, admin, owner
     *                            If the affiliation is 'none', the action is to remove
     */
    public function setRoomAffiliation($name, $userJid, $affiliation)
    {
        $this->sendRequest(
            'set_room_affiliation',
            [
                'name'        => $name,
                'service'     => 'conference.' . $this->host,
                'jid'         => $userJid,
                'affiliation' => $affiliation,
            ]
        );
    }
}