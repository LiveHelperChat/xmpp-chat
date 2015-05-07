# xmpp-chat
This is repository for future full XMPP chat support. At the moment it's just prototyping and playing around

## Requirements
 * https://www.ejabberd.im/ with mod_admin_extra enabled 15.04v
 * https://nodejs.org/
 * https://livehelperchat.com/
 * Advanced linux skills. This stuff is not for amateurs!!!

## Workflow which should be implemented over time

### LHC extension requirements
 * Option to create an XMPP users and assign operators to them. I do not want to map them directly to accounts and have tight integration. Perhaps it will be enough just to provide XMPP username in standalone enviroment and users themself will be created using ejabberd admin interface. Food for thoughts at the moment.
 * It should cleanup expired users from ejabberd virtual hosts.
 * Support for automated hosting... 

### Visitor comes to site (node server takes care of this API)
 * LHC extension checks does passed online user has assigned XMPP user
 * If user does not exists executes test-online-visitor-workfllow.php workflow
 * If user exists executes test.php workflow
 * Once hard timeout is reached in nodeserver it executes REST callback to LHC extension informing that user session has timed out.
 * Then we can just delete user from shared roaster or from xmpp itself and update online visitor record. [Not decided yet what's the best way to do it]
 * Visitor status should be appeneded with online information. It should come from request to NodeJS server.
 
### Operator sends a message to online visitor. [mod_lhc]
 * Operator writes message to user using XMPP client.
 * Intercept message at ejabberd lhc extension and there execute callback to lhc module.
 * Mod should send messages only if sender is an operator.
 * If we find active chat [based on assigned chat_id to online visitor] we append message to chat if not we write this as invitation to chat to online visitor.
 
### Visitor starts a chat [node]
 * We execute API call to node api server, with logins from online visitor. If logins does not exists, we create an account for chat user.
 * Message to be send based on department setting. To any random logged operator using XMPP. First check that online visitor has assigned sender and message was send using XMPP.
 * Basically we need intelligence operator selection based on department and online operators in XMPP node.
 * We can track operators status in XMPP based on lhc ejabberd extension callbacks execution [Implemented]
 
### Sends a message to operator from visitor (during the chat)
 * Connect as user and send message to assigned operator if it exists. It has to exists based on above requirement.
 * Once we do this, we just send http request to nodejs api or directly from PHP and send a message to ejabberd. Basically we login as visitor and send message to assigned operator xmpp user.
 
### Send a message from operator to visitor (during the chat)[mod_lhc]
 * We intercept a message and send post request to lhc module, which based on online visitor id, determines a chat and publishes a message to chat.
 * Messages from visitors to operators should be ignored and not send to LHC callbacks
 * We may need to take care of NodeJS stuff here using Redis publish notifications
 
### Operators status monitoring [mod_lhcping.erl,mod_lhc.erl]
 * This ejabberd module monitors statuses and sends pings to provided URL in JSON content
 * Callback is send also then operator logins
 * Callback is also send then operator logouts
 * Pings to online visitors should be ignored and pings only for operators should be send
 
### Autoamated hosting workflow
 * At the moment top secret :D