# xmpp-chat
This is repository for future full XMPP chat support. At the moment it's just prototyping and playing around.

## Workflow which should be implemented over time

### Visitor comes to site (node server takes care of this API)
 * LHC extension checks does passed online user has assigned XMPP user
 * If user does not exists executes test-online-visitor-workfllow.php workflow
 * If user exists executes test.php workflow
 * Once hard timeout is reached in nodeserver it executes REST callback to LHC extension informing that user session has timed out.
 * Then we can just delete user from shared roaster or from xmpp itself and update online visitor record. [Not decided yet what's the best way to do it]
 
### Operator sends a message to online visitor (need ejabberd mod here)
 * Operator writes message to user using XMPP client.
 * Two options here. Either receive message in node server and execute callback back to LHC extension to store operatror message
 * OR
 * Intercept message at ejabberd lhc extension and there execute callback to lhc module. Perhaps there is better place. Like it's more direct approach.
 
### Visitor starts a chat
 * We execute API call to node api server, with logins from online visitor. If logins does not exists, we create an account for chat user.
 * Message to be send based on department setting. To any random logged operator using XMPP. First check that online visitor has assigned sender and message was send using XMPP.
 * Basically we need intelligence opertor selection based on department and online operators in XMPP node.
 * We can track operators status in XMPP based on lhc ejabberd extension callbacks execution
 