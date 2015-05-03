<?php 
/**
 * Test's workflow which will be used from LHC
 * 
 * 1. Register visitor
 * 2. Assign visitor to rouster group
 * 3. Make online visitor as online
 * 
 * */
echo "Registering user visitor5\n";
    
$data = array(
    "user" => "visitor5",
    "host" => "xmpp.livehelperchat.com",
    "password" => "visitor5"
);

$data_string = json_encode($data);

$ch = curl_init('http://localhost:4567/xmpp-register');
curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "POST");
curl_setopt($ch, CURLOPT_POSTFIELDS, $data_string);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_HTTPHEADER, array(
    'Content-Type: application/json',
    'Content-Length: ' . strlen($data_string))
);

$result = curl_exec($ch);

echo $result,"\n";

echo "Assigning user to visitors roaster\n";

$data = array(
    "user" => "visitor5",
    "host" => "xmpp.livehelperchat.com",
    "group" => "visitors",
    "grouphost" => "xmpp.livehelperchat.com",
);

$data_string = json_encode($data);

$ch = curl_init('http://localhost:4567/xmpp-assign-user-to-roaster');
curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "POST");
curl_setopt($ch, CURLOPT_POSTFIELDS, $data_string);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_HTTPHEADER, array(
    'Content-Type: application/json',
    'Content-Length: ' . strlen($data_string))
);

$result = curl_exec($ch);

echo $result,"\n";

echo "Making user as online\n";

$data = array(
    "jid" => "visitor5@xmpp.livehelperchat.com",
    "pass" => "visitor5",
    "host" => "localhost",
    "domain" => "test1"

);

$data_string = json_encode($data);

$ch = curl_init('http://localhost:4567/xmpp');
curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "POST");
curl_setopt($ch, CURLOPT_POSTFIELDS, $data_string);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_HTTPHEADER, array(
    'Content-Type: application/json',
    'Content-Length: ' . strlen($data_string))
);

$result = curl_exec($ch);


?>