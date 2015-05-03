<?php 

echo "Registering user visitor7\n";

$data = array(
    "user" => "visitor7",
    "host" => "xmpp.livehelperchat.com",
    "password" => "visitor7",
    "hostlogin" => "localhost"
);

$data_string = json_encode($data);

$ch = curl_init('http://localhost:4567/xmpp-register-online-visitor');
curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "POST");
curl_setopt($ch, CURLOPT_POSTFIELDS, $data_string);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_HTTPHEADER, array(
    'Content-Type: application/json',
    'Content-Length: ' . strlen($data_string))
);

$result = curl_exec($ch);

echo $result;