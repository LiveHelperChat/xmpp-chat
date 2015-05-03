<?php 

$data = array(
    "jid" => "visitor4@xmpp.livehelperchat.com", 
    "pass" => "visitor4",
    "host" => "localhost",
    "domain" => false
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

echo $result;

$data = array(
    "jid" => "visitor1@xmpp.livehelperchat.com",
    "pass" => "visitor1",
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

echo $result;
?>