<?php 
/**
 * Example how to remove user from shared roaster
 * */
$data = array(
    "host" => "xmpp.livehelperchat.com",
    "group" => "visitors.test1",
    "secret_key" => "<change_me>",
);

$data_string = json_encode($data);

$ch = curl_init('http://127.0.0.1:4567/xmpp-does-shared-roaster-exists');
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