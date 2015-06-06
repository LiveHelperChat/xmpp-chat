<?php 

/**
 * Register shared roasters
* */
$data = array(
    "group" => "visitors.testing",
    "host" => "xmpp.livehelperchat.com",
    "name" => "Visitors",
    "desc" => "Visitors",
    "display" => '\"\"'
);

$data_string = json_encode($data);

$ch = curl_init('http://localhost:4567/xmpp-setup-instance-roasters');
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
    "group" => "operators.testing",
    "host" => "xmpp.livehelperchat.com",
    "name" => "Operators",
    "desc" => "Operators",
    "display" => '\"operatos\\\nvisitors\"'
);

$data_string = json_encode($data);

$ch = curl_init('http://localhost:4567/xmpp-setup-instance-roasters');
curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "POST");
curl_setopt($ch, CURLOPT_POSTFIELDS, $data_string);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_HTTPHEADER, array(
    'Content-Type: application/json',
    'Content-Length: ' . strlen($data_string))
);

$result = curl_exec($ch);

echo $result;

// Removent of shared roaster
$data = array(
    "group" => "visitors.testing",
    "host" => "xmpp.livehelperchat.com",
);

$data_string = json_encode($data);

$ch = curl_init('http://localhost:4567/xmpp-delete-instance-roasters');
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