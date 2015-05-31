CREATE TABLE `lhc_xmpp_service_account` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `username` varchar(250) NOT NULL,
  `password` varchar(250) NOT NULL,
  `user_id` bigint(20) NOT NULL,
  `type` int(11) NOT NULL,
  `ctime` int(11) NOT NULL,
  `lactivity` int(11) NOT NULL,
  `sendmessage` int(11) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `username` (`username`),
  KEY `user_id_type` (`user_id`,`type`),
  KEY `lactivity_type` (`lactivity`,`type`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;