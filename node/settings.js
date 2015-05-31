var settings = {}

/**
 * Main address of LHC installation. At the moment not used, but will be used in automated hosting enviroment
 * */
settings.http_address_lhc = 'livehelperchat.com';

/**
 * To what address listen
 * */
settings.listen_address = '95.85.55.134';

/**
 * To what port to listen
 * */
settings.listen_port = 4567;

/**
 * Path to ejabberdctl
 * */
settings.ejabberdctl = '/opt/ejabberd-15.04/bin/ejabberdctl'

//************************** CHAT SETTINGS

/**
 * How long online visitor should be considered as online. Miliseconds
 * 
 * It's 5 minutes at the moment 5 * 60 * 1000
 * */
settings.online_timeout = 300000;

settings.debug = {};

/**
 * Enable debug output
 * */
settings.debug.output = true;

module.exports = settings;