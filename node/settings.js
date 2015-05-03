var settings = {}

/**
 * Main address of LHC installation
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
 * How long online visitor should be considered as online. Miliseconds
 * */
settings.online_timeout = 5000;

/**
 * After how many seconds after disconnect we should inform LHC that visitor is offline
 * */
settings.online_timeout_destroy = 5000;

settings.debug = {};

/**
 * Enable debug output
 * */
settings.debug.output = true;

module.exports = settings;