-define(TIMEOUT, 5000).

-define(PATH_DIR__HOME,     os:getenv("HOME")).
-define(PATH_DIR__DATA,     filename:join([?PATH_DIR__HOME, ".data-mill"])).
-define(PATH_DIR__DATA_SSH, filename:join([?PATH_DIR__DATA, "ssh"])).
