-define(TIMEOUT, 5000).


-define(PATH_DIR__HOME,     os:getenv("HOME")).
-define(PATH_DIR__DATA,     filename:join([?PATH_DIR__HOME, ".data-mill"])).
-define(PATH_DIR__DATA_SSH, filename:join([?PATH_DIR__DATA, "ssh"])).

-define(PATH_FILE__SSH_KEY,     filename:join([?PATH_DIR__DATA_SSH, "id_rsa"])).
-define(PATH_FILE__SSH_HOSTKEY, filename:join([?PATH_DIR__DATA_SSH, "ssh_host_dsa_key"])).


-define(OS_CMD__SSH_KEY_GEN,
    "ssh-keygen -t rsa -b 2048 -N '' -f "++?PATH_FILE__SSH_KEY).

-define(OS_CMD__SSH_HOSTKEY_GEN,
    "ssh-keygen -t dsa -b 1024 -N '' -f "++?PATH_FILE__SSH_HOSTKEY).
