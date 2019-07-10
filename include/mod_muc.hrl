-record(muc_room, {
          name_host,
          opts
         }).

-record(muc_online_room, {name_host :: jid:simple_bare_jid(),
                          pid
                         }).

-record(muc_registered, {
          us_host,
          nick
         }).
