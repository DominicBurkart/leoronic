-module(ghost).
-export([
         init/0,
         get_clairvoyant/0,
         get_clairvoyant/1,
         add_tag/1,
         del_tag/1,
         has_tag/1,
         system_info/0,
         request_job/0,
         in_leoronic/1
       ]).


get_clairvoyant() -> get_clairvoyant(1).
