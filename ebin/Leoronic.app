{application, leoronic,
  [{description,  "Simple distributed computing."},
 %  {id,           Id},
   {vsn,          "0.0.02"},
   {modules,      [leoronic, head, link_to_leoronic, port, run_container, utils]},
%%   {maxP,         MaxP},
%%   {maxT,         MaxT},
   {registered,   [leoronic, leoronic_port, scheduler, alert_new_node, loop_check_should_be_head, check_should_be_head]},
%%   {included_applications, Apps},
   {applications, [kernel, stdlib, sasl, os_mon]},
%   {env,          Env},
   {mod,          {leoronic, []}}
%   {start_phases, Phases},
%   {runtime_dependencies, RTDeps}
]}.