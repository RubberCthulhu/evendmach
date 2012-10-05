
{application, vm_server,
  [{description, "A vending machine server"},
   {vsn, "0.1.0"},
   {modules, [
              vm_server_app,
              vm_server_sup,
              vm_server
             ]},
   {registered, [vm_server_sup]},
   {applications, [kernel, stdlib]},
   {mod, {vm_server_app, []}}
]}.

