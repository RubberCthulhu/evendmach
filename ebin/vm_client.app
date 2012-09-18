
{application, vm_client,
  [{description, "A vending machine client"},
   {vsn, "0.1.0"},
   {modules, [
              vm_client_app,
              vm_client_sup,
              vm_client
             ]},
   {registered, [vm_client_sup]},
   {applications, [kernel, stdlib]},
   {mod, {vm_client_app, []}}
]}.


