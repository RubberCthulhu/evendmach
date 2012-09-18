
{application, vendmach,
  [{description, "A vending machine server"},
   {vsn, "0.1.0"},
   {modules, [
              vendmach_app,
              vendmach_sup
             ]},
   {registered, [vendmach_sup]},
   {applications, [kernel, stdlib]},
   {mod, {vendmach_app, []}}
]}.


