%% app generated at {2013,9,3} {18,46,17}
{application,sockserv,
             [{description,"Socket server to forward ProcessQuest messages to a client"},
              {vsn,"1.0.0"},
              {id,[]},
              {modules,[sockserv,sockserv_pq_events,sockserv_serv,
                        sockserv_sup,sockserv_trans]},
              {registered,[sockserv_sup]},
              {applications,[stdlib,kernel,processquest]},
              {included_applications,[]},
              {env,[{port,8082}]},
              {start_phases,undefined},
              {maxT,infinity},
              {maxP,infinity},
              {mod,{sockserv,[]}}]}.

