-module(records).
-compile(export_all).

-record(robot, {name,
                type=industrial,
                hobbies,
                details=[]}).

first_robot() -> 
  #robot{name="Mechatron",
         type=handmade,
         details=["Moved by internal manual control"]
        }.

car_factory(CorpName) ->
  #robot{name=CorpName, hobbies="building cars"}.



-record(user, {id, name, group, age}).

%% filter with pattern matching
admin_panel(#user{name=Name, group=admin}) ->
  Name ++ " is authorized";
admin_panel(#user{name=Name}) ->
  Name ++ " is not authorized".

%% extending user
adult_section(U=#user{}) when U#user.age >= 18 ->
  allowed;
adult_section(_) ->
  forbidden.

repairman(Rob) ->
  Details = Rob#robot.details,
  NewRob = Rob#robot{details=["Repaired by repairman"| Details]},
  {repaired, NewRob}.

-include("records.hrl").

included() -> #included{some_field="Some value"}.


