with ircada;
with Ada.Strings.Unbounded;

procedure ircada_demo is

  package ASU    renames Ada.Strings.Unbounded;

  Connected : boolean;
begin
  ircada.ServerHost := ASU.To_Unbounded_String ("irc.freenode.org");
  ircada.ServerPort := 6667;
  Connected := ircada.Connect;
  if Connected then
    ircada.Join (ASU.To_Unbounded_String ("#testchannel"));
    ircada.Say (ASU.To_Unbounded_String ("#testchannel"), "wololo");
    ircada.Part (ASU.To_Unbounded_String ("#testchannel"));
  end if;

end ircada_demo;
