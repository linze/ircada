with ircada;
with Ada.Strings.Unbounded;
with Ada.Text_IO; -- TODO: Remove. Just for testing.

procedure ircada_demo is

  package ASU    renames Ada.Strings.Unbounded;

  Connected : boolean;
begin
  ircada.ServerHost := ASU.To_Unbounded_String ("irc.freenode.org");
  ircada.ServerPort := 6667;
  Connected := ircada.Connect;
  if Connected then
    -- Wait until MOTD is received
    while not ircada.Connected loop
      null;
    end loop;
    ircada.Join (ASU.To_Unbounded_String ("#wololo"));
    ircada.Say (ASU.To_Unbounded_String ("#wololo"), "wololo");
    ircada.Disconnect;
  end if;
  LLU.Finalize;
end ircada_demo;
