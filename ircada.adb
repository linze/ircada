with Lower_Layer_TCP;
with Lower_Layer_UDP; -- <-- It have a To_IP method!
with Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
with Ada.Text_IO; -- TODO: Remove this line. Just for testing.

use type Ada.Strings.Unbounded.Unbounded_String;


package body ircada is

  function Connect return boolean is
    ListeningTask : PIRCHandler;
  begin
    ircdEP := LLT.Build (LLU.To_IP (ASU.To_String (ServerHost)), ServerPort);
    LLT.Connect (ircdEP, ircdConnection);
    -- Create a task that will be receiving and processing all the upcoming
    -- messages.
    ListeningTask := new IRCHandler;
    -- Establish the nickname and send the USER line
    Handshake;
    return True;
  exception
    when others =>
      return False;
  end;


  task body IRCHandler is
  begin
    HandleMessages;
  end;

  procedure Handshake is
  begin
    String'Write (ircdConnection'Access, "USER " &
                  ASU.To_String (UserName) &
                  " " &
                  ASU.To_String (HostName) &
                  " " &
                  ASU.To_String (ServerName) &
                  " :" &
                  ASU.To_String (RealName) & ACLat.CR & ACLat.LF);
    ChangeNick (NickName);
  end Handshake;


  procedure ParsePrivMsg (IRCRawMessage : in TIRCRawMessage;
                          IRCMessage    : in out TIRCMessage) is
    Pos     : integer;
    TmpStr  : ASU.Unbounded_String;
  begin
    IRCMessage.NickName := IRCRawMessage.NickName;
    IRCMessage.UserName := IRCRawMessage.UserName;
    IRCMessage.Host     := IRCRawMessage.Host;

    Pos := ASU.Index (IRCRawMessage.Parameters, " ");
    IRCMessage.Where   := ASU.Head (IRCRawMessage.Parameters, Pos - 1);
    IRCMessage.Message := ASU.Tail (IRCRawMessage.Parameters,
                                    ASU.Length(IRCRawMessage.Parameters) - Pos - 1);

  end ParsePrivMsg;


  -- TODO: This is a quick solution. FromString should be "in" only.
  procedure ParseFromString (  FromString    : in out ASU.Unbounded_String;
                               IRCRawMessage : in out TIRCRawMessage) is
    Pos : integer;

  begin
    -- nick!~realname@host
    -- Nickname
    Pos := ASU.Index (FromString, "!");
    IRCRawMessage.NickName := ASU.Head (FromString, Pos - 1);
    FromString := ASU.Tail (FromString, ASU.Length(FromString) - Pos + 1);

    -- Username
    Pos := ASU.Index (FromString, "@");
    IRCRawMessage.UserName := ASU.Head (FromString, Pos + 1);
    FromString := ASU.Tail (FromString, ASU.Length (FromString) - Pos);

    -- Host
    IRCRawMessage.Host := FromString;
  end ParseFromString;


  -- TODO: This is a quick solution. RawString should be "in" only.
  procedure ParseRawString (RawString  : in out ASU.Unbounded_String;
                            IRCRawMessage : in out TIRCRawMessage) is
    Pos       : integer;
    TmpStr    : ASU.Unbounded_String;
    FromStr   : ASU.Unbounded_String;
  begin
    -- :nick!~realname@host PRIVMSG #channel :-<message>
    -- :<fromstring> <command> <from> :-<message>

    -- Remove the initial :
    RawString := ASU.Tail (RawString, ASU.Length (RawString) - 1);
    -- Catch FromString
    Pos := ASU.Index (RawString, " ");
    FromStr := ASU.Head (RawString, Pos - 1);
    RawString := ASU.Tail (RawString, Integer (ASU.Length (RawString) - Pos));

    -- Catch Command
    Pos := ASU.Index (RawString, " ");
    if Pos = 0 then
      IRCRawMessage.Command := RawString;
    else
      TmpStr := ASU.Head (RawString, Pos - 1);
      IRCRawMessage.Command := TmpStr;

      RawString := ASU.Tail (RawString, Integer(ASU.Length (RawString) - Pos));
      IRCRawMessage.Parameters := RawString;
    end if;

    if ASU.To_String (IRCRawMessage.Command) = "PRIVMSG" then
      ParseFromString (FromStr, IRCRawMessage);
    end if;
  end ParseRawString;


  procedure HandleMessages is
    ReceivedMsg   : ASU.Unbounded_String;
    LineCompleted : boolean;
    TmpChar       : Character;

    FromString    : ASU.Unbounded_String;

    IRCRawMessage : TIRCRawMessage;
    IRCMessage    : TIRCMessage;
  begin
    loop
      -- Catch the complete message
      ReceivedMsg := ASU.To_Unbounded_String ("");
      LineCompleted := False;
      while not LineCompleted loop
        Character'Read (ircdConnection'Access, TmpChar);
        if (TmpChar /= ACLat.CR) and (TmpChar /= ACLat.LF) then
          ReceivedMsg := ReceivedMsg & TmpChar;
        elsif TmpChar = ACLat.LF then
          LineCompleted := True;
        end if;
      end loop;

      -- Create a record with the brute message
      ParseRawString (ReceivedMsg, IRCRawMessage);

      if ASU.To_String (IRCRawMessage.Command) = "PRIVMSG" then
        Ada.Text_IO.Put_Line ("#####");
        ParsePrivMsg (IRCRawMessage, IRCMessage);
        Ada.Text_IO.Put_Line ("Message: " & ASU.To_String(IRCMessage.Message));
        if IsCommand (IRCMessage) then
          Ada.Text_IO.Put_Line ("Is command");
        end if;
      end if;

    end loop;
  end HandleMessages;

  function IsCommand ( IRCMessage : in TIRCMessage) return boolean is
    FirstCharacter : ASU.Unbounded_String;
  begin
    FirstCharacter := ASU.Head (IRCMessage.Message, 1);
    if FirstCharacter = ASU.To_Unbounded_String (CommandPrefix) then
      return True;
    else
      return False;
    end if;
  end IsCommand;

  procedure HandleCommand ( IRCMessage : in TIRCMessage) is
  begin
    null;
  end HandleCommand;


  procedure ChangeNick (Nick : in ASU.Unbounded_String) is
  begin
    -- TODO: Add detection of in use nicks
    String'Write (ircdConnection'Access, "NICK " &
                  ASU.To_String (Nick) & ACLat.CR & ACLat.LF);
    NickName := Nick;
  end ChangeNick;

  procedure Join  (Channel : in ASU.Unbounded_String) is
  begin
    String'Write (ircdConnection'Access, "JOIN " &
                  ASU.To_String (Channel) & ACLat.CR & ACLat.LF);
  end Join;

  procedure Part (Channel : in ASU.Unbounded_String;
                  Reason  : in String := ".") is
  begin
    --  PART <channel> :<reason>
    String'Write (ircdConnection'Access, "PART " &
                  ASU.To_String (Channel) & " :" &
                  Reason & ACLat.CR & ACLat.LF);
  end Part;


  procedure Say   (Destination : in ASU.Unbounded_String;
                   What        : in String) is
  begin
    --  PRIVMSG <destination> :<text>
    String'Write (ircdConnection'Access, "PRIVMSG " &
                  ASU.To_String (Destination) & " :" &
                  What & ACLat.CR & ACLat.LF);

  end Say;

end ircada;
