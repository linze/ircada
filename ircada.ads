with Lower_Layer_TCP;
with Lower_Layer_UDP;  -- <-- It have a To_IP method!
with Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;

package ircada is

  package LLT     renames Lower_Layer_TCP;
  package LLU     renames Lower_Layer_UDP;
  package ASU     renames Ada.Strings.Unbounded;
  package ACLat	  renames Ada.Characters.Latin_1;

  type TIRCRawMessage is record
    NickName     : ASU.Unbounded_String;
    UserName     : ASU.Unbounded_String;
    Host         : ASU.Unbounded_String;
    Command      : ASU.Unbounded_String;
    Parameters   : ASU.Unbounded_String;
  end record;


  type TIRCMessage is record
    NickName     : ASU.Unbounded_String;
    UserName     : ASU.Unbounded_String;
    Host         : ASU.Unbounded_String;
    Where        : ASU.Unbounded_String;
    Message      : ASU.Unbounded_String;
  end record;

  ServerHost  : ASU.Unbounded_String := ASU.To_Unbounded_String("irc.freenode.org");
  ServerPort  : Integer := 6667;

  NickName    : ASU.Unbounded_String := ASU.To_Unbounded_String ("Peerate");
  AltNickName : ASU.Unbounded_String := ASU.To_Unbounded_String ("P33r4t3");

  UserName    : ASU.Unbounded_String := ASU.To_Unbounded_String ("Guybrush");
  HostName    : ASU.Unbounded_String := ASU.To_Unbounded_String ("seti");
  ServerName  : ASU.Unbounded_String := ASU.To_Unbounded_String ("pircd");
  RealName    : ASU.Unbounded_String := ASU.To_Unbounded_String ("Guybrush Threepwood");

  CommandPrefix : String := "!";
  Connected     : boolean;

  function Connect return boolean;
  procedure HandleMessages;
  procedure ChangeNick (Nick : in ASU.Unbounded_String);
  procedure Join  (Channel : in ASU.Unbounded_String);
  procedure Part (Channel : in ASU.Unbounded_String;
                  Reason  : in String := "." );
  procedure Say   (Destination : in ASU.Unbounded_String;
                   What        : in String);
  procedure Disconnect;

private
  task type IRCHandler;
  type PIRCHandler is access IRCHandler;


  ircdEP             : LLT.End_Point;
  ircdConnection     : aliased LLT.Connection;
  HandlerAccess      : aliased PIRCHandler;
  -- This will indicate whenever the application should
  -- read from the connection or end the task.
  ReadFromConnection : boolean;

  procedure Handshake;
  procedure ParsePrivMsg (IRCRawMessage : in TIRCRawMessage;
                          IRCMessage    : in out TIRCMessage);
  procedure ParseFromString (  FromString    : in out ASU.Unbounded_String;
                               IRCRawMessage : in out TIRCRawMessage);
  procedure ParseRawString (RawString  : in out ASU.Unbounded_String;
                            IRCRawMessage : in out TIRCRawMessage);
  function IsCommand ( IRCMessage : in TIRCMessage) return boolean;
  procedure HandleCommand ( IRCMessage : in TIRCMessage);

end ircada;
