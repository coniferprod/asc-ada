with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Strings.Bounded;

procedure Asc is
    package CLI renames Ada.Command_Line;
    package TIO renames Ada.Text_IO;

    --  Subtype for ASCII characters with codes 0...127
    subtype ASCII_Character is
       Character range Ada.Characters.Latin_1.NUL ..
             Ada.Characters.Latin_1.DEL;

    procedure Print_Row (Char : ASCII_Character) is
        package Name_Str is new Ada.Strings.Bounded.Generic_Bounded_Length
           (Max => 3);
        use Name_Str;
        Names : constant array (ASCII_Character) of Bounded_String :=
           (To_Bounded_String ("NUL"), To_Bounded_String ("SOH"),
            To_Bounded_String ("STX"), To_Bounded_String ("ETX"),
            To_Bounded_String ("EOT"), To_Bounded_String ("ENQ"),
            To_Bounded_String ("ACK"), To_Bounded_String ("BEL"),
            To_Bounded_String ("BS"), To_Bounded_String ("TAB"),
            To_Bounded_String ("LF"), To_Bounded_String ("VT"),
            To_Bounded_String ("FF"), To_Bounded_String ("CR"),
            To_Bounded_String ("SO"), To_Bounded_String ("SI"),
            To_Bounded_String ("DLE"), To_Bounded_String ("DC1"),
            To_Bounded_String ("DC2"), To_Bounded_String ("DC3"),
            To_Bounded_String ("DC4"), To_Bounded_String ("NAK"),
            To_Bounded_String ("SYN"), To_Bounded_String ("ETB"),
            To_Bounded_String ("CAN"), To_Bounded_String ("EM"),
            To_Bounded_String ("SUB"), To_Bounded_String ("ESC"),
            To_Bounded_String ("FS"), To_Bounded_String ("GS"),
            To_Bounded_String ("RS"), To_Bounded_String ("US"),
            To_Bounded_String (" "), To_Bounded_String ("!"),
            To_Bounded_String (""""), To_Bounded_String ("#"),
            To_Bounded_String ("$"), To_Bounded_String ("%"),
            To_Bounded_String ("&"), To_Bounded_String ("'"),
            To_Bounded_String ("("), To_Bounded_String (")"),
            To_Bounded_String ("*"), To_Bounded_String ("+"),
            To_Bounded_String (","), To_Bounded_String ("-"),
            To_Bounded_String ("."), To_Bounded_String ("/"),
            To_Bounded_String ("0"), To_Bounded_String ("1"),
            To_Bounded_String ("2"), To_Bounded_String ("3"),
            To_Bounded_String ("4"), To_Bounded_String ("5"),
            To_Bounded_String ("6"), To_Bounded_String ("7"),
            To_Bounded_String ("8"), To_Bounded_String ("9"),
            To_Bounded_String (":"), To_Bounded_String (";"),
            To_Bounded_String ("<"), To_Bounded_String ("="),
            To_Bounded_String (">"), To_Bounded_String ("?"),
            To_Bounded_String ("@"), To_Bounded_String ("A"),
            To_Bounded_String ("B"), To_Bounded_String ("C"),
            To_Bounded_String ("D"), To_Bounded_String ("E"),
            To_Bounded_String ("F"), To_Bounded_String ("G"),
            To_Bounded_String ("H"), To_Bounded_String ("I"),
            To_Bounded_String ("J"), To_Bounded_String ("K"),
            To_Bounded_String ("L"), To_Bounded_String ("M"),
            To_Bounded_String ("N"), To_Bounded_String ("O"),
            To_Bounded_String ("P"), To_Bounded_String ("Q"),
            To_Bounded_String ("R"), To_Bounded_String ("S"),
            To_Bounded_String ("T"), To_Bounded_String ("U"),
            To_Bounded_String ("V"), To_Bounded_String ("W"),
            To_Bounded_String ("X"), To_Bounded_String ("Y"),
            To_Bounded_String ("Z"), To_Bounded_String ("["),
            To_Bounded_String ("\"), To_Bounded_String ("]"),
            To_Bounded_String ("^"), To_Bounded_String ("_"),
            To_Bounded_String ("`"), To_Bounded_String ("a"),
            To_Bounded_String ("b"), To_Bounded_String ("c"),
            To_Bounded_String ("d"), To_Bounded_String ("e"),
            To_Bounded_String ("f"), To_Bounded_String ("g"),
            To_Bounded_String ("h"), To_Bounded_String ("i"),
            To_Bounded_String ("j"), To_Bounded_String ("k"),
            To_Bounded_String ("l"), To_Bounded_String ("m"),
            To_Bounded_String ("n"), To_Bounded_String ("o"),
            To_Bounded_String ("p"), To_Bounded_String ("q"),
            To_Bounded_String ("r"), To_Bounded_String ("s"),
            To_Bounded_String ("t"), To_Bounded_String ("u"),
            To_Bounded_String ("v"), To_Bounded_String ("w"),
            To_Bounded_String ("x"), To_Bounded_String ("y"),
            To_Bounded_String ("z"), To_Bounded_String ("{"),
            To_Bounded_String ("|"), To_Bounded_String ("}"),
            To_Bounded_String ("~"), To_Bounded_String ("DEL"));
    begin
        Ada.Integer_Text_IO.Put (Item => Character'Pos (Char), Base => 10);
        Ada.Integer_Text_IO.Put
           (Item => Character'Pos (Char), Width => 2, Base => 16);
        Ada.Integer_Text_IO.Put
           (Item => Character'Pos (Char), Width => 7, Base => 2);
        Ada.Integer_Text_IO.Put
           (Item => Character'Pos (Char), Width => 3, Base => 8);
        Ada.Text_IO.Put (Item => To_String (Names (Char)));
        Ada.Text_IO.New_Line;
    end Print_Row;

    procedure Print_Table is
    begin
        for Char in ASCII_Character'First .. ASCII_Character'Last loop
            Print_Row (Char);
        end loop;
    end Print_Table;

begin
    if CLI.Argument_Count /= 0 then
        null;
    else  -- print the ASCII table
        Print_Table;
    end if;

end Asc;
