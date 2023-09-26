with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;

with Strings_Edit.Integers;

procedure Asc is
    package CLI renames Ada.Command_Line;
    package TIO renames Ada.Text_IO;

    package Number_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
           (Max => 8); -- varies from two (hex) to eight (binary)

    --  Subtype for ASCII characters with codes 0...127
    subtype ASCII_Character is
       Character range Ada.Characters.Latin_1.NUL ..
             Ada.Characters.Latin_1.DEL;

    function Trimmed_Number_String (Number_String : String; Final_Length: Positive) return String is
        First_Hash_Position : Positive;
        Second_Hash_Position : Positive;
        S : String := Number_String;
    begin
        --Ada.Text_IO.Put_Line ("String to trim = '" & S & "'");

        Ada.Strings.Fixed.Trim (Source => S, Side => Ada.Strings.Both);
        --Ada.Text_IO.Put_Line ("Trimmed = '" & S & "'");

        -- Find the first hash, indicating the start of the base specifier.
        First_Hash_Position := Ada.Strings.Fixed.Index (
            Source => S, 
            Pattern => "#");
        --Ada.Text_IO.Put ("First hash position = ");
        --Ada.Integer_Text_IO.Put (First_Hash_Position);
        --Ada.Text_IO.New_Line;

        -- Find the second hash, indicating the end of the base specifier.
        -- Starting the search from after the first hash.
        Second_Hash_Position := Ada.Strings.Fixed.Index (
            Source => S, 
            Pattern => "#",
            From => First_Hash_Position + 1);
        --Ada.Text_IO.Put ("Second hash position = ");
        --Ada.Integer_Text_IO.Put (Second_Hash_Position);
        --Ada.Text_IO.New_Line;

        -- Everything after the second hash up until the end of the string
        -- is the actual number representation. Add leading zero if necessary.
        declare
            Start_Position : constant Positive := First_Hash_Position + 1;
            End_Position : constant Positive := Second_Hash_Position - 1;
            Result : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String (S (Start_Position .. End_Position));
            Count : constant Integer := Final_Length - Ada.Strings.Unbounded.Length (Result); 
        begin
            --Ada.Text_IO.Put_Line ("Count = " & Count'Image);
            --Ada.Text_IO.Put_Line ("Result before padding = '" & Ada.Strings.Unbounded.To_String (Result) & "'");
            for I in 1..Count loop
                Ada.Strings.Unbounded.Insert (
                    Source => Result, 
                    Before => 1, 
                    New_Item => "0");
            end loop;
            --Ada.Text_IO.Put_Line ("Result after padding = '" & Ada.Strings.Unbounded.To_String (Result) & "'");
            return Ada.Strings.Unbounded.To_String (Result);
        end;
    end Trimmed_Number_String;

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

        Hex_String : String (1..6);  -- max length for "16#FF#"
        Oct_String : String (1..6);  -- max length for "8#666#"
        Bin_String : String (1..11);  -- max length for 2#11111111#

        Tab : constant Character := Ada.Characters.Latin_1.HT;
    begin
        -- Output the decimal character code. Base 10 has no base specifier.
        Ada.Integer_Text_IO.Put (Item => Character'Pos (Char), Base => 10);

        Ada.Text_IO.Put (Tab);

        -- Write the hex number into a string, to trim it later.
        Ada.Integer_Text_IO.Put (
            To => Hex_String, 
            Item => Character'Pos (Char),
            Base => 16);
        --Ada.Text_IO.Put ("'" & Hex_String & "'");
        Ada.Text_IO.Put (Trimmed_Number_String (Hex_String, Final_Length => 2));

        --Ada.Integer_Text_IO.Put
        --   (Item => Character'Pos (Char), Width => 2, Base => 16);

        Ada.Text_IO.Put (Tab);

        Ada.Integer_Text_IO.Put (
            To => Bin_String, Item => Character'Pos (Char), Base => 2
        );
        Ada.Text_IO.Put (Trimmed_Number_String (Bin_String, Final_Length => 8));

        --Ada.Integer_Text_IO.Put
        --   (Item => Character'Pos (Char), Width => 7, Base => 2);

        Ada.Text_IO.Put (Tab);

        Ada.Integer_Text_IO.Put (
            To => Oct_String, Item => Character'Pos (Char), Base => 8
        );
        Ada.Text_IO.Put (Trimmed_Number_String (Oct_String, Final_Length => 3));

        --Ada.Integer_Text_IO.Put
        --   (Item => Character'Pos (Char), Width => 3, Base => 8);
        
        Ada.Text_IO.Put (Tab);

        Ada.Text_IO.Put (Item => To_String (Names (Char)));
        Ada.Text_IO.New_Line;
    end Print_Row;

    procedure Print_Table is
    begin
        for Char in ASCII_Character'First .. ASCII_Character'Last loop
            Print_Row (Char);
        end loop;
    end Print_Table;

    function Starts_With (S : in String; Prefix : in String) return Boolean is
    begin
        return (Ada.Strings.Fixed.Index (
                Source => S, 
                Pattern => Prefix) /= 0);
    end Starts_With;

begin
    if CLI.Argument_Count /= 0 then
        declare
            -- Get the first command line argument string
            Arg : String := Ada.Command_Line.Argument (1);
            Base : Ada.Text_IO.Number_Base;
            Start_Position : Positive;
            Value : Integer;
        begin
            if Starts_With (Arg, "0x") then
                Base := 16;
                Start_Position := 3;
            elsif Starts_With (Arg, "0b") then
                Base := 2;
                Start_Position := 3;
            elsif Starts_With (Arg, "0o") then
                Base := 8;
                Start_Position := 3;
            else
                Base := 10;
                Start_Position := 1;
            end if;

            Strings_Edit.Integers.Get (
                Source => Arg,
                Pointer => Start_Position,
                Value => Value,
                Base => Base
            );

            Print_Row (Character'Val (Value));
        exception
            when Ada.Strings.Length_Error =>
                Ada.Text_IO.Put_Line ("Number string too long");
        end;
    else  -- print the ASCII table
        Print_Table;
    end if;

end Asc;
