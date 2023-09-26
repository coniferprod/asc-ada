with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

with Strings_Edit.Integers; -- for parsing number strings

procedure Asc is
    --  Subtype for ASCII characters with codes 0...127
    subtype ASCII_Character is
       Character range Ada.Characters.Latin_1.NUL ..
             Ada.Characters.Latin_1.DEL;

    Number_Error : exception;  -- raised if number is out of ASCII range

    procedure Print_Row (Char : ASCII_Character) is
        Names : array (ASCII_Character) of String (1..3) :=
           ("NUL", "SOH", "STX", "ETX",
            "EOT", "ENQ", "ACK", "BEL",
            "BS ", "TAB", "LF ", "VT ",
            "FF ", "CR ", "SO ", "SI ",
            "DLE", "DC1", "DC2", "DC3",
            "DC4", "NAK", "SYN", "ETB",
            "CAN", "EM ", "SUB", "ESC",
            "FS ", "GS ", "RS ", "US ",
                others => "   ");

        Tab : constant Character := Ada.Characters.Latin_1.HT;
    begin
        -- Update the Names array
        for Char in Ada.Characters.Latin_1.Exclamation .. ASCII_Character'Pred (Ada.Characters.Latin_1.DEL) loop
            Names (Char) := Char & "  "; -- pad to three characters
        end loop;
        Names (ASCII_Character'Last) := "DEL";

        declare
            Result_String : String (1..3);
            Position : Positive := Result_String'First;
        begin
            Strings_Edit.Integers.Put (
                Destination => Result_String,
                Pointer => Position,
                Value => Character'Pos (Char),
                Base => 10,
                Field => 3,
                Justify => Strings_Edit.Right
            );
            Ada.Text_IO.Put (Result_String);
        end;

        Ada.Text_IO.Put (Tab);

        declare
            Result_String : String (1..2);
            Position : Positive := Result_String'First;
        begin
            Strings_Edit.Integers.Put (
                Destination => Result_String,
                Pointer => Position,
                Value => Character'Pos (Char),
                Base => 16,
                Field => 2,
                Justify => Strings_Edit.Right,
                Fill => '0'
            );
            Ada.Text_IO.Put (Result_String);
        end;

        Ada.Text_IO.Put (Tab);
        declare
            Result_String : String (1..8);
            Position : Positive := Result_String'First;
        begin
            Strings_Edit.Integers.Put (
                Destination => Result_String,
                Pointer => Position,
                Value => Character'Pos (Char),
                Base => 2,
                Field => 8,
                Justify => Strings_Edit.Right,
                Fill => '0'
            );
            Ada.Text_IO.Put (Result_String);
        end;

        Ada.Text_IO.Put (Tab);

        declare
            Result_String : String (1..3);
            Position : Positive := Result_String'First;
        begin
            Strings_Edit.Integers.Put (
                Destination => Result_String,
                Pointer => Position,
                Value => Character'Pos (Char),
                Base => 8,
                Field => 3,
                Justify => Strings_Edit.Right,
                Fill => '0'
            );
            Ada.Text_IO.Put (Result_String);
        end;

        Ada.Text_IO.Put (Tab);
        Ada.Text_IO.Put (Item => Names (Char));

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
    if Ada.Command_Line.Argument_Count /= 0 then
        declare
            Arg : constant String := Ada.Command_Line.Argument (1);
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

            if Value in Character'Pos (ASCII_Character'First) .. 
                Character'Pos (ASCII_Character'Last) then
                Print_Row (Character'Val (Value));
            else
                raise Number_Error;
            end if;
        exception
            when Ada.Strings.Length_Error =>
                Ada.Text_IO.Put_Line ("Number string too long: " & Arg);
            when Number_Error =>
                Ada.Text_IO.Put_Line ("Number out of range: " & Arg);
        end;
    else  -- print the ASCII table
        Print_Table;
    end if;
end Asc;
