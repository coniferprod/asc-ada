with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

with Strings_Edit.Integers; -- for parsing number strings

procedure Asc is
    --  Subtype for ASCII characters with codes 0...127
    subtype ASCII_Character is
       Character range Ada.Characters.Latin_1.NUL ..
             Ada.Characters.Latin_1.DEL;

    procedure Print_Value (
        Value : in Integer; 
        Base : in Ada.Text_IO.Number_Base; 
        Width : in Positive;
        Fill : Boolean := False) is
        Result_String : String (1..Width);
        Position : Positive := Result_String'First;
    begin
        Strings_Edit.Integers.Put (
            Destination => Result_String,
            Pointer => Position,
            Value => Value,
            Base => Base,
            Field => Width,
            Justify => Strings_Edit.Right,
            Fill => (if Fill then '0' else ' ')
        );
        Ada.Text_IO.Put (Result_String);
    end Print_Value;

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
        Value : constant Integer := Character'Pos (Char); 
    begin
        -- Update the Names array
        declare
            Start_Char : constant Character := Ada.Characters.Latin_1.Exclamation;
            End_Char : constant Character := ASCII_Character'Pred (Ada.Characters.Latin_1.DEL);
        begin
            for Char in Start_Char .. End_Char loop
                Names (Char) := Char & "  "; -- pad to three characters
            end loop;
            Names (ASCII_Character'Last) := "DEL";
        end;

        Print_Value (Value => Value, Base => 10, Width => 3);

        Ada.Text_IO.Put (Tab);
        Print_Value (Value => Value, Base => 16, Width => 2, Fill => True);

        Ada.Text_IO.Put (Tab);
        Print_Value (Value => Value, Base => 2, Width => 8, Fill => True);

        Ada.Text_IO.Put (Tab);
        Print_Value (Value => Value, Base => 8, Width => 3, Fill => True);

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
            Start_Position : Positive := 3;  -- the most common case
            Value : Integer;
        begin
            if Starts_With (Arg, "0x") then
                Base := 16;
            elsif Starts_With (Arg, "0b") then
                Base := 2;
            elsif Starts_With (Arg, "0o") then
                Base := 8;
            else
                Base := 10;
                Start_Position := 1;
            end if;

            Strings_Edit.Integers.Get (
                Source => Arg,
                Pointer => Start_Position,
                Value => Value,
                Base => Base,
                First => 0,
                Last => 127
            );

            -- If we get here, no exceptions were raised from parsing
            Print_Row (Character'Val (Value));
        exception
            when Constraint_Error => 
                Ada.Text_IO.Put_Line ("Number out of range: " & Arg);
            when Ada.Text_IO.Data_Error | Ada.Text_IO.End_Error =>
                Ada.Text_IO.Put_Line ("Not a valid number: " & Arg);
        end;
    else  -- print the ASCII table
        Print_Table;
    end if;
end Asc;
