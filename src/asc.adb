with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Strings_Edit.Integers; -- for parsing number strings

procedure Asc is
    package CLI renames Ada.Command_Line;
    package TIO renames Ada.Text_IO;

    --  Subtype for ASCII characters with codes 0...127
    subtype ASCII_Character is
       Character range Ada.Characters.Latin_1.NUL ..
             Ada.Characters.Latin_1.DEL;

    Number_Error : exception;  -- raised if number is out of ASCII range

    function Trimmed_Number_String (Number_String : String; Final_Length: Positive) return String is
        First_Hash_Position : Positive;
        Second_Hash_Position : Positive;
        S : String := Number_String;
    begin
        Ada.Strings.Fixed.Trim (Source => S, Side => Ada.Strings.Both);

        -- Find the first hash, indicating the start of the base specifier.
        First_Hash_Position := Ada.Strings.Fixed.Index (
            Source => S, 
            Pattern => "#");

        -- Find the second hash, indicating the end of the base specifier.
        -- Start the search from after the first hash.
        Second_Hash_Position := Ada.Strings.Fixed.Index (
            Source => S, 
            Pattern => "#",
            From => First_Hash_Position + 1);

        -- Everything after the second hash up until the end of the string
        -- is the actual number representation. Add leading zeros if necessary.
        declare
            Start_Position : constant Positive := First_Hash_Position + 1;
            End_Position : constant Positive := Second_Hash_Position - 1;
            Result : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String (S (Start_Position .. End_Position));
            Count : constant Integer := Final_Length - Ada.Strings.Unbounded.Length (Result); 
        begin
            for I in 1..Count loop
                Ada.Strings.Unbounded.Insert (
                    Source => Result, 
                    Before => 1, 
                    New_Item => "0");
            end loop;
            return Ada.Strings.Unbounded.To_String (Result);
        end;
    end Trimmed_Number_String;

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

        Hex_String : String (1..6);  -- max length for "16#FF#"
        Oct_String : String (1..6);  -- max length for "8#666#"
        Bin_String : String (1..11);  -- max length for 2#11111111#

        Tab : constant Character := Ada.Characters.Latin_1.HT;
    begin
        -- Update the Names array
        for Char in Ada.Characters.Latin_1.Exclamation .. ASCII_Character'Pred (Ada.Characters.Latin_1.DEL) loop
            Names (Char) := Char & "  "; -- pad to three characters
        end loop;
        Names (ASCII_Character'Last) := "DEL";

        -- Output the decimal character code. Base 10 has no base specifier.
        Ada.Integer_Text_IO.Put (Item => Character'Pos (Char), Base => 10);

        Ada.Text_IO.Put (Tab);
        Ada.Integer_Text_IO.Put (
            To => Hex_String, 
            Item => Character'Pos (Char),
            Base => 16);
        Ada.Text_IO.Put (Trimmed_Number_String (Hex_String, Final_Length => 2));

        Ada.Text_IO.Put (Tab);
        Ada.Integer_Text_IO.Put (
            To => Bin_String, Item => Character'Pos (Char), Base => 2
        );
        Ada.Text_IO.Put (Trimmed_Number_String (Bin_String, Final_Length => 8));

        Ada.Text_IO.Put (Tab);
        Ada.Integer_Text_IO.Put (
            To => Oct_String, Item => Character'Pos (Char), Base => 8
        );
        Ada.Text_IO.Put (Trimmed_Number_String (Oct_String, Final_Length => 3));

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
