with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

procedure Asc is
   --  Subtypes for ASCII characters with codes 0...127
   subtype ASCII_Character is
     Character range Ada.Characters.Latin_1.NUL .. Ada.Characters.Latin_1.DEL;
   subtype ASCII_Code is Integer range 0 .. 127;

   --  Number base which can only be one of the specified
   subtype Our_Base is Ada.Text_IO.Number_Base with
      Static_Predicate => Our_Base in 2 | 8 | 10 | 16;

   --  Print a non-base-10 value.
   --  Based on ideas found here: https://stackoverflow.com/a/30423877
   procedure Print_Value (Value : ASCII_Code; Width : Positive; Base : Our_Base) is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      --  Make a temporary string with the maximum length (of 2#1111111#)
      Temp_String : String (1 .. 10);

      First_Hash_Position : Natural := 0;
      Second_Hash_Position : Natural := 0;
   begin
      --  Put the ASCII code value in the specified base into the temporary string. 
      --  We are not putting a base 10 value, so we know there will be hash characters.
      Ada.Integer_Text_IO.Put (To => Temp_String, Item => Value, Base => Base);

      -- Get the first hash position, starting from the front
      First_Hash_Position := Index (Source => Temp_String, 
         Pattern => "#", From => 1, Going => Forward);

      -- Get the second hash position, starting from the back
      Second_Hash_Position := Index (Source => Temp_String,
         Pattern => "#", From => Temp_String'Length, Going => Backward);

      -- Put the part between the hash positions, zero-padded from the left
      Ada.Text_IO.Put (
         Tail (
            Source => Temp_String (First_Hash_Position + 1 .. Second_Hash_Position - 1),
            Count   => Width,
            Pad     => '0'));
   end Print_Value;

   --  Print a full row for the character: decimal, hexadecimal, binary,
   --  octal, and the character name or literal.
   procedure Print_Row (Char : ASCII_Character) is
      use Ada.Text_IO;

      --  Names of the ASCII characters, initially with just the
      --  control characters. Later the rest will be filled in
      --  with their literal values. All are padded to three characters.
      Names : array (ASCII_Character) of String (1 .. 3) :=
         ("NUL", "SOH", "STX", "ETX",
          "EOT", "ENQ", "ACK", "BEL",
          "BS ", "TAB", "LF ", "VT ",
          "FF ", "CR ", "SO ", "SI ",
          "DLE", "DC1", "DC2", "DC3",
          "DC4", "NAK", "SYN", "ETB",
          "CAN", "EM ", "SUB", "ESC",
          "FS ", "GS ", "RS ", "US ",
         others => "   ");
      Tab   : constant Character := Ada.Characters.Latin_1.HT;

      --  The ordinal value of the character
      Value : constant ASCII_Code := Character'Pos (Char);
   begin
      --  Update the Names array with the printable ASCII characters.
      --  Go from exclamation mark to the one before DEL (127).
      declare
         Start_Char : constant Character := Ada.Characters.Latin_1.Exclamation;
         End_Char   : constant Character :=
            ASCII_Character'Pred (Ada.Characters.Latin_1.DEL);
      begin
         for Char in Start_Char .. End_Char loop
            Names (Char) := Char & "  "; -- pad to three characters
         end loop;

         --  Put the DEL character in place too
         Names (ASCII_Character'Last) := "DEL";
      end;

      --  Put the base 10 value, with the maximum width.
      --  This also gives us left padding with spaces.
      Ada.Integer_Text_IO.Put (Item => Value, Width => ASCII_Code'Width, Base => 10);

      --  Put the hexadecimal value (two hex digits, zero-padded from left)
      Put (Tab);
      Print_Value (Value => Value, Width => 2, Base => 16);

      --  Put the binary value (seven bits, zero-padded from left)
      Put (Tab);
      Print_Value (Value => Value, Width => 7, Base => 2);

      --  Put the octal value (three octal digits, zero-padded from left)
      Put (Tab);
      Put (Tab);
      Print_Value (Value => Value, Width => 3, Base => 8);

      --  Put the character name or literal
      Put (Tab);
      Put (Item => Names (Char));

      New_Line;
   end Print_Row;

   procedure Print_Table is
   begin
      for Char in ASCII_Character'First .. ASCII_Character'Last loop
         Print_Row (Char);
      end loop;
   end Print_Table;

   function Starts_With (S : String; Prefix : String) return Boolean is
   begin
      return (Ada.Strings.Fixed.Index (Source => S, Pattern => Prefix) /= 0);
   end Starts_With;

   procedure Print_Error (Message : String) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Message);
   end Print_Error;

begin
   --  If there are no command line arguments, just print
   --  the whole table and exit.
   if Ada.Command_Line.Argument_Count < 1 then
      Print_Table;
      return;
   end if;

   --  We are looking for exactly one argument, one of "0xhh", "0occc",
   --  "0bxxxxxxx" (seven bits), or a decimal number with no prefix.
   --  The shortest possible valid argument is a decimal number from 0 to 9
   --  (just one character), while the longest is a seven-bit binary number
   --  with a prefix (nine characters).

   declare
      Arg            : constant String := Ada.Command_Line.Argument (1);
      Base           : Our_Base        := 10; -- default to base 10

      --  The start position of the number part, after any prefix.
      --  The most common case is 3 (after a prefix line "0x").
      Start_Position : Positive := 3;

      --  The actual number we got out of the argument (7-bit)
      Value          : ASCII_Code;

      --  The number part of the argument, from after any possible prefix
      --  to the end of the string. Covers all the possible lengths.
      --Value_Part     : String (1 .. 7);

      --  The position of the last character that Get read (ignored)
      Last_Position_Ignored  : Positive;
   begin
      --  Bail out if the argument value is empty
      if Arg'Length < 1 then
         Print_Error ("Error in argument: empty");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;

      if Starts_With (Arg, "0x") then
         Base := 16;
      elsif Starts_With (Arg, "0b") then
         Base := 2;
      elsif Starts_With (Arg, "0o") then
         Base := 8;
      else -- no prefix, most likely a decimal number
         Start_Position := 1;
      end if;

      --  Construct an image like "10#65#" or "16#7E#" and parse it.
      --  Raises Constraint_Error if the value does not fit in
      --  the range of ASCII_Code.
      Ada.Integer_Text_IO.Get (
         From => Base'Image & "#" & Arg (Start_Position .. Arg'Length) & "#",
         Item => Value,
         Last => Last_Position_Ignored);

      Print_Row (Character'Val (Value));
   exception
      when Constraint_Error =>
         Print_Error ("Error in argument: " & Arg);
   end;
end Asc;
