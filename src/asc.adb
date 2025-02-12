with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

procedure Asc is
   --  Subtype for ASCII characters with codes 0...127
   subtype ASCII_Character is
     Character range Ada.Characters.Latin_1.NUL .. Ada.Characters.Latin_1.DEL;

   subtype ASCII_Code is Integer range 0 .. 127;

   --  Number base which can only be one of the specified
   subtype Our_Base is Ada.Text_IO.Number_Base with
      Static_Predicate => Our_Base in 2 | 8 | 10 | 16;

   procedure Print_Value (
      Value : Integer;
      Base : Our_Base;
      Width : Positive;
      Pad : Character) is
      
      -- Make a temporary string with the maximum length (of 2#1111111#)
      Temp_String : String (1 .. 10);
      
      First_Hash_Position : Natural := 0;
      Second_Hash_Position : Natural := 0;

      --  Each printed value ends up here, padded and justified
      Result_String : String (1 .. Width);
   begin
      declare
      begin
         --Ada.Text_IO.Put_Line ("Print_Value here: Value = " & Value'Image);

         Ada.Integer_Text_IO.Put (To => Temp_String, Item => Value, Base => Base);
         --  Put does not output the base if it is 10, so we need to check
         --  for hash characters later. The width is the length of the output string,
         --  so there will most likely be padding. So trim it away.
         Ada.Strings.Fixed.Trim (Source => Temp_String, Side => Ada.Strings.Both);

         --Ada.Text_IO.Put_Line ("Temp_String = " & Temp_String);
         First_Hash_Position := Ada.Strings.Fixed.Index (Source => Temp_String, 
            Pattern => "#", From => 1, Going => Ada.Strings.Forward);
         Second_Hash_Position := Ada.Strings.Fixed.Index (Source => Temp_String,
            Pattern => "#", From => Temp_String'Length, Going => Ada.Strings.Backward);
         --  Both hash positions are 0 if the pattern is not found.

         if First_Hash_Position /= 0 then
            --Ada.Text_IO.Put ("Going to take slice ");
            --Ada.Integer_Text_IO.Put (Item => First_Hash_Position, Width => 1);
            --Ada.Text_IO.Put (" .. ");
            --Ada.Integer_Text_IO.Put (Item => Second_Hash_Position, Width => 1);
            --Ada.Text_IO.New_Line;

            --  If there is one hash, there is another one as well,
            --  so take a slice of the temp string. Also justify as
            --  requested by the caller. 
            Ada.Strings.Fixed.Move (
               Source => Temp_String (First_Hash_Position + 1 .. Second_Hash_Position - 1), 
               Target => Result_String,
               Justify => Ada.Strings.Right,
               Pad => Pad);
         else
            Ada.Strings.Fixed.Move (
               Source => Temp_String, 
               Target => Result_String);
         end if;
         Ada.Text_IO.Put (Result_String);
      end;
   end Print_Value;

   procedure Print_Row (Char : ASCII_Character) is
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

      Print_Value (Value => Value, Base => 10, Width => 3, Pad => Ada.Characters.Latin_1.Space);

      Ada.Text_IO.Put (Tab);
      Print_Value (Value => Value, Base => 16, Width => 2, Pad => '0');

      Ada.Text_IO.Put (Tab);
      Print_Value (Value => Value, Base => 2, Width => 7, Pad => '0');

      Ada.Text_IO.Put (Tab);
      Ada.Text_IO.Put (Tab);
      Print_Value (Value => Value, Base => 8, Width => 3, Pad => '0');

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
      Value_Part     : String (1 .. 7);

      --  The position of the last character that Get read (ignored)
      Last_Position_Ignored  : Positive;
   begin
      --  The argument value could conceivably be empty, so check that it
      --  has at least something there.
      if Arg'Length < 1 then
         Print_Error ("Error in argument: empty");
         --Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Error in argument: " & Arg);
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

      --  Construct an image like "10#65#" or "16#7E#"
      --  so that it can be parsed. If the value is not
      --  a valid ASCII code, Constraint_Error will be raised.
      --Value := ASCII_Code'Value (Base'Image
      --   & "#" & Arg (Start_Position .. Arg'Length) & "#");
      Ada.Text_IO.Put_Line ("'" & Base'Image & "#" & Arg (Start_Position .. Arg'Length) & "#'");
      --Ada.Integer_Text_IO.Put (To => Value_Part, Item => )

      Ada.Integer_Text_IO.Get (
         From => Base'Image & "#" & Arg (Start_Position .. Arg'Length) & "#",
         Item => Value,
         Last => Last_Position_Ignored);
      Ada.Text_IO.Put ("Ada.Integer_Text_IO.Get returned ");
      Ada.Integer_Text_IO.Put (Item => Value);
      Ada.Text_IO.New_Line;

      Print_Row (Character'Val (Value));
   exception
      when Constraint_Error =>
         Print_Error ("Error in argument: " & Arg);
   end;
end Asc;
