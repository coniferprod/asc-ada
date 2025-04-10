with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Integer_Text_IO;

procedure Asc is

   subtype ASCII_Code is Integer range 0 .. 127;

   --  Number base which can only be one of the specified
   subtype Our_Base is Ada.Text_IO.Number_Base with
      Static_Predicate => Our_Base in 2 | 8 | 10 | 16;

   --  Print a non-base-10 value.
   --  Based on ideas found here: https://stackoverflow.com/a/30423877
   procedure Print_Value (Value : ASCII_Code; Width : Positive; Base : Our_Base) is
      --  Make a temporary string with the maximum length (of 2#1111111#)
      Temp_String : String (1 .. 10);

      First_Hash_Position : Natural := 0;
      Second_Hash_Position : Natural := 0;
   begin
      -- Get base 10 out of the way first. Just put it out.
      if Base = 10 then
         Ada.Integer_Text_IO.Put (Item => Value, Width => 3);
         return;
      end if;

      --  Put the ASCII code value in the specified base 
      --  into the temporary string. Since we are not putting 
      --  a base 10 value, we know there will be hash characters.
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
   procedure Print_Row (Char : ISO_646) is
      use Ada.Text_IO;

      --  The ordinal value of the character
      Value : constant ASCII_Code := ISO_646'Pos (Char);

      --  The separator between the fields
      Blanks : constant String := 2 * Space;
   begin
      Print_Value (Value, Width => 3, Base => 10);
      Put (Blanks);
      Print_Value (Value, Width => 2, Base => 16);
      Put (Blanks);
      Print_Value (Value, Width => 7, Base => 2);
      Put (Blanks);      
      Print_Value (Value, Width => 3, Base => 8);
      Put (Blanks);

      if Is_Control (Char) then    
         Put (Item => Char'Image);
      else
         Put (Char);
      end if;

      New_Line;
   end Print_Row;

   --  Print the full ASCII table.
   procedure Print_Table is
   begin
      for Char in ISO_646'Range loop
         Print_Row (Char);
      end loop;
   end Print_Table;

begin
   --  If there are no command line arguments, 
   --  just print the whole table and exit.
   if Ada.Command_Line.Argument_Count < 1 then
      Print_Table;
      return;
   end if;

   --  Show the first command line argument
   Ada.Text_IO.Put ("First argument = '" &
      Ada.Command_Line.Argument (1) & "'");
   Ada.Text_IO.New_Line;
end Asc;
