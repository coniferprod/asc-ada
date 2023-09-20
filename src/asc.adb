with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Characters.Latin_1;

procedure Asc is
   package CLI renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;

   --  Subtype for ASCII characters with codes 0...127
   subtype ASCII_Character is Character
      range Ada.Characters.Latin_1.NUL .. Ada.Characters.Latin_1.DEL;

   procedure Print_Row (Char : ASCII_Character) is
   begin
      TIO.Put (Char);
      TIO.New_Line;
   end Print_Row;

   procedure Print_Table is
   begin
      for Char in ASCII_Character'First .. ASCII_Character'Last loop
         Print_Row (Char);
         TIO.New_Line;
      end loop;
   end Print_Table;

begin
   if CLI.Argument_Count /= 0 then
      null;
   else  -- print the ASCII table
      Print_Table;
   end if;

end Asc;
