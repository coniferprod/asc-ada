with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Characters.Handling; use Ada.Characters.Handling;

procedure Asc is

   --  Print a row for an ASCII character.
   procedure Print_Row (Char : ISO_646) is
   begin
      Ada.Text_IO.Put_Line ("Row for character " & Char'Image);
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
