with Ada.Text_IO;
with Ada.Command_Line;
    
procedure Asc is    
   --  Print the full ASCII table.
   procedure Print_Table is
   begin
      Ada.Text_IO.Put_Line ("(table goes here)");
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
