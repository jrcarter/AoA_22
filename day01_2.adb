with Ada.Text_IO;

procedure Day01_2 is
   type Max_List is array (1 .. 3) of Natural;

   Input : Ada.Text_IO.File_Type;
   Max   : Max_List := (others => 0);
   Sum   : Natural;
begin -- Day01_2
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_01_1");

   All_Elves : loop
      exit All_Elves when Ada.Text_IO.End_Of_File (Input);

      Sum := 0;

      All_Food : loop
         exit All_Food when Ada.Text_IO.End_Of_File (Input);

         One_Food : declare
            Line : constant String := Ada.Text_IO.Get_Line (Input);
         begin -- One_Food
            exit All_Food when Line = "";

            Sum := Sum + Integer'Value (Line);
         end One_Food;
      end loop All_Food;

      if Sum > Max (1) then
         Max := Sum & Max (1 .. 2);
      elsif Sum > Max (2) then
         Max := Max (1) & Sum & Max (2);
      elsif Sum > Max (3) then
         Max := Max (1 .. 2) & Sum;
      else
         null;
      end if;
   end loop All_Elves;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Integer'Image (Max (1) + Max (2) + Max (3) ) );
end Day01_2;
