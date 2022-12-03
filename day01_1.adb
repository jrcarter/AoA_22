with Ada.Text_IO;

procedure Day01_1 is
   Input : Ada.Text_IO.File_Type;
   Max   : Natural := 0;
   Sum   : Natural;
begin -- Day01_1
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

      Max := Integer'Max (Max, Sum);
   end loop All_Elves;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Max'Image);
end Day01_1;
