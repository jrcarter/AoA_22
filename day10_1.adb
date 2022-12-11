with Ada.Text_IO;

procedure Day10_1 is
   Input : Ada.Text_IO.File_Type;
   X     : Integer  :=  1;
   Cycle : Natural  :=  0;
   Next  : Positive := 20;
   Sum   : Natural  :=  0;
begin -- Day10_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_10");

   All_Ops : loop
      exit All_Ops when Ada.Text_IO.End_Of_File (Input) or Cycle >= 220;

      One_Op : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);
      begin -- One_Op
         Cycle := Cycle + 1;

         if Cycle = Next then
            Sum := Sum + Cycle * X;
            Next := Next + 40;
         end if;

         if Line (1) = 'a' then
            Cycle := Cycle + 1;

            if Cycle = Next then
               Sum := Sum + Cycle * X;
               Next := Next + 40;
            end if;

            X := X + Integer'Value (Line (6 .. Line'Last) );
        end if;
      end One_Op;
   end loop All_Ops;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Sum'Image);
end Day10_1;
