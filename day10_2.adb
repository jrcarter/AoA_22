with Ada.Text_IO;

procedure Day10_2 is
   procedure Put (Column : in out Natural; X : in Integer) with
      Pre  => Column < 40,
      Post => Column < 40;
   -- If Column within 1 of X, outputs '#'; otherwise, outputs '.'
   -- Increments Column mod 40
   -- If Column becomes 0, outputs a new line

   procedure Put (Column : in out Natural; X : in Integer) is
      -- Empty
   begin -- Put
      Ada.Text_IO.Put (Item => (if Column in X - 1 .. X + 1 then '#' else '.') );
      Column := (Column + 1) rem 40;

      if Column = 0 then
         Ada.Text_IO.New_Line;
      end if;
   end Put;

   Input  : Ada.Text_IO.File_Type;
   X      : Integer  :=  1;
   Column : Natural := 0;
begin -- Day10_2
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_10");

   All_Ops : loop
      exit All_Ops when Ada.Text_IO.End_Of_File (Input);

      One_Op : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);
      begin -- One_Op
         Put (Column => Column, X => X);

         if Line (1) = 'a' then
            Put (Column => Column, X => X);
            X := X + Integer'Value (Line (6 .. Line'Last) );
        end if;
      end One_Op;
   end loop All_Ops;

   Ada.Text_IO.Close (File => Input);
end Day10_2;
