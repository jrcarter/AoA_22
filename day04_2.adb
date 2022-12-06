with Ada.Text_IO;
with Ada.Strings.Fixed;

procedure Day04_2 is
   Input : Ada.Text_IO.File_Type;
   Count : Natural := 0;
begin -- Day04_2
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_04");

   All_Sacks : loop
      exit All_Sacks when Ada.Text_IO.End_Of_File (Input);

      One_Sack : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);

         Dash1 : constant Positive := Ada.Strings.Fixed.Index (Line, "-");
         Comma : constant Positive := Ada.Strings.Fixed.Index (Line, ",");
         Dash2 : constant Positive := Ada.Strings.Fixed.Index (Line (Comma .. Line'Last), "-");

         Elf1_Lo : constant Natural := Integer'Value (Line (1 .. Dash1 - 1) );
         Elf1_Hi : constant Natural := Integer'Value (Line (Dash1 + 1 .. Comma - 1) );
         Elf2_Lo : constant Natural := Integer'Value (Line (Comma + 1 .. Dash2 - 1) );
         Elf2_Hi : constant Natural := Integer'Value (Line (Dash2 + 1 .. Line'Last) );

         subtype Elf1 is Integer range Elf1_Lo .. Elf1_Hi;
         subtype Elf2 is Integer range Elf2_Lo .. Elf2_Hi;
      begin -- One_Sack
         if Elf1_Lo in Elf2 or Elf1_Hi in Elf2 or Elf2_Lo in Elf1 or Elf2_Hi in Elf1 then -- These overlap somehow
            Count := Count + 1;
         end if;
      end One_Sack;
   end loop All_Sacks;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Count'Image);
end Day04_2;
