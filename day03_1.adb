with Ada.Text_IO;
with Ada.Strings.Fixed;

procedure Day03_1 is
   subtype Lower is Character range 'a' .. 'z';
   subtype Upper is Character range 'A' .. 'Z';
   subtype Item_ID is Character with Dynamic_Predicate => Item_ID in Lower | Upper;

   Input : Ada.Text_IO.File_Type;
   Sum   : Natural := 0;
begin -- Day03_1
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_03");

   All_Sacks : loop
      exit All_Sacks when Ada.Text_IO.End_Of_File (Input);

      One_Sack : declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);
         pragma Assert (Line'Last rem 2 = 0, Line);
      begin -- One_Sack
         Find_Dup : for C of Line (1 .. Line'Last / 2) loop
            if Ada.Strings.Fixed.Index (Line (Line'Last / 2 + 1 .. Line'Last), C & "") /= 0 then
               Sum := Sum + (if C in Lower then Character'Pos (C) - Character'Pos ('a') + 1
                             else Character'Pos (C) - Character'Pos ('A') + 27);

               exit Find_Dup;
            end if;
         end loop Find_Dup;
      end One_Sack;
   end loop All_Sacks;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Sum'Image);
end Day03_1;
