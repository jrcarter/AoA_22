with Ada.Text_IO;
with Ada.Strings.Fixed;

procedure Day03_2 is
   subtype Lower is Character range 'a' .. 'z';
   subtype Upper is Character range 'A' .. 'Z';
   subtype Item_ID is Character with Dynamic_Predicate => Item_ID in Lower | Upper;

   Input : Ada.Text_IO.File_Type;
   Sum   : Natural := 0;
begin -- Day03_2
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input_03");

   All_Groups : loop
      exit All_Groups when Ada.Text_IO.End_Of_File (Input);

      One_Group : declare
         Elf_1 : constant String := Ada.Text_IO.Get_Line (Input);
         pragma Assert (Elf_1'Last rem 2 = 0, Elf_1);
         Elf_2 : constant String := Ada.Text_IO.Get_Line (Input);
         pragma Assert (Elf_2'Last rem 2 = 0, Elf_2);
         Elf_3 : constant String := Ada.Text_IO.Get_Line (Input);
         pragma Assert (Elf_3'Last rem 2 = 0, Elf_3);
      begin -- One_Group
         Find_Common : for C of Elf_1 loop
            if Ada.Strings.Fixed.Index (Elf_2, C & "") /= 0 and Ada.Strings.Fixed.Index (Elf_3, C & "") /= 0 then
               Sum := Sum + (if C in Lower then Character'Pos (C) - Character'Pos ('a') + 1
                             else Character'Pos (C) - Character'Pos ('A') + 27);

               exit Find_Common;
            end if;
         end loop Find_Common;
      end One_Group;
   end loop All_Groups;

   Ada.Text_IO.Close (File => Input);
   Ada.Text_IO.Put_Line (Item => Sum'Image);
end Day03_2;
